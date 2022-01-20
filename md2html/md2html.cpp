/* md5cpp: fast Markdown parser in C++20.
Copyright 2021 Charlie Lin
This software is derived from md4c, and is licensed under the same terms as
md4c, reproduced below:
*/
/*
 * MD4C: Markdown parser for C
 * (http://github.com/mity/md4c)
 *
 * Copyright (c) 2016-2020 Martin Mitas
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "include/cxxopts.hpp"
#include "md4c-html.h"
#include "md4c.h"

#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <cassert>
#include <string>
#include <string_view>
#include <stdexcept>
#include <variant>

#ifdef __cpp_lib_format
#include <format>
#else

#include <iomanip>

#endif

using mdstring = std::basic_string<MD_CHAR>;
using mdstringview = std::basic_string_view<MD_CHAR>;

/* Global options. */
static std::unordered_set<Extensions> parser_flags{};
static std::unordered_set<RenderFlag> rendering_flags{RenderFlag::Debug,
#ifndef MD4C_USE_ASCII
                                                      RenderFlag::Skip_UTF8_BOM
#endif
};
static bool want_fullhtml;
static bool want_xhtml;
static bool want_stat;

/*********************************
 ***  Simple grow-able buffer  ***
 *********************************/

/* We render to a memory buffer instead of directly outputting the rendered
 * documents, as this allows using this utility for evaluating performance
 * of MD4C (--stat option). This allows us to measure just time of the parser,
 * without the I/O.
 */
std::string bof{};

/*
struct membuffer {
  char *data;
  size_t asize;
  size_t size;
};

static void
membuf_init(struct membuffer* buf, MD_SIZE new_asize)
{
    buf->size = 0;
    buf->asize = new_asize;
    buf->data = malloc(buf->asize);
    if(buf->data == NULL) {
        fprintf(stderr, "membuf_init: malloc() failed.\n");
        exit(1);
    }
}

static void
membuf_fini(struct membuffer* buf)
{
    if(buf->data)
        free(buf->data);
}

static void
membuf_grow(struct membuffer* buf, size_t new_asize)
{
    buf->data = realloc(buf->data, new_asize);
    if(buf->data == NULL) {
        fprintf(stderr, "membuf_grow: realloc() failed.\n");
        exit(1);
    }
    buf->asize = new_asize;
}

static void
membuf_append(struct membuffer* buf, const char* data, MD_SIZE size)
{
    if(buf->asize < buf->size + size)
        membuf_grow(buf, buf->size + buf->size / 2 + size);
    memcpy(buf->data + buf->size, data, size);
    buf->size += size;
}
*/

/**********************
 ***  Main program  ***
 **********************/

static void process_output(mdstringview text, void *userdata) {
    bof += text;
    // TODO: band-aid to compile
    // membuf_append((struct membuffer *)userdata, text, size);
}

static int process_file(std::istream &in, std::ostream &out) {
    int ret = -1;

    mdstring in_buf{}, out_buf{};

    const auto size{in.tellg()};
    try {
        assert(size != 0);
        in_buf.reserve(size);
    } catch (const std::length_error &e) {
        std::clog << "Caught this exception: " << e.what() << '\n'
                  << "Got size " << size;
        throw;
    }
    in.seekg(0);
    in.read(&in_buf[0], size);

    /* Input size is good estimation of output size. Add some more reserve to
     * deal with the HTML header/footer and tags. */
    out_buf.resize(in_buf.size() + in_buf.size() / 8 + 64);

    auto t0 = std::chrono::steady_clock::now();

    /* Parse the document. This shall call our callbacks provided via the
     * md_renderer_t structure. */
    ret = to_html(in_buf, process_output, &out_buf, parser_flags, rendering_flags);

    auto t1 = std::chrono::steady_clock::now();
    if (ret != 0) {
        std::cerr << "Parsing failed.\n";
        return ret;
    }

    /* Write down the document in the HTML format. */
    if (want_fullhtml) {
        if (want_xhtml) {
            out <<
                R"(<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns="http://www.w3.org/1999/xhtml">
)";
        } else
            out << R"(<!DOCTYPE html>
<html>
)";
        out << R"(<head>
<title></title>
<meta name="generator" content="md2html" />
</head>
<body>
)";
    }

    if (want_fullhtml)
        out << R"(
</body>
</html>
)";

    if (want_stat) {
        using std::chrono::duration_cast, std::chrono::seconds;
        const auto elapsed{duration_cast<seconds>(t1 - t0)};
        std::cerr <<
                  #ifdef __cpp_lib_format
                  std::format("Time spend on parsing: {:f.3}", elapsed.count());
                  #else
                  "Time spent on parsing: " << std::setprecision(2) << elapsed.count()
                  << " s.\n";
#endif
    }

    /* Success if we have reached here. */
    ret = 0;

    return ret;
}


struct Opt {
    char short_opt;
    std::string long_opt, description;
    std::variant<std::monostate, RenderFlag, Extensions> flag;
    std::string optional_arg{};
};

struct Option_Group {
    std::string name;
    std::vector<Opt> options;
};

std::string output_file_name;

using enum Extensions; using enum RenderFlag;
static const std::array cmdline_options{
        Option_Group{"General", {
                Opt{'o', "output", "Output file (default is stdout)", {}, output_file_name},
                Opt{'f', "full-html", "Generate full HTML, including header", {}},
                Opt{'s', "stat", "Measure time of input parsing", {}},
                Opt{'h', "help", "Print this help message", {}},
                Opt{'v', "version", "Display version", {}},
        }},
        Option_Group{"Dialect", {
                Opt{'c', "commonmark", "CommonMark (default)", {}},
                Opt{'g', "github", "Github-flavored Markdown", {}},
                Opt{'l', "gitlab", "Gitlab-flavored Markdown", {}},
        }},
        Option_Group{"Extensions", {
                Opt{'W', "fcollapse-whitespace", "Collapse non-trivial whitespace", Collapse_Whitespace,},
                Opt{'L', "flatex-math", "Enable LaTeX-style math spans", LaTeX_Math,},
                Opt{'A', "fpermissive-atx-headers", "Allow ATX headers without delimiting space",
                    No_Space_Needed_for_ATXHeaders,},
                Opt{'V', "fpermissive-autolinks", "Same as enabling the next three options", Permissive_Autolink},
                Opt{'@', "fpermissive-email-autolinks",
                    "Allow email autolinks without angle brackets and 'mailto:' scheme", Permissive_Email_Autolink},
                Opt{'U', "fpermissive-url-autolinks", "Allow URL autolinks without angle brackets",},
                Opt{'.', "fpermissive-www-autolinks", "Allow WWW autolinks without a scheme (e.g. http[s]://)",
                    Permissive_WWW_Autolink},
                Opt{'S', "fstrikethrough", "Enable strike-through spans", Strikethrough},
                Opt{'T', "ftables", "Enable tables", Tables},
                Opt{'X', "ftasklists", "Enable task lists", Tasklist},
                Opt{'_', "funderline", "Enable underline spans", Underline},
                Opt{'K', "fwiki-links", "Enable wiki links", Wikilinks},
                Opt{'t', "ftoc", "Enable table of contents", Table_of_Contents},
                Opt{'i', "finline-diff", "Enable Git-style inline diffs", Inline_Diff},
                Opt{'c', "fcolor", "Render RGB or HCL values", Color},
                Opt{'a', "fabbreviations", "Enable abbreviations using Markdown Extra's syntax", Abbreviation},
        }},
        Option_Group{"Markdown suppression", {
                Opt{'F', "fno-html-blocks", "Disable raw HTML blocks", No_Raw_HTML_Block},
                Opt{'G', "fno-html-spans", "Disable raw HTML spans", No_Raw_HTML_Inline},
                Opt{'H', "fno-html", "Disable HTML features listed above",},
                Opt{'I', "fno-indented-code", "Disable indented code blocks", No_Indented_Codeblock},
        }},
        Option_Group{"Rendering", {
                Opt{'E', "fverbatim-entities", "Do not translate entities", Verbatim_Entities},
                Opt{'x', "xhtml", "Generate XHTML instead of HTML", XHTML},
        }},
};

auto parse_opts(int a, char **v) {
    cxxopts::Options options(
            "md2html",
            "Convert input FILE (or standard input) in Markdown format to HTML.");
    options.positional_help("[ input file ]").show_positional_help();
    std::vector<std::string> groups{};
    for (const auto &group: cmdline_options) {
        groups.emplace_back(group.name);
        auto &&tmp = options.add_options(group.name);
        for (auto[short_o, long_o, desc, opt, optional_arg]: group.options) {
            if (std::get_if<std::monostate>(&opt) != nullptr) tmp(long_o.insert(0, ",").insert(0, short_o, 1), desc);
            //else tmp(long_o.insert(0, ",").insert(0, short_o, 1), desc, cxxopts::value<decltype(opt)>());
        }
    }

    cxxopts::ParseResult res;
    try {
        res = options.parse(a, v);
    } catch (const cxxopts::option_not_exists_exception &e) {
        std::cerr << e.what() << '\n';
        std::cout << options.help(groups);
        // std::exit(1);
    }
    if (res.count("h")) {
        std::cout << options.help(groups);
        std::exit(0);
    }
    return res;
}

/*
static void usage(void) {
  printf(
      R"(Usage: md2html [OPTION]... [FILE]
Convert input FILE (or standard input) in Markdown format to HTML.

General options:
  -o  --output=FILE    Output file (default is standard output)
  -f, --full-html      Generate full HTML document, including header
  -x, --xhtml          Generate XHTML instead of HTML
  -s, --stat           Measure time of input parsing
  -h, --help           Display this help and exit
  -v, --version        Display version and exit

Markdown dialect options:
(note these are equivalent to some combinations of the flags below)
      --commonmark     CommonMark (this is default)
      --github         Github Flavored Markdown
      --gitlab         Gitlab Flavored Markdown

Markdown extension options:
      --fcollapse-whitespace
                       Collapse non-trivial whitespace
      --flatex-math    Enable LaTeX style mathematics spans
      --fpermissive-atx-headers
                       Allow ATX headers without delimiting space
      --fpermissive-url-autolinks
                       Allow URL autolinks without '<', '>'
      --fpermissive-www-autolinks
                       Allow WWW autolinks without any scheme (e.g. 'www.example.com')
      --fpermissive-email-autolinks  
                       Allow e-mail autolinks without '<', '>' and 'mailto:'
      --fpermissive-autolinks
                       Same as --fpermissive-url-autolinks --fpermissive-www-autolinks
                       --fpermissive-email-autolinks
      --fstrikethrough Enable strike-through spans
      --ftables        Enable tables
      --ftasklists     Enable task lists
      --funderline     Enable underline spans
      --fwiki-links    Enable wiki links
      --ftoc           Enable table of contents
      --finline-diff   Enable Git-style inline diffs
      --fcolor         Render RGB or HCL values
      --fabbreviations Enable abbreviations using Markdown Extra's syntax 

Markdown suppression options:
      --fno-html-blocks
                       Disable raw HTML blocks
      --fno-html-spans
                       Disable raw HTML spans
      --fno-html       Same as --fno-html-blocks --fno-html-spans
      --fno-indented-code
                       Disable indented code blocks

HTML generator options:
      --fverbatim-entities
                       Do not translate entities

)");
}*/

static void apply_opts(const cxxopts::ParseResult &re) {
    for (const auto &opt_group: cmdline_options) {
        if (opt_group.name == "General")
            for (const auto &opt: opt_group.options)
                switch (opt.short_opt) {
                    case 'v':
                        std::cout << MD_VERSION << '\n';
                        std::exit(0);
                    case 'f':
                        want_fullhtml = true;
                        break;
                    case 's':
                        want_stat = true;
                        break;
                }
        else if (opt_group.name == "Extensions" or opt_group.name == "Markdown suppression")
            for (const auto &opt: opt_group.options) {
                if (re.count(opt.long_opt))
                    parser_flags.insert(std::get<Extensions>(opt.flag));
            }
        else if (opt_group.name == "Rendering")
            for (const auto &opt: opt_group.options)
                if (re.count(opt.long_opt))
                    rendering_flags.insert(std::get<RenderFlag>(opt.flag));
    }
}

using namespace std::filesystem;
static path input_path, output_path;

/*
static int cmdline_callback(int opt, char const *value,
                            [[maybe_unused]] void *data) {
    switch (opt) {
        case 0:
            if (!input_path.empty()) {
                fprintf(stderr,
                        "Too many arguments. Only one input file can be specified.\n");
                fprintf(stderr, "Use --help for more info.\n");
                exit(1);
            }
            input_path = value;
            break;

        case 'o':
            output_path = value;
            break;
        case 'f':
            want_fullhtml = true;
            break;
        case 'x':
            want_xhtml = true;
            renderer_flags |= MD_HTML_FLAG_XHTML;
            break;
        case 's':
            want_stat = true;
            break;
        case 'h':
            usage();
            exit(0);
            break;
        case 'v':
            version();
            exit(0);
            break;

        case 'c':
            parser_flags = MD_DIALECT_COMMONMARK;
            break;
        case 'g':
            parser_flags = MD_DIALECT_GITHUB;
            break;
        case 'l':
            parser_flags |= MD_DIALECT_GITLAB;
            break;

        case 'E':
            renderer_flags |= MD_HTML_FLAG_VERBATIM_ENTITIES;
            break;
        case 'A':
            parser_flags |= MD_FLAG_PERMISSIVEATXHEADERS;
            break;
        case 'I':
            parser_flags |= MD_FLAG_NOINDENTEDCODEBLOCKS;
            break;
        case 'F':
            parser_flags |= MD_FLAG_NOHTMLBLOCKS;
            break;
        case 'G':
            parser_flags |= MD_FLAG_NOHTMLSPANS;
            break;
        case 'H':
            parser_flags |= MD_FLAG_NOHTML;
            break;
        case 'W':
            parser_flags |= MD_FLAG_COLLAPSEWHITESPACE;
            break;
        case 'U':
            parser_flags |= MD_FLAG_PERMISSIVEURLAUTOLINKS;
            break;
        case '.':
            parser_flags |= MD_FLAG_PERMISSIVEWWWAUTOLINKS;
            break;
        case '@':
            parser_flags |= MD_FLAG_PERMISSIVEEMAILAUTOLINKS;
            break;
        case 'V':
            parser_flags |= MD_FLAG_PERMISSIVEAUTOLINKS;
            break;
        case 'T':
            parser_flags |= MD_FLAG_TABLES;
            break;
        case 'S':
            parser_flags |= MD_FLAG_STRIKETHROUGH;
            break;
        case 'L':
            parser_flags |= MD_FLAG_LATEXMATHSPANS;
            break;
        case 'K':
            parser_flags |= MD_FLAG_WIKILINKS;
            break;
        case 'X':
            parser_flags |= MD_FLAG_TASKLISTS;
            break;
        case '_':
            parser_flags |= MD_FLAG_UNDERLINE;
            break;
        case 'i':
            parser_flags |= MD_FLAG_INLINE_DIFF;
            break;
        case 'C':
            parser_flags |= MD_FLAG_COLOR;
            break;
        case 'a':
            parser_flags |= MD_FLAG_ABBREVIATIONS;
            break;
        case 't':
            parser_flags |= MD_FLAG_TOC;
            break;

        default:
            fprintf(stderr, "Illegal option: %s\n", value);
            fprintf(stderr, "Use --help for more info.\n");
            exit(1);
            break;
    }

    return 0;
}
 */

int main(int argc, char **argv) {
    int ret = 0;
    auto p{parse_opts(argc, argv)};
    apply_opts(p);
    std::ifstream input_file(input_path);
    std::ofstream output_file(output_path, std::ios_base::trunc);

    std::istream &in{input_path.compare("-") == 0 ? std::cin : input_file};
    std::ostream &out{input_path.compare("-") == 0 ? std::cout : output_file};
    ret = process_file(in, out);

    /*
      if (input_path != NULL && strcmp(input_path, "-") != 0) {
        in = fopen(input_path, "rb");
        if (in == NULL) {
          fprintf(stderr, "Cannot open %s.\n", input_path);
          exit(1);
        }
      }
      if (output_path != NULL && strcmp(output_path, "-") != 0) {
        out = fopen(output_path, "wt");
        if (out == NULL) {
          fprintf(stderr, "Cannot open %s.\n", output_path);
          exit(1);
        }
      }

      ret = process_file(in, out);
      if (in != stdin)
        fclose(in);
      if (out != stdout)
        fclose(out);
        */

    return ret;
}
