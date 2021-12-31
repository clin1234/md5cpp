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

#include <stdio.h>

#include "cmdline.h"
#include "md4c-html.h"
#include "md4c.h"

#include <array>
#include <chrono>
#include <fstream>
#include <iostream>
#include <sstream>

#ifdef __cpp_lib_format
#include <format>
#else
#include <iomanip>
#endif

/* Global options. */
static unsigned parser_flags = 0;
#ifndef MD4C_USE_ASCII
static unsigned renderer_flags =
    MD_HTML_FLAG_DEBUG | MD_HTML_FLAG_SKIP_UTF8_BOM;
#else
static unsigned renderer_flags = MD_HTML_FLAG_DEBUG;
#endif
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

static int process_file(std::ifstream &in, std::ofstream &out) {
  int ret = -1;

  mdstring in_buf{}, out_buf{};

  while (in.is_open()) {
    const auto size{in.tellg()};
    in_buf.reserve(size);
    in.seekg(0);
    in.read(&in_buf[0], size);
  }

  /* Input size is good estimation of output size. Add some more reserve to
   * deal with the HTML header/footer and tags. */
  out_buf.resize(in_buf.size() + in_buf.size() / 8 + 64);

  auto t0 = std::chrono::steady_clock::now();

  /* Parse the document. This shall call our callbacks provided via the
   * md_renderer_t structure. */
  ret = to_html(in_buf, process_output, &out_buf,
                parser_flags, renderer_flags);

  auto t1 = std::chrono::steady_clock::now();
  if (ret != 0) {
    fprintf(stderr, "Parsing failed.\n");
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
    auto elapsed = t1 - t0;
    std::cerr <<
#ifdef __cpp_lib_format
        std::format("Time spend on parsing: {:f.3}", elapsed.count());
#else
        "Time spent on parsing: " << std::setprecision(3) << elapsed.count()
              << " s.\n";
#endif
  }

  /* Success if we have reached here. */
  ret = 0;

  return ret;
}

static constexpr const std::array cmdline_options{
    Opt{'o', "output", 'o', CMDLINE_OPTFLAG_REQUIREDARG},
    Opt{'f', "full-html", 'f', 0},
    Opt{'x', "xhtml", 'x', 0},
    Opt{'s', "stat", 's', 0},
    Opt{'h', "help", 'h', 0},
    Opt{'v', "version", 'v', 0},

    Opt{0, "commonmark", 'c', 0},
    Opt{0, "github", 'g', 0},
    Opt{0, "gitlab", 'l', 0},

    Opt{0, "fcollapse-whitespace", 'W', 0},
    Opt{0, "flatex-math", 'L', 0},
    Opt{0, "fpermissive-atx-headers", 'A', 0},
    Opt{0, "fpermissive-autolinks", 'V', 0},
    Opt{0, "fpermissive-email-autolinks", '@', 0},
    Opt{0, "fpermissive-url-autolinks", 'U', 0},
    Opt{0, "fpermissive-www-autolinks", '.', 0},
    Opt{0, "fstrikethrough", 'S', 0},
    Opt{0, "ftables", 'T', 0},
    Opt{0, "ftasklists", 'X', 0},
    Opt{0, "funderline", '_', 0},
    Opt{0, "fverbatim-entities", 'E', 0},
    Opt{0, "fwiki-links", 'K', 0},
    Opt{0, "ftoc", 't', 0},
    Opt{0, "finline-diff", 'i', 0},
    Opt{0, "fcolor", 'C', 0},
    Opt{0, "fabbreviations", 'a', 0},

    Opt{0, "fno-html-blocks", 'F', 0},
    Opt{0, "fno-html-spans", 'G', 0},
    Opt{0, "fno-html", 'H', 0},
    Opt{0, "fno-indented-code", 'I', 0},

    Opt{0, NULL, 0, 0}};

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
}

static void version(void) { std::cout << MD_VERSION << '\n'; }

static const char *input_path = NULL;
static const char *output_path = NULL;

static int cmdline_callback(int opt, char const *value,
                            [[maybe_unused]] void *data) {
  switch (opt) {
  case 0:
    if (input_path) {
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
    want_fullhtml = 1;
    break;
  case 'x':
    want_xhtml = 1;
    renderer_flags |= MD_HTML_FLAG_XHTML;
    break;
  case 's':
    want_stat = 1;
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

int main(int argc, char **argv) {
  int ret = 0;

  if (cmdline_read(cmdline_options, argc, argv, cmdline_callback, NULL) != 0) {
    usage();
    exit(1);
  }

  std::ifstream input_file(input_path);
  std::ofstream output_file(output_path, std::ios_base::trunc);
  ret = process_file(input_file, output_file);

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
