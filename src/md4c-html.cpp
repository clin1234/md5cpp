/*
 * MD4C: Markdown parser for C
 * (http://github.com/mity/md4c)
 *
 * Copyright (c) 2016-2019 Martin Mitas
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
#include <string.h>

#include "entity.h"
#include "md4c-html.h"


#include <algorithm>
#include <cctype>
#include <limits>
#include <numeric>
#include <string>
#include <string_view>


#ifdef _WIN32
#define snprintf _snprintf
#endif

using HTML = struct MD_HTML_tag;
struct MD_HTML_tag {
  void (*process_output)(const MD_CHAR *, MD_SIZE, void *);
  void *userdata;
  unsigned flags;
  unsigned short image_nesting_level;
  char escape_map[256];
};

#define NEED_HTML_ESC_FLAG 0x1
#define NEED_URL_ESC_FLAG 0x2

static inline void render_verbatim(HTML &r, const MD_CHAR *text, MD_SIZE size) {
  r.process_output(text, size, r.userdata);
}

static inline void render_verbatim(HTML &r, const mdstringview text) {
  r.process_output(text.data(), text.size(), r.userdata);
}

/* Keep this as a macro. Most compiler should then be smart enough to replace
 * the strlen() call with a compile-time constexprant if the string is a C
 * literal. */
#define RENDER_VERBATIM(r, verbatim)                                           \
  render_verbatim((r), (verbatim), (MD_SIZE)(strlen(verbatim)))

static void render_html_escaped(HTML &r, mdstringview data) {
  MD_OFFSET beg = 0;
  MD_OFFSET off = 0;

/* Some characters need to be escaped in normal HTML text. */
#define NEED_HTML_ESC(ch)                                                      \
  (r.escape_map[(unsigned char)(ch)] & NEED_HTML_ESC_FLAG)

  while (1) {
    /* Optimization: Use some loop unrolling. */
    while (off + 3 < data.size() && !NEED_HTML_ESC(data[off + 0]) &&
           !NEED_HTML_ESC(data[off + 1]) && !NEED_HTML_ESC(data[off + 2]) &&
           !NEED_HTML_ESC(data[off + 3]))
      off += 4;
    while (off < data.size() && !NEED_HTML_ESC(data[off]))
      off++;

    if (off > beg)
      render_verbatim(r, data.substr(beg, off - beg));

    if (off < data.size()) {
      switch (data[off]) {
      case '&':
        RENDER_VERBATIM(r, "&amp;");
        break;
      case '<':
        RENDER_VERBATIM(r, "&lt;");
        break;
      case '>':
        RENDER_VERBATIM(r, "&gt;");
        break;
      case '"':
        RENDER_VERBATIM(r, "&quot;");
        break;
      }
      off++;
    } else {
      break;
    }
    beg = off;
  }
}

static void render_url_escaped(HTML &r, mdstringview data) {
  static constexpr MD_CHAR hex_chars[] = "0123456789ABCDEF";
  MD_OFFSET beg = 0;
  MD_OFFSET off = 0;

/* Some characters need to be escaped in URL attributes. */
#define NEED_URL_ESC(ch) (r.escape_map[(unsigned char)(ch)] & NEED_URL_ESC_FLAG)

  while (1) {
    while (off < data.size() && !NEED_URL_ESC(data[off]))
      off++;
    if (off > beg)
      render_verbatim(r, data.substr(beg, off - beg));

    if (off < data.size()) {
      char hex[3];

      switch (data[off]) {
      case '&':
        RENDER_VERBATIM(r, "&amp;");
        break;
      default:
        hex[0] = '%';
        hex[1] = hex_chars[(data[off] >> 4) & 0xf];
        hex[2] = hex_chars[(data[off] >> 0) & 0xf];
        render_verbatim(r, hex, 3);
        break;
      }
      off++;
    } else {
      break;
    }

    beg = off;
  }
}

static unsigned hex_val(char ch) {
  if ('0' <= ch && ch <= '9')
    return ch - '0';
  if ('A' <= ch && ch <= 'Z')
    return ch - 'A' + 10;
  else
    return ch - 'a' + 10;
}

using append_func = void (*)(HTML &, mdstringview);

static void render_utf8_codepoint(HTML &r, unsigned codepoint,
                                  append_func fn_append) {
  // Narrowing conversion warnings are a pain...
  static constexpr MD_CHAR utf8_replacement_char[]{'\xef', '\xbf', '\xbd'};

  unsigned char utf8[4];
  size_t n;

  if (codepoint <= 0x7f) {
    n = 1;
    utf8[0] = codepoint;
  } else if (codepoint <= 0x7ff) {
    n = 2;
    utf8[0] = 0xc0 | ((codepoint >> 6) & 0x1f);
    utf8[1] = 0x80 + ((codepoint >> 0) & 0x3f);
  } else if (codepoint <= 0xffff) {
    n = 3;
    utf8[0] = 0xe0 | ((codepoint >> 12) & 0xf);
    utf8[1] = 0x80 + ((codepoint >> 6) & 0x3f);
    utf8[2] = 0x80 + ((codepoint >> 0) & 0x3f);
  } else {
    n = 4;
    utf8[0] = 0xf0 | ((codepoint >> 18) & 0x7);
    utf8[1] = 0x80 + ((codepoint >> 12) & 0x3f);
    utf8[2] = 0x80 + ((codepoint >> 6) & 0x3f);
    utf8[3] = 0x80 + ((codepoint >> 0) & 0x3f);
  }

  if (0 < codepoint && codepoint <= 0x10ffff)
    // XXX: Might be a terrible idea...
    fn_append(r, mdstringview(reinterpret_cast<MD_CHAR *>(utf8), n));
  else
    fn_append(r, utf8_replacement_char);
}

/* Translate entity to its UTF-8 equivalent, or output the verbatim one
 * if such entity is unknown (or if the translation is disabled). */
static void render_entity(HTML &r, mdstringview text, append_func fn_append) {
  if (r.flags & MD_HTML_FLAG_VERBATIM_ENTITIES) {
    render_verbatim(r, text);
    return;
  }

  /* We assume UTF-8 output is what is desired. */
  if (text.size() > 3 && text[1] == '#') {
    unsigned codepoint = 0;

    if (text[2] == 'x' || text[2] == 'X')
      /* Hexadecimal entity (e.g. "&#x1234abcd;")). */
      for (const char c : text.substr(3))
        codepoint = 16 * codepoint + hex_val(c);
    else {
      /* Decimal entity (e.g. "&1234;") */
      for (const char c : text.substr(2))
        codepoint = 10 * codepoint + (c - '0');
    }

    render_utf8_codepoint(r, codepoint, fn_append);
    return;
  } else {
    /* Named entity (e.g. "&nbsp;"). */
    const auto entity{lookup(text)};
    if (entity) {
      render_utf8_codepoint(r, entity->codepoints[0], fn_append);
      if (entity->codepoints[1])
        render_utf8_codepoint(r, entity->codepoints[1], fn_append);
      return;
    }
  }

  fn_append(r, text);
}

static void render_attribute(HTML &r, const Attribute *attr,
                             append_func fn_append) {
  int i;

  for (i = 0; attr->substr_offsets[i] < attr->size; i++) {
    TextType type = attr->substr_types[i];
    MD_OFFSET off = attr->substr_offsets[i];
    MD_SIZE size = attr->substr_offsets[i + 1] - off;
    const MD_CHAR *text = attr->text + off;

    switch (type) {
    case TextType::null_char:
      render_utf8_codepoint(r, 0x0000, render_verbatim);
      break;
    case TextType::entity:
      render_entity(r, text, fn_append);
      break;
    default:
      fn_append(r, mdstringview(text, size));
      break;
    }
  }
}

static void render_open_ol_block(HTML &r, const ol_Detail &det) {
  char buf[64];

  if (det.start == 1) {
    RENDER_VERBATIM(r, "<ol>\n");
    return;
  }

  snprintf(buf, sizeof(buf), "<ol start=\"%u\">\n", det.start);
  RENDER_VERBATIM(r, buf);
}

static void render_open_li_block(HTML &r, const li_Detail *det) {
  if (det->is_task) {
    RENDER_VERBATIM(
        r,
        "<li class=\"task-list-item\">"
        "<input type=\"checkbox\" class=\"task-list-item-checkbox\" disabled");
    if (det->task_mark == 'x' || det->task_mark == 'X')
      RENDER_VERBATIM(r, " checked");
    RENDER_VERBATIM(r, ">");
  } else {
    RENDER_VERBATIM(r, "<li>");
  }
}

static void render_open_code_block(HTML &r, const code_Detail *det) {
  RENDER_VERBATIM(r, "<pre><code");

  /* If known, output the HTML 5 attribute class="language-LANGNAME". */
  if (det->lang.text != NULL) {
    RENDER_VERBATIM(r, " class=\"language-");
    render_attribute(r, &det->lang, render_html_escaped);
    RENDER_VERBATIM(r, "\"");
  }

  RENDER_VERBATIM(r, ">");
}

static void render_open_td_block(HTML &r, const MD_CHAR *cell_type,
                                 const td_Detail *det) {
  RENDER_VERBATIM(r, "<");
  RENDER_VERBATIM(r, cell_type);

  switch (det->align) {
  case Align::left:
    RENDER_VERBATIM(r, " align=\"left\">");
    break;
  case Align::center:
    RENDER_VERBATIM(r, " align=\"center\">");
    break;
  case Align::right:
    RENDER_VERBATIM(r, " align=\"right\">");
    break;
  default:
    RENDER_VERBATIM(r, ">");
    break;
  }
}

static void render_open_a_span(HTML &r, const a_Detail *det) {
  RENDER_VERBATIM(r, "<a href=\"");
  render_attribute(r, &det->href, render_url_escaped);

  if (det->title.text != NULL) {
    RENDER_VERBATIM(r, "\" title=\"");
    render_attribute(r, &det->title, render_html_escaped);
  }

  RENDER_VERBATIM(r, "\">");
}

static void render_open_img_span(HTML &r, const img_Detail *det) {
  RENDER_VERBATIM(r, "<img src=\"");
  render_attribute(r, &det->src, render_url_escaped);

  RENDER_VERBATIM(r, "\" alt=\"");

  r.image_nesting_level++;
}

static void render_close_img_span(HTML &r, const img_Detail *det) {
  if (det->title.text != NULL) {
    RENDER_VERBATIM(r, "\" title=\"");
    render_attribute(r, &det->title, render_html_escaped);
  }

  RENDER_VERBATIM(r, (r.flags & MD_HTML_FLAG_XHTML) ? "\" />" : "\">");

  r.image_nesting_level--;
}

static void render_open_wikilink_span(HTML &r, const Wikilink_Detail *det) {
  RENDER_VERBATIM(r, "<x-wikilink data-target=\"");
  render_attribute(r, &det->target, render_html_escaped);

  RENDER_VERBATIM(r, "\">");
}

/**************************************
 ***  HTML renderer implementation  ***
 **************************************/

static int enter_block_callback(BlockType type, void *detail, void *userdata) {
  static constexpr const char *head[6]{"<h1>", "<h2>", "<h3>",
                                       "<h4>", "<h5>", "<h6>"};
  HTML &r{*(HTML *)userdata};

  switch (type) {
  case BlockType::body: /* noop */
    break;
  case BlockType::block_quote:
    RENDER_VERBATIM(r, "<blockquote>\n");
    break;
  case BlockType::ul:
    RENDER_VERBATIM(r, "<ul>\n");
    break;
  case BlockType::ol:
    render_open_ol_block(r, *(const ol_Detail *)detail);
    break;
  case BlockType::li:
    render_open_li_block(r, (const li_Detail *)detail);
    break;
  case BlockType::hr:
    RENDER_VERBATIM(r, (r.flags & MD_HTML_FLAG_XHTML) ? "<hr />\n" : "<hr>\n");
    break;
  case BlockType::heading:
    RENDER_VERBATIM(r, head[((h_Detail *)detail)->level - 1]);
    break;
  case BlockType::code:
    render_open_code_block(r, (const code_Detail *)detail);
    break;
  case BlockType::raw_html: /* noop */
    break;
  case BlockType::paragraph:
    RENDER_VERBATIM(r, "<p>");
    break;
  case BlockType::table:
    RENDER_VERBATIM(r, "<table>\n");
    break;
  case BlockType::table_head:
    RENDER_VERBATIM(r, "<thead>\n");
    break;
  case BlockType::table_body:
    RENDER_VERBATIM(r, "<tbody>\n");
    break;
  case BlockType::tr:
    RENDER_VERBATIM(r, "<tr>\n");
    break;
  case BlockType::th:
    render_open_td_block(r, "th", (td_Detail *)detail);
    break;
  case BlockType::td:
    render_open_td_block(r, "td", (td_Detail *)detail);
    break;
  }

  return 0;
}

static int leave_block_callback(BlockType type, void *detail, void *userdata) {
  static constexpr const MD_CHAR *head[6] = {"</h1>\n", "</h2>\n", "</h3>\n",
                                             "</h4>\n", "</h5>\n", "</h6>\n"};
  HTML &r{*(HTML *)userdata};

  switch (type) {
  case BlockType::body: /*noop*/
    break;
  case BlockType::block_quote:
    RENDER_VERBATIM(r, "</blockquote>\n");
    break;
  case BlockType::ul:
    RENDER_VERBATIM(r, "</ul>\n");
    break;
  case BlockType::ol:
    RENDER_VERBATIM(r, "</ol>\n");
    break;
  case BlockType::li:
    RENDER_VERBATIM(r, "</li>\n");
    break;
  case BlockType::hr: /*noop*/
    break;
  case BlockType::heading:
    RENDER_VERBATIM(r, head[((h_Detail *)detail)->level - 1]);
    break;
  case BlockType::code:
    RENDER_VERBATIM(r, "</code></pre>\n");
    break;
  case BlockType::raw_html: /* noop */
    break;
  case BlockType::paragraph:
    RENDER_VERBATIM(r, "</p>\n");
    break;
  case BlockType::table:
    RENDER_VERBATIM(r, "</table>\n");
    break;
  case BlockType::table_head:
    RENDER_VERBATIM(r, "</thead>\n");
    break;
  case BlockType::table_body:
    RENDER_VERBATIM(r, "</tbody>\n");
    break;
  case BlockType::tr:
    RENDER_VERBATIM(r, "</tr>\n");
    break;
  case BlockType::th:
    RENDER_VERBATIM(r, "</th>\n");
    break;
  case BlockType::td:
    RENDER_VERBATIM(r, "</td>\n");
    break;
  }

  return 0;
}

static int enter_span_callback(SpanType type, void *detail, void *userdata) {
  HTML &r = *(HTML *)userdata;

  if (r.image_nesting_level > 0) {
    /* We are inside a Markdown image label. Markdown allows to use any
     * emphasis and other rich contents in that context similarly as in
     * any link label.
     *
     * However, unlike in the case of links (where that contents becomes
     * contents of the <a>...</a> tag), in the case of images the contents
     * is supposed to fall into the attribute alt: <img alt="...">.
     *
     * In that context we naturally cannot output nested HTML tags. So lets
     * suppress them and only output the plain text (i.e. what falls into
     * text() callback).
     *
     * This make-it-a-plain-text approach is the recommended practice by
     * CommonMark specification (for HTML output).
     */
    return 0;
  }

  switch (type) {
  case SpanType::em:
    RENDER_VERBATIM(r, "<em>");
    break;
  case SpanType::string:
    RENDER_VERBATIM(r, "<strong>");
    break;
  case SpanType::u:
    RENDER_VERBATIM(r, "<u>");
    break;
  case SpanType::a:
    render_open_a_span(r, (a_Detail *)detail);
    break;
  case SpanType::img:
    render_open_img_span(r, (img_Detail *)detail);
    break;
  case SpanType::code:
    RENDER_VERBATIM(r, "<code>");
    break;
  case SpanType::del:
    RENDER_VERBATIM(r, "<del>");
    break;
  case SpanType::latex_inline:
    RENDER_VERBATIM(r, "<x-equation>");
    break;
  case SpanType::latex_display:
    RENDER_VERBATIM(r, "<x-equation type=\"display\">");
    break;
  case SpanType::wiki_link:
    render_open_wikilink_span(r, (Wikilink_Detail *)detail);
    break;
  }

  return 0;
}

static int leave_span_callback(SpanType type, void *detail, void *userdata) {
  HTML &r = *(HTML *)userdata;

  if (r.image_nesting_level > 0) {
    /* Ditto as in enter_span_callback(), except we have to allow the
     * end of the <img> tag. */
    if (r.image_nesting_level == 1 && type == SpanType::img)
      render_close_img_span(r, (img_Detail *)detail);
    return 0;
  }

  switch (type) {
  case SpanType::em:
    RENDER_VERBATIM(r, "</em>");
    break;
  case SpanType::string:
    RENDER_VERBATIM(r, "</strong>");
    break;
  case SpanType::u:
    RENDER_VERBATIM(r, "</u>");
    break;
  case SpanType::a:
    RENDER_VERBATIM(r, "</a>");
    break;
  case SpanType::img: /*noop, handled above*/
    break;
  case SpanType::code:
    RENDER_VERBATIM(r, "</code>");
    break;
  case SpanType::del:
    RENDER_VERBATIM(r, "</del>");
    break;
  case SpanType::latex_inline: /*fall through*/
  case SpanType::latex_display:
    RENDER_VERBATIM(r, "</x-equation>");
    break;
  case SpanType::wiki_link:
    RENDER_VERBATIM(r, "</x-wikilink>");
    break;
  }

  return 0;
}

static int text_callback(TextType type, mdstringview text, void *userdata) {
  HTML &r = *(HTML *)userdata;

  switch (type) {
  case TextType::null_char:
    render_utf8_codepoint(r, 0x0000, render_verbatim);
    break;
  case TextType::br:
    RENDER_VERBATIM(r, (r.image_nesting_level == 0 ? "<br />\n" : " "));
    break;
  case TextType::soft_br:
    RENDER_VERBATIM(r, (r.image_nesting_level == 0 ? "\n" : " "));
    break;
  case TextType::raw_html:
    render_verbatim(r, text);
    break;
  case TextType::entity:
    render_entity(r, text, render_html_escaped);
    break;
  default:
    render_html_escaped(r, text);
    break;
  }

  return 0;
}

static void debug_log_callback(const char *msg, void *userdata) {
  HTML &r = *(HTML *)userdata;
  if (r.flags & MD_HTML_FLAG_DEBUG)
    fprintf(stderr, "MD4C: %s\n", msg);
}

int md_html(const MD_CHAR *input, MD_SIZE input_size,
            void (*process_output)(const MD_CHAR *, MD_SIZE, void *),
            void *userdata, unsigned parser_flags, unsigned renderer_flags) {
  HTML &&render{process_output, userdata, renderer_flags, 0, {0}};

  MD_PARSER &&parser{0,
                     parser_flags,
                     enter_block_callback,
                     leave_block_callback,
                     enter_span_callback,
                     leave_span_callback,
                     text_callback,
                     debug_log_callback,
                     NULL};

  /* Build map of characters which need escaping. */
  for (unsigned char i = 0; i <= std::numeric_limits<unsigned char>::max();
       i++) {
    auto ch = i;

    if (strchr("\"&<>", ch) != NULL)
      render.escape_map[i] |= NEED_HTML_ESC_FLAG;

    if (!std::isalnum(ch) && strchr("~-_.+!*(),%#@?=;:/,+$", ch) == NULL)
      render.escape_map[i] |= NEED_URL_ESC_FLAG;
  }

  /* Consider skipping UTF-8 byte order mark (BOM). */
  if (renderer_flags & MD_HTML_FLAG_SKIP_UTF8_BOM && sizeof(MD_CHAR) == 1) {
    static constexpr char8_t bom[3] = {0xef, 0xbb, 0xbf};
    if (input_size >= sizeof(bom) && memcmp(input, bom, sizeof(bom)) == 0) {
      input += sizeof(bom);
      input_size -= sizeof(bom);
    }
  }

  return md_parse(input, input_size, &parser, (void *)&render);
}
