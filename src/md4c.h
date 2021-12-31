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

#ifndef MD4C_H
#define MD4C_H

#ifdef __cplusplus
#include <string>
#include <vector>
#include <string_view>
#endif

#if defined MD4C_USE_UTF16
/* Magic to support UTF-16. Note that in order to use it, you have to define
 * the macro MD4C_USE_UTF16 both when building MD4C as well as when
 * including this header in your code. */
#ifdef _WIN32
#include <windows.h>
typedef WCHAR MD_CHAR;
#else
#error MD4C_USE_UTF16 is only supported on Windows.
#endif
#else
typedef char MD_CHAR;
#endif

#ifdef __cplusplus
using mdstring = std::basic_string<MD_CHAR>;
using mdstringview = std::basic_string_view<MD_CHAR>;
#endif

typedef unsigned MD_SIZE;
typedef size_t MD_OFFSET;

/* Block represents a part of document hierarchy structure like a paragraph
 * or list item.
 */
enum class MD_BLOCKTYPE {
  /* <body>...</body> */
  body,

  /* <blockquote>...</blockquote> */
  block_quote,

  /* <ul>...</ul>
   * Detail: Structure MD_BLOCK_UL_DETAIL. */
  ul,

  /* <ol>...</ol>
   * Detail: Structure MD_BLOCK_OL_DETAIL. */
  ol,

  /* <li>...</li>
   * Detail: Structure MD_BLOCK_LI_DETAIL. */
  li,

  /* <hr> */
  hr,

  /* <h1>...</h1> (for levels up to 6)
   * Detail: Structure MD_BLOCK_H_DETAIL. */
  heading,

  /* <pre><code>...</code></pre>
   * Note the text lines within code blocks are terminated with '\n'
   * instead of explicit MD_TEXT_BR. */
  code,

  /* Raw HTML block. This itself does not correspond to any particular HTML
   * tag. The contents of it _is_ raw HTML source intended to be put
   * in verbatim form to the HTML output. */
  raw_html,

  /* <p>...</p> */
  paragraph,

  /* <table>...</table> and its contents.
   * Detail: Structure MD_BLOCK_TABLE_DETAIL (for MD_BLOCK_TABLE),
   *         structure MD_BLOCK_TD_DETAIL (for MD_BLOCK_TH and MD_BLOCK_TD)
   * Note all of these are used only if extension MD_FLAG_TABLES is enabled. */
  table,
  table_head,
  table_body,
  tr,
  th,
  td
};
using BlockType = MD_BLOCKTYPE;

/* Span represents an in-line piece of a document which should be rendered with
 * the same font, color and other attributes. A sequence of spans forms a block
 * like paragraph or list item. */
enum class MD_SPANTYPE {
  /* <em>...</em> */
  em,

  /* <strong>...</strong> */
  string,

  /* <a href="xxx">...</a>
   * Detail: Structure MD_SPAN_A_DETAIL. */
  a,

  /* <img src="xxx">...</a>
   * Detail: Structure MD_SPAN_IMG_DETAIL.
   * Note: Image text can contain nested spans and even nested images.
   * If rendered into ALT attribute of HTML <IMG> tag, it's responsibility
   * of the parser to deal with it.
   */
  img,

  /* <code>...</code> */
  code,

  /* <del>...</del>
   * Note: Recognized only when MD_FLAG_STRIKETHROUGH is enabled.
   */
  del,

  /* For recognizing inline ($) and display ($$) equations
   * Note: Recognized only when MD_FLAG_LATEXMATHSPANS is enabled.
   */
  latex_inline,
  latex_display,

  /* Wiki links
   * Note: Recognized only when MD_FLAG_WIKILINKS is enabled.
   */
  wiki_link,

  /* <u>...</u>
   * Note: Recognized only when MD_FLAG_UNDERLINE is enabled. */
  u
};
using SpanType = MD_SPANTYPE;

/* Text is the actual textual contents of span. */
enum class MD_TEXTTYPE {
  /* Normal text. */
  normal,

  /* NULL character. CommonMark requires replacing NULL character with
   * the replacement char U+FFFD, so this allows caller to do that easily. */
  null_char,

  /* Line breaks.
   * Note these are not sent from blocks with verbatim output (MD_BLOCK_CODE
   * or MD_BLOCK_HTML). In such cases, '\n' is part of the text itself. */
  br,      /* <br> (hard break) */
  soft_br, /* '\n' in source text where it is not semantically meaningful
                     (soft break) */

  /* Entity.
   * (a) Named entity, e.g. &nbsp;
   *     (Note MD4C does not have a list of known entities.
   *     Anything matching the regexp /&[A-Za-z][A-Za-z0-9]{1,47};/ is
   *     treated as a named entity.)
   * (b) Numerical entity, e.g. &#1234;
   * (c) Hexadecimal entity, e.g. &#x12AB;
   *
   * As MD4C is mostly encoding agnostic, application gets the verbatim
   * entity text into the MD_PARSER::text_callback(). */
  entity,

  /* Text in a code block (inside MD_BLOCK_CODE) or inlined code (`code`).
   * If it is inside MD_BLOCK_CODE, it includes spaces for indentation and
   * '\n' for new lines. MD_TEXT_BR and MD_TEXT_SOFTBR are not sent for this
   * kind of text. */
  code,

  /* Text is a raw HTML. If it is contents of a raw HTML block (i.e. not
   * an inline raw HTML), then MD_TEXT_BR and MD_TEXT_SOFTBR are not used.
   * The text contains verbatim '\n' for the new lines. */
  raw_html,

  /* Text is inside an equation. This is processed the same way as inlined code
   * spans (`code`). */
  latex
};
using TextType = MD_TEXTTYPE;

/* Alignment enumeration. */
enum class MD_ALIGN {
  default_align, /* When unspecified. */
  left,
  center,
  right
};
using Align = MD_ALIGN;

/* String attribute.
 *
 * This wraps strings which are outside of a normal text flow and which are
 * propagated within various detailed structures, but which still may contain
 * string portions of different types like e.g. entities.
 *
 * So, for example, lets consider this image:
 *
 *     ![image alt text](http://example.org/image.png 'foo &quot; bar')
 *
 * The image alt text is propagated as a normal text via the MD_PARSER::text()
 * callback. However, the image title ('foo &quot; bar') is propagated as
 * Attribute in MD_SPAN_IMG_DETAIL::title.
 *
 * Then the attribute MD_SPAN_IMG_DETAIL::title shall provide the following:
 *  -- [0]: "foo "   (substr_types[0] == MD_TEXT_NORMAL; substr_offsets[0] == 0)
 *  -- [1]: "&quot;" (substr_types[1] == MD_TEXT_ENTITY; substr_offsets[1] == 4)
 *  -- [2]: " bar"   (substr_types[2] == MD_TEXT_NORMAL; substr_offsets[2] ==
 * 10)
 *  -- [3]: (n/a)    (n/a                              ; substr_offsets[3] ==
 * 14)
 *
 * Note that these invariants are always guaranteed:
 *  -- substr_offsets[0] == 0
 *  -- substr_offsets[LAST+1] == size
 *  -- Currently, only MD_TEXT_NORMAL, MD_TEXT_ENTITY, MD_TEXT_NULLCHAR
 *     substrings can appear. This could change only of the specification
 *     changes.
 */
struct _MD_ATTRIBUTE {
  mdstring text;
  std::vector<TextType> substr_types;
  std::vector<MD_OFFSET> substr_offsets;
};
using Attribute = _MD_ATTRIBUTE;

/* Detailed info for MD_BLOCK_UL. */
struct MD_BLOCK_UL_DETAIL {
  bool is_tight; /* Non-zero if tight list, zero if loose. */
  MD_CHAR mark;  /* Item bullet character in MarkDown source of the list, e.g.
                    '-', '+', '*'. */
};
using ul_Detail = MD_BLOCK_UL_DETAIL;

/* Detailed info for MD_BLOCK_OL. */
struct _MD_BLOCK_OL_DETAIL {
  unsigned start;         /* Start index of the ordered list. */
  bool is_tight;          /* Non-zero if tight list, zero if loose. */
  MD_CHAR mark_delimiter; /* Character delimiting the item marks in MarkDown
                             source, e.g. '.' or ')' */
};
using ol_Detail = _MD_BLOCK_OL_DETAIL;

/* Detailed info for MD_BLOCK_LI. */
struct _MD_BLOCK_LI_DETAIL {
  bool is_task;      /* Can be non-zero only with MD_FLAG_TASKLISTS */
  MD_CHAR task_mark; /* If is_task, then one of 'x', 'X' or ' '. Undefined
                        otherwise. */
  MD_OFFSET task_mark_offset; /* If is_task, then offset in the input of the
                                 char between '[' and ']'. */
};
using li_Detail = _MD_BLOCK_LI_DETAIL;

/* Detailed info for MD_BLOCK_H. */
struct _MD_BLOCK_H_DETAIL {
  unsigned short level; /* Header level (1 - 6) */
};
using h_Detail = _MD_BLOCK_H_DETAIL;

/* Detailed info for MD_BLOCK_CODE. */
struct _MD_BLOCK_CODE_DETAIL {
  Attribute info;
  Attribute lang;
  MD_CHAR fence_char; /* The character used for fenced code block; or zero for
                         indented code block. */
};
using code_Detail = _MD_BLOCK_CODE_DETAIL;

/* Detailed info for MD_BLOCK_TABLE. */
struct _MD_BLOCK_TABLE_DETAIL {
  unsigned col_count;      /* Count of columns in the table. */
  unsigned head_row_count; /* Count of rows in the table header (currently
                              always 1) */
  unsigned body_row_count; /* Count of rows in the table body */
};
using table_Detail = _MD_BLOCK_TABLE_DETAIL;

/* Detailed info for MD_BLOCK_TH and MD_BLOCK_TD. */
struct _MD_BLOCK_TD_DETAIL {
  Align align;
};
using td_Detail = _MD_BLOCK_TD_DETAIL;
/* Detailed info for MD_SPAN_A. */
struct _MD_SPAN_A_DETAIL {
  Attribute href;
  Attribute title;
};
using a_Detail = _MD_SPAN_A_DETAIL;

/* Detailed info for MD_SPAN_IMG. */
struct _MD_SPAN_IMG_DETAIL {
  Attribute src;
  Attribute title;
};
using img_Detail = _MD_SPAN_IMG_DETAIL;

/* Detailed info for MD_SPAN_WIKILINK. */
struct _MD_SPAN_WIKILINK {
  Attribute target;
};
using Wikilink_Detail = _MD_SPAN_WIKILINK;

/* Flags specifying extensions/deviations from CommonMark specification.
 *
 * By default (when MD_PARSER::flags == 0), we follow CommonMark specification.
 * The following flags may allow some extensions or deviations from it.
 */
#define MD_FLAG_COLLAPSEWHITESPACE                                             \
  0x0001 /* In MD_TEXT_NORMAL, collapse non-trivial whitespace into single ' ' \
          */
#define MD_FLAG_PERMISSIVEATXHEADERS                                           \
  0x0002 /* Do not require space in ATX headers ( ###header ) */
#define MD_FLAG_PERMISSIVEURLAUTOLINKS                                         \
  0x0004 /* Recognize URLs as autolinks even without '<', '>' */
#define MD_FLAG_PERMISSIVEEMAILAUTOLINKS                                       \
  0x0008 /* Recognize e-mails as autolinks even without '<', '>' and 'mailto:' \
          */
#define MD_FLAG_NOINDENTEDCODEBLOCKS                                           \
  0x0010 /* Disable indented code blocks. (Only fenced code works.) */
#define MD_FLAG_NOHTMLBLOCKS 0x0020  /* Disable raw HTML blocks. */
#define MD_FLAG_NOHTMLSPANS 0x0040   /* Disable raw HTML (inline). */
#define MD_FLAG_TABLES 0x0100        /* Enable tables extension. */
#define MD_FLAG_STRIKETHROUGH 0x0200 /* Enable strikethrough extension. */
#define MD_FLAG_PERMISSIVEWWWAUTOLINKS                                         \
  0x0400 /* Enable WWW autolinks (even without any scheme prefix, if they      \
            begin with 'www.') */
#define MD_FLAG_TASKLISTS 0x0800 /* Enable task list extension. */
#define MD_FLAG_LATEXMATHSPANS                                                 \
  0x1000 /* Enable $ and $$ containing LaTeX equations. */
#define MD_FLAG_WIKILINKS 0x2000 /* Enable wiki links extension. */
#define MD_FLAG_UNDERLINE                                                      \
  0x4000 /* Enable underline extension (and disables '_' for normal emphasis). \
          */
#define MD_FLAG_ABBREVIATIONS                                                  \
  0x10000 /* Allow abbreviations using Markdown Extra's                        \
syntax Example: *[HTML]: Hyper Text Markup Language -> <abbr title="Hyper      \
Text Markup Language">HTML</abbr> */
#define MD_FLAG_INLINE_DIFF                                                    \
  0x20000 /* Display Git-style additions and deletions in inline text          \
as having green and red backgrounds, respectively.  */
#define MD_FLAG_COLOR 0x40000 /* Render RGB and HSL values in HTML output */
#define MD_FLAG_TOC 0x100000  /* Generate table of contents from headings */

#define MD_FLAG_PERMISSIVEAUTOLINKS                                            \
  (MD_FLAG_PERMISSIVEEMAILAUTOLINKS | MD_FLAG_PERMISSIVEURLAUTOLINKS |         \
   MD_FLAG_PERMISSIVEWWWAUTOLINKS)
#define MD_FLAG_NOHTML (MD_FLAG_NOHTMLBLOCKS | MD_FLAG_NOHTMLSPANS)

/* Convenient sets of flags corresponding to well-known Markdown dialects.
 *
 * Note we may only support subset of features of the referred dialect.
 * The constant just enables those extensions which bring us as close as
 * possible given what features we implement.
 *
 * ABI compatibility note: Meaning of these can change in time as new
 * extensions, bringing the dialect closer to the original, are implemented.
 */
#define MD_DIALECT_COMMONMARK 0
#define MD_DIALECT_GITHUB                                                      \
  (MD_FLAG_PERMISSIVEAUTOLINKS | MD_FLAG_TABLES | MD_FLAG_STRIKETHROUGH |      \
   MD_FLAG_TASKLISTS)
#define MD_DIALECT_GITLAB                                                      \
  MD_DIALECT_GITHUB | MD_FLAG_INLINE_DIFF | MD_FLAG_COLOR

enum class Extensions : unsigned long {
  // Strictly CommonMark-compliant, no extensions
  None = 0,
  /* In MD_TEXT_NORMAL, collapse non-trivial whitespace into single ' ' */
  Collapse_Whitespace = 1 << 0,
  /* Do not require space in ATX headers ( ###header ) */
  No_Space_Needed_for_ATXHeaders = 1 << 1,
  /* Recognize URLs as autolinks even without '<', '>' */
  Permissive_Autolink = 1 << 2,
  /* Recognize e-mails as autolinks even without '<', '>' and 'mailto:' */
  Permissive_Email_Autolink = 1 << 3,
  /* Disable indented code blocks. (Only fenced code works.) */
  No_Indented_Codeblock = 1 << 4,
  /* Disable raw HTML blocks. */
  No_Raw_HTML_Block = 1 << 5,
  /* Disable raw HTML (inline). */
  No_Raw_HTML_Inline = 1 << 6,
  /* Enable tables extension. */
  Tables = 1 << 7,
  /* Enable strikethrough extension. */
  Strikethrough = 1 << 8,
  /* Enable WWW autolinks (even without any scheme prefix, if they begin with 'www.') */
  Permissive_WWW_Autolink = 1 << 9,
  /* Enable task list extension. */
  Tasklist = 1 << 10,
  /* Enable $ and $$ containing LaTeX equations. */
  LaTeX_Math = 1 << 11,
  /* Enable wiki links extension. */
  Wikilinks = 1 << 12,
  /* Enable underline extension (and disables '_' for normal emphasis).  */
  Underline = 1 << 13,
  /* Allow abbreviations using Markdown Extra's syntax
   * Example: [HTML]: Hyper Text Markup Language ->
   * <abbr title="Hyper Text Markup Language">HTML</abbr> */
  Abbreviation = 1 << 14,
  /* Display Git-style additions and deletions in inline text as
                  having green and red backgrounds, respectively.  */
  Inline_Diff = 1 << 15,
  /* Render RGB and HSL values in HTML output */
  Color = 1 << 16,
  /* Generate table of contents from headings */
  Table_of_Contents = 1 << 17
};

inline Extensions operator|(Extensions lhs, Extensions rhs) {
  using T = std::underlying_type_t<Extensions>;
  return static_cast<Extensions>(static_cast<T>(lhs) | static_cast<T>(rhs));
}

/* Parser structure.
 */
struct _MD_PARSER {
  /* Reserved. Set to zero.
   */
  unsigned abi_version;

  /* Dialect options. Bitmask of MD_FLAG_xxxx values.
   */
  unsigned flags;

  /* Caller-provided rendering callbacks.
   *
   * For some block/span types, more detailed information is provided in a
   * type-specific structure pointed by the argument 'detail'.
   *
   * The last argument of all callbacks, 'userdata', is just propagated from
   * md_parse() and is available for any use by the application.
   *
   * Note any strings provided to the callbacks as their arguments or as
   * members of any detail structure are generally not zero-terminated.
   * Application has to take the respective size information into account.
   *
   * Any rendering callback may abort further parsing of the document by
   * returning non-zero.
   */
  int (*enter_block)(BlockType /*type*/, void * /*detail*/,
                     void * /*userdata*/);
  int (*leave_block)(BlockType /*type*/, void * /*detail*/,
                     void * /*userdata*/);

  int (*enter_span)(SpanType /*type*/, void * /*detail*/, void * /*userdata*/);
  int (*leave_span)(SpanType /*type*/, void * /*detail*/, void * /*userdata*/);

  int (*text)(TextType /*type*/, mdstringview, void * /*userdata*/);

  /* Debug callback. Optional (may be NULL).
   *
   * If provided and something goes wrong, this function gets called.
   * This is intended for debugging and problem diagnosis for developers;
   * it is not intended to provide any errors suitable for displaying to an
   * end user.
   */
  void (*debug_log)(mdstringview /*msg*/, void * /*userdata*/);

  /* Reserved. Set to NULL.
   */
  void (*syntax)(void);
};
using MD_PARSER = _MD_PARSER;

/* Parse the Markdown document passed in mdstringview type (aka
 * std::basic_string_view<MD_CHAR>). The parser provides callbacks to be called
 * during the parsing so the caller can render the document on the screen or
 * convert the Markdown to another format.
 *
 * Zero is returned on success. If a runtime error occurs (e.g. a memory
 * fails), -1 is returned. If the processing is aborted due any callback
 * returning non-zero, the return value of the callback is returned.
 */
int md_parse(mdstringview, const MD_PARSER &parser, void *uerdata);

extern "C" {
int md_parse(const MD_CHAR *text, MD_SIZE size, const MD_PARSER *parser,
             void *userdata);
}
#endif /* MD4C_H */
