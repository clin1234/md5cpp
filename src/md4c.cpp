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

#include "md4c.h"

#include <cctype>
#include <memory>
#include <ranges>
#include <span>
#include <string.h>
#include <string>
#include <variant>
#include <vector>


/*****************************
 ***  Miscellaneous Stuff  ***
 *****************************/

/* Make the UTF-8 support the default. */
#if !defined MD4C_USE_ASCII && !defined MD4C_USE_UTF8 && !defined MD4C_USE_UTF16
#define MD4C_USE_UTF8
#endif

/* Magic for making wide literals with MD4C_USE_UTF16. */
#ifdef _T
#undef _T
#endif
#if defined MD4C_USE_UTF16
#define _T(x) L##x
#else
#define _T(x) x
#endif

/* Misc. macros. */
#define SIZEOF_ARRAY(a) (sizeof(a) / sizeof(a[0]))

#define STRINGIZE_(x) #x
#define STRINGIZE(x) STRINGIZE_(x)

#define MD_LOG(msg)                                                            \
  do {                                                                         \
    if (ctx.parser.debug_log != nullptr)                                       \
      ctx.parser.debug_log((msg), ctx.userdata);                               \
  } while (0)

#ifdef DEBUG
#define MD_ASSERT(cond)                                                        \
  do {                                                                         \
    if (!(cond)) {                                                             \
      MD_LOG(__FILE__ ":" STRINGIZE(__LINE__) ": "                             \
                                              "Assertion '" STRINGIZE(cond) "' failed.");          \
      exit(1);                                                                 \
    }                                                                          \
  } while (0)

#define MD_UNREACHABLE() MD_ASSERT(1 == 0)
#else
#ifdef __GNUG__
#define MD_ASSERT(cond)                                                        \
  do {                                                                         \
    if (!(cond))                                                               \
      __builtin_unreachable();                                                 \
  } while (0)
#define MD_UNREACHABLE()                                                       \
  do {                                                                         \
    __builtin_unreachable();                                                   \
  } while (0)
#elif defined _MSC_VER && _MSC_VER > 120
#define MD_ASSERT(cond)                                                        \
  do {                                                                         \
    __assume(cond);                                                            \
  } while (0)
#define MD_UNREACHABLE()                                                       \
  do {                                                                         \
    __assume(0);                                                               \
  } while (0)
#else
#define MD_ASSERT(cond)                                                        \
  do {                                                                         \
  } while (0)
#define MD_UNREACHABLE()                                                       \
  do {                                                                         \
  } while (0)
#endif
#endif

static const auto make_unique_failure_str{
    mdstringview("std::make_unique failed because:\n")},
    vector_emplace_back_str{
        mdstringview{"std::vector.emplace_back failed because:\n"}};

/************************
 ***  Internal Types  ***
 ************************/

/* These are omnipresent so lets save some typing. */
#define CHAR MD_CHAR
#define SZ MD_SIZE
#define OFF MD_OFFSET

typedef struct MD_MARK_tag Mark;
typedef struct MD_BLOCK_tag Block;
typedef struct MD_CONTAINER_tag Container;
typedef struct MD_REF_DEF_tag Ref_Def;

/* During analyzes of inline marks, we need to manage some "mark chains",
 * of (yet unresolved) openers. This structure holds start/end of the chain.
 * The chain internals are then realized through MD_MARK::prev and ::next.
 */
struct MarkChain {
  int head; /* Index of first mark in the chain, or -1 if empty. */
  int tail; /* Index of last mark in the chain, or -1 if empty. */
};

/* Context propagated through all the parsing. */
struct Parsing_Context {
  /* Immutable stuff (parameters of md_parse()). */
  mdstring text;
  MD_PARSER parser;
  void *userdata;

  /* When this is true, it allows some optimizations. */
  bool doc_ends_with_newline;

  /* Helper temporary growing buffer. */
  mdstring buffer;

  /* Reference definitions. */
  Ref_Def *ref_defs;
  int n_ref_defs;
  int alloc_ref_defs;
  void **ref_def_hashtable;
  int ref_def_hashtable_size;

  /* Stack of inline/span markers.
   * This is only used for parsing a single block contents but by storing it
   * here we may reuse the stack for subsequent blocks; i.e. we have fewer
   * (re)allocations. */
  Mark *marks;
  int n_marks;
  int alloc_marks;

#if defined MD4C_USE_UTF16
  char mark_char_map[128];
#else
  signed char mark_char_map[256];
#endif

  /* For resolving of inline spans. */
  MarkChain mark_chains[13];
#define PTR_CHAIN (ctx.mark_chains[0])
#define TABLECELLBOUNDARIES (ctx.mark_chains[1])
#define ASTERISK_OPENERS_extraword_mod3_0 (ctx.mark_chains[2])
#define ASTERISK_OPENERS_extraword_mod3_1 (ctx.mark_chains[3])
#define ASTERISK_OPENERS_extraword_mod3_2 (ctx.mark_chains[4])
#define ASTERISK_OPENERS_intraword_mod3_0 (ctx.mark_chains[5])
#define ASTERISK_OPENERS_intraword_mod3_1 (ctx.mark_chains[6])
#define ASTERISK_OPENERS_intraword_mod3_2 (ctx.mark_chains[7])
#define UNDERSCORE_OPENERS (ctx.mark_chains[8])
#define TILDE_OPENERS_1 (ctx.mark_chains[9])
#define TILDE_OPENERS_2 (ctx.mark_chains[10])
#define BRACKET_OPENERS (ctx.mark_chains[11])
#define DOLLAR_OPENERS (ctx.mark_chains[12])
#define OPENERS_CHAIN_FIRST 2
#define OPENERS_CHAIN_LAST 12

  int n_table_cell_boundaries;

  /* For resolving links. */
  int unresolved_link_head;
  int unresolved_link_tail;

  /* For resolving raw HTML. */
  OFF html_comment_horizon;
  OFF html_proc_instr_horizon;
  OFF html_decl_horizon;
  OFF html_cdata_horizon;

  /* For block analysis.
   * Notes:
   *   -- It holds MD_BLOCK as well as MD_LINE structures. After each
   *      MD_BLOCK, its (multiple) MD_LINE(s) follow.
   *   -- For MD_BLOCK_HTML and MD_BLOCK_CODE, MD_VERBATIMLINE(s) are used
   *      instead of MD_LINE(s).
   */
  void *block_bytes;
  Block *current_block;
  int n_block_bytes;
  int alloc_block_bytes;

  /* For container block analysis. */
  std::vector<Container> cont;
  int n_containers;

  /* Minimal indentation to call the block "indented code block". */
  OFF code_indent_offset;

  /* Contextual info for line analysis. */
  SZ code_fence_length; /* For checking closing fence length. */
  int html_block_type;  /* For checking closing raw HTML condition. */
  bool last_line_has_list_loosening_effect;
  bool last_list_item_starts_with_two_blank_lines;
};

enum class LineType {
  blank,
  hr,
  MD_LINE_ATXHEADER,
  MD_LINE_SETEXTHEADER,
  MD_LINE_SETEXTUNDERLINE,
  MD_LINE_INDENTEDCODE,
  MD_LINE_FENCEDCODE,
  raw_html,
  MD_LINE_TEXT,
  MD_LINE_TABLE,
  MD_LINE_TABLEUNDERLINE
};

struct Line_Analysis {
  LineType type : 16;
  unsigned data : 16;
  OFF beg;
  OFF end;
  unsigned indent; /* Indentation level. */
};

struct Line {
  OFF beg;
  OFF end;
  bool operator==(const Line &) const = default;
};

typedef struct MD_VERBATIMLINE_tag MD_VERBATIMLINE;
struct MD_VERBATIMLINE_tag {
  OFF beg;
  OFF end;
  OFF indent;
};

/*****************
 ***  Helpers  ***
 *****************/

/* Character accessors. */
#define CH(off) (ctx.text[(off)])
#define STR(off) (ctx.text.substr(off))

/* Character classification.
 * Note we assume ASCII compatibility of code points < 128 here. */
#define ISIN_(ch, ch_min, ch_max)                                              \
  ((ch_min) <= (unsigned)(ch) && (unsigned)(ch) <= (ch_max))
#define ISANYOF_(ch, palette)                                                  \
  ((ch) != _T('\0') && md_strchr((palette), (ch)) != nullptr)
#define ISANYOF2_(ch, ch1, ch2) ((ch) == (ch1) || (ch) == (ch2))
#define ISANYOF3_(ch, ch1, ch2, ch3)                                           \
  ((ch) == (ch1) || (ch) == (ch2) || (ch) == (ch3))
#define ISASCII_(ch) ((unsigned)(ch) <= 127)
#define ISBLANK_(ch) (ISANYOF2_((ch), _T(' '), _T('\t')))
#define ISNEWLINE_(ch) (ISANYOF2_((ch), _T('\r'), _T('\n')))
#define ISWHITESPACE_(ch) (ISBLANK_(ch) || ISANYOF2_((ch), _T('\v'), _T('\f')))
#define ISCNTRL_(ch) ((unsigned)(ch) <= 31 || (unsigned)(ch) == 127)
#define ISPUNCT_(ch)                                                           \
  (ISIN_(ch, 33, 47) || ISIN_(ch, 58, 64) || ISIN_(ch, 91, 96) ||              \
   ISIN_(ch, 123, 126))
#define ISUPPER_(ch) (ISIN_(ch, _T('A'), _T('Z')))
#define ISLOWER_(ch) (ISIN_(ch, _T('a'), _T('z')))
#define ISALPHA_(ch) (ISUPPER_(ch) || ISLOWER_(ch))
#define ISDIGIT_(ch) (ISIN_(ch, _T('0'), _T('9')))
#define ISXDIGIT_(ch)                                                          \
  (ISDIGIT_(ch) || ISIN_(ch, _T('A'), _T('F')) || ISIN_(ch, _T('a'), _T('f')))
#define ISALNUM_(ch) (ISALPHA_(ch) || ISDIGIT_(ch))

#define ISANYOF(off, palette) ISANYOF_(CH(off), (palette))
#define ISANYOF2(off, ch1, ch2) ISANYOF2_(CH(off), (ch1), (ch2))
#define ISANYOF3(off, ch1, ch2, ch3) ISANYOF3_(CH(off), (ch1), (ch2), (ch3))
#define ISASCII(off) ISASCII_(CH(off))
#define ISBLANK(off) ISBLANK_(CH(off))
#define ISNEWLINE(off) ISNEWLINE_(CH(off))
#define ISWHITESPACE(off) ISWHITESPACE_(CH(off))
#define ISCNTRL(off) ISCNTRL_(CH(off))
#define ISPUNCT(off) ISPUNCT_(CH(off))
#define ISUPPER(off) ISUPPER_(CH(off))
#define ISLOWER(off) ISLOWER_(CH(off))
#define ISALPHA(off) ISALPHA_(CH(off))
#define ISDIGIT(off) ISDIGIT_(CH(off))
#define ISXDIGIT(off) ISXDIGIT_(CH(off))
#define ISALNUM(off) ISALNUM_(CH(off))

#if defined MD4C_USE_UTF16
#define md_strchr wcschr
#else
#define md_strchr strchr
#endif

static bool is_case_insensitive_equal(mdstringview s1, mdstringview s2) {
  return std::ranges::equal(s1, s2, [](CHAR x, CHAR y) {
    return std::tolower(x) == std::tolower(y);
  });
}

static int md_text_with_null_replacement(Parsing_Context &ctx, MD_TEXTTYPE type,
                                         mdstring &str) {
  OFF off = 0;
  int ret = 0;
  size_t new_size = str.size();

  while (1) {
    while (off < new_size && str[off] != _T('\0'))
      off++;

    if (off > 0) {
      ret = ctx.parser.text(type, str.substr(off), ctx.userdata);
      if (ret != 0)
        return ret;

      new_size -= off;
      off = 0;
    }

    if (off >= new_size)
      return 0;

    ret = ctx.parser.text(TextType::null_char, _T(""), ctx.userdata);
    if (ret != 0)
      return ret;
    off++;
  }
  str = str.substr(off, new_size);
}

#define MD_CHECK(func)                                                         \
  do {                                                                         \
    ret = (func);                                                              \
    if (ret < 0)                                                               \
      goto abort;                                                              \
  } while (0)

#define MD_ENTER_BLOCK(type, arg)                                              \
  do {                                                                         \
    ret = ctx.parser.enter_block((type), (arg), ctx.userdata);                 \
    if (ret != 0) {                                                            \
      MD_LOG("Aborted from enter_block() callback.");                          \
      goto abort;                                                              \
    }                                                                          \
  } while (0)

#define MD_LEAVE_BLOCK(type, arg)                                              \
  do {                                                                         \
    ret = ctx.parser.leave_block((type), (arg), ctx.userdata);                 \
    if (ret != 0) {                                                            \
      MD_LOG("Aborted from leave_block() callback.");                          \
      goto abort;                                                              \
    }                                                                          \
  } while (0)

#define MD_ENTER_SPAN(type, arg)                                               \
  do {                                                                         \
    ret = ctx.parser.enter_span((type), (arg), ctx.userdata);                  \
    if (ret != 0) {                                                            \
      MD_LOG("Aborted from enter_span() callback.");                           \
      goto abort;                                                              \
    }                                                                          \
  } while (0)

#define MD_LEAVE_SPAN(type, arg)                                               \
  do {                                                                         \
    ret = ctx.parser.leave_span((type), (arg), ctx.userdata);                  \
    if (ret != 0) {                                                            \
      MD_LOG("Aborted from leave_span() callback.");                           \
      goto abort;                                                              \
    }                                                                          \
  } while (0)

#define MD_TEXT(type, str)                                                     \
  do {                                                                         \
    if (str.size() > 0) {                                                      \
      ret = ctx.parser.text((type), (str), ctx.userdata);                      \
      if (ret != 0) {                                                          \
        MD_LOG("Aborted from text() callback.");                               \
        goto abort;                                                            \
      }                                                                        \
    }                                                                          \
  } while (0)

#define MD_TEXT_INSECURE(type, str)                                            \
  do {                                                                         \
    if (str.size() > 0) {                                                      \
      ret = md_text_with_null_replacement(ctx, type, str);                     \
      if (ret != 0) {                                                          \
        MD_LOG("Aborted from text() callback.");                               \
        goto abort;                                                            \
      }                                                                        \
    }                                                                          \
  } while (0)

/*************************
 ***  Unicode Support  ***
 *************************/

struct Unicode_Fold_Info {
  std::vector<unsigned> codepoints;
};

#if defined MD4C_USE_UTF16 || defined MD4C_USE_UTF8
/* Binary search over sorted "map" of codepoints. Consecutive sequences
 * of codepoints may be encoded in the map by just using the
 * (MIN_CODEPOINT | 0x40000000) and (MAX_CODEPOINT | 0x80000000).
 *
 * Returns index of the found record in the map (in the case of ranges,
 * the minimal value is used); or -1 on failure. */
static int md_unicode_bsearch__(unsigned codepoint,
                                std::span<const unsigned> map) {
  int beg, end;
  int pivot_beg, pivot_end;

  beg = 0;
  end = map.size() - 1;
  while (beg <= end) {
    /* Pivot may be a range, not just a single value. */
    pivot_beg = pivot_end = (beg + end) / 2;
    if (map[pivot_end] & 0x40000000)
      pivot_end++;
    if (map[pivot_beg] & 0x80000000)
      pivot_beg--;

    if (codepoint < (map[pivot_beg] & 0x00ffffff))
      end = pivot_beg - 1;
    else if (codepoint > (map[pivot_end] & 0x00ffffff))
      beg = pivot_end + 1;
    else
      return pivot_beg;
  }

  return -1;
}

static bool md_is_unicode_whitespace__(unsigned codepoint) {
#define R(cp_min, cp_max) ((cp_min) | 0x40000000), ((cp_max) | 0x80000000)
#define S(cp) (cp)
  /* Unicode "Zs" category.
   * (generated by scripts/build_whitespace_map.py) */
  static const unsigned WHITESPACE_MAP[] = {
      S(0x0020), S(0x00a0), S(0x1680), R(0x2000, 0x200a),
      S(0x202f), S(0x205f), S(0x3000)};
#undef R
#undef S

  /* The ASCII ones are the most frequently used ones, also CommonMark
   * specification requests few more in this range. */
  if (codepoint <= 0x7f)
    return ISWHITESPACE_(codepoint);

  return (md_unicode_bsearch__(codepoint, std::span(WHITESPACE_MAP)) >= 0);
}

static bool md_is_unicode_punct__(unsigned codepoint) {
#define R(cp_min, cp_max) ((cp_min) | 0x40000000), ((cp_max) | 0x80000000)
#define S(cp) (cp)
  /* Unicode "Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps" categories.
   * (generated by scripts/build_punct_map.py) */
  static const unsigned PUNCT_MAP[] = {
      R(0x0021, 0x0023),   R(0x0025, 0x002a),   R(0x002c, 0x002f),
      R(0x003a, 0x003b),   R(0x003f, 0x0040),   R(0x005b, 0x005d),
      S(0x005f),           S(0x007b),           S(0x007d),
      S(0x00a1),           S(0x00a7),           S(0x00ab),
      R(0x00b6, 0x00b7),   S(0x00bb),           S(0x00bf),
      S(0x037e),           S(0x0387),           R(0x055a, 0x055f),
      R(0x0589, 0x058a),   S(0x05be),           S(0x05c0),
      S(0x05c3),           S(0x05c6),           R(0x05f3, 0x05f4),
      R(0x0609, 0x060a),   R(0x060c, 0x060d),   S(0x061b),
      R(0x061e, 0x061f),   R(0x066a, 0x066d),   S(0x06d4),
      R(0x0700, 0x070d),   R(0x07f7, 0x07f9),   R(0x0830, 0x083e),
      S(0x085e),           R(0x0964, 0x0965),   S(0x0970),
      S(0x09fd),           S(0x0a76),           S(0x0af0),
      S(0x0c77),           S(0x0c84),           S(0x0df4),
      S(0x0e4f),           R(0x0e5a, 0x0e5b),   R(0x0f04, 0x0f12),
      S(0x0f14),           R(0x0f3a, 0x0f3d),   S(0x0f85),
      R(0x0fd0, 0x0fd4),   R(0x0fd9, 0x0fda),   R(0x104a, 0x104f),
      S(0x10fb),           R(0x1360, 0x1368),   S(0x1400),
      S(0x166e),           R(0x169b, 0x169c),   R(0x16eb, 0x16ed),
      R(0x1735, 0x1736),   R(0x17d4, 0x17d6),   R(0x17d8, 0x17da),
      R(0x1800, 0x180a),   R(0x1944, 0x1945),   R(0x1a1e, 0x1a1f),
      R(0x1aa0, 0x1aa6),   R(0x1aa8, 0x1aad),   R(0x1b5a, 0x1b60),
      R(0x1bfc, 0x1bff),   R(0x1c3b, 0x1c3f),   R(0x1c7e, 0x1c7f),
      R(0x1cc0, 0x1cc7),   S(0x1cd3),           R(0x2010, 0x2027),
      R(0x2030, 0x2043),   R(0x2045, 0x2051),   R(0x2053, 0x205e),
      R(0x207d, 0x207e),   R(0x208d, 0x208e),   R(0x2308, 0x230b),
      R(0x2329, 0x232a),   R(0x2768, 0x2775),   R(0x27c5, 0x27c6),
      R(0x27e6, 0x27ef),   R(0x2983, 0x2998),   R(0x29d8, 0x29db),
      R(0x29fc, 0x29fd),   R(0x2cf9, 0x2cfc),   R(0x2cfe, 0x2cff),
      S(0x2d70),           R(0x2e00, 0x2e2e),   R(0x2e30, 0x2e4f),
      S(0x2e52),           R(0x3001, 0x3003),   R(0x3008, 0x3011),
      R(0x3014, 0x301f),   S(0x3030),           S(0x303d),
      S(0x30a0),           S(0x30fb),           R(0xa4fe, 0xa4ff),
      R(0xa60d, 0xa60f),   S(0xa673),           S(0xa67e),
      R(0xa6f2, 0xa6f7),   R(0xa874, 0xa877),   R(0xa8ce, 0xa8cf),
      R(0xa8f8, 0xa8fa),   S(0xa8fc),           R(0xa92e, 0xa92f),
      S(0xa95f),           R(0xa9c1, 0xa9cd),   R(0xa9de, 0xa9df),
      R(0xaa5c, 0xaa5f),   R(0xaade, 0xaadf),   R(0xaaf0, 0xaaf1),
      S(0xabeb),           R(0xfd3e, 0xfd3f),   R(0xfe10, 0xfe19),
      R(0xfe30, 0xfe52),   R(0xfe54, 0xfe61),   S(0xfe63),
      S(0xfe68),           R(0xfe6a, 0xfe6b),   R(0xff01, 0xff03),
      R(0xff05, 0xff0a),   R(0xff0c, 0xff0f),   R(0xff1a, 0xff1b),
      R(0xff1f, 0xff20),   R(0xff3b, 0xff3d),   S(0xff3f),
      S(0xff5b),           S(0xff5d),           R(0xff5f, 0xff65),
      R(0x10100, 0x10102), S(0x1039f),          S(0x103d0),
      S(0x1056f),          S(0x10857),          S(0x1091f),
      S(0x1093f),          R(0x10a50, 0x10a58), S(0x10a7f),
      R(0x10af0, 0x10af6), R(0x10b39, 0x10b3f), R(0x10b99, 0x10b9c),
      S(0x10ead),          R(0x10f55, 0x10f59), R(0x11047, 0x1104d),
      R(0x110bb, 0x110bc), R(0x110be, 0x110c1), R(0x11140, 0x11143),
      R(0x11174, 0x11175), R(0x111c5, 0x111c8), S(0x111cd),
      S(0x111db),          R(0x111dd, 0x111df), R(0x11238, 0x1123d),
      S(0x112a9),          R(0x1144b, 0x1144f), R(0x1145a, 0x1145b),
      S(0x1145d),          S(0x114c6),          R(0x115c1, 0x115d7),
      R(0x11641, 0x11643), R(0x11660, 0x1166c), R(0x1173c, 0x1173e),
      S(0x1183b),          R(0x11944, 0x11946), S(0x119e2),
      R(0x11a3f, 0x11a46), R(0x11a9a, 0x11a9c), R(0x11a9e, 0x11aa2),
      R(0x11c41, 0x11c45), R(0x11c70, 0x11c71), R(0x11ef7, 0x11ef8),
      S(0x11fff),          R(0x12470, 0x12474), R(0x16a6e, 0x16a6f),
      S(0x16af5),          R(0x16b37, 0x16b3b), S(0x16b44),
      R(0x16e97, 0x16e9a), S(0x16fe2),          S(0x1bc9f),
      R(0x1da87, 0x1da8b), R(0x1e95e, 0x1e95f)};
#undef R
#undef S

  /* The ASCII ones are the most frequently used ones, also CommonMark
   * specification requests few more in this range. */
  if (codepoint <= 0x7f)
    return ISPUNCT_(codepoint);

  return (md_unicode_bsearch__(codepoint, std::span(PUNCT_MAP)) >= 0);
}

static void md_get_unicode_fold_info(unsigned codepoint,
                                     Unicode_Fold_Info *info) {
#define R(cp_min, cp_max) ((cp_min) | 0x40000000), ((cp_max) | 0x80000000)
#define S(cp) (cp)
  /* Unicode "Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps" categories.
   * (generated by scripts/build_folding_map.py) */
  static const unsigned FOLD_MAP_1[] = {
      R(0x0041, 0x005a),   S(0x00b5),           R(0x00c0, 0x00d6),
      R(0x00d8, 0x00de),   R(0x0100, 0x012e),   R(0x0132, 0x0136),
      R(0x0139, 0x0147),   R(0x014a, 0x0176),   S(0x0178),
      R(0x0179, 0x017d),   S(0x017f),           S(0x0181),
      S(0x0182),           S(0x0184),           S(0x0186),
      S(0x0187),           S(0x0189),           S(0x018a),
      S(0x018b),           S(0x018e),           S(0x018f),
      S(0x0190),           S(0x0191),           S(0x0193),
      S(0x0194),           S(0x0196),           S(0x0197),
      S(0x0198),           S(0x019c),           S(0x019d),
      S(0x019f),           R(0x01a0, 0x01a4),   S(0x01a6),
      S(0x01a7),           S(0x01a9),           S(0x01ac),
      S(0x01ae),           S(0x01af),           S(0x01b1),
      S(0x01b2),           S(0x01b3),           S(0x01b5),
      S(0x01b7),           S(0x01b8),           S(0x01bc),
      S(0x01c4),           S(0x01c5),           S(0x01c7),
      S(0x01c8),           S(0x01ca),           R(0x01cb, 0x01db),
      R(0x01de, 0x01ee),   S(0x01f1),           S(0x01f2),
      S(0x01f4),           S(0x01f6),           S(0x01f7),
      R(0x01f8, 0x021e),   S(0x0220),           R(0x0222, 0x0232),
      S(0x023a),           S(0x023b),           S(0x023d),
      S(0x023e),           S(0x0241),           S(0x0243),
      S(0x0244),           S(0x0245),           R(0x0246, 0x024e),
      S(0x0345),           S(0x0370),           S(0x0372),
      S(0x0376),           S(0x037f),           S(0x0386),
      R(0x0388, 0x038a),   S(0x038c),           S(0x038e),
      S(0x038f),           R(0x0391, 0x03a1),   R(0x03a3, 0x03ab),
      S(0x03c2),           S(0x03cf),           S(0x03d0),
      S(0x03d1),           S(0x03d5),           S(0x03d6),
      R(0x03d8, 0x03ee),   S(0x03f0),           S(0x03f1),
      S(0x03f4),           S(0x03f5),           S(0x03f7),
      S(0x03f9),           S(0x03fa),           R(0x03fd, 0x03ff),
      R(0x0400, 0x040f),   R(0x0410, 0x042f),   R(0x0460, 0x0480),
      R(0x048a, 0x04be),   S(0x04c0),           R(0x04c1, 0x04cd),
      R(0x04d0, 0x052e),   R(0x0531, 0x0556),   R(0x10a0, 0x10c5),
      S(0x10c7),           S(0x10cd),           R(0x13f8, 0x13fd),
      S(0x1c80),           S(0x1c81),           S(0x1c82),
      S(0x1c83),           S(0x1c84),           S(0x1c85),
      S(0x1c86),           S(0x1c87),           S(0x1c88),
      R(0x1c90, 0x1cba),   R(0x1cbd, 0x1cbf),   R(0x1e00, 0x1e94),
      S(0x1e9b),           R(0x1ea0, 0x1efe),   R(0x1f08, 0x1f0f),
      R(0x1f18, 0x1f1d),   R(0x1f28, 0x1f2f),   R(0x1f38, 0x1f3f),
      R(0x1f48, 0x1f4d),   S(0x1f59),           S(0x1f5b),
      S(0x1f5d),           S(0x1f5f),           R(0x1f68, 0x1f6f),
      S(0x1fb8),           S(0x1fb9),           S(0x1fba),
      S(0x1fbb),           S(0x1fbe),           R(0x1fc8, 0x1fcb),
      S(0x1fd8),           S(0x1fd9),           S(0x1fda),
      S(0x1fdb),           S(0x1fe8),           S(0x1fe9),
      S(0x1fea),           S(0x1feb),           S(0x1fec),
      S(0x1ff8),           S(0x1ff9),           S(0x1ffa),
      S(0x1ffb),           S(0x2126),           S(0x212a),
      S(0x212b),           S(0x2132),           R(0x2160, 0x216f),
      S(0x2183),           R(0x24b6, 0x24cf),   R(0x2c00, 0x2c2e),
      S(0x2c60),           S(0x2c62),           S(0x2c63),
      S(0x2c64),           R(0x2c67, 0x2c6b),   S(0x2c6d),
      S(0x2c6e),           S(0x2c6f),           S(0x2c70),
      S(0x2c72),           S(0x2c75),           S(0x2c7e),
      S(0x2c7f),           R(0x2c80, 0x2ce2),   S(0x2ceb),
      S(0x2ced),           S(0x2cf2),           R(0xa640, 0xa66c),
      R(0xa680, 0xa69a),   R(0xa722, 0xa72e),   R(0xa732, 0xa76e),
      S(0xa779),           S(0xa77b),           S(0xa77d),
      R(0xa77e, 0xa786),   S(0xa78b),           S(0xa78d),
      S(0xa790),           S(0xa792),           R(0xa796, 0xa7a8),
      S(0xa7aa),           S(0xa7ab),           S(0xa7ac),
      S(0xa7ad),           S(0xa7ae),           S(0xa7b0),
      S(0xa7b1),           S(0xa7b2),           S(0xa7b3),
      R(0xa7b4, 0xa7be),   S(0xa7c2),           S(0xa7c4),
      S(0xa7c5),           S(0xa7c6),           S(0xa7c7),
      S(0xa7c9),           S(0xa7f5),           R(0xab70, 0xabbf),
      R(0xff21, 0xff3a),   R(0x10400, 0x10427), R(0x104b0, 0x104d3),
      R(0x10c80, 0x10cb2), R(0x118a0, 0x118bf), R(0x16e40, 0x16e5f),
      R(0x1e900, 0x1e921)};
  static const unsigned FOLD_MAP_1_DATA[] = {
      0x0061,  0x007a,  0x03bc,  0x00e0,  0x00f6,  0x00f8,  0x00fe,  0x0101,
      0x012f,  0x0133,  0x0137,  0x013a,  0x0148,  0x014b,  0x0177,  0x00ff,
      0x017a,  0x017e,  0x0073,  0x0253,  0x0183,  0x0185,  0x0254,  0x0188,
      0x0256,  0x0257,  0x018c,  0x01dd,  0x0259,  0x025b,  0x0192,  0x0260,
      0x0263,  0x0269,  0x0268,  0x0199,  0x026f,  0x0272,  0x0275,  0x01a1,
      0x01a5,  0x0280,  0x01a8,  0x0283,  0x01ad,  0x0288,  0x01b0,  0x028a,
      0x028b,  0x01b4,  0x01b6,  0x0292,  0x01b9,  0x01bd,  0x01c6,  0x01c6,
      0x01c9,  0x01c9,  0x01cc,  0x01cc,  0x01dc,  0x01df,  0x01ef,  0x01f3,
      0x01f3,  0x01f5,  0x0195,  0x01bf,  0x01f9,  0x021f,  0x019e,  0x0223,
      0x0233,  0x2c65,  0x023c,  0x019a,  0x2c66,  0x0242,  0x0180,  0x0289,
      0x028c,  0x0247,  0x024f,  0x03b9,  0x0371,  0x0373,  0x0377,  0x03f3,
      0x03ac,  0x03ad,  0x03af,  0x03cc,  0x03cd,  0x03ce,  0x03b1,  0x03c1,
      0x03c3,  0x03cb,  0x03c3,  0x03d7,  0x03b2,  0x03b8,  0x03c6,  0x03c0,
      0x03d9,  0x03ef,  0x03ba,  0x03c1,  0x03b8,  0x03b5,  0x03f8,  0x03f2,
      0x03fb,  0x037b,  0x037d,  0x0450,  0x045f,  0x0430,  0x044f,  0x0461,
      0x0481,  0x048b,  0x04bf,  0x04cf,  0x04c2,  0x04ce,  0x04d1,  0x052f,
      0x0561,  0x0586,  0x2d00,  0x2d25,  0x2d27,  0x2d2d,  0x13f0,  0x13f5,
      0x0432,  0x0434,  0x043e,  0x0441,  0x0442,  0x0442,  0x044a,  0x0463,
      0xa64b,  0x10d0,  0x10fa,  0x10fd,  0x10ff,  0x1e01,  0x1e95,  0x1e61,
      0x1ea1,  0x1eff,  0x1f00,  0x1f07,  0x1f10,  0x1f15,  0x1f20,  0x1f27,
      0x1f30,  0x1f37,  0x1f40,  0x1f45,  0x1f51,  0x1f53,  0x1f55,  0x1f57,
      0x1f60,  0x1f67,  0x1fb0,  0x1fb1,  0x1f70,  0x1f71,  0x03b9,  0x1f72,
      0x1f75,  0x1fd0,  0x1fd1,  0x1f76,  0x1f77,  0x1fe0,  0x1fe1,  0x1f7a,
      0x1f7b,  0x1fe5,  0x1f78,  0x1f79,  0x1f7c,  0x1f7d,  0x03c9,  0x006b,
      0x00e5,  0x214e,  0x2170,  0x217f,  0x2184,  0x24d0,  0x24e9,  0x2c30,
      0x2c5e,  0x2c61,  0x026b,  0x1d7d,  0x027d,  0x2c68,  0x2c6c,  0x0251,
      0x0271,  0x0250,  0x0252,  0x2c73,  0x2c76,  0x023f,  0x0240,  0x2c81,
      0x2ce3,  0x2cec,  0x2cee,  0x2cf3,  0xa641,  0xa66d,  0xa681,  0xa69b,
      0xa723,  0xa72f,  0xa733,  0xa76f,  0xa77a,  0xa77c,  0x1d79,  0xa77f,
      0xa787,  0xa78c,  0x0265,  0xa791,  0xa793,  0xa797,  0xa7a9,  0x0266,
      0x025c,  0x0261,  0x026c,  0x026a,  0x029e,  0x0287,  0x029d,  0xab53,
      0xa7b5,  0xa7bf,  0xa7c3,  0xa794,  0x0282,  0x1d8e,  0xa7c8,  0xa7ca,
      0xa7f6,  0x13a0,  0x13ef,  0xff41,  0xff5a,  0x10428, 0x1044f, 0x104d8,
      0x104fb, 0x10cc0, 0x10cf2, 0x118c0, 0x118df, 0x16e60, 0x16e7f, 0x1e922,
      0x1e943};
  static const unsigned FOLD_MAP_2[] = {
      S(0x00df),         S(0x0130),         S(0x0149),
      S(0x01f0),         S(0x0587),         S(0x1e96),
      S(0x1e97),         S(0x1e98),         S(0x1e99),
      S(0x1e9a),         S(0x1e9e),         S(0x1f50),
      R(0x1f80, 0x1f87), R(0x1f88, 0x1f8f), R(0x1f90, 0x1f97),
      R(0x1f98, 0x1f9f), R(0x1fa0, 0x1fa7), R(0x1fa8, 0x1faf),
      S(0x1fb2),         S(0x1fb3),         S(0x1fb4),
      S(0x1fb6),         S(0x1fbc),         S(0x1fc2),
      S(0x1fc3),         S(0x1fc4),         S(0x1fc6),
      S(0x1fcc),         S(0x1fd6),         S(0x1fe4),
      S(0x1fe6),         S(0x1ff2),         S(0x1ff3),
      S(0x1ff4),         S(0x1ff6),         S(0x1ffc),
      S(0xfb00),         S(0xfb01),         S(0xfb02),
      S(0xfb05),         S(0xfb06),         S(0xfb13),
      S(0xfb14),         S(0xfb15),         S(0xfb16),
      S(0xfb17)};
  static const unsigned FOLD_MAP_2_DATA[] = {
      0x0073, 0x0073, 0x0069, 0x0307, 0x02bc, 0x006e, 0x006a, 0x030c, 0x0565,
      0x0582, 0x0068, 0x0331, 0x0074, 0x0308, 0x0077, 0x030a, 0x0079, 0x030a,
      0x0061, 0x02be, 0x0073, 0x0073, 0x03c5, 0x0313, 0x1f00, 0x03b9, 0x1f07,
      0x03b9, 0x1f00, 0x03b9, 0x1f07, 0x03b9, 0x1f20, 0x03b9, 0x1f27, 0x03b9,
      0x1f20, 0x03b9, 0x1f27, 0x03b9, 0x1f60, 0x03b9, 0x1f67, 0x03b9, 0x1f60,
      0x03b9, 0x1f67, 0x03b9, 0x1f70, 0x03b9, 0x03b1, 0x03b9, 0x03ac, 0x03b9,
      0x03b1, 0x0342, 0x03b1, 0x03b9, 0x1f74, 0x03b9, 0x03b7, 0x03b9, 0x03ae,
      0x03b9, 0x03b7, 0x0342, 0x03b7, 0x03b9, 0x03b9, 0x0342, 0x03c1, 0x0313,
      0x03c5, 0x0342, 0x1f7c, 0x03b9, 0x03c9, 0x03b9, 0x03ce, 0x03b9, 0x03c9,
      0x0342, 0x03c9, 0x03b9, 0x0066, 0x0066, 0x0066, 0x0069, 0x0066, 0x006c,
      0x0073, 0x0074, 0x0073, 0x0074, 0x0574, 0x0576, 0x0574, 0x0565, 0x0574,
      0x056b, 0x057e, 0x0576, 0x0574, 0x056d};
  static const unsigned FOLD_MAP_3[] = {
      S(0x0390), S(0x03b0), S(0x1f52), S(0x1f54), S(0x1f56), S(0x1fb7),
      S(0x1fc7), S(0x1fd2), S(0x1fd3), S(0x1fd7), S(0x1fe2), S(0x1fe3),
      S(0x1fe7), S(0x1ff7), S(0xfb03), S(0xfb04)};
  static const unsigned FOLD_MAP_3_DATA[] = {
      0x03b9, 0x0308, 0x0301, 0x03c5, 0x0308, 0x0301, 0x03c5, 0x0313,
      0x0300, 0x03c5, 0x0313, 0x0301, 0x03c5, 0x0313, 0x0342, 0x03b1,
      0x0342, 0x03b9, 0x03b7, 0x0342, 0x03b9, 0x03b9, 0x0308, 0x0300,
      0x03b9, 0x0308, 0x0301, 0x03b9, 0x0308, 0x0342, 0x03c5, 0x0308,
      0x0300, 0x03c5, 0x0308, 0x0301, 0x03c5, 0x0308, 0x0342, 0x03c9,
      0x0342, 0x03b9, 0x0066, 0x0066, 0x0069, 0x0066, 0x0066, 0x006c};
#undef R
#undef S
  struct Fold_Map {
    std::span<const unsigned> map, data;
    unsigned short n_codepoints : 2;
  };
  Fold_Map one{FOLD_MAP_1, FOLD_MAP_1_DATA, 1},
      two{FOLD_MAP_2, FOLD_MAP_2_DATA, 2},
      three{FOLD_MAP_3, FOLD_MAP_3_DATA, 3};
  std::array<Fold_Map, 3> FOLD_MAP_LIST{one, two, three};

  /* Fast path for ASCII characters. */
  if (codepoint <= 0x7f) {
    info->codepoints.resize(1);
    info->codepoints[0] = codepoint;
    if (ISUPPER_(codepoint))
      info->codepoints[0] += 'a' - 'A';
    return;
  }

  /* Try to locate the codepoint in any of the maps. */
  for (const auto &fold_map : FOLD_MAP_LIST) {
    int index = md_unicode_bsearch__(codepoint, std::span(fold_map.map));
    if (index >= 0) {
      /* Found the mapping. */
      unsigned n_codepoints = fold_map.n_codepoints;
      const auto map{fold_map.map};
      const auto codepoints{fold_map.data.first(index * n_codepoints)};
      info->codepoints.assign(codepoints.begin(), codepoints.end());
      // Maybe not needed?
      // info->codepoints.resize(n_codepoints);

      // memcpy(info->codepoints, codepoints, sizeof(unsigned) * n_codepoints);

      if (fold_map.map[index] != codepoint) {
        /* The found mapping maps whole range of codepoints,
         * i.e. we have to offset info->codepoints[0] accordingly. */
        if ((map[index] & 0x00ffffff) + 1 == codepoints[0]) {
          /* Alternating type of the range. */
          info->codepoints[0] =
              codepoint + ((codepoint & 0x1) == (map[index] & 0x1) ? 1 : 0);
        } else {
          /* Range to range kind of mapping. */
          info->codepoints[0] += (codepoint - (map[index] & 0x00ffffff));
        }
      }

      return;
    }
  }

  /* No mapping found. Map the codepoint to itself. */
  info->codepoints[0] = codepoint;
  info->codepoints.resize(1);
}
#endif

#if defined MD4C_USE_UTF16
#define IS_UTF16_SURROGATE_HI(word) (((WORD)(word)&0xfc00) == 0xd800)
#define IS_UTF16_SURROGATE_LO(word) (((WORD)(word)&0xfc00) == 0xdc00)
#define UTF16_DECODE_SURROGATE(hi, lo)                                         \
  (0x10000 + ((((unsigned)(hi)&0x3ff) << 10) | (((unsigned)(lo)&0x3ff) << 0)))

static unsigned md_decode_utf16le__(const CHAR *str, SZ str_size, SZ *p_size) {
  if (IS_UTF16_SURROGATE_HI(str[0])) {
    if (1 < str_size && IS_UTF16_SURROGATE_LO(str[1])) {
      if (p_size != nullptr)
        *p_size = 2;
      return UTF16_DECODE_SURROGATE(str[0], str[1]);
    }
  }

  if (p_size != nullptr)
    *p_size = 1;
  return str[0];
}

static unsigned md_decode_utf16le_before__(MD_CTX &ctx, OFF off) {
  if (off > 2 && IS_UTF16_SURROGATE_HI(CH(off - 2)) &&
      IS_UTF16_SURROGATE_LO(CH(off - 1)))
    return UTF16_DECODE_SURROGATE(CH(off - 2), CH(off - 1));

  return CH(off);
}

/* No whitespace uses surrogates, so no decoding needed here. */
#define ISUNICODEWHITESPACE_(codepoint) md_is_unicode_whitespace__(codepoint)
#define ISUNICODEWHITESPACE(off) md_is_unicode_whitespace__(CH(off))
#define ISUNICODEWHITESPACEBEFORE(off) md_is_unicode_whitespace__(CH((off)-1))

#define ISUNICODEPUNCT(off)                                                    \
  md_is_unicode_punct__(                                                       \
      md_decode_utf16le__(STR(off), ctx.text.size() - (off), nullptr))
#define ISUNICODEPUNCTBEFORE(off)                                              \
  md_is_unicode_punct__(md_decode_utf16le_before__(ctx, off))

static inline int md_decode_unicode(const CHAR *str, OFF off, SZ str_size,
                                    SZ *p_char_size) {
  return md_decode_utf16le__(str + off, str_size - off, p_char_size);
}
#elif defined MD4C_USE_UTF8
#define IS_UTF8_LEAD1(byte) ((unsigned char)(byte) <= 0x7f)
#define IS_UTF8_LEAD2(byte) (((unsigned char)(byte)&0xe0) == 0xc0)
#define IS_UTF8_LEAD3(byte) (((unsigned char)(byte)&0xf0) == 0xe0)
#define IS_UTF8_LEAD4(byte) (((unsigned char)(byte)&0xf8) == 0xf0)
#define IS_UTF8_TAIL(byte) (((unsigned char)(byte)&0xc0) == 0x80)

static unsigned md_decode_utf8__(mdstringview str, SZ *p_size) {
  if (!IS_UTF8_LEAD1(str[0])) {
    if (IS_UTF8_LEAD2(str[0])) {
      if (1 < str.size() && IS_UTF8_TAIL(str[1])) {
        if (p_size != nullptr)
          *p_size = 2;

        return (((unsigned int)str[0] & 0x1f) << 6) |
               (((unsigned int)str[1] & 0x3f) << 0);
      }
    } else if (IS_UTF8_LEAD3(str[0])) {
      if (2 < str.size() && IS_UTF8_TAIL(str[1]) && IS_UTF8_TAIL(str[2])) {
        if (p_size != nullptr)
          *p_size = 3;

        return (((unsigned int)str[0] & 0x0f) << 12) |
               (((unsigned int)str[1] & 0x3f) << 6) |
               (((unsigned int)str[2] & 0x3f) << 0);
      }
    } else if (IS_UTF8_LEAD4(str[0])) {
      if (3 < str.size() && IS_UTF8_TAIL(str[1]) && IS_UTF8_TAIL(str[2]) &&
          IS_UTF8_TAIL(str[3])) {
        if (p_size != nullptr)
          *p_size = 4;

        return (((unsigned int)str[0] & 0x07) << 18) |
               (((unsigned int)str[1] & 0x3f) << 12) |
               (((unsigned int)str[2] & 0x3f) << 6) |
               (((unsigned int)str[3] & 0x3f) << 0);
      }
    }
  }

  if (p_size != nullptr)
    *p_size = 1;
  return (unsigned)str[0];
}

static unsigned md_decode_utf8_before__(Parsing_Context &ctx, OFF off) {
  if (!IS_UTF8_LEAD1(CH(off - 1))) {
    if (off > 1 && IS_UTF8_LEAD2(CH(off - 2)) && IS_UTF8_TAIL(CH(off - 1)))
      return (((unsigned int)CH(off - 2) & 0x1f) << 6) |
             (((unsigned int)CH(off - 1) & 0x3f) << 0);

    if (off > 2 && IS_UTF8_LEAD3(CH(off - 3)) && IS_UTF8_TAIL(CH(off - 2)) &&
        IS_UTF8_TAIL(CH(off - 1)))
      return (((unsigned int)CH(off - 3) & 0x0f) << 12) |
             (((unsigned int)CH(off - 2) & 0x3f) << 6) |
             (((unsigned int)CH(off - 1) & 0x3f) << 0);

    if (off > 3 && IS_UTF8_LEAD4(CH(off - 4)) && IS_UTF8_TAIL(CH(off - 3)) &&
        IS_UTF8_TAIL(CH(off - 2)) && IS_UTF8_TAIL(CH(off - 1)))
      return (((unsigned int)CH(off - 4) & 0x07) << 18) |
             (((unsigned int)CH(off - 3) & 0x3f) << 12) |
             (((unsigned int)CH(off - 2) & 0x3f) << 6) |
             (((unsigned int)CH(off - 1) & 0x3f) << 0);
  }

  return (unsigned)CH(off - 1);
}

#define ISUNICODEWHITESPACE_(codepoint) md_is_unicode_whitespace__(codepoint)
#define ISUNICODEWHITESPACE(off)                                               \
  md_is_unicode_whitespace__(md_decode_utf8__(STR(off), nullptr))
#define ISUNICODEWHITESPACEBEFORE(off)                                         \
  md_is_unicode_whitespace__(md_decode_utf8_before__(ctx, off))

#define ISUNICODEPUNCT(off)                                                    \
  md_is_unicode_punct__(md_decode_utf8__(STR(off), nullptr))
#define ISUNICODEPUNCTBEFORE(off)                                              \
  md_is_unicode_punct__(md_decode_utf8_before__(ctx, off))

static inline unsigned md_decode_unicode(mdstringview str, OFF off,
                                         SZ *p_char_size) {
  return md_decode_utf8__(str.substr(off), p_char_size);
}
#else
#define ISUNICODEWHITESPACE_(codepoint) ISWHITESPACE_(codepoint)
#define ISUNICODEWHITESPACE(off) ISWHITESPACE(off)
#define ISUNICODEWHITESPACEBEFORE(off) ISWHITESPACE((off)-1)

#define ISUNICODEPUNCT(off) ISPUNCT(off)
#define ISUNICODEPUNCTBEFORE(off) ISPUNCT((off)-1)

static inline void md_get_unicode_fold_info(unsigned codepoint,
                                            MD_UNICODE_FOLD_INFO *info) {
  info->codepoints[0] = codepoint;
  if (ISUPPER_(codepoint))
    info->codepoints[0] += 'a' - 'A';
  info->n_codepoints = 1;
}

static inline unsigned md_decode_unicode(const CHAR *str, OFF off, SZ str_size,
                                         SZ *p_size) {
  *p_size = 1;
  return (unsigned)str[off];
}
#endif

/*************************************
 ***  Helper string manipulations  ***
 *************************************/

/* Fill buffer with copy of the string between 'beg' and 'end' but replace any
 * line breaks with given replacement character. */
static mdstring merge_lines(Parsing_Context &ctx, OFF beg, OFF end,
                            std::span<Line> lines,
                            CHAR line_break_replacement_char) {
  mdstring buf;
  buf.reserve(end - beg);
  OFF off = beg;
  unsigned line_idx = 0;
  while (1) {
    OFF line_end = std::min(end, lines[line_idx].end);
    buf += ctx.text.substr(off, line_end - off);
    if (off >= line_end)
      return buf;
    buf[line_end] = line_break_replacement_char;
    line_idx++;
    off = lines[line_idx].beg;
  }
}

/* Fill buffer with copy of the string between 'beg' and 'end' but replace any
 * line breaks with given replacement character.
 *
 * NOTE: Caller is responsible to make sure the buffer is large enough.
 * (Given the output is always shorter then input, (end - beg) is good idea
 * what the caller should allocate.)
 */
/*
static void md_merge_lines(Parsing_Context &ctx, OFF beg, OFF end,
                          std::span<Line> lines,
                          CHAR line_break_replacement_char, CHAR *buffer,
                          SZ *p_size) {
 CHAR *ptr = buffer;
 unsigned line_index = 0;
 OFF off = beg;

 while (1) {
   const Line &line = lines[line_index];
   OFF line_end = line.end;
   if (end < line_end)
     line_end = end;

   while (off < line_end) {
     *ptr = CH(off);
     ptr++;
     off++;
   }

   if (off >= end) {
     *p_size = (MD_SIZE)(ptr - buffer);
     return;
   }

   *ptr = line_break_replacement_char;
   ptr++;

   line_index++;
   off = lines[line_index].beg;
 }
}*/

/* Wrapper of md_merge_lines() which allocates new buffer for the output string.
 */
/*
static int md_merge_lines_alloc(Parsing_Context &ctx, OFF beg, OFF end,
                               std::span<Line> lines,
                               CHAR line_break_replacement_char, CHAR **p_str,
                               SZ *p_size) {
 CHAR *buffer;

 buffer = (CHAR *)malloc(sizeof(CHAR) * (end - beg));
 if (buffer == nullptr) {
   MD_LOG("malloc() failed.");
   return -1;
 }

 md_merge_lines(ctx, beg, end, lines, line_break_replacement_char, buffer,
                p_size);

 *p_str = buffer;
 return 0;
}*/

static OFF md_skip_unicode_whitespace(mdstringview label, OFF off) {
  SZ char_size;
  unsigned codepoint;

  while (off < label.size()) {
    codepoint = md_decode_unicode(label, off, &char_size);
    if (!ISUNICODEWHITESPACE_(codepoint) && !ISNEWLINE_(label[off]))
      break;
    off += char_size;
  }

  return off;
}

/******************************
 ***  Recognizing raw HTML  ***
 ******************************/

/* md_is_html_tag() may be called when processing inlines (inline raw HTML)
 * or when breaking document to blocks (checking for start of HTML block type
 * 7).
 *
 * When breaking document to blocks, we do not yet know line boundaries, but
 * in that case the whole tag has to live on a single line. We distinguish this
 * by lines.size == 0.
 */
static int md_is_html_tag(Parsing_Context &ctx, std::span<Line> lines, OFF beg,
                          OFF max_end, OFF *p_end) {
  int attr_state;
  OFF off = beg;
  OFF line_end = (lines.size() > 0) ? lines[0].end : ctx.text.size();
  size_t i = 0;

  MD_ASSERT(CH(beg) == _T('<'));

  if (off + 1 >= line_end)
    return false;
  off++;

  /* For parsing attributes, we need a little state automaton below.
   * State -1: no attributes are allowed.
   * State 0: attribute could follow after some whitespace.
   * State 1: after a whitespace (attribute name may follow).
   * State 2: after attribute name ('=' MAY follow).
   * State 3: after '=' (value specification MUST follow).
   * State 41: in middle of unquoted attribute value.
   * State 42: in middle of single-quoted attribute value.
   * State 43: in middle of double-quoted attribute value.
   */
  attr_state = 0;

  if (CH(off) == _T('/')) {
    /* Closer tag "</ ... >". No attributes may be present. */
    attr_state = -1;
    off++;
  }

  /* Tag name */
  if (off >= line_end || !ISALPHA(off))
    return false;
  off++;
  while (off < line_end && (ISALNUM(off) || CH(off) == _T('-')))
    off++;

  /* (Optional) attributes (if not closer), (optional) '/' (if not closer)
   * and final '>'. */
  while (1) {
    while (off < line_end && !ISNEWLINE(off)) {
      if (attr_state > 40) {
        if (attr_state == 41 && (ISBLANK(off) || ISANYOF(off, _T("\"'=<>`")))) {
          attr_state = 0;
          off--; /* Put the char back for re-inspection in the new state. */
        } else if (attr_state == 42 && CH(off) == _T('\'')) {
          attr_state = 0;
        } else if (attr_state == 43 && CH(off) == _T('"')) {
          attr_state = 0;
        }
        off++;
      } else if (ISWHITESPACE(off)) {
        if (attr_state == 0)
          attr_state = 1;
        off++;
      } else if (attr_state <= 2 && CH(off) == _T('>')) {
        /* End. */
        goto done;
      } else if (attr_state <= 2 && CH(off) == _T('/') && off + 1 < line_end &&
                 CH(off + 1) == _T('>')) {
        /* End with digraph '/>' */
        off++;
        goto done;
      } else if ((attr_state == 1 || attr_state == 2) &&
                 (ISALPHA(off) || CH(off) == _T('_') || CH(off) == _T(':'))) {
        off++;
        /* Attribute name */
        while (off < line_end && (ISALNUM(off) || ISANYOF(off, _T("_.:-"))))
          off++;
        attr_state = 2;
      } else if (attr_state == 2 && CH(off) == _T('=')) {
        /* Attribute assignment sign */
        off++;
        attr_state = 3;
      } else if (attr_state == 3) {
        /* Expecting start of attribute value. */
        if (CH(off) == _T('"'))
          attr_state = 43;
        else if (CH(off) == _T('\''))
          attr_state = 42;
        else if (!ISANYOF(off, _T("\"'=<>`")) && !ISNEWLINE(off))
          attr_state = 41;
        else
          return false;
        off++;
      } else {
        /* Anything unexpected. */
        return false;
      }
    }

    /* We have to be on a single line. See definition of start condition
     * of HTML block, type 7. */
    if (lines.size() == 0)
      return false;

    i++;
    if (i >= lines.size())
      return false;

    off = lines[i].beg;
    line_end = lines[i].end;

    if (attr_state == 0 || attr_state == 41)
      attr_state = 1;

    if (off >= max_end)
      return false;
  }

done:
  if (off >= max_end)
    return false;

  *p_end = off + 1;
  return true;
}

static int md_scan_for_html_closer(Parsing_Context &ctx, mdstringview str,
                                   std::span<Line> lines, OFF beg, OFF max_end,
                                   OFF *p_end, OFF *p_scan_horizon) {
  OFF off = beg;
  size_t i = 0;

  if (off < *p_scan_horizon && *p_scan_horizon >= max_end - str.size()) {
    /* We have already scanned the range up to the max_end so we know
     * there is nothing to see. */
    return false;
  }

  while (true) {
    while (off + str.size() <= lines[i].end && off + str.size() <= max_end) {
      if (STR(off) == str) {
        /* Success. */
        *p_end = off + str.size();
        return true;
      }
      off++;
    }

    i++;
    if (off >= max_end || i >= lines.size()) {
      /* Failure. */
      *p_scan_horizon = off;
      return false;
    }

    off = lines[i].beg;
  }
}

static int md_is_html_comment(Parsing_Context &ctx, std::span<Line> lines,
                              OFF beg, OFF max_end, OFF *p_end) {
  OFF off = beg;

  MD_ASSERT(CH(beg) == _T('<'));

  if (off + 4 >= lines[0].end)
    return false;
  if (CH(off + 1) != _T('!') || CH(off + 2) != _T('-') ||
      CH(off + 3) != _T('-'))
    return false;
  off += 4;

  /* ">" and "->" must not follow the opening. */
  if (off < lines[0].end && CH(off) == _T('>'))
    return false;
  if (off + 1 < lines[0].end && CH(off) == _T('-') && CH(off + 1) == _T('>'))
    return false;

  /* HTML comment must not contain "--", so we scan just for "--" instead
   * of "-->" and verify manually that '>' follows. */
  if (md_scan_for_html_closer(ctx, _T("--"), lines, off, max_end, p_end,
                              &ctx.html_comment_horizon)) {
    if (*p_end < max_end && CH(*p_end) == _T('>')) {
      *p_end = *p_end + 1;
      return true;
    }
  }

  return false;
}

static int md_is_html_processing_instruction(Parsing_Context &ctx,
                                             std::span<Line> lines, OFF beg,
                                             OFF max_end, OFF *p_end) {
  OFF off = beg;

  if (off + 2 >= lines[0].end)
    return false;
  if (CH(off + 1) != _T('?'))
    return false;
  off += 2;

  return md_scan_for_html_closer(ctx, _T("?>"), lines, off, max_end, p_end,
                                 &ctx.html_proc_instr_horizon);
}

static int md_is_html_declaration(Parsing_Context &ctx, std::span<Line> lines,
                                  OFF beg, OFF max_end, OFF *p_end) {
  OFF off = beg;

  if (off + 2 >= lines[0].end)
    return false;
  if (CH(off + 1) != _T('!'))
    return false;
  off += 2;

  /* Declaration name. */
  if (off >= lines[0].end || !ISALPHA(off))
    return false;
  off++;
  while (off < lines[0].end && ISALPHA(off))
    off++;
  if (off < lines[0].end && !ISWHITESPACE(off))
    return false;

  return md_scan_for_html_closer(ctx, _T(">"), lines, off, max_end, p_end,
                                 &ctx.html_decl_horizon);
}

static int md_is_html_cdata(Parsing_Context &ctx, std::span<Line> lines,
                            OFF beg, OFF max_end, OFF *p_end) {
  static const mdstringview open_str{_T("<![CDATA[")};

  OFF off = beg;

  if (off + open_str.size() >= lines[0].end)
    return false;
  if (STR(off) != open_str)
    return false;
  off += open_str.size();

  if (lines[lines.size() - 1].end < max_end)
    max_end = lines[lines.size() - 1].end - 2;

  return md_scan_for_html_closer(ctx, _T("]]>"), lines, off, max_end, p_end,
                                 &ctx.html_cdata_horizon);
}

static int md_is_html_any(Parsing_Context &ctx, std::span<Line> lines, OFF beg,
                          OFF max_end, OFF *p_end) {
  MD_ASSERT(CH(beg) == _T('<'));
  return (md_is_html_tag(ctx, lines, beg, max_end, p_end) ||
          md_is_html_comment(ctx, lines, beg, max_end, p_end) ||
          md_is_html_processing_instruction(ctx, lines, beg, max_end, p_end) ||
          md_is_html_declaration(ctx, lines, beg, max_end, p_end) ||
          md_is_html_cdata(ctx, lines, beg, max_end, p_end));
}

/****************************
 ***  Recognizing Entity  ***
 ****************************/

static bool md_is_hex_entity_contents(mdstringview text, OFF beg, OFF max_end,
                                      OFF *p_end) {
  OFF off = beg;

  while (off < max_end && ISXDIGIT_(text[off]) && off - beg <= 8)
    off++;

  if (1 <= off - beg && off - beg <= 6) {
    *p_end = off;
    return true;
  } else
    return false;
}

static bool md_is_dec_entity_contents(mdstringview text, OFF beg, OFF max_end,
                                      OFF *p_end) {
  OFF off = beg;

  while (off < max_end && ISDIGIT_(text[off]) && off - beg <= 8)
    off++;

  if (1 <= off - beg && off - beg <= 7) {
    *p_end = off;
    return true;
  } else {
    return false;
  }
}

static bool md_is_named_entity_contents(mdstringview text, OFF beg, OFF max_end,
                                        OFF *p_end) {
  OFF off = beg;

  if (off < max_end && ISALPHA_(text[off]))
    off++;
  else
    return false;

  while (off < max_end && ISALNUM_(text[off]) && off - beg <= 48)
    off++;

  if (2 <= off - beg && off - beg <= 48) {
    *p_end = off;
    return true;
  } else {
    return false;
  }
}

static bool md_is_entity(mdstringview text, OFF beg, OFF max_end, OFF *p_end) {
  int is_contents;
  OFF off = beg;

  MD_ASSERT(text[off] == _T('&'));
  off++;

  if (off + 2 < max_end && text[off] == _T('#') &&
      (text[off + 1] == _T('x') || text[off + 1] == _T('X')))
    is_contents = md_is_hex_entity_contents(text, off + 2, max_end, &off);
  else if (off + 1 < max_end && text[off] == _T('#'))
    is_contents = md_is_dec_entity_contents(text, off + 1, max_end, &off);
  else
    is_contents = md_is_named_entity_contents(text, off, max_end, &off);

  if (is_contents && off < max_end && text[off] == _T(';')) {
    *p_end = off + 1;
    return true;
  } else {
    return false;
  }
}

/******************************
 ***  Attribute Management  ***
 ******************************/
/*
struct Attribute_Build {
 CHAR *text;
 MD_TEXTTYPE *substr_types;
 OFF *substr_offsets;
 int substr_count, substr_alloc;
 MD_TEXTTYPE trivial_types[1];
 OFF trivial_offsets[2];
};*/

class Attribute_Build {
  mdstring text;
  std::vector<TextType> substr_types;
  std::vector<OFF> substr_offsets;
  TextType trivial_type;
  OFF trivial_offsets[2];
  int append_substr(Parsing_Context &, TextType, OFF);

public:
  int build(Parsing_Context &, mdstring, unsigned, Attribute &);
};

#define MD_BUILD_ATTR_NO_ESCAPES 0x0001

int Attribute_Build::append_substr(Parsing_Context &ctx, TextType type,
                                   OFF off) {
  try {
    substr_types.emplace_back(type);
    substr_offsets.emplace_back(off);
  } catch (const std::bad_alloc &e) {
    MD_LOG(mdstring(vector_emplace_back_str) + mdstring(e.what()));
    return -1;
  }
  return 0;
}

int Attribute_Build::build(Parsing_Context &ctx, mdstring t, unsigned flags,
                           Attribute &attr) {

  /* If there is no backslash and no ampersand, build attribute as trivial.*/
  bool is_trivial = true;
  int ret;
  for (const auto ch : t)
    if (ISANYOF3_(ch, _T('\\'), _T('&'), _T('\0'))) {
      is_trivial = false;
      break;
    }
  OFF off = is_trivial ? t.size() : 0;
  if (is_trivial) {
    text = t;
    substr_types = {trivial_type};
    substr_offsets = {trivial_offsets[0], trivial_offsets[1]};
    trivial_offsets[0] = 0;
    trivial_type = TextType::normal;
    trivial_offsets[1] = t.size();
  } else {
    OFF raw_off = 0;
    while (raw_off < t.size()) {
      if (t[raw_off] == _T('\0')) {
        MD_CHECK(Attribute_Build::append_substr(ctx, TextType::null_char, off));
        off++;
        raw_off++;
        continue;
      }

      if (t[raw_off] == _T('&')) {
        OFF ent_end;

        if (md_is_entity(t, raw_off, t.size(), &ent_end)) {
          MD_CHECK(Attribute_Build::append_substr(ctx, TextType::entity, off));
          off += ent_end - raw_off;
          raw_off = ent_end;
          continue;
        }
      }

      if (substr_offsets.size() == 0 || substr_types.back() != TextType::normal)
        MD_CHECK(Attribute_Build::append_substr(ctx, TextType::normal, off));

      if (!(flags & MD_BUILD_ATTR_NO_ESCAPES) && t[raw_off] == _T('\\') &&
          raw_off + 1 < t.size() &&
          (ISPUNCT_(t[raw_off + 1]) || ISNEWLINE_(t[raw_off + 1])))
        raw_off++;

      text[off++] = t[raw_off++];
    }
    substr_offsets.back() = off;
  }

  attr.text = text;
  attr.substr_offsets = substr_offsets;
  attr.substr_types = substr_types;
  return 0;
abort:
  return -1;
}

/*
static void md_free_attribute(Attribute_Build *build) {
  if (build->substr_alloc > 0) {
    free(build->text);
    free(build->substr_types);
    free(build->substr_offsets);
  }
}

static int md_build_attribute(Parsing_Context &ctx, mdstringview raw_text,
                              unsigned flags, Attribute *attr,
                              Attribute_Build *build) {
  int ret = 0;
  OFF off;

  memset(build, 0, sizeof(Attribute_Build));

  bool is_trivial = true;
  for (const auto ch : raw_text)
    if (ISANYOF3_(ch, _T('\\'), _T('&'), _T('\0'))) {
      is_trivial = false;
      break;
    }

  if (is_trivial) {
    // build->text = (CHAR *)(!raw_text.empty() ? raw_text : nullptr);
    build->substr_types = build->trivial_types;
    build->substr_offsets = build->trivial_offsets;
    build->substr_count = 1;
    build->substr_alloc = 0;
    build->trivial_types[0] = TextType::normal;
    build->trivial_offsets[0] = 0;
    build->trivial_offsets[1] = raw_text.size();
    off = raw_text.size();
  } else {
    build->text = (CHAR *)malloc(raw_text.size() * sizeof(CHAR));
    if (build->text == nullptr) {
      MD_LOG("malloc() failed.");
      goto abort;
    }

    OFF raw_off = 0;
    off = 0;

    while (raw_off < raw_text.size()) {
      if (raw_text[raw_off] == _T('\0')) {
        MD_CHECK(
            md_build_attr_append_substr(ctx, build, TextType::null_char, off));
        // memcpy(build->text + off, raw_text + raw_off, 1);
        off++;
        raw_off++;
        continue;
      }

      if (raw_text[raw_off] == _T('&')) {
        OFF ent_end;

        if (md_is_entity(raw_text, raw_off, raw_text.size(), &ent_end)) {
          MD_CHECK(
              md_build_attr_append_substr(ctx, build, TextType::entity, off));
          // memcpy(build->text + off, raw_text + raw_off, ent_end - raw_off);
          off += ent_end - raw_off;
          raw_off = ent_end;
          continue;
        }
      }

      if (build->substr_count == 0 ||
          build->substr_types[build->substr_count - 1] != TextType::normal)
        MD_CHECK(
            md_build_attr_append_substr(ctx, build, TextType::normal, off));

      if (!(flags & MD_BUILD_ATTR_NO_ESCAPES) &&
          raw_text[raw_off] == _T('\\') && raw_off + 1 < raw_text.size() &&
          (ISPUNCT_(raw_text[raw_off + 1]) ||
           ISNEWLINE_(raw_text[raw_off + 1])))
        raw_off++;

      build->text[off++] = raw_text[raw_off++];
    }
    build->substr_offsets[build->substr_count] = off;
  }

  attr->text = build->text;
  attr->size = off;
  attr->substr_offsets = build->substr_offsets;
  attr->substr_types = build->substr_types;
  return 0;

abort:
  md_free_attribute(build);
  return -1;
}*/

/*********************************************
 ***  Dictionary of Reference Definitions  ***
 *********************************************/

static constexpr auto MD_FNV1A_BASE = 2166136261U;
static constexpr auto MD_FNV1A_PRIME = 16777619U;

template <class T>
static inline unsigned md_fnv1a(unsigned base, const T &data) {
  unsigned hash = base;
  for (const auto e : data) {
    hash ^= e;
    hash *= MD_FNV1A_PRIME;
  }
  return hash;
}

struct MD_REF_DEF_tag {
  mdstring label, title;
  unsigned hash;
  OFF dest_beg;
  OFF dest_end;
  bool label_needs_free;
  bool title_needs_free;
};

/* Label equivalence is quite complicated with regards to whitespace and case
 * folding. This complicates computing a hash of it as well as direct comparison
 * of two labels. */

static unsigned md_link_label_hash(mdstringview label) {
  unsigned hash = MD_FNV1A_BASE;
  OFF off;
  unsigned codepoint;
  int is_whitespace = false;

  off = md_skip_unicode_whitespace(label, 0);
  while (off < label.size()) {
    SZ char_size;

    codepoint = md_decode_unicode(label, off, &char_size);
    is_whitespace = ISUNICODEWHITESPACE_(codepoint) || ISNEWLINE_(label[off]);

    if (is_whitespace) {
      codepoint = ' ';
      unsigned dummy[1]{codepoint};
      hash = md_fnv1a(hash, dummy);
      off = md_skip_unicode_whitespace(label, off);
    } else {
      Unicode_Fold_Info fold_info;

      md_get_unicode_fold_info(codepoint, &fold_info);
      hash = md_fnv1a(hash, fold_info.codepoints);
      off += char_size;
    }
  }

  return hash;
}

static OFF md_link_label_cmp_load_fold_info(mdstringview label, OFF off,
                                            Unicode_Fold_Info *fold_info) {
  unsigned codepoint;
  SZ char_size;

  if (off >= label.size()) {
    /* Treat end of a link label as a whitespace. */
    goto whitespace;
  }

  codepoint = md_decode_unicode(label, off, &char_size);
  off += char_size;
  if (ISUNICODEWHITESPACE_(codepoint)) {
    /* Treat all whitespace as equivalent */
    goto whitespace;
  }

  /* Get real folding info. */
  md_get_unicode_fold_info(codepoint, fold_info);
  return off;

whitespace:
  fold_info->codepoints[0] = _T(' ');
  fold_info->codepoints.resize(1);
  return md_skip_unicode_whitespace(label, off);
}

static int md_link_label_cmp(mdstringview a_label, mdstringview b_label) {
  OFF a_off;
  OFF b_off;
  Unicode_Fold_Info a_fi{}, b_fi{};
  OFF a_fi_off = 0;
  OFF b_fi_off = 0;
  int cmp;

  a_off = md_skip_unicode_whitespace(a_label, 0);
  b_off = md_skip_unicode_whitespace(b_label, 0);
  while (a_off < a_label.size() || a_fi_off < a_fi.codepoints.size() ||
         b_off < b_label.size() || b_fi_off < b_fi.codepoints.size()) {
    /* If needed, load fold info for next char. */
    if (a_fi_off >= a_fi.codepoints.size()) {
      a_fi_off = 0;
      a_off = md_link_label_cmp_load_fold_info(a_label, a_off, &a_fi);
    }
    if (b_fi_off >= b_fi.codepoints.size()) {
      b_fi_off = 0;
      b_off = md_link_label_cmp_load_fold_info(b_label, b_off, &b_fi);
    }

    cmp = b_fi.codepoints[b_fi_off] - a_fi.codepoints[a_fi_off];
    if (cmp != 0)
      return cmp;

    a_fi_off++;
    b_fi_off++;
  }

  return 0;
}

typedef struct MD_REF_DEF_LIST_tag Ref_Def_List;
struct MD_REF_DEF_LIST_tag {
  int n_ref_defs;
  int alloc_ref_defs;
  Ref_Def *ref_defs[]; /* Valid items always  point into ctx.ref_defs[] */
};

static int md_ref_def_cmp(const void *a, const void *b) {
  const Ref_Def *a_ref = *(const Ref_Def **)a;
  const Ref_Def *b_ref = *(const Ref_Def **)b;

  if (a_ref->hash < b_ref->hash)
    return -1;
  else if (a_ref->hash > b_ref->hash)
    return +1;
  else
    return md_link_label_cmp(a_ref->label, b_ref->label);
}

static int md_ref_def_cmp_for_sort(const void *a, const void *b) {
  int cmp;

  cmp = md_ref_def_cmp(a, b);

  /* Ensure stability of the sorting. */
  if (cmp == 0) {
    const Ref_Def *a_ref = *(const Ref_Def **)a;
    const Ref_Def *b_ref = *(const Ref_Def **)b;

    if (a_ref < b_ref)
      cmp = -1;
    else if (a_ref > b_ref)
      cmp = +1;
    else
      cmp = 0;
  }

  return cmp;
}

static int md_build_ref_def_hashtable(Parsing_Context &ctx) {
  int i, j;

  if (ctx.n_ref_defs == 0)
    return 0;

  ctx.ref_def_hashtable_size = (ctx.n_ref_defs * 5) / 4;
  ctx.ref_def_hashtable =
      static_cast<void **>(malloc(ctx.ref_def_hashtable_size * sizeof(void *)));
  if (ctx.ref_def_hashtable == nullptr) {
    MD_LOG("malloc() failed.");
    goto abort;
  }
  memset(ctx.ref_def_hashtable, 0, ctx.ref_def_hashtable_size * sizeof(void *));

  /* Each member of ctx.ref_def_hashtable[] can be:
   *  -- nullptr,
   *  -- pointer to the MD_REF_DEF in ctx.ref_defs[], or
   *  -- pointer to a MD_REF_DEF_LIST, which holds multiple pointers to
   *     such MD_REF_DEFs.
   */
  for (i = 0; i < ctx.n_ref_defs; i++) {
    Ref_Def *def = &ctx.ref_defs[i];
    void *bucket;
    Ref_Def_List *list;

    def->hash = md_link_label_hash(def->label);
    bucket = ctx.ref_def_hashtable[def->hash % ctx.ref_def_hashtable_size];

    if (bucket == nullptr) {
      /* The bucket is empty. Make it just point to the def. */
      ctx.ref_def_hashtable[def->hash % ctx.ref_def_hashtable_size] = def;
      continue;
    }

    if (ctx.ref_defs <= (Ref_Def *)bucket &&
        (Ref_Def *)bucket < ctx.ref_defs + ctx.n_ref_defs) {
      /* The bucket already contains one ref. def. Lets see whether it
       * is the same label (ref. def. duplicate) or different one
       * (hash conflict). */
      Ref_Def *old_def = (Ref_Def *)bucket;

      if (md_link_label_cmp(def->label, old_def->label) == 0) {
        /* Duplicate label: Ignore this ref. def. */
        continue;
      }

      /* Make the bucket complex, i.e. able to hold more ref. defs. */
      list =
          (Ref_Def_List *)malloc(sizeof(Ref_Def_List) + 2 * sizeof(Ref_Def *));
      if (list == nullptr) {
        MD_LOG("malloc() failed.");
        goto abort;
      }
      list->ref_defs[0] = old_def;
      list->ref_defs[1] = def;
      list->n_ref_defs = 2;
      list->alloc_ref_defs = 2;
      ctx.ref_def_hashtable[def->hash % ctx.ref_def_hashtable_size] = list;
      continue;
    }

    /* Append the def to the complex bucket list.
     *
     * Note in this case we ignore potential duplicates to avoid expensive
     * iterating over the complex bucket. Below, we revisit all the complex
     * buckets and handle it more cheaply after the complex bucket contents
     * is sorted. */
    list = (Ref_Def_List *)bucket;
    if (list->n_ref_defs >= list->alloc_ref_defs) {
      int alloc_ref_defs = list->alloc_ref_defs + list->alloc_ref_defs / 2;
      Ref_Def_List *list_tmp = (Ref_Def_List *)realloc(
          list, sizeof(Ref_Def_List) + alloc_ref_defs * sizeof(Ref_Def *));
      if (list_tmp == nullptr) {
        MD_LOG("realloc() failed.");
        goto abort;
      }
      list = list_tmp;
      list->alloc_ref_defs = alloc_ref_defs;
      ctx.ref_def_hashtable[def->hash % ctx.ref_def_hashtable_size] = list;
    }

    list->ref_defs[list->n_ref_defs] = def;
    list->n_ref_defs++;
  }

  /* Sort the complex buckets so we can use bsearch() with them. */
  for (i = 0; i < ctx.ref_def_hashtable_size; i++) {
    void *bucket = ctx.ref_def_hashtable[i];
    Ref_Def_List *list;

    if (bucket == nullptr)
      continue;
    if (ctx.ref_defs <= (Ref_Def *)bucket &&
        (Ref_Def *)bucket < ctx.ref_defs + ctx.n_ref_defs)
      continue;

    list = (Ref_Def_List *)bucket;
    qsort(list->ref_defs, list->n_ref_defs, sizeof(Ref_Def *),
          md_ref_def_cmp_for_sort);

    /* Disable all duplicates in the complex bucket by forcing all such
     * records to point to the 1st such ref. def. I.e. no matter which
     * record is found during the lookup, it will always point to the right
     * ref. def. in ctx.ref_defs[]. */
    for (j = 1; j < list->n_ref_defs; j++) {
      if (md_ref_def_cmp(&list->ref_defs[j - 1], &list->ref_defs[j]) == 0)
        list->ref_defs[j] = list->ref_defs[j - 1];
    }
  }

  return 0;

abort:
  return -1;
}

static void md_free_ref_def_hashtable(Parsing_Context &ctx) {
  if (ctx.ref_def_hashtable != nullptr) {
    int i;

    for (i = 0; i < ctx.ref_def_hashtable_size; i++) {
      void *bucket = ctx.ref_def_hashtable[i];
      if (bucket == nullptr)
        continue;
      if (ctx.ref_defs <= (Ref_Def *)bucket &&
          (Ref_Def *)bucket < ctx.ref_defs + ctx.n_ref_defs)
        continue;
      free(bucket);
    }

    free(ctx.ref_def_hashtable);
  }
}

static const Ref_Def *md_lookup_ref_def(Parsing_Context &ctx,
                                        mdstringview label) {
  unsigned hash;
  void *bucket;

  if (ctx.ref_def_hashtable_size == 0)
    return nullptr;

  hash = md_link_label_hash(label);
  bucket = ctx.ref_def_hashtable[hash % ctx.ref_def_hashtable_size];

  if (bucket == nullptr) {
    return nullptr;
  } else if (ctx.ref_defs <= (Ref_Def *)bucket &&
             (Ref_Def *)bucket < ctx.ref_defs + ctx.n_ref_defs) {
    const Ref_Def *def = (Ref_Def *)bucket;

    if (md_link_label_cmp(def->label, label) == 0)
      return def;
    else
      return nullptr;
  } else {
    Ref_Def_List *list = (Ref_Def_List *)bucket;
    Ref_Def key_buf;
    const Ref_Def *key = &key_buf;
    const Ref_Def **ret;

    key_buf.label = mdstring(label);
    key_buf.hash = md_link_label_hash(key_buf.label);

    ret = (const Ref_Def **)bsearch(&key, list->ref_defs, list->n_ref_defs,
                                    sizeof(Ref_Def *), md_ref_def_cmp);
    if (ret != nullptr)
      return *ret;
    else
      return nullptr;
  }
}

/***************************
 ***  Recognizing Links  ***
 ***************************/

/* Note this code is partially shared between processing inlines and blocks
 * as reference definitions and links share some helper parser functions.
 */

typedef struct MD_LINK_ATTR_tag MD_LINK_ATTR;
struct MD_LINK_ATTR_tag {
  OFF dest_beg;
  OFF dest_end;

  mdstring title;
  bool title_needs_free;
};

static bool md_is_link_label(Parsing_Context &ctx, std::span<Line> lines,
                             OFF beg, OFF *p_end, unsigned *p_beg_line_index,
                             unsigned *p_end_line_index, OFF *p_contents_beg,
                             OFF *p_contents_end) {
  OFF off = beg;
  OFF contents_beg = 0;
  OFF contents_end = 0;
  size_t line_index = 0;
  int len = 0;

  if (CH(off) != _T('['))
    return false;
  off++;

  while (1) {
    OFF line_end = lines[line_index].end;

    while (off < line_end) {
      if (CH(off) == _T('\\') && off + 1 < ctx.text.size() &&
          (ISPUNCT(off + 1) || ISNEWLINE(off + 1))) {
        if (contents_end == 0) {
          contents_beg = off;
          *p_beg_line_index = line_index;
        }
        contents_end = off + 2;
        off += 2;
      } else if (CH(off) == _T('[')) {
        return false;
      } else if (CH(off) == _T(']')) {
        if (contents_beg < contents_end) {
          /* Success. */
          *p_contents_beg = contents_beg;
          *p_contents_end = contents_end;
          *p_end = off + 1;
          *p_end_line_index = line_index;
          return true;
        } else {
          /* Link label must have some non-whitespace contents. */
          return false;
        }
      } else {
        unsigned codepoint;
        SZ char_size;

        codepoint = md_decode_unicode(ctx.text, off, &char_size);
        if (!ISUNICODEWHITESPACE_(codepoint)) {
          if (contents_end == 0) {
            contents_beg = off;
            *p_beg_line_index = line_index;
          }
          contents_end = off + char_size;
        }

        off += char_size;
      }

      len++;
      if (len > 999)
        return false;
    }

    line_index++;
    len++;
    if (line_index < lines.size())
      off = lines[line_index].beg;
    else
      break;
  }

  return false;
}

static int md_is_link_destination_A(Parsing_Context &ctx, OFF beg, OFF max_end,
                                    OFF *p_end, OFF *p_contents_beg,
                                    OFF *p_contents_end) {
  OFF off = beg;

  if (off >= max_end || CH(off) != _T('<'))
    return false;
  off++;

  while (off < max_end) {
    if (CH(off) == _T('\\') && off + 1 < max_end && ISPUNCT(off + 1)) {
      off += 2;
      continue;
    }

    if (ISNEWLINE(off) || CH(off) == _T('<'))
      return false;

    if (CH(off) == _T('>')) {
      /* Success. */
      *p_contents_beg = beg + 1;
      *p_contents_end = off;
      *p_end = off + 1;
      return true;
    }

    off++;
  }

  return false;
}

static int md_is_link_destination_B(Parsing_Context &ctx, OFF beg, OFF max_end,
                                    OFF *p_end, OFF *p_contents_beg,
                                    OFF *p_contents_end) {
  OFF off = beg;
  int parenthesis_level = 0;

  while (off < max_end) {
    if (CH(off) == _T('\\') && off + 1 < max_end && ISPUNCT(off + 1)) {
      off += 2;
      continue;
    }

    if (ISWHITESPACE(off) || ISCNTRL(off))
      break;

    /* Link destination may include balanced pairs of unescaped '(' ')'.
     * Note we limit the maximal nesting level by 32 to protect us from
     * https://github.com/jgm/cmark/issues/214 */
    if (CH(off) == _T('(')) {
      parenthesis_level++;
      if (parenthesis_level > 32)
        return false;
    } else if (CH(off) == _T(')')) {
      if (parenthesis_level == 0)
        break;
      parenthesis_level--;
    }

    off++;
  }

  if (parenthesis_level != 0 || off == beg)
    return false;

  /* Success. */
  *p_contents_beg = beg;
  *p_contents_end = off;
  *p_end = off;
  return true;
}

static inline int md_is_link_destination(Parsing_Context &ctx, OFF beg,
                                         OFF max_end, OFF *p_end,
                                         OFF *p_contents_beg,
                                         OFF *p_contents_end) {
  if (CH(beg) == _T('<'))
    return md_is_link_destination_A(ctx, beg, max_end, p_end, p_contents_beg,
                                    p_contents_end);
  else
    return md_is_link_destination_B(ctx, beg, max_end, p_end, p_contents_beg,
                                    p_contents_end);
}

static int md_is_link_title(Parsing_Context &ctx, std::span<Line> lines,
                            OFF beg, OFF *p_end, unsigned *p_beg_line_index,
                            unsigned *p_end_line_index, OFF *p_contents_beg,
                            OFF *p_contents_end) {
  OFF off = beg;
  CHAR closer_char;
  size_t line_index = 0;

  /* White space with up to one line break. */
  while (off < lines[line_index].end && ISWHITESPACE(off))
    off++;
  if (off >= lines[line_index].end) {
    line_index++;
    if (line_index >= lines.size())
      return false;
    off = lines[line_index].beg;
  }
  if (off == beg)
    return false;

  *p_beg_line_index = line_index;

  /* First char determines how to detect end of it. */
  switch (CH(off)) {
  case _T('"'):
    closer_char = _T('"');
    break;
  case _T('\''):
    closer_char = _T('\'');
    break;
  case _T('('):
    closer_char = _T(')');
    break;
  default:
    return false;
  }
  off++;

  *p_contents_beg = off;

  while (line_index < lines.size()) {
    OFF line_end = lines[line_index].end;

    while (off < line_end) {
      if (CH(off) == _T('\\') && off + 1 < ctx.text.size() &&
          (ISPUNCT(off + 1) || ISNEWLINE(off + 1))) {
        off++;
      } else if (CH(off) == closer_char) {
        /* Success. */
        *p_contents_end = off;
        *p_end = off + 1;
        *p_end_line_index = line_index;
        return true;
      } else if (closer_char == _T(')') && CH(off) == _T('(')) {
        /* ()-style title cannot contain (unescaped '(')) */
        return false;
      }

      off++;
    }

    line_index++;
  }

  return false;
}

/* Returns 0 if it is not a reference definition.
 *
 * Returns N > 0 if it is a reference definition. N then corresponds to the
 * number of lines forming it). In this case the definition is stored for
 * resolving any links referring to it.
 *
 * Returns -1 in case of an error (out of memory).
 */
static int md_is_link_reference_definition(Parsing_Context &ctx,
                                           std::span<Line> lines) {
  OFF label_contents_beg;
  OFF label_contents_end;
  unsigned label_contents_line_index = 0;
  bool label_is_multiline = false;
  OFF dest_contents_beg;
  OFF dest_contents_end;
  OFF title_contents_beg;
  OFF title_contents_end;
  unsigned title_contents_line_index;
  bool title_is_multiline = false;
  OFF off;
  unsigned line_index = 0;
  unsigned tmp_line_index;
  Ref_Def *def = nullptr;
  int ret = 0;

  /* Link label. */
  if (!md_is_link_label(ctx, lines, lines[0].beg, &off,
                        &label_contents_line_index, &line_index,
                        &label_contents_beg, &label_contents_end))
    return false;
  label_is_multiline = (label_contents_line_index != line_index);

  /* Colon. */
  if (off >= lines[line_index].end || CH(off) != _T(':'))
    return false;
  off++;

  /* Optional white space with up to one line break. */
  while (off < lines[line_index].end && ISWHITESPACE(off))
    off++;
  if (off >= lines[line_index].end) {
    line_index++;
    if (line_index >= lines.size())
      return false;
    off = lines[line_index].beg;
  }

  /* Link destination. */
  if (!md_is_link_destination(ctx, off, lines[line_index].end, &off,
                              &dest_contents_beg, &dest_contents_end))
    return false;

  /* (Optional) title. Note we interpret it as an title only if nothing
   * more follows on its last line. */
  if (md_is_link_title(ctx, lines.subspan(line_index), off, &off,
                       &title_contents_line_index, &tmp_line_index,
                       &title_contents_beg, &title_contents_end) &&
      off >= lines[line_index + tmp_line_index].end) {
    title_is_multiline = (tmp_line_index != title_contents_line_index);
    title_contents_line_index += line_index;
    line_index += tmp_line_index;
  } else {
    /* Not a title. */
    title_is_multiline = false;
    title_contents_beg = off;
    title_contents_end = off;
    title_contents_line_index = 0;
  }

  /* Nothing more can follow on the last line. */
  if (off < lines[line_index].end)
    return false;

  /* So, it _is_ a reference definition. Remember it. */
  if (ctx.n_ref_defs >= ctx.alloc_ref_defs) {
    Ref_Def *new_defs;

    ctx.alloc_ref_defs =
        (ctx.alloc_ref_defs > 0 ? ctx.alloc_ref_defs + ctx.alloc_ref_defs / 2
                                : 16);
    new_defs =
        (Ref_Def *)realloc(ctx.ref_defs, ctx.alloc_ref_defs * sizeof(Ref_Def));
    if (new_defs == nullptr) {
      MD_LOG("realloc() failed.");
      goto abort;
    }

    ctx.ref_defs = new_defs;
  }
  def = &ctx.ref_defs[ctx.n_ref_defs];
  memset(def, 0, sizeof(Ref_Def));

  if (label_is_multiline) {
    def->label = merge_lines(ctx, label_contents_beg, label_contents_end,
                             lines.subspan(label_contents_line_index), _T(' '));
    def->label_needs_free = true;
  } else {
    def->label = mdstring(STR(label_contents_beg),
                          label_contents_end - label_contents_beg);
  }

  if (title_is_multiline) {
    def->title =
        merge_lines(ctx, title_contents_beg, title_contents_end,
                    lines.subspan(title_contents_line_index), _T('\n'));
    def->title_needs_free = true;
  } else
    def->title = mdstring(STR(title_contents_beg),
                          title_contents_beg - title_contents_beg);

  def->dest_beg = dest_contents_beg;
  def->dest_end = dest_contents_end;

  /* Success. */
  ctx.n_ref_defs++;
  return line_index + 1;

abort:
  /* Failure. */
  return ret;
}

static int md_is_link_reference(Parsing_Context &ctx, std::span<Line> lines,
                                OFF beg, OFF end, MD_LINK_ATTR *attr) {
  const Ref_Def *def;
  mdstring label{};

  MD_ASSERT(CH(beg) == _T('[') || CH(beg) == _T('!'));
  MD_ASSERT(CH(end - 1) == _T(']'));

  beg += (CH(beg) == _T('!') ? 2 : 1);
  end--;

  /* Find lines corresponding to the beg and end positions. */
  MD_ASSERT(lines.front().beg <= beg);
  const auto &beg_line_iter{std::ranges::find_if_not(
      lines, [beg](const Line &line) { return beg >= line.end; })};
  const Line beg_line{*beg_line_iter};

  MD_ASSERT(end <= lines.back().end);
  const Line &end_line = *std::ranges::find_if_not(
      beg_line_iter, lines.end(),
      [end](const Line &line) { return end >= line.end; });

  if (beg_line != end_line) {
    label = merge_lines(ctx, beg, end, std::span(beg_line_iter, lines.end()),
                        _T(' '));
  } else {
    label = ctx.text.substr(beg, end - beg);
  }

  def = md_lookup_ref_def(ctx, label);
  if (def != nullptr) {
    attr->dest_beg = def->dest_beg;
    attr->dest_end = def->dest_end;
    attr->title = def->title;
    attr->title_needs_free = false;
  }

  if (beg_line != end_line)
    label.clear();
  // free(label);

  return (def != nullptr);
}

static bool md_is_inline_link_spec(Parsing_Context &ctx, std::span<Line> lines,
                                   OFF beg, OFF *p_end, MD_LINK_ATTR *attr) {
  size_t line_index = 0;
  unsigned tmp_line_index;
  OFF title_contents_beg;
  OFF title_contents_end;
  unsigned title_contents_line_index;
  int title_is_multiline;
  OFF off = beg;
  int ret = false;

  while (off >= lines[line_index].end)
    line_index++;

  MD_ASSERT(CH(off) == _T('('));
  off++;

  /* Optional white space with up to one line break. */
  while (off < lines[line_index].end && ISWHITESPACE(off))
    off++;
  if (off >= lines[line_index].end && ISNEWLINE(off)) {
    line_index++;
    if (line_index >= lines.size())
      return false;
    off = lines[line_index].beg;
  }

  /* Link destination may be omitted, but only when not also having a title. */
  if (off < ctx.text.size() && CH(off) == _T(')')) {
    attr->dest_beg = off;
    attr->dest_end = off;
    attr->title.clear();
    attr->title_needs_free = false;
    off++;
    *p_end = off;
    return true;
  }

  /* Link destination. */
  if (!md_is_link_destination(ctx, off, lines[line_index].end, &off,
                              &attr->dest_beg, &attr->dest_end))
    return false;

  /* (Optional) title. */
  if (md_is_link_title(ctx, lines.subspan(line_index), off, &off,
                       &title_contents_line_index, &tmp_line_index,
                       &title_contents_beg, &title_contents_end)) {
    title_is_multiline = (tmp_line_index != title_contents_line_index);
    title_contents_line_index += line_index;
    line_index += tmp_line_index;
  } else {
    /* Not a title. */
    title_is_multiline = false;
    title_contents_beg = off;
    title_contents_end = off;
    title_contents_line_index = 0;
  }

  /* Optional whitespace followed with final ')'. */
  while (off < lines[line_index].end && ISWHITESPACE(off))
    off++;
  if (off >= lines[line_index].end && ISNEWLINE(off)) {
    line_index++;
    if (line_index >= lines.size())
      return false;
    off = lines[line_index].beg;
  }
  if (CH(off) != _T(')'))
    goto abort;
  off++;

  if (title_contents_beg >= title_contents_end) {
    attr->title.clear();
    attr->title_needs_free = false;
  } else if (!title_is_multiline) {
    attr->title = mdstring(STR(title_contents_beg),
                           title_contents_end - title_contents_beg);
    attr->title_needs_free = false;
  } else {
    attr->title =
        merge_lines(ctx, title_contents_beg, title_contents_end,
                    lines.subspan(title_contents_line_index), _T('\n'));
    attr->title_needs_free = true;
  }

  *p_end = off;
  ret = true;

abort:
  return ret;
}

static void md_free_ref_defs(Parsing_Context &ctx) { free(ctx.ref_defs); }

/******************************************
 ***  Processing Inlines (a.k.a Spans)  ***
 ******************************************/

/* We process inlines in few phases:
 *
 * (1) We go through the block text and collect all significant characters
 *     which may start/end a span or some other significant position into
 *     ctx.marks[]. Core of this is what md_collect_marks() does.
 *
 *     We also do some very brief preliminary context-less analysis, whether
 *     it might be opener or closer (e.g. of an emphasis span).
 *
 *     This speeds the other steps as we do not need to re-iterate over all
 *     characters anymore.
 *
 * (2) We analyze each potential mark types, in order by their precedence.
 *
 *     In each md_analyze_XXX() function, we re-iterate list of the marks,
 *     skipping already resolved regions (in preceding precedences) and try to
 *     resolve them.
 *
 * (2.1) For trivial marks, which are single (e.g. HTML entity), we just mark
 *       them as resolved.
 *
 * (2.2) For range-type marks, we analyze whether the mark could be closer
 *       and, if yes, whether there is some preceding opener it could satisfy.
 *
 *       If not we check whether it could be really an opener and if yes, we
 *       remember it so subsequent closers may resolve it.
 *
 * (3) Finally, when all marks were analyzed, we render the block contents
 *     by calling MD_RENDERER::text() callback, interrupting by ::enter_span()
 *     or ::close_span() whenever we reach a resolved mark.
 */

/* The mark structure.
 *
 * '\\': Maybe escape sequence.
 * '\0': nullptr char.
 *  '*': Maybe (strong) emphasis start/end.
 *  '_': Maybe (strong) emphasis start/end.
 *  '~': Maybe strikethrough start/end (needs MD_FLAG_STRIKETHROUGH).
 *  '`': Maybe code span start/end.
 *  '&': Maybe start of entity.
 *  ';': Maybe end of entity.
 *  '<': Maybe start of raw HTML or autolink.
 *  '>': Maybe end of raw HTML or autolink.
 *  '[': Maybe start of link label or link text.
 *  '!': Equivalent of '[' for image.
 *  ']': Maybe end of link label or link text.
 *  '@': Maybe permissive e-mail auto-link (needs
 * MD_FLAG_PERMISSIVEEMAILAUTOLINKS).
 *  ':': Maybe permissive URL auto-link (needs MD_FLAG_PERMISSIVEURLAUTOLINKS).
 *  '.': Maybe permissive WWW auto-link (needs MD_FLAG_PERMISSIVEWWWAUTOLINKS).
 *  'D': Dummy mark, it reserves a space for splitting a previous mark
 *       (e.g. emphasis) or to make more space for storing some special data
 *       related to the preceding mark (e.g. link).
 *
 * Note that not all instances of these chars in the text imply creation of the
 * structure. Only those which have (or may have, after we see more context)
 * the special meaning.
 *
 * (Keep this struct as small as possible to fit as much of them into CPU
 * cache line.)
 */
struct MD_MARK_tag {
  OFF beg;
  OFF end;

  /* For unresolved openers, 'prev' and 'next' form the chain of open openers
   * of given type 'ch'.
   *
   * During resolving, we disconnect from the chain and point to the
   * corresponding counterpart so opener points to its closer and vice versa.
   */
  int prev;
  int next;
  CHAR ch;
  unsigned char flags;
};

/* Mark flags (these apply to ALL mark types). */
#define MD_MARK_POTENTIAL_OPENER 0x01 /* Maybe opener. */
#define MD_MARK_POTENTIAL_CLOSER 0x02 /* Maybe closer. */
#define MD_MARK_OPENER 0x04           /* Definitely opener. */
#define MD_MARK_CLOSER 0x08           /* Definitely closer. */
#define MD_MARK_RESOLVED 0x10         /* Resolved in any definite way. */

/* Mark flags specific for various mark types (so they can share bits). */
#define MD_MARK_EMPH_INTRAWORD 0x20 /* Helper for the "rule of 3". */
#define MD_MARK_EMPH_MOD3_0 0x40
#define MD_MARK_EMPH_MOD3_1 0x80
#define MD_MARK_EMPH_MOD3_2 (0x40 | 0x80)
#define MD_MARK_EMPH_MOD3_MASK (0x40 | 0x80)
#define MD_MARK_AUTOLINK 0x20                /* Distinguisher for '<', '>'. */
#define MD_MARK_VALIDPERMISSIVEAUTOLINK 0x20 /* For permissive autolinks. */

static MarkChain *md_asterisk_chain(Parsing_Context &ctx, unsigned flags) {
  switch (flags & (MD_MARK_EMPH_INTRAWORD | MD_MARK_EMPH_MOD3_MASK)) {
  case MD_MARK_EMPH_INTRAWORD | MD_MARK_EMPH_MOD3_0:
    return &ASTERISK_OPENERS_intraword_mod3_0;
  case MD_MARK_EMPH_INTRAWORD | MD_MARK_EMPH_MOD3_1:
    return &ASTERISK_OPENERS_intraword_mod3_1;
  case MD_MARK_EMPH_INTRAWORD | MD_MARK_EMPH_MOD3_2:
    return &ASTERISK_OPENERS_intraword_mod3_2;
  case MD_MARK_EMPH_MOD3_0:
    return &ASTERISK_OPENERS_extraword_mod3_0;
  case MD_MARK_EMPH_MOD3_1:
    return &ASTERISK_OPENERS_extraword_mod3_1;
  case MD_MARK_EMPH_MOD3_2:
    return &ASTERISK_OPENERS_extraword_mod3_2;
  default:
    MD_UNREACHABLE();
  }
  return nullptr;
}

static MarkChain *md_mark_chain(Parsing_Context &ctx, int mark_index) {
  Mark *mark = &ctx.marks[mark_index];

  switch (mark->ch) {
  case _T('*'):
    return md_asterisk_chain(ctx, mark->flags);
  case _T('_'):
    return &UNDERSCORE_OPENERS;
  case _T('~'):
    return (mark->end - mark->beg == 1) ? &TILDE_OPENERS_1 : &TILDE_OPENERS_2;
  case _T('['):
    return &BRACKET_OPENERS;
  case _T('|'):
    return &TABLECELLBOUNDARIES;
  default:
    return nullptr;
  }
}

static Mark *md_push_mark(Parsing_Context &ctx) {
  if (ctx.n_marks >= ctx.alloc_marks) {
    Mark *new_marks;

    ctx.alloc_marks =
        (ctx.alloc_marks > 0 ? ctx.alloc_marks + ctx.alloc_marks / 2 : 64);
    new_marks = (Mark *)realloc(ctx.marks, ctx.alloc_marks * sizeof(Mark));
    if (new_marks == nullptr) {
      MD_LOG("realloc() failed.");
      return nullptr;
    }

    ctx.marks = new_marks;
  }

  return &ctx.marks[ctx.n_marks++];
}

#define PUSH_MARK_()                                                           \
  do {                                                                         \
    mark = md_push_mark(ctx);                                                  \
    if (mark == nullptr) {                                                     \
      ret = -1;                                                                \
      goto abort;                                                              \
    }                                                                          \
  } while (0)

#define PUSH_MARK(ch_, beg_, end_, flags_)                                     \
  do {                                                                         \
    PUSH_MARK_();                                                              \
    mark->beg = (beg_);                                                        \
    mark->end = (end_);                                                        \
    mark->prev = -1;                                                           \
    mark->next = -1;                                                           \
    mark->ch = (char)(ch_);                                                    \
    mark->flags = (flags_);                                                    \
  } while (0)

static void md_mark_chain_append(Parsing_Context &ctx, MarkChain *chain,
                                 int mark_index) {
  if (chain->tail >= 0)
    ctx.marks[chain->tail].next = mark_index;
  else
    chain->head = mark_index;

  ctx.marks[mark_index].prev = chain->tail;
  ctx.marks[mark_index].next = -1;
  chain->tail = mark_index;
}

/* Sometimes, we need to store a pointer into the mark. It is quite rare
 * so we do not bother to make MD_MARK use union, and it can only happen
 * for dummy marks. */
// XXX: only called with attr.title
static inline void md_mark_store_ptr(Parsing_Context &ctx, int mark_index,
                                     void *ptr) {
  Mark *mark{&ctx.marks[mark_index]};
  MD_ASSERT(mark->ch == 'D');

  /* Check only members beg and end are misused for this. */
  MD_ASSERT(sizeof(void *) <= 2 * sizeof(OFF));
  memcpy(mark, &ptr, sizeof(void *));
}

static inline void *md_mark_get_ptr(Parsing_Context &ctx, int mark_index) {
  void *ptr;
  Mark *mark = &ctx.marks[mark_index];
  MD_ASSERT(mark->ch == 'D');
  memcpy(&ptr, mark, sizeof(void *));
  return ptr;
}

static void md_resolve_range(Parsing_Context &ctx, MarkChain *chain,
                             int opener_index, int closer_index) {
  Mark *opener = &ctx.marks[opener_index];
  Mark *closer = &ctx.marks[closer_index];

  /* Remove opener from the list of openers. */
  if (chain != nullptr) {
    if (opener->prev >= 0)
      ctx.marks[opener->prev].next = opener->next;
    else
      chain->head = opener->next;

    if (opener->next >= 0)
      ctx.marks[opener->next].prev = opener->prev;
    else
      chain->tail = opener->prev;
  }

  /* Interconnect opener and closer and mark both as resolved. */
  opener->next = closer_index;
  opener->flags |= MD_MARK_OPENER | MD_MARK_RESOLVED;
  closer->prev = opener_index;
  closer->flags |= MD_MARK_CLOSER | MD_MARK_RESOLVED;
}

#define MD_ROLLBACK_ALL 0
#define MD_ROLLBACK_CROSSING 1

/* In the range ctx.marks[opener_index] ... [closer_index], undo some or all
 * resolvings accordingly to these rules:
 *
 * (1) All openers BEFORE the range corresponding to any closer inside the
 *     range are un-resolved and they are re-added to their respective chains
 *     of unresolved openers. This ensures we can reuse the opener for closers
 *     AFTER the range.
 *
 * (2) If 'how' is MD_ROLLBACK_ALL, then ALL resolved marks inside the range
 *     are discarded.
 *
 * (3) If 'how' is MD_ROLLBACK_CROSSING, only closers with openers handled
 *     in (1) are discarded. I.e. pairs of openers and closers which are both
 *     inside the range are retained as well as any unpaired marks.
 */
static void md_rollback(Parsing_Context &ctx, int opener_index,
                        int closer_index, int how) {
  int i;
  int mark_index;

  /* Cut all unresolved openers at the mark index. */
  for (i = OPENERS_CHAIN_FIRST; i < OPENERS_CHAIN_LAST + 1; i++) {
    MarkChain *chain = &ctx.mark_chains[i];

    while (chain->tail >= opener_index)
      chain->tail = ctx.marks[chain->tail].prev;

    if (chain->tail >= 0)
      ctx.marks[chain->tail].next = -1;
    else
      chain->head = -1;
  }

  /* Go backwards so that unresolved openers are re-added into their
   * respective chains, in the right order. */
  mark_index = closer_index - 1;
  while (mark_index > opener_index) {
    Mark *mark = &ctx.marks[mark_index];
    int mark_flags = mark->flags;
    int discard_flag = (how == MD_ROLLBACK_ALL);

    if (mark->flags & MD_MARK_CLOSER) {
      int mark_opener_index = mark->prev;

      /* Undo opener BEFORE the range. */
      if (mark_opener_index < opener_index) {
        Mark *mark_opener = &ctx.marks[mark_opener_index];
        MarkChain *chain;

        mark_opener->flags &=
            ~(MD_MARK_OPENER | MD_MARK_CLOSER | MD_MARK_RESOLVED);
        chain = md_mark_chain(ctx, opener_index);
        if (chain != nullptr) {
          md_mark_chain_append(ctx, chain, mark_opener_index);
          discard_flag = 1;
        }
      }
    }

    /* And reset our flags. */
    if (discard_flag)
      mark->flags &= ~(MD_MARK_OPENER | MD_MARK_CLOSER | MD_MARK_RESOLVED);

    /* Jump as far as we can over unresolved or non-interesting marks. */
    switch (how) {
    case MD_ROLLBACK_CROSSING:
      if ((mark_flags & MD_MARK_CLOSER) && mark->prev > opener_index) {
        /* If we are closer with opener INSIDE the range, there may
         * not be any other crosser inside the subrange. */
        mark_index = mark->prev;
        break;
      }
      [[fallthrough]];
    default:
      mark_index--;
      break;
    }
  }
}

static void md_build_mark_char_map(Parsing_Context &ctx) {
  memset(ctx.mark_char_map, 0, sizeof(ctx.mark_char_map));

  ctx.mark_char_map['\\'] = 1;
  ctx.mark_char_map['*'] = 1;
  ctx.mark_char_map['_'] = 1;
  ctx.mark_char_map['`'] = 1;
  ctx.mark_char_map['&'] = 1;
  ctx.mark_char_map[';'] = 1;
  ctx.mark_char_map['<'] = 1;
  ctx.mark_char_map['>'] = 1;
  ctx.mark_char_map['['] = 1;
  ctx.mark_char_map['!'] = 1;
  ctx.mark_char_map[']'] = 1;
  ctx.mark_char_map['\0'] = 1;

  if (ctx.parser.flags & MD_FLAG_STRIKETHROUGH)
    ctx.mark_char_map['~'] = 1;

  if (ctx.parser.flags & MD_FLAG_LATEXMATHSPANS)
    ctx.mark_char_map['$'] = 1;

  if (ctx.parser.flags & MD_FLAG_PERMISSIVEEMAILAUTOLINKS)
    ctx.mark_char_map['@'] = 1;

  if (ctx.parser.flags & MD_FLAG_PERMISSIVEURLAUTOLINKS)
    ctx.mark_char_map[':'] = 1;

  if (ctx.parser.flags & MD_FLAG_PERMISSIVEWWWAUTOLINKS)
    ctx.mark_char_map['.'] = 1;

  if ((ctx.parser.flags & MD_FLAG_TABLES) ||
      (ctx.parser.flags & MD_FLAG_WIKILINKS))
    ctx.mark_char_map['|'] = 1;

  if (ctx.parser.flags & MD_FLAG_COLLAPSEWHITESPACE)
    for (unsigned i = 0; i < sizeof(ctx.mark_char_map); i++)
      if (ISWHITESPACE_(i))
        ctx.mark_char_map[i] = 1;
}

/* We limit code span marks to lower than 32 backticks. This solves the
 * pathologic case of too many openers, each of different length: Their
 * resolving would be then O(n^2). */
#define CODESPAN_MARK_MAXLEN 32

static int md_is_code_span(Parsing_Context &ctx, std::span<Line> lines, OFF beg,
                           OFF *p_opener_beg, OFF *p_opener_end,
                           OFF *p_closer_beg, OFF *p_closer_end,
                           OFF last_potential_closers[CODESPAN_MARK_MAXLEN],
                           bool p_reached_paragraph_end) {
  OFF opener_beg = beg;
  OFF opener_end;
  OFF closer_beg;
  OFF closer_end;
  SZ mark_len;
  OFF line_end;
  int has_space_after_opener = false;
  int has_eol_after_opener = false;
  int has_space_before_closer = false;
  int has_eol_before_closer = false;
  int has_only_space = true;
  size_t line_index = 0;

  line_end = lines[0].end;
  opener_end = opener_beg;
  while (opener_end < line_end && CH(opener_end) == _T('`'))
    opener_end++;
  has_space_after_opener = (opener_end < line_end && CH(opener_end) == _T(' '));
  has_eol_after_opener = (opener_end == line_end);

  /* The caller needs to know end of the opening mark even if we fail. */
  *p_opener_end = opener_end;

  mark_len = opener_end - opener_beg;
  if (mark_len > CODESPAN_MARK_MAXLEN)
    return false;

  /* Check whether we already know there is no closer of this length.
   * If so, re-scan does no sense. This fixes issue #59. */
  if (last_potential_closers[mark_len - 1] >= lines[lines.size() - 1].end ||
      (p_reached_paragraph_end &&
       last_potential_closers[mark_len - 1] < opener_end))
    return false;

  closer_beg = opener_end;
  closer_end = opener_end;

  /* Find closer mark. */
  while (true) {
    while (closer_beg < line_end && CH(closer_beg) != _T('`')) {
      if (CH(closer_beg) != _T(' '))
        has_only_space = false;
      closer_beg++;
    }
    closer_end = closer_beg;
    while (closer_end < line_end && CH(closer_end) == _T('`'))
      closer_end++;

    if (closer_end - closer_beg == mark_len) {
      /* Success. */
      has_space_before_closer =
          (closer_beg > lines[line_index].beg && CH(closer_beg - 1) == _T(' '));
      has_eol_before_closer = (closer_beg == lines[line_index].beg);
      break;
    }

    if (closer_end - closer_beg > 0) {
      /* We have found a back-tick which is not part of the closer. */
      has_only_space = false;

      /* But if we eventually fail, remember it as a potential closer
       * of its own length for future attempts. This mitigates needs for
       * rescans. */
      if (closer_end - closer_beg < CODESPAN_MARK_MAXLEN) {
        if (closer_beg > last_potential_closers[closer_end - closer_beg - 1])
          last_potential_closers[closer_end - closer_beg - 1] = closer_beg;
      }
    }

    if (closer_end >= line_end) {
      line_index++;
      if (line_index >= lines.size()) {
        /* Reached end of the paragraph and still nothing. */
        p_reached_paragraph_end = true;
        return false;
      }
      /* Try on the next line. */
      line_end = lines[line_index].end;
      closer_beg = lines[line_index].beg;
    } else {
      closer_beg = closer_end;
    }
  }

  /* If there is a space or a new line both after and before the opener
   * (and if the code span is not made of spaces only), consume one initial
   * and one trailing space as part of the marks. */
  if (!has_only_space && (has_space_after_opener || has_eol_after_opener) &&
      (has_space_before_closer || has_eol_before_closer)) {
    if (has_space_after_opener)
      opener_end++;
    else
      opener_end = lines[1].beg;

    if (has_space_before_closer)
      closer_beg--;
    else {
      closer_beg = lines[line_index - 1].end;
      /* We need to eat the preceding "\r\n" but not any line trailing
       * spaces. */
      while (closer_beg < ctx.text.size() && ISBLANK(closer_beg))
        closer_beg++;
    }
  }

  *p_opener_beg = opener_beg;
  *p_opener_end = opener_end;
  *p_closer_beg = closer_beg;
  *p_closer_end = closer_end;
  return true;
}

static int md_is_autolink_uri(Parsing_Context &ctx, OFF beg, OFF max_end,
                              OFF *p_end) {
  OFF off = beg + 1;

  MD_ASSERT(CH(beg) == _T('<'));

  /* Check for scheme. */
  if (off >= max_end || !ISASCII(off))
    return false;
  off++;
  while (1) {
    if (off >= max_end)
      return false;
    if (off - beg > 32)
      return false;
    if (CH(off) == _T(':') && off - beg >= 3)
      break;
    if (!ISALNUM(off) && CH(off) != _T('+') && CH(off) != _T('-') &&
        CH(off) != _T('.'))
      return false;
    off++;
  }

  /* Check the path after the scheme. */
  while (off < max_end && CH(off) != _T('>')) {
    if (ISWHITESPACE(off) || ISCNTRL(off) || CH(off) == _T('<'))
      return false;
    off++;
  }

  if (off >= max_end)
    return false;

  MD_ASSERT(CH(off) == _T('>'));
  *p_end = off + 1;
  return true;
}

static int md_is_autolink_email(Parsing_Context &ctx, OFF beg, OFF max_end,
                                OFF *p_end) {
  OFF off = beg + 1;
  int label_len;

  MD_ASSERT(CH(beg) == _T('<'));

  /* The code should correspond to this regexp:
          /^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+
          @[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?
          (?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/
   */

  /* Username (before '@'). */
  while (off < max_end &&
         (ISALNUM(off) || ISANYOF(off, _T(".!#$%&'*+/=?^_`{|}~-"))))
    off++;
  if (off <= beg + 1)
    return false;

  /* '@' */
  if (off >= max_end || CH(off) != _T('@'))
    return false;
  off++;

  /* Labels delimited with '.'; each label is sequence of 1 - 63 alnum
   * characters or '-', but '-' is not allowed as first or last char. */
  label_len = 0;
  while (off < max_end) {
    if (ISALNUM(off))
      label_len++;
    else if (CH(off) == _T('-') && label_len > 0)
      label_len++;
    else if (CH(off) == _T('.') && label_len > 0 && CH(off - 1) != _T('-'))
      label_len = 0;
    else
      break;

    if (label_len > 63)
      return false;

    off++;
  }

  if (label_len <= 0 || off >= max_end || CH(off) != _T('>') ||
      CH(off - 1) == _T('-'))
    return false;

  *p_end = off + 1;
  return true;
}

static int md_is_autolink(Parsing_Context &ctx, OFF beg, OFF max_end,
                          OFF *p_end, int *p_missing_mailto) {
  if (md_is_autolink_uri(ctx, beg, max_end, p_end)) {
    *p_missing_mailto = false;
    return true;
  }

  if (md_is_autolink_email(ctx, beg, max_end, p_end)) {
    *p_missing_mailto = true;
    return true;
  }

  return false;
}

static int md_collect_marks(Parsing_Context &ctx, std::span<Line> lines,
                            bool table_mode) {
  int ret = 0;
  Mark *mark;
  OFF codespan_last_potential_closers[CODESPAN_MARK_MAXLEN] = {0};
  bool codespan_scanned_till_paragraph_end = false;

  for (size_t i = 0; i < lines.size(); i++) {
    auto &line{lines[i]};
    OFF off = line.beg;
    OFF line_end = line.end;

    while (true) {
      CHAR ch;

#ifdef MD4C_USE_UTF16
/* For UTF-16, mark_char_map[] covers only ASCII. */
#define IS_MARK_CHAR(off)                                                      \
  ((CH(off) < SIZEOF_ARRAY(ctx.mark_char_map)) &&                              \
   (ctx.mark_char_map[(unsigned char)CH(off)]))
#else
/* For 8-bit encodings, mark_char_map[] covers all 256 elements. */
#define IS_MARK_CHAR(off) (ctx.mark_char_map[(unsigned char)CH(off)])
#endif

      /* Optimization: Use some loop unrolling.
      Update: perhaps manual loop unrolling is not needed in modern C++
      compilers */
      while (off + 3 < line_end && !IS_MARK_CHAR(off + 0) &&
             !IS_MARK_CHAR(off + 1) && !IS_MARK_CHAR(off + 2) &&
             !IS_MARK_CHAR(off + 3))
        off += 4;
      while (off < line_end && !IS_MARK_CHAR(off + 0))
        off++;

      if (off >= line_end)
        break;

      ch = CH(off);

      /* A backslash escape.
       * It can go beyond line->end as it may involve escaped new
       * line to form a hard break. */
      if (ch == _T('\\') && off + 1 < ctx.text.size() &&
          (ISPUNCT(off + 1) || ISNEWLINE(off + 1))) {
        /* Hard-break cannot be on the last line of the block. */
        if (!ISNEWLINE(off + 1) || i + 1 < lines.size())
          PUSH_MARK(ch, off, off + 2, MD_MARK_RESOLVED);
        off += 2;
        continue;
      }

      /* A potential (string) emphasis start/end. */
      if (ch == _T('*') || ch == _T('_')) {
        OFF tmp = off + 1;
        int left_level;  /* What precedes: 0 = whitespace; 1 = punctuation; 2 =
                            other char. */
        int right_level; /* What follows: 0 = whitespace; 1 = punctuation; 2 =
                            other char. */

        while (tmp < line_end && CH(tmp) == ch)
          tmp++;

        if (off == line.beg || ISUNICODEWHITESPACEBEFORE(off))
          left_level = 0;
        else if (ISUNICODEPUNCTBEFORE(off))
          left_level = 1;
        else
          left_level = 2;

        if (tmp == line_end || ISUNICODEWHITESPACE(tmp))
          right_level = 0;
        else if (ISUNICODEPUNCT(tmp))
          right_level = 1;
        else
          right_level = 2;

        /* Intra-word underscore doesn't have special meaning. */
        if (ch == _T('_') && left_level == 2 && right_level == 2) {
          left_level = 0;
          right_level = 0;
        }

        if (left_level != 0 || right_level != 0) {
          unsigned flags = 0;

          if (left_level > 0 && left_level >= right_level)
            flags |= MD_MARK_POTENTIAL_CLOSER;
          if (right_level > 0 && right_level >= left_level)
            flags |= MD_MARK_POTENTIAL_OPENER;
          if (left_level == 2 && right_level == 2)
            flags |= MD_MARK_EMPH_INTRAWORD;

          /* For "the rule of three" we need to remember the original
           * size of the mark (modulo three), before we potentially
           * split the mark when being later resolved partially by some
           * shorter closer. */
          switch ((tmp - off) % 3) {
          case 0:
            flags |= MD_MARK_EMPH_MOD3_0;
            break;
          case 1:
            flags |= MD_MARK_EMPH_MOD3_1;
            break;
          case 2:
            flags |= MD_MARK_EMPH_MOD3_2;
            break;
          }

          PUSH_MARK(ch, off, tmp, flags);

          /* During resolving, multiple asterisks may have to be
           * split into independent span start/ends. Consider e.g.
           * "**foo* bar*". Therefore we push also some empty dummy
           * marks to have enough space for that. */
          off++;
          while (off < tmp) {
            PUSH_MARK('D', off, off, 0);
            off++;
          }
          continue;
        }

        off = tmp;
        continue;
      }

      /* A potential code span start/end. */
      if (ch == _T('`')) {
        OFF opener_beg, opener_end;
        OFF closer_beg, closer_end;
        bool is_code_span = md_is_code_span(
            ctx, lines.subspan(i), off, &opener_beg, &opener_end, &closer_beg,
            &closer_end, codespan_last_potential_closers,
            codespan_scanned_till_paragraph_end);
        if (is_code_span) {
          PUSH_MARK(_T('`'), opener_beg, opener_end,
                    MD_MARK_OPENER | MD_MARK_RESOLVED);
          PUSH_MARK(_T('`'), closer_beg, closer_end,
                    MD_MARK_CLOSER | MD_MARK_RESOLVED);
          ctx.marks[ctx.n_marks - 2].next = ctx.n_marks - 1;
          ctx.marks[ctx.n_marks - 1].prev = ctx.n_marks - 2;

          off = closer_end;

          /* Advance the current line accordingly. */
          auto &&tmp_iter{lines.begin()};
          while (*tmp_iter != line)
            tmp_iter++;
          while (off > line_end) {
            i++;
            line = *tmp_iter++;
            line_end = line.end;
          }
          continue;
        }

        off = opener_end;
        continue;
      }

      /* A potential entity start. */
      if (ch == _T('&')) {
        PUSH_MARK(ch, off, off + 1, MD_MARK_POTENTIAL_OPENER);
        off++;
        continue;
      }

      /* A potential entity end. */
      if (ch == _T(';')) {
        /* We surely cannot be entity unless the previous mark is '&'. */
        if (ctx.n_marks > 0 && ctx.marks[ctx.n_marks - 1].ch == _T('&'))
          PUSH_MARK(ch, off, off + 1, MD_MARK_POTENTIAL_CLOSER);

        off++;
        continue;
      }

      /* A potential autolink or raw HTML start/end. */
      if (ch == _T('<')) {
        OFF autolink_end;
        int missing_mailto;

        if (!(ctx.parser.flags & MD_FLAG_NOHTMLSPANS)) {
          OFF html_end;

          /* Given the nature of the raw HTML, we have to recognize
           * it here. Doing so later in md_analyze_lt_gt() could
           * open can of worms of quadratic complexity. */
          bool is_html = md_is_html_any(ctx, lines.subspan(i), off,
                                        lines.back().end, &html_end);
          if (is_html) {
            PUSH_MARK(_T('<'), off, off, MD_MARK_OPENER | MD_MARK_RESOLVED);
            PUSH_MARK(_T('>'), html_end, html_end,
                      MD_MARK_CLOSER | MD_MARK_RESOLVED);
            ctx.marks[ctx.n_marks - 2].next = ctx.n_marks - 1;
            ctx.marks[ctx.n_marks - 1].prev = ctx.n_marks - 2;
            off = html_end;

            /* Advance the current line accordingly. */
            auto &&tmp_iter{lines.begin()};
            while (*tmp_iter != line)
              tmp_iter++;
            while (off > line_end) {
              i++;
              line = *tmp_iter++;
              line_end = line.end;
            }
            continue;
          }
        }

        bool is_autolink = md_is_autolink(ctx, off, lines.back().end,
                                          &autolink_end, &missing_mailto);
        if (is_autolink) {
          PUSH_MARK((missing_mailto ? _T('@') : _T('<')), off, off + 1,
                    MD_MARK_OPENER | MD_MARK_RESOLVED | MD_MARK_AUTOLINK);
          PUSH_MARK(_T('>'), autolink_end - 1, autolink_end,
                    MD_MARK_CLOSER | MD_MARK_RESOLVED | MD_MARK_AUTOLINK);
          ctx.marks[ctx.n_marks - 2].next = ctx.n_marks - 1;
          ctx.marks[ctx.n_marks - 1].prev = ctx.n_marks - 2;
          off = autolink_end;
          continue;
        }

        off++;
        continue;
      }

      /* A potential link or its part. */
      if (ch == _T('[') ||
          (ch == _T('!') && off + 1 < line_end && CH(off + 1) == _T('['))) {
        OFF tmp = (ch == _T('[') ? off + 1 : off + 2);
        PUSH_MARK(ch, off, tmp, MD_MARK_POTENTIAL_OPENER);
        off = tmp;
        /* Two dummies to make enough place for data we need if it is
         * a link. */
        PUSH_MARK('D', off, off, 0);
        PUSH_MARK('D', off, off, 0);
        continue;
      }
      if (ch == _T(']')) {
        PUSH_MARK(ch, off, off + 1, MD_MARK_POTENTIAL_CLOSER);
        off++;
        continue;
      }

      /* A potential permissive e-mail autolink. */
      if (ch == _T('@')) {
        if (line.beg + 1 <= off && ISALNUM(off - 1) && off + 3 < line.end &&
            ISALNUM(off + 1)) {
          PUSH_MARK(ch, off, off + 1, MD_MARK_POTENTIAL_OPENER);
          /* Push a dummy as a reserve for a closer. */
          PUSH_MARK('D', off, off, 0);
        }

        off++;
        continue;
      }

      /* A potential permissive URL autolink. */
      if (ch == _T(':')) {
        static struct {
          mdstring scheme;
          mdstring suffix;
        } scheme_map[] = {
            /* In the order from the most frequently used, arguably. */
            {_T("http"), _T("//")},
            {_T("https"), _T("//")},
            {_T("ftp"), _T("//")}};

        for (const auto &[scheme, suffix] : scheme_map) {
          if (line.beg + scheme.size() <= off &&
              (scheme == ctx.text.substr(off - scheme.size(), scheme.size())) &&
              (line.beg + scheme.size() == off ||
               ISWHITESPACE(off - scheme.size() - 1) ||
               ISANYOF(off - scheme.size() - 1, _T("*_~(["))) &&
              off + 1 + suffix.size() < line.end &&
              (suffix == ctx.text.substr(off + 1, suffix.size()))) {
            PUSH_MARK(ch, off - scheme.size(), off + 1 + suffix.size(),
                      MD_MARK_POTENTIAL_OPENER);
            /* Push a dummy as a reserve for a closer. */
            PUSH_MARK('D', off, off, 0);
            off += 1 + suffix.size();
            break;
          }
        }

        off++;
        continue;
      }

      /* A potential permissive WWW autolink. */
      if (ch == _T('.')) {
        if (line.beg + 3 <= off && (STR(off - 3) == _T("www")) &&
            (line.beg + 3 == off || ISWHITESPACE(off - 4) ||
             ISANYOF(off - 4, _T("*_~(["))) &&
            off + 1 < line_end) {
          PUSH_MARK(ch, off - 3, off + 1, MD_MARK_POTENTIAL_OPENER);
          /* Push a dummy as a reserve for a closer. */
          PUSH_MARK('D', off, off, 0);
          off++;
          continue;
        }

        off++;
        continue;
      }

      /* A potential table cell boundary or wiki link label delimiter. */
      if ((table_mode || ctx.parser.flags & MD_FLAG_WIKILINKS) &&
          ch == _T('|')) {
        PUSH_MARK(ch, off, off + 1, 0);
        off++;
        continue;
      }

      /* A potential strikethrough start/end. */
      if (ch == _T('~')) {
        OFF tmp = off + 1;

        while (tmp < line_end && CH(tmp) == _T('~'))
          tmp++;

        if (tmp - off < 3) {
          unsigned flags = 0;

          if (tmp < line_end && !ISUNICODEWHITESPACE(tmp))
            flags |= MD_MARK_POTENTIAL_OPENER;
          if (off > line.beg && !ISUNICODEWHITESPACEBEFORE(off))
            flags |= MD_MARK_POTENTIAL_CLOSER;
          if (flags != 0)
            PUSH_MARK(ch, off, tmp, flags);
        }

        off = tmp;
        continue;
      }

      /* A potential equation start/end */
      if (ch == _T('$')) {
        /* We can have at most two consecutive $ signs,
         * where two dollar signs signify a display equation. */
        OFF tmp = off + 1;

        while (tmp < line_end && CH(tmp) == _T('$'))
          tmp++;

        if (tmp - off <= 2)
          PUSH_MARK(ch, off, tmp,
                    MD_MARK_POTENTIAL_OPENER | MD_MARK_POTENTIAL_CLOSER);
        off = tmp;
        continue;
      }

      /* Turn non-trivial whitespace into single space. */
      if (ISWHITESPACE_(ch)) {
        OFF tmp = off + 1;

        while (tmp < line_end && ISWHITESPACE(tmp))
          tmp++;

        if (tmp - off > 1 || ch != _T(' '))
          PUSH_MARK(ch, off, tmp, MD_MARK_RESOLVED);

        off = tmp;
        continue;
      }

      /* null character. */
      if (ch == _T('\0')) {
        PUSH_MARK(ch, off, off + 1, MD_MARK_RESOLVED);
        off++;
        continue;
      }

      off++;
    }
  }

  /* Add a dummy mark at the end of the mark vector to simplify
   * process_inlines(). */
  PUSH_MARK(127, ctx.text.size(), ctx.text.size(), MD_MARK_RESOLVED);

abort:
  return ret;
}

static void md_analyze_bracket(Parsing_Context &ctx, int mark_index) {
  /* We cannot really resolve links here as for that we would need
   * more context. E.g. a following pair of brackets (reference link),
   * or enclosing pair of brackets (if the inner is the link, the outer
   * one cannot be.)
   *
   * Therefore we here only construct a list of resolved '[' ']' pairs
   * ordered by position of the closer. This allows ur to analyze what is
   * or is not link in the right order, from inside to outside in case
   * of nested brackets.
   *
   * The resolving itself is deferred into md_resolve_links().
   */

  Mark *mark = &ctx.marks[mark_index];

  if (mark->flags & MD_MARK_POTENTIAL_OPENER) {
    md_mark_chain_append(ctx, &BRACKET_OPENERS, mark_index);
    return;
  }

  if (BRACKET_OPENERS.tail >= 0) {
    /* Pop the opener from the chain. */
    int opener_index = BRACKET_OPENERS.tail;
    Mark *opener = &ctx.marks[opener_index];
    if (opener->prev >= 0)
      ctx.marks[opener->prev].next = -1;
    else
      BRACKET_OPENERS.head = -1;
    BRACKET_OPENERS.tail = opener->prev;

    /* Interconnect the opener and closer. */
    opener->next = mark_index;
    mark->prev = opener_index;

    /* Add the pair into chain of potential links for md_resolve_links().
     * Note we misuse opener->prev for this as opener->next points to its
     * closer. */
    if (ctx.unresolved_link_tail >= 0)
      ctx.marks[ctx.unresolved_link_tail].prev = opener_index;
    else
      ctx.unresolved_link_head = opener_index;
    ctx.unresolved_link_tail = opener_index;
    opener->prev = -1;
  }
}

/* Forward declaration. */
static void md_analyze_link_contents(Parsing_Context &ctx, int mark_beg,
                                     int mark_end);

static int md_resolve_links(Parsing_Context &ctx, std::span<Line> lines) {
  int opener_index = ctx.unresolved_link_head;
  OFF last_link_beg = 0;
  OFF last_link_end = 0;
  OFF last_img_beg = 0;
  OFF last_img_end = 0;

  while (opener_index >= 0) {
    Mark *opener = &ctx.marks[opener_index];
    int closer_index = opener->next;
    Mark *closer = &ctx.marks[closer_index];
    int next_index = opener->prev;
    Mark *next_opener;
    Mark *next_closer;
    MD_LINK_ATTR attr;
    bool is_link{};

    if (next_index >= 0) {
      next_opener = &ctx.marks[next_index];
      next_closer = &ctx.marks[next_opener->next];
    } else {
      next_opener = nullptr;
      next_closer = nullptr;
    }

    /* If nested ("[ [ ] ]"), we need to make sure that:
     *   - The outer does not end inside of (...) belonging to the inner.
     *   - The outer cannot be link if the inner is link (i.e. not image).
     *
     * (Note we here analyze from inner to outer as the marks are ordered
     * by closer->beg.)
     */
    if ((opener->beg < last_link_beg && closer->end < last_link_end) ||
        (opener->beg < last_img_beg && closer->end < last_img_end) ||
        (opener->beg < last_link_end && opener->ch == '[')) {
      opener_index = next_index;
      continue;
    }

    /* Recognize and resolve wiki links.
     * Wiki-links maybe '[[destination]]' or '[[destination|label]]'.
     */
    if ((ctx.parser.flags & MD_FLAG_WIKILINKS) &&
        (opener->end - opener->beg == 1) && /* not image */
        next_opener != nullptr &&           /* double '[' opener */
        next_opener->ch == '[' && (next_opener->beg == opener->beg - 1) &&
        (next_opener->end - next_opener->beg == 1) &&
        next_closer != nullptr && /* double ']' closer */
        next_closer->ch == ']' && (next_closer->beg == closer->beg + 1) &&
        (next_closer->end - next_closer->beg == 1)) {
      Mark *delim = nullptr;
      int delim_index;
      OFF dest_beg, dest_end;

      is_link = true;

      /* We don't allow destination to be longer than 100 characters.
       * Lets scan to see whether there is '|'. (If not then the whole
       * wiki-link has to be below the 100 characters.) */
      delim_index = opener_index + 1;
      while (delim_index < closer_index) {
        Mark *m = &ctx.marks[delim_index];
        if (m->ch == '|') {
          delim = m;
          break;
        }
        if (m->ch != 'D' && m->beg - opener->end > 100)
          break;
        delim_index++;
      }
      dest_beg = opener->end;
      dest_end = (delim != nullptr) ? delim->beg : closer->beg;
      if (dest_end - dest_beg == 0 || dest_end - dest_beg > 100)
        is_link = false;

      /* There may not be any new line in the destination. */
      if (is_link) {
        OFF off;
        for (off = dest_beg; off < dest_end; off++) {
          if (ISNEWLINE(off)) {
            is_link = false;
            break;
          }
        }
      }

      if (is_link) {
        if (delim != nullptr) {
          if (delim->end < closer->beg) {
            opener->end = delim->beg;
          } else {
            /* The pipe is just before the closer: [[foo|]] */
            closer->beg = delim->beg;
            delim = nullptr;
          }
        }

        opener->beg = next_opener->beg;
        opener->next = closer_index;
        opener->flags |= MD_MARK_OPENER | MD_MARK_RESOLVED;

        closer->end = next_closer->end;
        closer->prev = opener_index;
        closer->flags |= MD_MARK_CLOSER | MD_MARK_RESOLVED;

        last_link_beg = opener->beg;
        last_link_end = closer->end;

        if (delim != nullptr) {
          delim->flags |= MD_MARK_RESOLVED;
          md_rollback(ctx, opener_index, delim_index, MD_ROLLBACK_ALL);
          md_analyze_link_contents(ctx, opener_index + 1, closer_index);
        } else {
          md_rollback(ctx, opener_index, closer_index, MD_ROLLBACK_ALL);
        }

        opener_index = next_opener->prev;
        continue;
      }
    }

    if (next_opener != nullptr && next_opener->beg == closer->end) {
      if (next_closer->beg > closer->end + 1) {
        /* Might be full reference link. */
        is_link = md_is_link_reference(ctx, lines, next_opener->beg,
                                       next_closer->end, &attr);
      } else {
        /* Might be shortcut reference link. */
        is_link =
            md_is_link_reference(ctx, lines, opener->beg, closer->end, &attr);
      }

      if (not is_link)
        return -1;

      if (is_link) {
        /* Eat the 2nd "[...]". */
        closer->end = next_closer->end;

        /* Do not analyze the label as a standalone link in the next
         * iteration. */
        next_index = ctx.marks[next_index].prev;
      }
    } else {
      if (closer->end < ctx.text.size() && CH(closer->end) == _T('(')) {
        /* Might be inline link. */
        OFF inline_link_end = UINT_MAX;

        is_link = md_is_inline_link_spec(ctx, lines, closer->end,
                                         &inline_link_end, &attr);
        if (not is_link)
          return -1;

        /* Check the closing ')' is not inside an already resolved range
         * (i.e. a range with a higher priority), e.g. a code span. */
        if (is_link) {
          int i = closer_index + 1;

          while (i < ctx.n_marks) {
            Mark *mark = &ctx.marks[i];

            if (mark->beg >= inline_link_end)
              break;
            if ((mark->flags & (MD_MARK_OPENER | MD_MARK_RESOLVED)) ==
                (MD_MARK_OPENER | MD_MARK_RESOLVED)) {
              if (ctx.marks[mark->next].beg >= inline_link_end) {
                is_link = false;
                break;
              }

              i = mark->next + 1;
            } else {
              i++;
            }
          }
        }

        if (is_link) {
          /* Eat the "(...)" */
          closer->end = inline_link_end;
        }
      }

      if (!is_link) {
        /* Might be collapsed reference link. */
        is_link =
            md_is_link_reference(ctx, lines, opener->beg, closer->end, &attr);
        if (not is_link)
          return -1;
      }
    }

    if (is_link) {
      /* Resolve the brackets as a link. */
      opener->flags |= MD_MARK_OPENER | MD_MARK_RESOLVED;
      closer->flags |= MD_MARK_CLOSER | MD_MARK_RESOLVED;

      /* If it is a link, we store the destination and title in the two
       * dummy marks after the opener. */
      MD_ASSERT(ctx.marks[opener_index + 1].ch == 'D');
      ctx.marks[opener_index + 1].beg = attr.dest_beg;
      ctx.marks[opener_index + 1].end = attr.dest_end;

      MD_ASSERT(ctx.marks[opener_index + 2].ch == 'D');
      md_mark_store_ptr(ctx, opener_index + 2, attr.title.data());
      /* The title might or might not have been allocated for us. */
      if (attr.title_needs_free)
        md_mark_chain_append(ctx, &PTR_CHAIN, opener_index + 2);
      ctx.marks[opener_index + 2].prev = attr.title.size();

      if (opener->ch == '[') {
        last_link_beg = opener->beg;
        last_link_end = closer->end;
      } else {
        last_img_beg = opener->beg;
        last_img_end = closer->end;
      }

      md_analyze_link_contents(ctx, opener_index + 1, closer_index);

      /* If the link text is formed by nothing but permissive autolink,
       * suppress the autolink.
       * See https://github.com/mity/md4c/issues/152 for more info. */
      if (ctx.parser.flags & MD_FLAG_PERMISSIVEAUTOLINKS) {
        Mark *first_nested;
        Mark *last_nested;

        first_nested = opener + 1;
        while (first_nested->ch == _T('D') && first_nested < closer)
          first_nested++;

        last_nested = closer - 1;
        while (first_nested->ch == _T('D') && last_nested > opener)
          last_nested--;

        if ((first_nested->flags & MD_MARK_RESOLVED) &&
            first_nested->beg == opener->end &&
            ISANYOF_(first_nested->ch, _T("@:.")) &&
            first_nested->next == (last_nested - ctx.marks) &&
            last_nested->end == closer->beg) {
          first_nested->ch = _T('D');
          first_nested->flags &= ~MD_MARK_RESOLVED;
          last_nested->ch = _T('D');
          last_nested->flags &= ~MD_MARK_RESOLVED;
        }
      }
    }

    opener_index = next_index;
  }

  return 0;
}

/* Analyze whether the mark '&' starts a HTML entity.
 * If so, update its flags as well as flags of corresponding closer ';'. */
static void md_analyze_entity(Parsing_Context &ctx, int mark_index) {
  Mark &opener{ctx.marks[mark_index]};
  OFF off;

  /* Cannot be entity if there is no closer as the next mark.
   * (Any other mark between would mean strange character which cannot be
   * part of the entity.
   *
   * So we can do all the work on '&' and do not call this later for the
   * closing mark ';'.
   */
  if (mark_index + 1 >= ctx.n_marks)
    return;
  Mark &closer{ctx.marks[mark_index + 1]};
  if (closer.ch != ';')
    return;

  if (md_is_entity(ctx.text, opener.beg, closer.end, &off)) {
    MD_ASSERT(off == closer.end);

    md_resolve_range(ctx, nullptr, mark_index, mark_index + 1);
    opener.end = closer.end;
  }
}

static void md_analyze_table_cell_boundary(Parsing_Context &ctx,
                                           int mark_index) {
  Mark *mark = &ctx.marks[mark_index];
  mark->flags |= MD_MARK_RESOLVED;

  md_mark_chain_append(ctx, &TABLECELLBOUNDARIES, mark_index);
  ctx.n_table_cell_boundaries++;
}

/* Split a longer mark into two. The new mark takes the given count of
 * characters. May only be called if an adequate number of dummy 'D' marks
 * follows.
 */
static int md_split_emph_mark(Parsing_Context &ctx, int mark_index, SZ n) {
  Mark *mark{&ctx.marks[mark_index]};
  int new_mark_index = mark_index + (mark->end - mark->beg - n);
  Mark *dummy = &ctx.marks[new_mark_index];

  MD_ASSERT(mark->end - mark->beg > n);
  MD_ASSERT(dummy->ch == 'D');

  memcpy(dummy, mark, sizeof(Mark));
  mark->end -= n;
  dummy->beg = mark->end;

  return new_mark_index;
}

static void md_analyze_emph(Parsing_Context &ctx, int mark_index) {
  Mark *mark = &ctx.marks[mark_index];
  MarkChain *chain = md_mark_chain(ctx, mark_index);

  /* If we can be a closer, try to resolve with the preceding opener. */
  if (mark->flags & MD_MARK_POTENTIAL_CLOSER) {
    Mark *opener = nullptr;
    int opener_index = 0;

    if (mark->ch == _T('*')) {
      MarkChain *opener_chains[6];
      int i, n_opener_chains;
      unsigned flags = mark->flags;

      /* Apply the "rule of three". */
      n_opener_chains = 0;
      opener_chains[n_opener_chains++] = &ASTERISK_OPENERS_intraword_mod3_0;
      if ((flags & MD_MARK_EMPH_MOD3_MASK) != MD_MARK_EMPH_MOD3_2)
        opener_chains[n_opener_chains++] = &ASTERISK_OPENERS_intraword_mod3_1;
      if ((flags & MD_MARK_EMPH_MOD3_MASK) != MD_MARK_EMPH_MOD3_1)
        opener_chains[n_opener_chains++] = &ASTERISK_OPENERS_intraword_mod3_2;
      opener_chains[n_opener_chains++] = &ASTERISK_OPENERS_extraword_mod3_0;
      if (!(flags & MD_MARK_EMPH_INTRAWORD) ||
          (flags & MD_MARK_EMPH_MOD3_MASK) != MD_MARK_EMPH_MOD3_2)
        opener_chains[n_opener_chains++] = &ASTERISK_OPENERS_extraword_mod3_1;
      if (!(flags & MD_MARK_EMPH_INTRAWORD) ||
          (flags & MD_MARK_EMPH_MOD3_MASK) != MD_MARK_EMPH_MOD3_1)
        opener_chains[n_opener_chains++] = &ASTERISK_OPENERS_extraword_mod3_2;

      /* Opener is the most recent mark from the allowed chains. */
      for (i = 0; i < n_opener_chains; i++) {
        if (opener_chains[i]->tail >= 0) {
          int tmp_index = opener_chains[i]->tail;
          Mark *tmp_mark = &ctx.marks[tmp_index];
          if (opener == nullptr || tmp_mark->end > opener->end) {
            opener_index = tmp_index;
            opener = tmp_mark;
          }
        }
      }
    } else {
      /* Simple emph. mark */
      if (chain->tail >= 0) {
        opener_index = chain->tail;
        opener = &ctx.marks[opener_index];
      }
    }

    /* Resolve, if we have found matching opener. */
    if (opener != nullptr) {
      SZ opener_size = opener->end - opener->beg;
      SZ closer_size = mark->end - mark->beg;
      MarkChain *opener_chain = md_mark_chain(ctx, opener_index);

      if (opener_size > closer_size) {
        opener_index = md_split_emph_mark(ctx, opener_index, closer_size);
        md_mark_chain_append(ctx, opener_chain, opener_index);
      } else if (opener_size < closer_size) {
        md_split_emph_mark(ctx, mark_index, closer_size - opener_size);
      }

      md_rollback(ctx, opener_index, mark_index, MD_ROLLBACK_CROSSING);
      md_resolve_range(ctx, opener_chain, opener_index, mark_index);
      return;
    }
  }

  /* If we could not resolve as closer, we may be yet be an opener. */
  if (mark->flags & MD_MARK_POTENTIAL_OPENER)
    md_mark_chain_append(ctx, chain, mark_index);
}

static void md_analyze_tilde(Parsing_Context &ctx, int mark_index) {
  Mark *mark = &ctx.marks[mark_index];
  MarkChain *chain = md_mark_chain(ctx, mark_index);

  /* We attempt to be Github Flavored Markdown compatible here. GFM accepts
   * only tildes sequences of length 1 and 2, and the length of the opener
   * and closer has to match. */

  if ((mark->flags & MD_MARK_POTENTIAL_CLOSER) && chain->head >= 0) {
    int opener_index = chain->head;

    md_rollback(ctx, opener_index, mark_index, MD_ROLLBACK_CROSSING);
    md_resolve_range(ctx, chain, opener_index, mark_index);
    return;
  }

  if (mark->flags & MD_MARK_POTENTIAL_OPENER)
    md_mark_chain_append(ctx, chain, mark_index);
}

static void md_analyze_dollar(Parsing_Context &ctx, int mark_index) {
  /* This should mimic the way inline equations work in LaTeX, so there
   * can only ever be one item in the chain (i.e. the dollars can't be
   * nested). This is basically the same as the md_analyze_tilde function,
   * except that we require matching openers and closers to be of the same
   * length.
   *
   * E.g.: $abc$$def$$ => abc (display equation) def (end equation) */
  if (DOLLAR_OPENERS.head >= 0) {
    /* If the potential closer has a non-matching number of $, discard */
    Mark *open = &ctx.marks[DOLLAR_OPENERS.head];
    Mark *close = &ctx.marks[mark_index];

    int opener_index = DOLLAR_OPENERS.head;
    md_rollback(ctx, opener_index, mark_index, MD_ROLLBACK_ALL);
    if (open->end - open->beg == close->end - close->beg) {
      /* We are the matching closer */
      md_resolve_range(ctx, &DOLLAR_OPENERS, opener_index, mark_index);
    } else {
      /* We don't match the opener, so discard old opener and insert as opener
       */
      md_mark_chain_append(ctx, &DOLLAR_OPENERS, mark_index);
    }
  } else {
    /* No unmatched openers, so we are opener */
    md_mark_chain_append(ctx, &DOLLAR_OPENERS, mark_index);
  }
}

static void md_analyze_permissive_url_autolink(Parsing_Context &ctx,
                                               int mark_index) {
  Mark *opener = &ctx.marks[mark_index];
  int closer_index = mark_index + 1;
  Mark *closer = &ctx.marks[closer_index];
  Mark *next_resolved_mark;
  OFF off = opener->end;
  int n_dots = false;
  int has_underscore_in_last_seg = false;
  int has_underscore_in_next_to_last_seg = false;
  int n_opened_parenthesis = 0;
  int n_excess_parenthesis = 0;

  /* Check for domain. */
  while (off < ctx.text.size()) {
    if (ISALNUM(off) || CH(off) == _T('-')) {
      off++;
    } else if (CH(off) == _T('.')) {
      /* We must see at least one period. */
      n_dots++;
      has_underscore_in_next_to_last_seg = has_underscore_in_last_seg;
      has_underscore_in_last_seg = false;
      off++;
    } else if (CH(off) == _T('_')) {
      /* No underscore may be present in the last two domain segments. */
      has_underscore_in_last_seg = true;
      off++;
    } else {
      break;
    }
  }
  if (off > opener->end && CH(off - 1) == _T('.')) {
    off--;
    n_dots--;
  }
  if (off <= opener->end || n_dots == 0 || has_underscore_in_next_to_last_seg ||
      has_underscore_in_last_seg)
    return;

  /* Check for path. */
  next_resolved_mark = closer + 1;
  while (next_resolved_mark->ch == 'D' ||
         !(next_resolved_mark->flags & MD_MARK_RESOLVED))
    next_resolved_mark++;
  while (off < next_resolved_mark->beg && CH(off) != _T('<') &&
         !ISWHITESPACE(off) && !ISNEWLINE(off)) {
    /* Parenthesis must be balanced. */
    if (CH(off) == _T('(')) {
      n_opened_parenthesis++;
    } else if (CH(off) == _T(')')) {
      if (n_opened_parenthesis > 0)
        n_opened_parenthesis--;
      else
        n_excess_parenthesis++;
    }

    off++;
  }

  /* Trim a trailing punctuation from the end. */
  while (true) {
    if (ISANYOF(off - 1, _T("?!.,:*_~")))
      off--;
    else if (CH(off - 1) == ')' && n_excess_parenthesis > 0) {
      /* Unmatched ')' can be in an interior of the path but not at the
       * of it, so the auto-link may be safely nested in a parenthesis
       * pair. */
      off--;
      n_excess_parenthesis--;
    } else {
      break;
    }
  }

  /* Ok. Lets call it an auto-link. Adapt opener and create closer to zero
   * length so all the contents becomes the link text. */
  MD_ASSERT(closer->ch == 'D');
  opener->end = opener->beg;
  closer->ch = opener->ch;
  closer->beg = off;
  closer->end = off;
  md_resolve_range(ctx, nullptr, mark_index, closer_index);
}

/* The permissive autolinks do not have to be enclosed in '<' '>' but we
 * instead impose stricter rules what is understood as an e-mail address
 * here. Actually any non-alphanumeric characters with exception of '.'
 * are prohibited both in username and after '@'. */
static void md_analyze_permissive_email_autolink(Parsing_Context &ctx,
                                                 int mark_index) {
  Mark *opener = &ctx.marks[mark_index];
  int closer_index;
  Mark *closer;
  OFF beg = opener->beg;
  OFF end = opener->end;
  int dot_count = 0;

  MD_ASSERT(CH(beg) == _T('@'));

  /* Scan for name before '@'. */
  while (beg > 0 && (ISALNUM(beg - 1) || ISANYOF(beg - 1, _T(".-_+"))))
    beg--;

  /* Scan for domain after '@'. */
  while (end < ctx.text.size() && (ISALNUM(end) || ISANYOF(end, _T(".-_")))) {
    if (CH(end) == _T('.'))
      dot_count++;
    end++;
  }
  if (CH(end - 1) == _T('.')) { /* Final '.' not part of it. */
    dot_count--;
    end--;
  } else if (ISANYOF2(end - 1, _T('-'),
                      _T('_'))) /* These are forbidden at the end. */
    return;
  if (CH(end - 1) == _T('@') || dot_count == 0)
    return;

  /* Ok. Lets call it auto-link. Adapt opener and create closer to zero
   * length so all the contents becomes the link text. */
  closer_index = mark_index + 1;
  closer = &ctx.marks[closer_index];
  MD_ASSERT(closer->ch == 'D');

  opener->beg = beg;
  opener->end = beg;
  closer->ch = opener->ch;
  closer->beg = end;
  closer->end = end;
  md_resolve_range(ctx, nullptr, mark_index, closer_index);
}

static inline void md_analyze_marks(Parsing_Context &ctx, int mark_beg,
                                    int mark_end, const CHAR *mark_chars) {
  int i = mark_beg;

  while (i < mark_end) {
    Mark *mark = &ctx.marks[i];

    /* Skip resolved spans. */
    if (mark->flags & MD_MARK_RESOLVED) {
      if (mark->flags & MD_MARK_OPENER) {
        MD_ASSERT(i < mark->next);
        i = mark->next + 1;
      } else {
        i++;
      }
      continue;
    }

    /* Skip marks we do not want to deal with. */
    if (!ISANYOF_(mark->ch, mark_chars)) {
      i++;
      continue;
    }

    /* Analyze the mark. */
    switch (mark->ch) {
    case '[': /* Pass through. */
    case '!': /* Pass through. */
    case ']':
      md_analyze_bracket(ctx, i);
      break;
    case '&':
      md_analyze_entity(ctx, i);
      break;
    case '|':
      md_analyze_table_cell_boundary(ctx, i);
      break;
    case '_': /* Pass through. */
    case '*':
      md_analyze_emph(ctx, i);
      break;
    case '~':
      md_analyze_tilde(ctx, i);
      break;
    case '$':
      md_analyze_dollar(ctx, i);
      break;
    case '.': /* Pass through. */
    case ':':
      md_analyze_permissive_url_autolink(ctx, i);
      break;
    case '@':
      md_analyze_permissive_email_autolink(ctx, i);
      break;
    }

    i++;
  }
}

/* Analyze marks (build ctx.marks). */
static int md_analyze_inlines(Parsing_Context &ctx, std::span<Line> lines,
                              bool table_mode) {
  int ret;

  /* Reset the previously collected stack of marks. */
  ctx.n_marks = 0;

  /* Collect all marks. */
  MD_CHECK(md_collect_marks(ctx, lines, table_mode));

  /* We analyze marks in few groups to handle their precedence. */
  /* (1) Entities; code spans; autolinks; raw HTML. */
  md_analyze_marks(ctx, 0, ctx.n_marks, _T("&"));

  /* (2) Links. */
  md_analyze_marks(ctx, 0, ctx.n_marks, _T("[]!"));
  MD_CHECK(md_resolve_links(ctx, lines));
  BRACKET_OPENERS.head = -1;
  BRACKET_OPENERS.tail = -1;
  ctx.unresolved_link_head = -1;
  ctx.unresolved_link_tail = -1;

  if (table_mode) {
    /* (3) Analyze table cell boundaries.
     * Note we reset TABLECELLBOUNDARIES chain prior to the call
     * md_analyze_marks(), not after, because caller may need it. */
    MD_ASSERT(lines.size() == 1);
    TABLECELLBOUNDARIES.head = -1;
    TABLECELLBOUNDARIES.tail = -1;
    ctx.n_table_cell_boundaries = 0;
    md_analyze_marks(ctx, 0, ctx.n_marks, _T("|"));
    return ret;
  }

  /* (4) Emphasis and strong emphasis; permissive autolinks. */
  md_analyze_link_contents(ctx, 0, ctx.n_marks);

abort:
  return ret;
}

static void md_analyze_link_contents(Parsing_Context &ctx, int mark_beg,
                                     int mark_end) {
  int i;

  md_analyze_marks(ctx, mark_beg, mark_end, _T("*_~$@:."));

  for (i = OPENERS_CHAIN_FIRST; i <= OPENERS_CHAIN_LAST; i++) {
    ctx.mark_chains[i].head = -1;
    ctx.mark_chains[i].tail = -1;
  }
}

static int md_enter_leave_span_a(Parsing_Context &ctx, int enter,
                                 MD_SPANTYPE type, mdstringview dest,
                                 bool prohibit_escapes_in_dest,
                                 mdstringview title) {
  Attribute_Build href_build{};
  Attribute_Build title_build{};
  a_Detail det;
  int ret = 0;

  /* Note we here rely on fact that MD_SPAN_A_DETAIL and
   * MD_SPAN_IMG_DETAIL are binary-compatible. */
  memset(&det, 0, sizeof(a_Detail));
  MD_CHECK(href_build.build(
      ctx, mdstring(dest),
      (prohibit_escapes_in_dest ? MD_BUILD_ATTR_NO_ESCAPES : 0), det.href));
  MD_CHECK(title_build.build(ctx, mdstring(title), 0, det.title));

  if (enter)
    MD_ENTER_SPAN(type, &det);
  else
    MD_LEAVE_SPAN(type, &det);

abort:
  return ret;
}

static int md_enter_leave_span_wikilink(Parsing_Context &ctx, bool enter,
                                        mdstringview target) {
  Attribute_Build target_build{};
  Wikilink_Detail det{};
  int ret = 0;

  MD_CHECK(target_build.build(ctx, mdstring(target), 0, det.target));

  if (enter)
    MD_ENTER_SPAN(SpanType::wiki_link, &det);
  else
    MD_LEAVE_SPAN(SpanType::wiki_link, &det);

abort:
  return ret;
}

/* Render the output, accordingly to the analyzed ctx.marks. */
static int md_process_inlines(Parsing_Context &ctx, std::span<Line> lines) {
  MD_TEXTTYPE text_type;
  Line &line{lines[0]};
  Mark *prev_mark = nullptr;
  Mark *mark;
  OFF off = lines[0].beg;
  OFF end = lines.back().end;
  bool enforce_hardbreak = false;
  int ret = 0;

  /* Find first resolved mark. Note there is always at least one resolved
   * mark,  the dummy last one after the end of the latest line we actually
   * never really reach. This saves us of a lot of special checks and cases
   * in this function. */
  mark = ctx.marks;
  while (!(mark->flags & MD_MARK_RESOLVED))
    mark++;

  text_type = TextType::normal;

  while (1) {
    /* Process the text up to the next mark or end-of-line. */
    OFF tmp = (line.end < mark->beg ? line.end : mark->beg);
    if (tmp > off) {
      MD_TEXT(text_type, ctx.text.substr(off, tmp - off));
      off = tmp;
    }

    /* If reached the mark, process it and move to next one. */
    if (off >= mark->beg) {
      switch (mark->ch) {
      case '\\': /* Backslash escape. */
        if (ISNEWLINE(mark->beg + 1))
          enforce_hardbreak = true;
        else
          MD_TEXT(text_type, ctx.text.substr(mark->beg + 1, 1));
        break;

      case ' ': /* Non-trivial space. */
        MD_TEXT(text_type, mdstringview(_T(" ")));
        break;

      case '`': /* Code span. */
        if (mark->flags & MD_MARK_OPENER) {
          MD_ENTER_SPAN(SpanType::code, nullptr);
          text_type = TextType::code;
        } else {
          MD_LEAVE_SPAN(SpanType::code, nullptr);
          text_type = TextType::normal;
        }
        break;

      case '_': /* Underline (or emphasis if we fall through). */
        if (ctx.parser.flags & MD_FLAG_UNDERLINE) {
          if (mark->flags & MD_MARK_OPENER) {
            while (off < mark->end) {
              MD_ENTER_SPAN(SpanType::u, nullptr);
              off++;
            }
          } else {
            while (off < mark->end) {
              MD_LEAVE_SPAN(SpanType::u, nullptr);
              off++;
            }
          }
          break;
        }
        [[fallthrough]];

      case '*': /* Emphasis, strong emphasis. */
        if (mark->flags & MD_MARK_OPENER) {
          if ((mark->end - off) % 2) {
            MD_ENTER_SPAN(SpanType::em, nullptr);
            off++;
          }
          while (off + 1 < mark->end) {
            MD_ENTER_SPAN(SpanType::string, nullptr);
            off += 2;
          }
        } else {
          while (off + 1 < mark->end) {
            MD_LEAVE_SPAN(SpanType::string, nullptr);
            off += 2;
          }
          if ((mark->end - off) % 2) {
            MD_LEAVE_SPAN(SpanType::em, nullptr);
            off++;
          }
        }
        break;

      case '~':
        if (mark->flags & MD_MARK_OPENER)
          MD_ENTER_SPAN(SpanType::del, nullptr);
        else
          MD_LEAVE_SPAN(SpanType::del, nullptr);
        break;

      case '$':
        if (mark->flags & MD_MARK_OPENER) {
          MD_ENTER_SPAN((mark->end - off) % 2 ? SpanType::latex_inline
                                              : SpanType::latex_display,
                        nullptr);
          text_type = TextType::latex;
        } else {
          MD_LEAVE_SPAN((mark->end - off) % 2 ? SpanType::latex_inline
                                              : SpanType::latex_display,
                        nullptr);
          text_type = TextType::normal;
        }
        break;

      case '[': /* Link, wiki link, image. */
      case '!':
      case ']': {
        const Mark *opener = (mark->ch != ']' ? mark : &ctx.marks[mark->prev]);
        const Mark *closer = &ctx.marks[opener->next];
        const Mark *dest_mark;
        const Mark *title_mark;

        if ((opener->ch == '[' && closer->ch == ']') &&
            opener->end - opener->beg >= 2 && closer->end - closer->beg >= 2) {
          int has_label = (opener->end - opener->beg > 2);
          SZ target_sz;

          if (has_label)
            target_sz = opener->end - (opener->beg + 2);
          else
            target_sz = closer->beg - opener->end;

          MD_CHECK(md_enter_leave_span_wikilink(
              ctx, (mark->ch != ']'),
              ctx.text.substr((has_label ? opener->beg + 2 : opener->end),
                              target_sz)));

          break;
        }

        dest_mark = opener + 1;
        MD_ASSERT(dest_mark->ch == 'D');
        title_mark = opener + 2;
        MD_ASSERT(title_mark->ch == 'D');

        MD_CHECK(md_enter_leave_span_a(
            ctx, (mark->ch != ']'),
            (opener->ch == '!' ? SpanType::img : SpanType::a),
            ctx.text.substr(dest_mark->beg, dest_mark->end - dest_mark->beg),
            false,
            // XXX: ugly cast
            mdstringview(reinterpret_cast<CHAR *>(md_mark_get_ptr(
                             ctx, (int)(title_mark - ctx.marks))),
                         title_mark->prev)));

        /* link/image closer may span multiple lines. */
        if (mark->ch == ']') {
          while (mark->end > line.end) {
            auto &&tmp_iter{lines.begin()};
            while (*tmp_iter != line)
              tmp_iter++;
            line = *tmp_iter++;
          }
        }

        break;
      }

      case '<':
      case '>': /* Autolink or raw HTML. */
        if (!(mark->flags & MD_MARK_AUTOLINK)) {
          /* Raw HTML. */
          if (mark->flags & MD_MARK_OPENER)
            text_type = TextType::raw_html;
          else
            text_type = TextType::normal;
          break;
        }
        /* Pass through, if auto-link. */
        [[fallthrough]];

      case '@': /* Permissive e-mail autolink. */
      case ':': /* Permissive URL autolink. */
      case '.': /* Permissive WWW autolink. */
      {
        Mark *opener =
            ((mark->flags & MD_MARK_OPENER) ? mark : &ctx.marks[mark->prev]);
        Mark *closer = &ctx.marks[opener->next];
        auto dest{ctx.text.substr(opener->end, closer->beg - opener->end)};

        /* For permissive auto-links we do not know closer mark
         * position at the time of md_collect_marks(), therefore
         * it can be out-of-order in ctx.marks[].
         *
         * With this flag, we make sure that we output the closer
         * only if we processed the opener. */
        if (mark->flags & MD_MARK_OPENER)
          closer->flags |= MD_MARK_VALIDPERMISSIVEAUTOLINK;

        if (opener->ch == '@' || opener->ch == '.') {
          ctx.buffer = opener->ch == '@' ? _T("mailto:") : _T("http://");
          ctx.buffer += dest;
          /*
          memcpy(ctx.buffer,
                 (opener->ch == '@' ? _T("mailto:") : _T("http://")),
                 7 * sizeof(CHAR));
          memcpy(ctx.buffer + 7, dest, (dest.size() - 7) * sizeof(CHAR));*/
          dest = ctx.buffer;
        }

        if (closer->flags & MD_MARK_VALIDPERMISSIVEAUTOLINK)
          MD_CHECK(md_enter_leave_span_a(ctx, (mark->flags & MD_MARK_OPENER),
                                         SpanType::a, dest, true,
                                         mdstringview("")));
        break;
      }

      case '&': /* Entity. */
        MD_TEXT(TextType::entity,
                ctx.text.substr(mark->beg, mark->end - mark->beg));
        break;

      case '\0':
        MD_TEXT(TextType::null_char, mdstringview(_T("")));
        break;

      case 127:
        goto abort;
      }

      off = mark->end;

      /* Move to next resolved mark. */
      prev_mark = mark;
      mark++;
      while (!(mark->flags & MD_MARK_RESOLVED) || mark->beg < off)
        mark++;
    }

    /* If reached end of line, move to next one. */
    if (off >= line.end) {
      /* If it is the last line, we are done. */
      if (off >= end)
        break;

      if (text_type == TextType::code || text_type == TextType::latex) {
        OFF tmp;

        MD_ASSERT(prev_mark != nullptr);
        MD_ASSERT(ISANYOF2_(prev_mark->ch, '`', '$') &&
                  (prev_mark->flags & MD_MARK_OPENER));
        MD_ASSERT(ISANYOF2_(mark->ch, '`', '$') &&
                  (mark->flags & MD_MARK_CLOSER));

        /* Inside a code span, trailing line whitespace has to be
         * outputted. */
        tmp = off;
        while (off < ctx.text.size() && ISBLANK(off))
          off++;
        if (off > tmp)
          MD_TEXT(text_type, ctx.text.substr(tmp, off - tmp));

        /* and new lines are transformed into single spaces. */
        if (prev_mark->end < off && off < mark->beg)
          MD_TEXT(text_type, mdstringview(_T(" ")));
      } else if (text_type == TextType::raw_html) {
        /* Inside raw HTML, we output the new line verbatim, including
         * any trailing spaces. */
        OFF tmp = off;

        while (tmp < end && ISBLANK(tmp))
          tmp++;
        if (tmp > off)
          MD_TEXT(TextType::raw_html, ctx.text.substr(off, tmp - off));
        MD_TEXT(TextType::raw_html, mdstringview(_T("\n")));
      } else {
        /* Output soft or hard line break. */
        TextType break_type{TextType::soft_br};

        if (text_type == TextType::normal) {
          if (enforce_hardbreak)
            break_type = TextType::br;
          else if ((CH(line.end) == _T(' ') && CH(line.end + 1) == _T(' ')))
            break_type = TextType::br;
        }

        MD_TEXT(break_type, mdstringview(_T("\n")));
      }

      /* Move to the next line. */
      auto &&tmp_iter{lines.begin()};
      while (*tmp_iter != line)
        tmp_iter++;
      line = *tmp_iter++;
      off = line.beg;

      enforce_hardbreak = false;
    }
  }

abort:
  return ret;
}

/***************************
 ***  Processing Tables  ***
 ***************************/

static void md_analyze_table_alignment(Parsing_Context &ctx, OFF beg, OFF end,
                                       Align *align, int n_align) {
  static const Align align_map[] = {Align::default_align, Align::left,
                                    Align::right, Align::center};
  OFF off = beg;

  while (n_align > 0) {
    int index = 0; /* index into align_map[] */

    while (CH(off) != _T('-'))
      off++;
    if (off > beg && CH(off - 1) == _T(':'))
      index |= 1;
    while (off < end && CH(off) == _T('-'))
      off++;
    if (off < end && CH(off) == _T(':'))
      index |= 2;

    *align = align_map[index];
    align++;
    n_align--;
  }
}

/* Forward declaration. */
static int md_process_normal_block_contents(Parsing_Context &ctx,
                                            std::span<Line> lines);

static int md_process_table_cell(Parsing_Context &ctx, MD_BLOCKTYPE cell_type,
                                 Align align, OFF beg, OFF end) {
  Line line;
  td_Detail det;
  int ret = 0;

  while (beg < end && ISWHITESPACE(beg))
    beg++;
  while (end > beg && ISWHITESPACE(end - 1))
    end--;

  det.align = align;
  line.beg = beg;
  line.end = end;

  Line dummy[1]{line};

  MD_ENTER_BLOCK(cell_type, &det);
  MD_CHECK(md_process_normal_block_contents(ctx, dummy));
  MD_LEAVE_BLOCK(cell_type, &det);

abort:
  return ret;
}

static int md_process_table_row(Parsing_Context &ctx, MD_BLOCKTYPE cell_type,
                                OFF beg, OFF end, const Align *align,
                                int col_count) {
  int k;
  int ret = 0;
  Line dummy[1]{Line{beg, end}};
  int n = ctx.n_table_cell_boundaries + 2;

  /* Break the line into table cells by identifying pipe characters who
   * form the cell boundary. */
  MD_CHECK(md_analyze_inlines(ctx, dummy, true));

  /* We have to remember the cell boundaries in local buffer because
   * ctx.marks[] shall be reused during cell contents processing. */
  try {
    auto pipe_offs{std::make_unique<OFF[]>(n)};
    unsigned j = 0;
    pipe_offs[j++] = beg;
    for (int i = TABLECELLBOUNDARIES.head; i >= 0; i = ctx.marks[i].next) {
      Mark *mark = &ctx.marks[i];
      pipe_offs[j++] = mark->end;
    }
    pipe_offs[j++] = end + 1;

    /* Process cells. */
    MD_ENTER_BLOCK(BlockType::tr, nullptr);
    k = 0;
    for (unsigned i = 0; i < j - 1 && k < col_count; i++) {
      if (pipe_offs[i] < pipe_offs[i + 1] - 1)
        MD_CHECK(md_process_table_cell(ctx, cell_type, align[k++], pipe_offs[i],
                                       pipe_offs[i + 1] - 1));
    }
    /* Make sure we call enough table cells even if the current table contains
     * too few of them. */
    while (k < col_count)
      MD_CHECK(md_process_table_cell(ctx, cell_type, align[k++], 0, 0));
    MD_LEAVE_BLOCK(BlockType::tr, nullptr);
  } catch (const std::bad_alloc &e) {
    MD_LOG(mdstring(make_unique_failure_str) + mdstring(e.what()));
    ret = -1;
    goto abort;
  }

abort:

  /* Free any temporary memory blocks stored within some dummy marks. */
  for (int i = PTR_CHAIN.head; i >= 0; i = ctx.marks[i].next)
    free(md_mark_get_ptr(ctx, i));
  PTR_CHAIN.head = -1;
  PTR_CHAIN.tail = -1;

  return ret;
}

static int md_process_table_block_contents(Parsing_Context &ctx,
                                           unsigned col_count,
                                           std::span<Line> lines) {
  int ret = 0;

  /* At least two lines have to be present: The column headers and the line
   * with the underlines. */
  MD_ASSERT(lines.size() >= 2);

  try {
    const auto align{std::make_unique<Align[]>(col_count)};

    md_analyze_table_alignment(ctx, lines[1].beg, lines[1].end, align.get(),
                               col_count);

    MD_ENTER_BLOCK(BlockType::table_head, nullptr);
    MD_CHECK(md_process_table_row(ctx, BlockType::th, lines[0].beg,
                                  lines[0].end, align.get(), col_count));
    MD_LEAVE_BLOCK(BlockType::table_head, nullptr);

    if (lines.size() > 2) {
      MD_ENTER_BLOCK(BlockType::table_body, nullptr);
      for (const auto &line : lines.subspan(2)) {
        MD_CHECK(md_process_table_row(ctx, BlockType::td, line.beg, line.end,
                                      align.get(), col_count));
      }
      MD_LEAVE_BLOCK(BlockType::table_body, nullptr);
    }
  } catch (const std::bad_alloc &e) {
    MD_LOG(mdstring(make_unique_failure_str) + mdstring(e.what()));
    ret = -1;
    goto abort;
  }

abort:
  return ret;
}

/**************************
 ***  Processing Block  ***
 **************************/

#define MD_BLOCK_CONTAINER_OPENER 0x01
#define MD_BLOCK_CONTAINER_CLOSER 0x02
#define MD_BLOCK_CONTAINER                                                     \
  (MD_BLOCK_CONTAINER_OPENER | MD_BLOCK_CONTAINER_CLOSER)
#define MD_BLOCK_LOOSE_LIST 0x04
#define MD_BLOCK_SETEXT_HEADER 0x08

struct MD_BLOCK_tag {
  BlockType type : 8;
  unsigned flags : 8;

  /* MD_BLOCK_H:      Header level (1 - 6)
   * MD_BLOCK_CODE:   Non-zero if fenced, zero if indented.
   * MD_BLOCK_LI:     Task mark character (0 if not task list item, 'x', 'X' or
   * ' '). MD_BLOCK_TABLE:  Column count (as determined by the table underline).
   */
  unsigned data : 16;

  /* Leaf blocks:     Count of lines (MD_LINE or MD_VERBATIMLINE) on the block.
   * MD_BLOCK_LI:     Task mark offset in the input doc.
   * MD_BLOCK_OL:     Start item number.
   */
  unsigned n_lines;
};

struct MD_CONTAINER_tag {
  CHAR ch;
  bool is_loose;
  bool is_task;
  unsigned start;
  unsigned mark_indent;
  unsigned contents_indent;
  OFF block_byte_off;
  OFF task_mark_off;
};

static int md_process_normal_block_contents(Parsing_Context &ctx,
                                            std::span<Line> lines) {
  int i;
  int ret;

  MD_CHECK(md_analyze_inlines(ctx, lines, false));
  MD_CHECK(md_process_inlines(ctx, lines));

abort:
  /* Free any temporary memory blocks stored within some dummy marks. */
  for (i = PTR_CHAIN.head; i >= 0; i = ctx.marks[i].next)
    free(md_mark_get_ptr(ctx, i));
  PTR_CHAIN.head = -1;
  PTR_CHAIN.tail = -1;

  return ret;
}

static int
md_process_verbatim_block_contents(Parsing_Context &ctx, MD_TEXTTYPE text_type,
                                   std::span<MD_VERBATIMLINE> lines) {
  static const mdstring indent_chunk_str{_T("                ")};

  int ret = 0;

  for (auto &line : lines) {
    OFF indent = line.indent;

    /* Output code indentation. */
    while (indent > indent_chunk_str.size()) {
      MD_TEXT(text_type, indent_chunk_str);
      indent -= indent_chunk_str.size();
    }
    if (indent > 0)
      MD_TEXT(text_type, indent_chunk_str.substr(0, indent));

    /* Output the code line itself. */
    auto &&inout_line{ctx.text.substr(line.beg, line.end - line.beg)};
    MD_TEXT_INSECURE(text_type, inout_line);

    /* Enforce end-of-line. */
    MD_TEXT(text_type, mdstringview(_T("\n")));
  }

abort:
  return ret;
}

static int md_process_code_block_contents(Parsing_Context &ctx, int is_fenced,
                                          std::span<MD_VERBATIMLINE> lines) {
  auto line_iter{lines.begin()};
  // TODO: kludge
  auto n_lines{lines.size()};
  if (is_fenced) {
    /* Skip the first line in case of fenced code: It is the fence.
     * (Only the starting fence is present due to logic in md_analyze_line().)
     */
    line_iter++;
    n_lines--;
  } else {
    /* Ignore blank lines at start/end of indented code block. */
    while (line_iter->beg == line_iter->end) {
      line_iter++;
      n_lines--;
    }
    while (lines.back().beg == lines.back().end) {
      n_lines--;
    }
  }

  if (n_lines == 0)
    return 0;

  return md_process_verbatim_block_contents(
      ctx, TextType::code, std::span(line_iter, line_iter + n_lines));
}

static int md_setup_fenced_code_detail(Parsing_Context &ctx, const Block *block,
                                       code_Detail *det,
                                       Attribute_Build *info_build,
                                       Attribute_Build *lang_build) {
  const MD_VERBATIMLINE *fence_line = (const MD_VERBATIMLINE *)(block + 1);
  OFF beg = fence_line->beg;
  OFF end = fence_line->end;
  OFF lang_end;
  CHAR fence_ch = CH(fence_line->beg);
  int ret = 0;

  /* Skip the fence itself. */
  while (beg < ctx.text.size() && CH(beg) == fence_ch)
    beg++;
  /* Trim initial spaces. */
  while (beg < ctx.text.size() && CH(beg) == _T(' '))
    beg++;

  /* Trim trailing spaces. */
  while (end > beg && CH(end - 1) == _T(' '))
    end--;

  /* Build info string attribute. */
  MD_CHECK(
      info_build->build(ctx, ctx.text.substr(beg, end - beg), 0, det->info));

  /* Build info string attribute. */
  lang_end = beg;
  while (lang_end < end && !ISWHITESPACE(lang_end))
    lang_end++;
  MD_CHECK(lang_build->build(ctx, ctx.text.substr(beg, lang_end - beg), 0,
                             det->lang));

  det->fence_char = fence_ch;

abort:
  return ret;
}

static int md_process_leaf_block(Parsing_Context &ctx, const Block *block) {
  std::variant<h_Detail, code_Detail, table_Detail> det{};
  Attribute_Build info_build;
  Attribute_Build lang_build;
  bool is_in_tight_list;
  bool clean_fence_code_detail = false;
  int ret = 0;

  // memset(&det, 0, sizeof(det));

  if (ctx.cont.empty())
    is_in_tight_list = false;
  else
    is_in_tight_list = !ctx.cont[ctx.n_containers - 1].is_loose;

  switch (block->type) {
  case BlockType::heading:
    std::get<h_Detail>(det).level = block->data;
    break;

  case BlockType::code:
    /* For fenced code block, we may need to set the info string. */
    if (block->data != 0) {
      // memset(&det.code, 0, sizeof(code_Detail));
      clean_fence_code_detail = true;
      MD_CHECK(md_setup_fenced_code_detail(
          ctx, block, &std::get<code_Detail>(det), &info_build, &lang_build));
    }
    break;

  case BlockType::table:
    det = table_Detail{.col_count = block->data,
                       .head_row_count = 1,
                       .body_row_count = block->n_lines - 2};
    /*
     table_Detail& table {std::get<table_Detail>(detail)};
      table.col_count = block->data;
      table.head_row_count = 1;
      table.body_row_count = block->n_lines - 2;*/
    break;

  default:
    /* Noop. */
    break;
  }

  if (!is_in_tight_list || block->type != BlockType::paragraph)
    MD_ENTER_BLOCK(block->type, static_cast<void *>(&det));

  /* Process the block contents accordingly to is type. */
  switch (block->type) {
  case BlockType::hr:
    /* noop */
    break;

  case BlockType::code:
    MD_CHECK(md_process_code_block_contents(
        ctx, (block->data != 0),
        std::span((MD_VERBATIMLINE *)(block + 1), block->n_lines)));
    break;

  case BlockType::raw_html:
    MD_CHECK(md_process_verbatim_block_contents(
        ctx, TextType::raw_html,
        std::span((MD_VERBATIMLINE *)(block + 1), block->n_lines)));
    break;

  case BlockType::table:
    /* XXX: probably a terrible alternative to making each function take a
       std::span<const Line>... */
    {
      auto tmp = const_cast<Line *>(reinterpret_cast<const Line *>(block + 1));
      std::span lines(tmp, block->n_lines);
      MD_CHECK(md_process_table_block_contents(ctx, block->data, lines));
    }
    break;

  default:
    // See above comment.
    {
      auto tmp = const_cast<Line *>(reinterpret_cast<const Line *>(block + 1));
      std::span lines(tmp, block->n_lines);
      MD_CHECK(md_process_normal_block_contents(ctx, lines));
    }
    break;
  }

  if (!is_in_tight_list || block->type != BlockType::paragraph)
    MD_LEAVE_BLOCK(block->type, static_cast<void *>(&det));

abort:
  return ret;
}

static int md_process_all_blocks(Parsing_Context &ctx) {
  int byte_off = 0;
  int ret = 0;

  /* ctx.containers now is not needed for detection of lists and list items
   * so we reuse it for tracking what lists are loose or tight. We rely
   * on the fact the vector is large enough to hold the deepest nesting
   * level of lists. */
  ctx.n_containers = 0;

  while (byte_off < ctx.n_block_bytes) {
    Block *block = (Block *)((char *)ctx.block_bytes + byte_off);
    union {
      ul_Detail ul;
      ol_Detail ol;
      li_Detail li;
    } det;

    switch (block->type) {
    case BlockType::ul:
      det.ul.is_tight = !(block->flags & MD_BLOCK_LOOSE_LIST);
      det.ul.mark = (CHAR)block->data;
      break;

    case BlockType::ol:
      det.ol.start = block->n_lines;
      det.ol.is_tight = !(block->flags & MD_BLOCK_LOOSE_LIST);
      det.ol.mark_delimiter = (CHAR)block->data;
      break;

    case BlockType::li:
      det.li.is_task = (block->data != 0);
      det.li.task_mark = (CHAR)block->data;
      det.li.task_mark_offset = (OFF)block->n_lines;
      break;

    default:
      /* noop */
      break;
    }

    if (block->flags & MD_BLOCK_CONTAINER) {
      if (block->flags & MD_BLOCK_CONTAINER_CLOSER) {
        MD_LEAVE_BLOCK(block->type, &det);

        if (block->type == BlockType::ul ||
            block->type == BlockType::block_quote ||
            block->type == BlockType::ol)
          ctx.n_containers--;
      }

      if (block->flags & MD_BLOCK_CONTAINER_OPENER) {
        MD_ENTER_BLOCK(block->type, &det);

        if (block->type == BlockType::ul || block->type == BlockType::ol) {
          ctx.cont.back().is_loose = (block->flags & MD_BLOCK_LOOSE_LIST);
          ctx.n_containers++;
        } else if (block->type == BlockType::block_quote) {
          /* This causes that any text in a block quote, even if
           * nested inside a tight list item, is wrapped with
           * <p>...</p>. */
          ctx.cont.back().is_loose = true;
          ctx.n_containers++;
        }
      }
    } else {
      MD_CHECK(md_process_leaf_block(ctx, block));

      if (block->type == BlockType::code || block->type == BlockType::raw_html)
        byte_off += block->n_lines * sizeof(MD_VERBATIMLINE);
      else
        byte_off += block->n_lines * sizeof(Line);
    }

    byte_off += sizeof(Block);
  }

  ctx.n_block_bytes = 0;

abort:
  return ret;
}

/************************************
 ***  Grouping Lines into Blocks  ***
 ************************************/

static void *md_push_block_bytes(Parsing_Context &ctx, int n_bytes) {
  void *ptr;

  if (ctx.n_block_bytes + n_bytes > ctx.alloc_block_bytes) {
    void *new_block_bytes;

    ctx.alloc_block_bytes =
        (ctx.alloc_block_bytes > 0
             ? ctx.alloc_block_bytes + ctx.alloc_block_bytes / 2
             : 512);
    new_block_bytes = realloc(ctx.block_bytes, ctx.alloc_block_bytes);
    if (new_block_bytes == nullptr) {
      MD_LOG("realloc() failed.");
      return nullptr;
    }

    /* Fix the ->current_block after the reallocation. */
    if (ctx.current_block != nullptr) {
      OFF off_current_block =
          (OFF)((char *)ctx.current_block - (char *)ctx.block_bytes);
      ctx.current_block =
          (Block *)((char *)new_block_bytes + off_current_block);
    }

    ctx.block_bytes = new_block_bytes;
  }

  ptr = (char *)ctx.block_bytes + ctx.n_block_bytes;
  ctx.n_block_bytes += n_bytes;
  return ptr;
}

static int md_start_new_block(Parsing_Context &ctx, const Line_Analysis *line) {
  Block *block;

  MD_ASSERT(ctx.current_block == nullptr);

  block = (Block *)md_push_block_bytes(ctx, sizeof(Block));
  if (block == nullptr)
    return -1;

  switch (line->type) {
  case LineType::hr:
    block->type = BlockType::hr;
    break;

  case LineType::MD_LINE_ATXHEADER:
  case LineType::MD_LINE_SETEXTHEADER:
    block->type = BlockType::heading;
    break;

  case LineType::MD_LINE_FENCEDCODE:
  case LineType::MD_LINE_INDENTEDCODE:
    block->type = BlockType::code;
    break;

  case LineType::MD_LINE_TEXT:
    block->type = BlockType::paragraph;
    break;

  case LineType::raw_html:
    block->type = BlockType::raw_html;
    break;

  case LineType::blank:
  case LineType::MD_LINE_SETEXTUNDERLINE:
  case LineType::MD_LINE_TABLEUNDERLINE:
  default:
    MD_UNREACHABLE();
    break;
  }

  block->flags = 0;
  block->data = line->data;
  block->n_lines = 0;

  ctx.current_block = block;
  return 0;
}

/* Eat from start of current (textual) block any reference definitions and
 * remember them so we can resolve any links referring to them.
 *
 * (Reference definitions can only be at start of it as they cannot break
 * a paragraph.)
 */
static int md_consume_link_reference_definitions(Parsing_Context &ctx) {
  /* XXX: WTF? current_block's type is MD_BLOCK*, which when deferenced,
  contains the following memebers, with sizeof(MD_BLOCK)==8:
  BlockType type : 8;
  unsigned flags : 8;
  unsigned data : 16;
  unsigned n_lines; Probably dangerous assumption that sizeof(unsigned)==4 for
  all platforms
  How does this even convert to a Line*? sizeof(Line)==8, because
  it contains two members of type MD_OFFSET ~= unsigned (sizeof == 4)
  Also, does +1 increment the address by 1, or by sizeof(MD_BLOCK)?
  */
  Line *li = reinterpret_cast<Line *>(ctx.current_block + 1);
  std::span lines(li, ctx.current_block->n_lines);
  unsigned n = 0;

  /* Compute how many lines at the start of the block form one or more
   * reference definitions. */
  while (n < lines.size()) {
    int n_link_ref_lines =
        md_is_link_reference_definition(ctx, lines.subspan(n));
    /* Not a reference definition? */
    if (n_link_ref_lines == 0)
      break;

    /* We fail if it is the ref. def. but it could not be stored due
     * a memory allocation error. */
    if (n_link_ref_lines < 0)
      return -1;

    n += n_link_ref_lines;
  }

  /* If there was at least one reference definition, we need to remove
   * its lines from the block, or perhaps even the whole block. */
  if (n > 0) {
    if (n == lines.size()) {
      /* Remove complete block. */
      ctx.n_block_bytes -= n * sizeof(Line);
      ctx.n_block_bytes -= sizeof(Block);
      ctx.current_block = nullptr;
    } else {
      /* Remove just some initial lines from the block. */
      // TODO: get rid of memmove
      memmove(li, li + n, (lines.size() - n) * sizeof(Line));
      ctx.current_block->n_lines -= n;
      ctx.n_block_bytes -= n * sizeof(Line);
    }
  }

  return 0;
}

static int md_end_current_block(Parsing_Context &ctx) {
  int ret = 0;

  if (ctx.current_block == nullptr)
    return ret;

  /* Check whether there is a reference definition. (We do this here instead
   * of in md_analyze_line() because reference definition can take multiple
   * lines.) */
  if (ctx.current_block->type == BlockType::paragraph ||
      (ctx.current_block->type == BlockType::heading &&
       (ctx.current_block->flags & MD_BLOCK_SETEXT_HEADER))) {
    Line &line = *(Line *)(ctx.current_block + 1);
    if (CH(line.beg) == _T('[')) {
      MD_CHECK(md_consume_link_reference_definitions(ctx));
      if (ctx.current_block == nullptr)
        return ret;
    }
  }

  if (ctx.current_block->type == BlockType::heading &&
      (ctx.current_block->flags & MD_BLOCK_SETEXT_HEADER)) {
    unsigned n_lines = ctx.current_block->n_lines;

    if (n_lines > 1) {
      /* Get rid of the underline. */
      ctx.current_block->n_lines--;
      ctx.n_block_bytes -= sizeof(Line);
    } else {
      /* Only the underline has left after eating the ref. defs.
       * Keep the line as beginning of a new ordinary paragraph. */
      ctx.current_block->type = BlockType::paragraph;
      return 0;
    }
  }

  /* Mark we are not building any block anymore. */
  ctx.current_block = nullptr;

abort:
  return ret;
}

static int md_add_line_into_current_block(Parsing_Context &ctx,
                                          const Line_Analysis *analysis) {
  MD_ASSERT(ctx.current_block != nullptr);

  if (ctx.current_block->type == BlockType::code ||
      ctx.current_block->type == BlockType::raw_html) {
    MD_VERBATIMLINE *line;

    line = (MD_VERBATIMLINE *)md_push_block_bytes(ctx, sizeof(MD_VERBATIMLINE));
    if (line == nullptr)
      return -1;

    line->indent = analysis->indent;
    line->beg = analysis->beg;
    line->end = analysis->end;
  } else {
    Line *line;

    line = (Line *)md_push_block_bytes(ctx, sizeof(Line));
    if (line == nullptr)
      return -1;

    line->beg = analysis->beg;
    line->end = analysis->end;
  }
  ctx.current_block->n_lines++;

  return 0;
}

static int md_push_container_bytes(Parsing_Context &ctx, MD_BLOCKTYPE type,
                                   unsigned start, unsigned data,
                                   unsigned flags) {
  Block *block;
  int ret = 0;

  MD_CHECK(md_end_current_block(ctx));

  block = (Block *)md_push_block_bytes(ctx, sizeof(Block));
  if (block == nullptr)
    return -1;

  block->type = type;
  block->flags = flags;
  block->data = data;
  block->n_lines = start;

abort:
  return ret;
}

/***********************
 ***  Line Analysis  ***
 ***********************/

static int md_is_hr_line(Parsing_Context &ctx, OFF beg, OFF *p_end,
                         OFF *p_killer) {
  OFF off = beg + 1;
  int n = 1;

  while (off < ctx.text.size() &&
         (CH(off) == CH(beg) || CH(off) == _T(' ') || CH(off) == _T('\t'))) {
    if (CH(off) == CH(beg))
      n++;
    off++;
  }

  if (n < 3) {
    *p_killer = off;
    return false;
  }

  /* Nothing else can be present on the line. */
  if (off < ctx.text.size() && !ISNEWLINE(off)) {
    *p_killer = off;
    return false;
  }

  *p_end = off;
  return true;
}

static int md_is_atxheader_line(Parsing_Context &ctx, OFF beg, OFF *p_beg,
                                OFF *p_end, unsigned *p_level) {
  int n;
  OFF off = beg + 1;

  while (off < ctx.text.size() && CH(off) == _T('#') && off - beg < 7)
    off++;
  n = off - beg;

  if (n > 6)
    return false;
  *p_level = n;

  if (!(ctx.parser.flags & MD_FLAG_PERMISSIVEATXHEADERS) &&
      off < ctx.text.size() && CH(off) != _T(' ') && CH(off) != _T('\t') &&
      !ISNEWLINE(off))
    return false;

  while (off < ctx.text.size() && CH(off) == _T(' '))
    off++;
  *p_beg = off;
  *p_end = off;
  return true;
}

static bool md_is_setext_underline(Parsing_Context &ctx, OFF beg, OFF *p_end,
                                   unsigned *p_level) {
  OFF off = beg + 1;

  while (off < ctx.text.size() && CH(off) == CH(beg))
    off++;

  /* Optionally, space(s) can follow. */
  while (off < ctx.text.size() && CH(off) == _T(' '))
    off++;

  /* But nothing more is allowed on the line. */
  if (off < ctx.text.size() && !ISNEWLINE(off))
    return false;

  *p_level = (CH(beg) == _T('=') ? 1 : 2);
  *p_end = off;
  return true;
}

static bool md_is_table_underline(Parsing_Context &ctx, OFF beg, OFF *p_end,
                                  unsigned *p_col_count) {
  OFF off = beg;
  int found_pipe = false;
  unsigned col_count = 0;

  if (off < ctx.text.size() && CH(off) == _T('|')) {
    found_pipe = true;
    off++;
    while (off < ctx.text.size() && ISWHITESPACE(off))
      off++;
  }

  while (1) {
    OFF cell_beg;
    int delimited = false;

    /* Cell underline ("-----", ":----", "----:" or ":----:") */
    cell_beg = off;
    if (off < ctx.text.size() && CH(off) == _T(':'))
      off++;
    while (off < ctx.text.size() && CH(off) == _T('-'))
      off++;
    if (off < ctx.text.size() && CH(off) == _T(':'))
      off++;
    if (off - cell_beg < 3)
      return false;

    col_count++;

    /* Pipe delimiter (optional at the end of line). */
    while (off < ctx.text.size() && ISWHITESPACE(off))
      off++;
    if (off < ctx.text.size() && CH(off) == _T('|')) {
      delimited = true;
      found_pipe = true;
      off++;
      while (off < ctx.text.size() && ISWHITESPACE(off))
        off++;
    }

    /* Success, if we reach end of line. */
    if (off >= ctx.text.size() || ISNEWLINE(off))
      break;

    if (!delimited)
      return false;
  }

  if (!found_pipe)
    return false;

  *p_end = off;
  *p_col_count = col_count;
  return true;
}

static bool md_is_opening_code_fence(Parsing_Context &ctx, OFF beg,
                                     OFF *p_end) {
  OFF off = beg;

  while (off < ctx.text.size() && CH(off) == CH(beg))
    off++;

  /* Fence must have at least three characters. */
  if (off - beg < 3)
    return false;

  ctx.code_fence_length = off - beg;

  /* Optionally, space(s) can follow. */
  while (off < ctx.text.size() && CH(off) == _T(' '))
    off++;

  /* Optionally, an info string can follow. */
  while (off < ctx.text.size() && !ISNEWLINE(off)) {
    /* Backtick-based fence must not contain '`' in the info string. */
    if (CH(beg) == _T('`') && CH(off) == _T('`'))
      return false;
    off++;
  }

  *p_end = off;
  return true;
}

static bool md_is_closing_code_fence(Parsing_Context &ctx, CHAR ch, OFF beg,
                                     OFF *p_end) {
  OFF off = beg;
  int ret = false;

  /* Closing fence must have at least the same length and use same char as
   * opening one. */
  while (off < ctx.text.size() && CH(off) == ch)
    off++;
  if (off - beg < ctx.code_fence_length)
    goto out;

  /* Optionally, space(s) can follow */
  while (off < ctx.text.size() && CH(off) == _T(' '))
    off++;

  /* But nothing more is allowed on the line. */
  if (off < ctx.text.size() && !ISNEWLINE(off))
    goto out;

  ret = true;

out:
  /* Note we set *p_end even on failure: If we are not closing fence, caller
   * would eat the line anyway without any parsing. */
  *p_end = off;
  return ret;
}

/* Returns type of the raw HTML block, or false if it is not HTML block.
 * (Refer to CommonMark specification for details about the types.)
 */
static unsigned short md_is_html_block_start_condition(Parsing_Context &ctx,
                                                       OFF beg) {
  typedef struct TAG_tag TAG;
  struct TAG_tag {
    const CHAR *name;
    unsigned len : 8;
  };

  /* Type 6 is started by a long list of allowed tags. We use two-level
   * tree to speed-up the search. */
#ifdef X
#undef X
#endif
#define X(name)                                                                \
  { _T(name), (sizeof(name) - 1) / sizeof(CHAR) }
#define Xend                                                                   \
  { nullptr, 0 }
  static const TAG t1[] = {X("pre"), X("script"), X("style"), X("textarea"),
                           Xend};

  static const TAG a6[] = {X("address"), X("article"), X("aside"), Xend};
  static const TAG b6[] = {X("base"), X("basefont"), X("blockquote"), X("body"),
                           Xend};
  static const TAG c6[] = {X("caption"), X("center"), X("col"), X("colgroup"),
                           Xend};
  static const TAG d6[] = {X("dd"),  X("details"), X("dialog"), X("dir"),
                           X("div"), X("dl"),      X("dt"),     Xend};
  static const TAG f6[] = {
      X("fieldset"), X("figcaption"), X("figure"),   X("footer"),
      X("form"),     X("frame"),      X("frameset"), Xend};
  static const TAG h6[] = {X("h1"), X("head"), X("header"),
                           X("hr"), X("html"), Xend};
  static const TAG i6[] = {X("iframe"), Xend};
  static const TAG l6[] = {X("legend"), X("li"), X("link"), Xend};
  static const TAG m6[] = {X("main"), X("menu"), X("menuitem"), Xend};
  static const TAG n6[] = {X("nav"), X("noframes"), Xend};
  static const TAG o6[] = {X("ol"), X("optgroup"), X("option"), Xend};
  static const TAG p6[] = {X("p"), X("param"), Xend};
  static const TAG s6[] = {X("section"), X("source"), X("summary"), Xend};
  static const TAG t6[] = {X("table"), X("tbody"), X("td"), X("tfoot"), X("th"),
                           X("thead"), X("title"), X("tr"), X("track"), Xend};
  static const TAG u6[] = {X("ul"), Xend};
  static const TAG xx[] = {Xend};
#undef X

  static const TAG *map6[26] = {a6, b6, c6, d6, xx, f6, xx, h6, i6,
                                xx, xx, l6, m6, n6, o6, p6, xx, xx,
                                s6, t6, u6, xx, xx, xx, xx, xx};
  OFF off = beg + 1;
  int i;

  /* Check for type 1: <script, <pre, or <style */
  for (i = 0; t1[i].name != nullptr; i++)
    if (off + t1[i].len <= ctx.text.size())
      // auto t{std::string{STR(off)}};
      if (STR(off) == t1[i].name)
        return 1;

  /* Check for type 2: <!-- */
  if (off + 3 < ctx.text.size() && CH(off) == _T('!') &&
      CH(off + 1) == _T('-') && CH(off + 2) == _T('-'))
    return 2;

  /* Check for type 3: <? */
  if (off < ctx.text.size() && CH(off) == _T('?'))
    return 3;

  /* Check for type 4 or 5: <! */
  if (off < ctx.text.size() && CH(off) == _T('!')) {
    /* Check for type 4: <! followed by uppercase letter. */
    if (off + 1 < ctx.text.size() && ISASCII(off + 1))
      return 4;

    /* Check for type 5: <![CDATA[ */
    if (off + 8 < ctx.text.size())
      if (STR(off) == _T("![CDATA["))
        return 5;
  }

  /* Check for type 6: Many possible starting tags listed above. */
  if (off + 1 < ctx.text.size() &&
      (ISALPHA(off) || (CH(off) == _T('/') && ISALPHA(off + 1)))) {
    int slot;
    const TAG *tags;

    if (CH(off) == _T('/'))
      off++;

    slot = (ISUPPER(off) ? CH(off) - 'A' : CH(off) - 'a');
    tags = map6[slot];

    for (i = 0; tags[i].name != nullptr; i++) {
      if (off + tags[i].len <= ctx.text.size()) {
        if (is_case_insensitive_equal(STR(off), tags[i].name)) {
          OFF tmp = off + tags[i].len;
          if (tmp >= ctx.text.size())
            return 6;
          if (ISBLANK(tmp) || ISNEWLINE(tmp) || CH(tmp) == _T('>'))
            return 6;
          if (tmp + 1 < ctx.text.size() && CH(tmp) == _T('/') &&
              CH(tmp + 1) == _T('>'))
            return 6;
          break;
        }
      }
    }
  }

  /* Check for type 7: any COMPLETE other opening or closing tag. */
  if (off + 1 < ctx.text.size()) {
    OFF end;
    Line *dummy = nullptr;

    if (md_is_html_tag(ctx, std::span(dummy, 0), beg, ctx.text.size(), &end)) {
      /* Only optional whitespace and new line may follow. */
      while (end < ctx.text.size() && ISWHITESPACE(end))
        end++;
      if (end >= ctx.text.size() || ISNEWLINE(end))
        return 7;
    }
  }

  return false;
}

/* Case sensitive check whether there is a substring 'what' between 'beg'
 * and end of line. */
static bool md_line_contains(Parsing_Context &ctx, OFF beg, mdstringview what,
                             OFF *p_end) {
  auto newline_idx{ctx.text.find_first_of('\n', beg)},
      beginning_idx{ctx.text.substr(beg, newline_idx).find(what, beg)};

  if (beginning_idx != mdstring::npos) {
    *p_end = beginning_idx;
    return true;
  } else {
    *p_end = mdstring::npos;
    return false;
  }
  /*
  OFF i;
  for (i = beg; i + what_len < ctx.text.size(); i++) {
    if (ISNEWLINE(i))
      break;
    if (memcmp(STR(i), what, what_len * sizeof(CHAR)) == 0) {
      *p_end = i + what_len;
      return true;
    }
  }

  *p_end = i;
  return false;
  */
}

/* Returns type of HTML block end condition or false if not an end condition.
 *
 * Note it fills p_end even when it is not end condition as the caller
 * does not need to analyze contents of a raw HTML block.
 */
static int md_is_html_block_end_condition(Parsing_Context &ctx, OFF beg,
                                          OFF *p_end) {
  switch (ctx.html_block_type) {
  case 1: {
    OFF off = beg;

    while (off < ctx.text.size() && !ISNEWLINE(off)) {
      if (CH(off) == _T('<')) {
        if (is_case_insensitive_equal(STR(off), _T("</script>"))) {
          *p_end = off + 9;
          return true;
        }

        if (is_case_insensitive_equal(STR(off), _T("</style>"))) {
          *p_end = off + 8;
          return true;
        }

        if (is_case_insensitive_equal(STR(off), _T("</pre>"))) {
          *p_end = off + 6;
          return true;
        }
      }

      off++;
    }
    *p_end = off;
    return false;
  }

  case 2:
    return (md_line_contains(ctx, beg, _T("-->"), p_end) ? 2 : false);

  case 3:
    return (md_line_contains(ctx, beg, _T("?>"), p_end) ? 3 : false);

  case 4:
    return (md_line_contains(ctx, beg, _T(">"), p_end) ? 4 : false);

  case 5:
    return (md_line_contains(ctx, beg, _T("]]>"), p_end) ? 5 : false);

  case 6: /* Pass through */
  case 7:
    *p_end = beg;
    return (ISNEWLINE(beg) ? ctx.html_block_type : false);

  default:
    MD_UNREACHABLE();
  }
  return false;
}

static bool md_is_container_compatible(const Container &pivot,
                                       const Container &container) {
  /* Block quote has no "items" like lists. */
  if (container.ch == _T('>'))
    return false;

  if (container.ch != pivot.ch)
    return false;
  if (container.mark_indent > pivot.contents_indent)
    return false;

  return true;
}

static int md_push_container(Parsing_Context &ctx, const Container *container) {
  try {
    ctx.cont.emplace_back(*container);
    return 0;
  } catch (const std::exception &e) {
    MD_LOG(mdstring(vector_emplace_back_str) + mdstring(e.what()));
    return -1;
  }
  /*
  if (ctx.n_containers >= ctx.alloc_containers) {
    Container *new_containers;

    ctx.alloc_containers =
        (ctx.alloc_containers > 0
             ? ctx.alloc_containers + ctx.alloc_containers / 2
             : 16);
    new_containers = (Container *)realloc(
        ctx.containers, ctx.alloc_containers * sizeof(Container));
    if (new_containers == nullptr) {
      MD_LOG("realloc() failed.");
      return -1;
    }

    ctx.containers = new_containers;
  }

  memcpy(&ctx.containers[ctx.n_containers++], container, sizeof(Container));
  return 0;
  */
}

static int md_enter_child_containers(Parsing_Context &ctx,
                                     unsigned n_children) {
  int ret = 0;

  for (size_t i = ctx.cont.size() - n_children; i < ctx.cont.size(); i++) {
    Container &c{ctx.cont[i]};
    bool is_ordered_list = false;

    switch (c.ch) {
    case _T(')'):
    case _T('.'):
      is_ordered_list = true;
      [[fallthrough]];
    case _T('-'):
    case _T('+'):
    case _T('*'):
      /* Remember offset in ctx.block_bytes so we can revisit the
       * block if we detect it is a loose list. */
      md_end_current_block(ctx);
      c.block_byte_off = ctx.n_block_bytes;

      MD_CHECK(md_push_container_bytes(
          ctx, (is_ordered_list ? BlockType::ol : BlockType::ul), c.start, c.ch,
          MD_BLOCK_CONTAINER_OPENER));
      MD_CHECK(md_push_container_bytes(ctx, BlockType::li, c.task_mark_off,
                                       (c.is_task ? CH(c.task_mark_off) : 0),
                                       MD_BLOCK_CONTAINER_OPENER));
      break;

    case _T('>'):
      MD_CHECK(md_push_container_bytes(ctx, BlockType::block_quote, 0, 0,
                                       MD_BLOCK_CONTAINER_OPENER));
      break;

    default:
      MD_UNREACHABLE();
      break;
    }
  }

abort:
  return ret;
}

static int md_leave_child_containers(Parsing_Context &ctx, int n_keep) {
  int ret = 0;

  while (ctx.n_containers > n_keep) {
    const Container &c{ctx.cont[ctx.n_containers - 1]};
    int is_ordered_list = false;

    switch (c.ch) {
    case _T(')'):
    case _T('.'):
      is_ordered_list = true;
      [[fallthrough]];

    case _T('-'):
    case _T('+'):
    case _T('*'):
      MD_CHECK(md_push_container_bytes(ctx, BlockType::li, c.task_mark_off,
                                       (c.is_task ? CH(c.task_mark_off) : 0),
                                       MD_BLOCK_CONTAINER_CLOSER));
      MD_CHECK(md_push_container_bytes(
          ctx, (is_ordered_list ? BlockType::ol : BlockType::ul), 0, c.ch,
          MD_BLOCK_CONTAINER_CLOSER));
      break;

    case _T('>'):
      MD_CHECK(md_push_container_bytes(ctx, BlockType::block_quote, 0, 0,
                                       MD_BLOCK_CONTAINER_CLOSER));
      break;

    default:
      MD_UNREACHABLE();
      break;
    }

    ctx.n_containers--;
  }

abort:
  return ret;
}

static int md_is_container_mark(Parsing_Context &ctx, unsigned indent, OFF beg,
                                OFF *p_end, Container *p_container) {
  OFF off = beg;
  OFF max_end;

  if (off >= ctx.text.size() || indent >= ctx.code_indent_offset)
    return false;

  /* Check for block quote mark. */
  if (CH(off) == _T('>')) {
    off++;
    p_container->ch = _T('>');
    p_container->is_loose = false;
    p_container->is_task = false;
    p_container->mark_indent = indent;
    p_container->contents_indent = indent + 1;
    *p_end = off;
    return true;
  }

  /* Check for list item bullet mark. */
  if (ISANYOF(off, _T("-+*")) &&
      (off + 1 >= ctx.text.size() || ISBLANK(off + 1) || ISNEWLINE(off + 1))) {
    p_container->ch = CH(off);
    p_container->is_loose = false;
    p_container->is_task = false;
    p_container->mark_indent = indent;
    p_container->contents_indent = indent + 1;
    *p_end = off + 1;
    return true;
  }

  /* Check for ordered list item marks. */
  max_end = off + 9;
  if (max_end > ctx.text.size())
    max_end = ctx.text.size();
  p_container->start = 0;
  while (off < max_end && ISDIGIT(off)) {
    p_container->start = p_container->start * 10 + CH(off) - _T('0');
    off++;
  }
  if (off > beg && (CH(off) == _T('.') || CH(off) == _T(')')) &&
      (off + 1 >= ctx.text.size() || ISBLANK(off + 1) || ISNEWLINE(off + 1))) {
    p_container->ch = CH(off);
    p_container->is_loose = false;
    p_container->is_task = false;
    p_container->mark_indent = indent;
    p_container->contents_indent = indent + off - beg + 1;
    *p_end = off + 1;
    return true;
  }

  return false;
}

static unsigned md_line_indentation(Parsing_Context &ctx, unsigned total_indent,
                                    OFF beg, OFF *p_end) {
  OFF off = beg;
  unsigned indent = total_indent;

  while (off < ctx.text.size() && ISBLANK(off)) {
    if (CH(off) == _T('\t'))
      indent = (indent + 4) & ~3;
    else
      indent++;
    off++;
  }

  *p_end = off;
  return indent - total_indent;
}

static const Line_Analysis md_dummy_blank_line{LineType::blank, 0, 0, 0, 0};

/* Analyze type of the line and find some its properties. This serves as a
 * main input for determining type and boundaries of a block. */
static int md_analyze_line(Parsing_Context &ctx, OFF beg, OFF *p_end,
                           const Line_Analysis *pivot_line,
                           Line_Analysis *line) {
  unsigned total_indent = 0;
  unsigned n_parents = 0, n_brothers = 0, n_children = 0;
  Container container{};
  bool prev_line_has_list_loosening_effect =
      ctx.last_line_has_list_loosening_effect;
  OFF off = beg;
  OFF hr_killer = 0;
  int ret = 0;

  line->indent = md_line_indentation(ctx, total_indent, off, &off);
  total_indent += line->indent;
  line->beg = off;

  /* Given the indentation and block quote marks '>', determine how many of
   * the current containers are our parents. */
  while (n_parents < ctx.cont.size()) {
    Container &c{ctx.cont[n_parents]};

    if (c.ch == _T('>') && line->indent < ctx.code_indent_offset &&
        off < ctx.text.size() && CH(off) == _T('>')) {
      /* Block quote mark. */
      off++;
      total_indent++;
      line->indent = md_line_indentation(ctx, total_indent, off, &off);
      total_indent += line->indent;

      /* The optional 1st space after '>' is part of the block quote mark. */
      if (line->indent > 0)
        line->indent--;

      line->beg = off;

    } else if (c.ch != _T('>') && line->indent >= c.contents_indent) {
      /* List. */
      line->indent -= c.contents_indent;
    } else {
      break;
    }

    n_parents++;
  }

  if (off >= ctx.text.size() || ISNEWLINE(off)) {
    /* Blank line does not need any real indentation to be nested inside
     * a list. */
    if (n_brothers + n_children == 0) {
      while (n_parents < ctx.cont.size() && ctx.cont[n_parents].ch != _T('>'))
        n_parents++;
    }
  }

  while (true) {
    /* Check whether we are fenced code continuation. */
    if (pivot_line->type == LineType::MD_LINE_FENCEDCODE) {
      line->beg = off;

      /* We are another MD_LINE_FENCEDCODE unless we are closing fence
       * which we transform into MD_LINE_BLANK. */
      if (line->indent < ctx.code_indent_offset) {
        if (md_is_closing_code_fence(ctx, CH(pivot_line->beg), off, &off)) {
          line->type = LineType::blank;
          ctx.last_line_has_list_loosening_effect = false;
          break;
        }
      }

      /* Change indentation accordingly to the initial code fence. */
      if (n_parents == ctx.cont.size()) {
        if (line->indent > pivot_line->indent)
          line->indent -= pivot_line->indent;
        else
          line->indent = 0;

        line->type = LineType::MD_LINE_FENCEDCODE;
        break;
      }
    }

    /* Check whether we are HTML block continuation. */
    if (pivot_line->type == LineType::raw_html && ctx.html_block_type > 0) {
      if (n_parents < ctx.cont.size()) {
        /* HTML block is implicitly ended if the enclosing container
         * block ends. */
        ctx.html_block_type = 0;
      } else {
        int html_block_type = md_is_html_block_end_condition(ctx, off, &off);
        if (html_block_type > 0) {
          MD_ASSERT(html_block_type == ctx.html_block_type);

          /* Make sure this is the last line of the block. */
          ctx.html_block_type = 0;

          /* Some end conditions serve as blank lines at the same time. */
          if (html_block_type == 6 || html_block_type == 7) {
            line->type = LineType::blank;
            line->indent = 0;
            break;
          }
        }

        line->type = LineType::raw_html;
        n_parents = ctx.cont.size();
        break;
      }
    }

    /* Check for blank line. */
    if (off >= ctx.text.size() || ISNEWLINE(off)) {
      if (pivot_line->type == LineType::MD_LINE_INDENTEDCODE &&
          n_parents == ctx.cont.size()) {
        line->type = LineType::MD_LINE_INDENTEDCODE;
        if (line->indent > ctx.code_indent_offset)
          line->indent -= ctx.code_indent_offset;
        else
          line->indent = 0;
        ctx.last_line_has_list_loosening_effect = false;
      } else {
        line->type = LineType::blank;
        ctx.last_line_has_list_loosening_effect =
            (n_parents > 0 && n_brothers + n_children == 0 &&
             ctx.cont[n_parents - 1].ch != _T('>'));

#if 1
        /* See https://github.com/mity/md4c/issues/6
         *
         * This ugly checking tests we are in (yet empty) list item but
         * not its very first line (i.e. not the line with the list
         * item mark).
         *
         * If we are such a blank line, then any following non-blank
         * line which would be part of the list item actually has to
         * end the list because according to the specification, "a list
         * item can begin with at most one blank line."
         */
        if (n_parents > 0 && ctx.cont[n_parents - 1].ch != _T('>') &&
            n_brothers + n_children == 0 && ctx.current_block == nullptr &&
            ctx.n_block_bytes > (int)sizeof(Block)) {
          Block *top_block = reinterpret_cast<Block *>(
              (char *)ctx.block_bytes + ctx.n_block_bytes - sizeof(Block));
          if (top_block->type == BlockType::li)
            ctx.last_list_item_starts_with_two_blank_lines = true;
        }
#endif
      }
      break;
    } else {
#if 1
      /* This is the 2nd half of the hack. If the flag is set (i.e. there
       * was a 2nd blank line at the beginning of the list item) and if
       * we would otherwise still belong to the list item, we enforce
       * the end of the list. */
      ctx.last_line_has_list_loosening_effect = false;
      if (ctx.last_list_item_starts_with_two_blank_lines) {
        if (n_parents > 0 && ctx.cont[n_parents - 1].ch != _T('>') &&
            n_brothers + n_children == 0 && ctx.current_block == nullptr &&
            ctx.n_block_bytes > (int)sizeof(Block)) {
          Block *top_block = reinterpret_cast<Block *>(
              (char *)ctx.block_bytes + ctx.n_block_bytes - sizeof(Block));
          if (top_block->type == BlockType::li)
            n_parents--;
        }

        ctx.last_list_item_starts_with_two_blank_lines = false;
      }
#endif
    }

    /* Check whether we are Setext underline. */
    if (line->indent < ctx.code_indent_offset &&
        pivot_line->type == LineType::MD_LINE_TEXT && off < ctx.text.size() &&
        ISANYOF2(off, _T('='), _T('-')) && (n_parents == ctx.cont.size())) {
      unsigned level;

      if (md_is_setext_underline(ctx, off, &off, &level)) {
        line->type = LineType::MD_LINE_SETEXTUNDERLINE;
        line->data = level;
        break;
      }
    }

    /* Check for thematic break line. */
    if (line->indent < ctx.code_indent_offset && off < ctx.text.size() &&
        off >= hr_killer && ISANYOF(off, _T("-_*"))) {
      if (md_is_hr_line(ctx, off, &off, &hr_killer)) {
        line->type = LineType::hr;
        break;
      }
    }

    /* Check for "brother" container. I.e. whether we are another list item
     * in already started list. */
    if (n_parents < ctx.cont.size() && n_brothers + n_children == 0) {
      OFF tmp;

      if (md_is_container_mark(ctx, line->indent, off, &tmp, &container) &&
          md_is_container_compatible(ctx.cont[n_parents], container)) {
        pivot_line = &md_dummy_blank_line;

        off = tmp;

        total_indent += container.contents_indent - container.mark_indent;
        line->indent = md_line_indentation(ctx, total_indent, off, &off);
        total_indent += line->indent;
        line->beg = off;

        /* Some of the following whitespace actually still belongs to the mark.
         */
        if (off >= ctx.text.size() || ISNEWLINE(off)) {
          container.contents_indent++;
        } else if (line->indent <= ctx.code_indent_offset) {
          container.contents_indent += line->indent;
          line->indent = 0;
        } else {
          container.contents_indent += 1;
          line->indent--;
        }

        ctx.cont[n_parents].mark_indent = container.mark_indent;
        ctx.cont[n_parents].contents_indent = container.contents_indent;

        n_brothers++;
        continue;
      }
    }

    /* Check for indented code.
     * Note indented code block cannot interrupt a paragraph. */
    if (line->indent >= ctx.code_indent_offset &&
        (pivot_line->type == LineType::blank ||
         pivot_line->type == LineType::MD_LINE_INDENTEDCODE)) {
      line->type = LineType::MD_LINE_INDENTEDCODE;
      MD_ASSERT(line->indent >= ctx.code_indent_offset);
      line->indent -= ctx.code_indent_offset;
      line->data = 0;
      break;
    }

    /* Check for start of a new container block. */
    if (line->indent < ctx.code_indent_offset &&
        md_is_container_mark(ctx, line->indent, off, &off, &container)) {
      if (pivot_line->type == LineType::MD_LINE_TEXT &&
          n_parents == ctx.cont.size() &&
          (off >= ctx.text.size() || ISNEWLINE(off)) &&
          container.ch != _T('>')) {
        /* Noop. List mark followed by a blank line cannot interrupt a
         * paragraph. */
      } else if (pivot_line->type == LineType::MD_LINE_TEXT &&
                 n_parents == ctx.cont.size() &&
                 ISANYOF2_(container.ch, _T('.'), _T(')')) &&
                 container.start != 1) {
        /* Noop. Ordered list cannot interrupt a paragraph unless the start
         * index is 1. */
      } else {
        total_indent += container.contents_indent - container.mark_indent;
        line->indent = md_line_indentation(ctx, total_indent, off, &off);
        total_indent += line->indent;

        line->beg = off;
        line->data = container.ch;

        /* Some of the following whitespace actually still belongs to the mark.
         */
        if (off >= ctx.text.size() || ISNEWLINE(off)) {
          container.contents_indent++;
        } else if (line->indent <= ctx.code_indent_offset) {
          container.contents_indent += line->indent;
          line->indent = 0;
        } else {
          container.contents_indent += 1;
          line->indent--;
        }

        if (n_brothers + n_children == 0)
          pivot_line = &md_dummy_blank_line;

        if (n_children == 0)
          MD_CHECK(md_leave_child_containers(ctx, n_parents + n_brothers));

        n_children++;
        MD_CHECK(md_push_container(ctx, &container));
        continue;
      }
    }

    /* Check whether we are table continuation. */
    if (pivot_line->type == LineType::MD_LINE_TABLE &&
        n_parents == ctx.cont.size()) {
      line->type = LineType::MD_LINE_TABLE;
      break;
    }

    /* Check for ATX header. */
    if (line->indent < ctx.code_indent_offset && off < ctx.text.size() &&
        CH(off) == _T('#')) {
      unsigned level;

      if (md_is_atxheader_line(ctx, off, &line->beg, &off, &level)) {
        line->type = LineType::MD_LINE_ATXHEADER;
        line->data = level;
        break;
      }
    }

    /* Check whether we are starting code fence. */
    if (off < ctx.text.size() && ISANYOF2(off, _T('`'), _T('~'))) {
      if (md_is_opening_code_fence(ctx, off, &off)) {
        line->type = LineType::MD_LINE_FENCEDCODE;
        line->data = 1;
        break;
      }
    }

    /* Check for start of raw HTML block. */
    if (off < ctx.text.size() && CH(off) == _T('<') &&
        !(ctx.parser.flags & MD_FLAG_NOHTMLBLOCKS)) {
      ctx.html_block_type = md_is_html_block_start_condition(ctx, off);

      /* HTML block type 7 cannot interrupt paragraph. */
      if (ctx.html_block_type == 7 &&
          pivot_line->type == LineType::MD_LINE_TEXT)
        ctx.html_block_type = 0;

      if (ctx.html_block_type > 0) {
        /* The line itself also may immediately close the block. */
        if (md_is_html_block_end_condition(ctx, off, &off) ==
            ctx.html_block_type) {
          /* Make sure this is the last line of the block. */
          ctx.html_block_type = 0;
        }

        line->type = LineType::raw_html;
        break;
      }
    }

    /* Check for table underline. */
    if ((ctx.parser.flags & MD_FLAG_TABLES) &&
        pivot_line->type == LineType::MD_LINE_TEXT && off < ctx.text.size() &&
        ISANYOF3(off, _T('|'), _T('-'), _T(':')) &&
        n_parents == ctx.cont.size()) {
      unsigned col_count;

      if (ctx.current_block != nullptr && ctx.current_block->n_lines == 1 &&
          md_is_table_underline(ctx, off, &off, &col_count)) {
        line->data = col_count;
        line->type = LineType::MD_LINE_TABLEUNDERLINE;
        break;
      }
    }

    /* By default, we are normal text line. */
    line->type = LineType::MD_LINE_TEXT;
    if (pivot_line->type == LineType::MD_LINE_TEXT &&
        n_brothers + n_children == 0) {
      /* Lazy continuation. */
      n_parents = ctx.cont.size();
    }

    /* Check for task mark. */
    if ((ctx.parser.flags & MD_FLAG_TASKLISTS) && n_brothers + n_children > 0 &&
        ISANYOF_(ctx.cont[ctx.cont.size() - 1].ch, _T("-+*.)"))) {
      OFF tmp = off;

      while (tmp < ctx.text.size() && tmp < off + 3 && ISBLANK(tmp))
        tmp++;
      if (tmp + 2 < ctx.text.size() && CH(tmp) == _T('[') &&
          ISANYOF(tmp + 1, _T("xX ")) && CH(tmp + 2) == _T(']') &&
          (tmp + 3 == ctx.text.size() || ISBLANK(tmp + 3) ||
           ISNEWLINE(tmp + 3))) {
        Container &task_container{
            (n_children > 0 ? ctx.cont[ctx.n_containers - 1] : container)};
        task_container.is_task = true;
        task_container.task_mark_off = tmp + 1;
        off = tmp + 3;
        while (ISWHITESPACE(off))
          off++;
        line->beg = off;
      }
    }

    break;
  }

  /* Scan for end of the line.
   *
   * Note this is quite a bottleneck of the parsing as we here iterate almost
   * over compete document.
   */
#if defined __linux__ && !defined MD4C_USE_UTF16
  /* Recent glibc versions have superbly optimized strcspn(), even using
   * vectorization if available. */
  if (ctx.doc_ends_with_newline && off < ctx.text.size()) {
    while (true) {
      off += (OFF)strcspn(STR(off), "\r\n");

      /* strcspn() can stop on zero terminator; but that can appear
       * anywhere in the Markfown input... */
      if (CH(off) == _T('\0'))
        off++;
      else
        break;
    }
  } else
#endif
  {
    /* Optimization: Use some loop unrolling. */
    while (off + 3 < ctx.text.size() && !ISNEWLINE(off + 0) &&
           !ISNEWLINE(off + 1) && !ISNEWLINE(off + 2) && !ISNEWLINE(off + 3))
      off += 4;
    while (off < ctx.text.size() && !ISNEWLINE(off))
      off++;
  }

  /* Set end of the line. */
  line->end = off;

  /* But for ATX header, we should exclude the optional trailing mark. */
  if (line->type == LineType::MD_LINE_ATXHEADER) {
    OFF tmp = line->end;
    while (tmp > line->beg && CH(tmp - 1) == _T(' '))
      tmp--;
    while (tmp > line->beg && CH(tmp - 1) == _T('#'))
      tmp--;
    if (tmp == line->beg || CH(tmp - 1) == _T(' ') ||
        (ctx.parser.flags & MD_FLAG_PERMISSIVEATXHEADERS))
      line->end = tmp;
  }

  /* Trim trailing spaces. */
  if (line->type != LineType::MD_LINE_INDENTEDCODE &&
      line->type != LineType::MD_LINE_FENCEDCODE) {
    while (line->end > line->beg && CH(line->end - 1) == _T(' '))
      line->end--;
  }

  /* Eat also the new line. */
  if (off < ctx.text.size() && CH(off) == _T('\r'))
    off++;
  if (off < ctx.text.size() && CH(off) == _T('\n'))
    off++;

  *p_end = off;

  /* If we belong to a list after seeing a blank line, the list is loose. */
  if (prev_line_has_list_loosening_effect && line->type != LineType::blank &&
      n_parents + n_brothers > 0) {
    Container &c{ctx.cont[n_parents + n_brothers - 1]};
    if (c.ch != _T('>')) {
      Block *block = reinterpret_cast<Block *>(((char *)ctx.block_bytes) +
                                               c.block_byte_off);
      block->flags |= MD_BLOCK_LOOSE_LIST;
    }
  }

  /* Leave any containers we are not part of anymore. */
  if (n_children == 0 && n_parents + n_brothers < ctx.cont.size())
    MD_CHECK(md_leave_child_containers(ctx, n_parents + n_brothers));

  /* Enter any container we found a mark for. */
  if (n_brothers > 0) {
    MD_ASSERT(n_brothers == 1);
    MD_CHECK(md_push_container_bytes(
        ctx, BlockType::li, ctx.cont[n_parents].task_mark_off,
        (ctx.cont[n_parents].is_task ? CH(ctx.cont[n_parents].task_mark_off)
                                     : 0),
        MD_BLOCK_CONTAINER_CLOSER));
    MD_CHECK(md_push_container_bytes(
        ctx, BlockType::li, container.task_mark_off,
        (container.is_task ? CH(container.task_mark_off) : 0),
        MD_BLOCK_CONTAINER_OPENER));
    ctx.cont[n_parents].is_task = container.is_task;
    ctx.cont[n_parents].task_mark_off = container.task_mark_off;
  }

  if (n_children > 0)
    MD_CHECK(md_enter_child_containers(ctx, n_children));

abort:
  return ret;
}

static int md_process_line(Parsing_Context &ctx,
                           const Line_Analysis **p_pivot_line,
                           Line_Analysis *line) {
  const Line_Analysis *pivot_line = *p_pivot_line;
  int ret = 0;

  /* Blank line ends current leaf block. */
  if (line->type == LineType::blank) {
    MD_CHECK(md_end_current_block(ctx));
    *p_pivot_line = &md_dummy_blank_line;
    return 0;
  }

  /* Some line types form block on their own. */
  if (line->type == LineType::hr || line->type == LineType::MD_LINE_ATXHEADER) {
    MD_CHECK(md_end_current_block(ctx));

    /* Add our single-line block. */
    MD_CHECK(md_start_new_block(ctx, line));
    MD_CHECK(md_add_line_into_current_block(ctx, line));
    MD_CHECK(md_end_current_block(ctx));
    *p_pivot_line = &md_dummy_blank_line;
    return 0;
  }

  /* LineType::MD_LINE_SETEXTUNDERLINE changes meaning of the current block and
   * ends it.
   */
  if (line->type == LineType::MD_LINE_SETEXTUNDERLINE) {
    MD_ASSERT(ctx.current_block != nullptr);
    ctx.current_block->type = BlockType::heading;
    ctx.current_block->data = line->data;
    ctx.current_block->flags |= MD_BLOCK_SETEXT_HEADER;
    MD_CHECK(md_add_line_into_current_block(ctx, line));
    MD_CHECK(md_end_current_block(ctx));
    if (ctx.current_block == nullptr) {
      *p_pivot_line = &md_dummy_blank_line;
    } else {
      /* This happens if we have consumed all the body as link ref. defs.
       * and downgraded the underline into start of a new paragraph block. */
      line->type = LineType::MD_LINE_TEXT;
      *p_pivot_line = line;
    }
    return 0;
  }

  /* LineType::MD_LINE_TABLEUNDERLINE changes meaning of the current block. */
  if (line->type == LineType::MD_LINE_TABLEUNDERLINE) {
    MD_ASSERT(ctx.current_block != nullptr);
    MD_ASSERT(ctx.current_block->n_lines == 1);
    ctx.current_block->type = BlockType::table;
    ctx.current_block->data = line->data;
    MD_ASSERT(pivot_line != &md_dummy_blank_line);
    ((Line_Analysis *)pivot_line)->type = LineType::MD_LINE_TABLE;
    MD_CHECK(md_add_line_into_current_block(ctx, line));
    return 0;
  }

  /* The current block also ends if the line has different type. */
  if (line->type != pivot_line->type)
    MD_CHECK(md_end_current_block(ctx));

  /* The current line may start a new block. */
  if (ctx.current_block == nullptr) {
    MD_CHECK(md_start_new_block(ctx, line));
    *p_pivot_line = line;
  }

  /* In all other cases the line is just a continuation of the current block. */
  MD_CHECK(md_add_line_into_current_block(ctx, line));

abort:
  return ret;
}

static int md_process_doc(Parsing_Context &ctx) {
  const Line_Analysis *pivot_line = &md_dummy_blank_line;
  Line_Analysis line_buf[2];
  Line_Analysis *line = &line_buf[0];
  OFF off = 0;
  int ret = 0;

  MD_ENTER_BLOCK(BlockType::body, nullptr);

  while (off < ctx.text.size()) {
    if (line == pivot_line)
      line = (line == &line_buf[0] ? &line_buf[1] : &line_buf[0]);

    MD_CHECK(md_analyze_line(ctx, off, &off, pivot_line, line));
    MD_CHECK(md_process_line(ctx, &pivot_line, line));
  }

  md_end_current_block(ctx);

  MD_CHECK(md_build_ref_def_hashtable(ctx));

  /* Process all blocks. */
  MD_CHECK(md_leave_child_containers(ctx, 0));
  MD_CHECK(md_process_all_blocks(ctx));

  MD_LEAVE_BLOCK(BlockType::body, nullptr);

abort:

#if 0
    /* Output some memory consumption statistics. */
    {
        char buffer[256];
        sprintf(buffer, "Alloced %u bytes for block buffer.",
                    (unsigned)(ctx.alloc_block_bytes));
        MD_LOG(buffer);

        sprintf(buffer, "Alloced %u bytes for containers buffer.",
                    (unsigned)(ctx.alloc_containers * sizeof(MD_CONTAINER)));
        MD_LOG(buffer);

        sprintf(buffer, "Alloced %u bytes for marks buffer.",
                    (unsigned)(ctx.alloc_marks * sizeof(MD_MARK)));
        MD_LOG(buffer);

        sprintf(buffer, "Alloced %u bytes for aux. buffer.",
                    (unsigned)(ctx.alloc_buffer * sizeof(MD_CHAR)));
        MD_LOG(buffer);
    }
#endif

  return ret;
}

/********************
 ***  Public API  ***
 ********************/
int md_parse(mdstringview text, const MD_PARSER &parser, void *userdata) {

  Parsing_Context ctx{};
  int ret;

  if (parser.abi_version != 0) {
    if (parser.debug_log != nullptr)
      parser.debug_log("Unsupported abi_version.", userdata);
    return -1;
  }

  /* Setup context structure. */
  ctx.text = text;
  ctx.parser = parser;
  ctx.userdata = userdata;
  ctx.code_indent_offset =
      (ctx.parser.flags & MD_FLAG_NOINDENTEDCODEBLOCKS) ? -1 : 4;
  md_build_mark_char_map(ctx);
  ctx.doc_ends_with_newline = (text.size() > 0 && ISNEWLINE_(text.back()));

  /* Reset all unresolved opener mark chains. */
  for (auto &mark : ctx.mark_chains)
    mark = {-1, -1};
  ctx.unresolved_link_head = -1;
  ctx.unresolved_link_tail = -1;

  /* All the work. */
  ret = md_process_doc(ctx);

  /* Clean-up. */
  md_free_ref_defs(ctx);
  md_free_ref_def_hashtable(ctx);
  free(ctx.marks);
  free(ctx.block_bytes);

  return ret;
}

extern "C" {
int md_parse(const MD_CHAR *text, MD_SIZE size, const MD_PARSER *parser,
             void *userdata) {
  return md_parse(mdstringview(text, size), *parser, userdata);
}
}
