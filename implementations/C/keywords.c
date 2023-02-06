/* ANSI-C code produced by gperf version 3.1 */
/* Command-line: gperf --output-file=keywords.c --readonly-tables --enum --includes --hash-function-name=keyword_hash KEYWORDS.txt  */
/* Computed positions: -k'1-3,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

#line 9 "KEYWORDS.txt"

#include "joy.h"
#include "definitions.h"
#line 13 "KEYWORDS.txt"
struct dict_entry;
#include <string.h>
/* maximum key range = 243, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
keyword_hash (register const char *str, register size_t len)
{
  static const unsigned char asso_values[] =
    {
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244,  65, 244, 244,
      244, 244,  60,  50, 244,  30, 244,  25, 244,  70,
       60,  35, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244,   5, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244,  25,  10,   0,
        5,  30,  65, 105,  40,  15, 244,  85,  40,  25,
        0,  15,   0,  50,  70,  30,  25,   0,  35,  75,
        0,  10,   0, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244, 244, 244, 244, 244,
      244, 244, 244, 244, 244, 244
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[2]];
      /*FALLTHROUGH*/
      case 2:
        hval += asso_values[(unsigned char)str[1]];
      /*FALLTHROUGH*/
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

const struct dict_entry *
in_word_set (register const char *str, register size_t len)
{
  enum
    {
      TOTAL_KEYWORDS = 115,
      MIN_WORD_LENGTH = 1,
      MAX_WORD_LENGTH = 14,
      MIN_HASH_VALUE = 1,
      MAX_HASH_VALUE = 243
    };

  static const struct dict_entry wordlist[] =
    {
      {""},
#line 129 "KEYWORDS.txt"
      {"x", def_x},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 27 "KEYWORDS.txt"
      {"dup", dup},
      {""}, {""},
#line 65 "KEYWORDS.txt"
      {"dupdip", def_dupdip},
      {""}, {""},
#line 63 "KEYWORDS.txt"
      {"dupd", def_dupd},
#line 64 "KEYWORDS.txt"
      {"dupdd", def_dupdd},
      {""},
#line 66 "KEYWORDS.txt"
      {"dupdipd", def_dupdipd},
#line 31 "KEYWORDS.txt"
      {"pop", pop},
      {""},
#line 91 "KEYWORDS.txt"
      {"popop", def_popop},
#line 50 "KEYWORDS.txt"
      {"b", def_b},
#line 92 "KEYWORDS.txt"
      {"popopop", def_popopop},
#line 26 "KEYWORDS.txt"
      {"dip", dip},
#line 89 "KEYWORDS.txt"
      {"popd", def_popd},
#line 90 "KEYWORDS.txt"
      {"popdd", def_popdd},
#line 93 "KEYWORDS.txt"
      {"popopd", def_popopd},
#line 94 "KEYWORDS.txt"
      {"popopdd", def_popopdd},
#line 23 "KEYWORDS.txt"
      {"cmp", cmp_joyfunc},
#line 59 "KEYWORDS.txt"
      {"dipd", def_dipd},
      {""},
#line 29 "KEYWORDS.txt"
      {"i", i_joyfunc},
      {""}, {""},
#line 47 "KEYWORDS.txt"
      {"appN", def_appN},
      {""},
#line 124 "KEYWORDS.txt"
      {"uncons", def_uncons},
#line 52 "KEYWORDS.txt"
      {"ccccons", def_ccccons},
      {""},
#line 56 "KEYWORDS.txt"
      {"codi", def_codi},
#line 123 "KEYWORDS.txt"
      {"unary", def_unary},
#line 51 "KEYWORDS.txt"
      {"binary", def_binary},
      {""},
#line 57 "KEYWORDS.txt"
      {"codireco", def_codireco},
#line 125 "KEYWORDS.txt"
      {"unit", def_unit},
      {""},
#line 25 "KEYWORDS.txt"
      {"concat", concat},
#line 77 "KEYWORDS.txt"
      {"ii", def_ii},
      {""},
#line 24 "KEYWORDS.txt"
      {"cons", cons},
#line 53 "KEYWORDS.txt"
      {"ccons", def_ccons},
#line 19 "KEYWORDS.txt"
      {"/", tdiv_q},
#line 88 "KEYWORDS.txt"
      {"pm", def_pm},
#line 81 "KEYWORDS.txt"
      {"mod", def_mod},
#line 58 "KEYWORDS.txt"
      {"dinfrirst", def_dinfrirst},
      {""}, {""},
#line 85 "KEYWORDS.txt"
      {"nullary", def_nullary},
      {""},
#line 55 "KEYWORDS.txt"
      {"clop", def_clop},
#line 84 "KEYWORDS.txt"
      {"nulco", def_nulco},
#line 18 "KEYWORDS.txt"
      {"-", sub},
#line 60 "KEYWORDS.txt"
      {"disenstacken", def_disenstacken},
#line 126 "KEYWORDS.txt"
      {"unquoted", def_unquoted},
#line 46 "KEYWORDS.txt"
      {"app3", def_app3},
      {""}, {""},
#line 127 "KEYWORDS.txt"
      {"unswons", def_unswons},
#line 83 "KEYWORDS.txt"
      {"not", def_not},
#line 67 "KEYWORDS.txt"
      {"enstacken", def_enstacken},
      {""},
#line 103 "KEYWORDS.txt"
      {"second", def_second},
#line 118 "KEYWORDS.txt"
      {"tailrec", def_tailrec},
#line 102 "KEYWORDS.txt"
      {"run", def_run},
#line 30 "KEYWORDS.txt"
      {"loop", loop},
      {""},
#line 96 "KEYWORDS.txt"
      {"quoted", def_quoted},
#line 48 "KEYWORDS.txt"
      {"at", def_at},
#line 87 "KEYWORDS.txt"
      {"pam", def_pam},
#line 106 "KEYWORDS.txt"
      {"size", def_size},
#line 75 "KEYWORDS.txt"
      {"hypot", def_hypot},
#line 107 "KEYWORDS.txt"
      {"spiral_next", def_spiral_next},
      {""},
#line 114 "KEYWORDS.txt"
      {"sum", def_sum},
#line 20 "KEYWORDS.txt"
      {"bool", truthy},
      {""},
#line 43 "KEYWORDS.txt"
      {"anamorphism", def_anamorphism},
      {""}, {""},
#line 45 "KEYWORDS.txt"
      {"app2", def_app2},
#line 121 "KEYWORDS.txt"
      {"third", def_third},
#line 111 "KEYWORDS.txt"
      {"stackd", def_stackd},
#line 38 "KEYWORDS.txt"
      {"lt", def_lt},
#line 113 "KEYWORDS.txt"
      {"stuncons", def_stuncons},
#line 62 "KEYWORDS.txt"
      {"drop", def_drop},
      {""}, {""}, {""},
#line 42 "KEYWORDS.txt"
      {"abs", def_abs},
#line 44 "KEYWORDS.txt"
      {"app1", def_app1},
#line 105 "KEYWORDS.txt"
      {"shunt", def_shunt},
#line 17 "KEYWORDS.txt"
      {"+", add},
#line 40 "KEYWORDS.txt"
      {"le", def_le},
#line 108 "KEYWORDS.txt"
      {"split_at", def_split_at},
      {""},
#line 109 "KEYWORDS.txt"
      {"split_list", def_split_list},
#line 54 "KEYWORDS.txt"
      {"cleave", def_cleave},
      {""}, {""},
#line 112 "KEYWORDS.txt"
      {"step_zero", def_step_zero},
#line 78 "KEYWORDS.txt"
      {"infra", def_infra},
#line 79 "KEYWORDS.txt"
      {"infrst", def_infrst},
      {""}, {""},
#line 122 "KEYWORDS.txt"
      {"tuck", def_tuck},
#line 104 "KEYWORDS.txt"
      {"shift", def_shift},
      {""},
#line 95 "KEYWORDS.txt"
      {"product", def_product},
#line 71 "KEYWORDS.txt"
      {"gcd", def_gcd},
#line 99 "KEYWORDS.txt"
      {"reco", def_reco},
      {""},
#line 16 "KEYWORDS.txt"
      {"*", mul},
#line 61 "KEYWORDS.txt"
      {"down_to_zero", def_down_to_zero},
#line 98 "KEYWORDS.txt"
      {"range_to_zero", def_range_to_zero},
      {""}, {""},
#line 70 "KEYWORDS.txt"
      {"fourth", def_fourth},
#line 49 "KEYWORDS.txt"
      {"average", def_average},
      {""}, {""},
#line 97 "KEYWORDS.txt"
      {"range", def_range},
#line 15 "KEYWORDS.txt"
      {"%", tdiv_r},
#line 36 "KEYWORDS.txt"
      {"eq", def_eq},
#line 39 "KEYWORDS.txt"
      {"neq", def_neq},
#line 35 "KEYWORDS.txt"
      {"swap", swap},
      {""}, {""},
#line 68 "KEYWORDS.txt"
      {"flatten", def_flatten},
      {""},
#line 76 "KEYWORDS.txt"
      {"ifte", def_ifte},
#line 115 "KEYWORDS.txt"
      {"swapd", def_swapd},
#line 72 "KEYWORDS.txt"
      {"genrec", def_genrec},
#line 120 "KEYWORDS.txt"
      {"ternary", def_ternary},
      {""}, {""},
#line 22 "KEYWORDS.txt"
      {"clear", clear},
      {""},
#line 86 "KEYWORDS.txt"
      {"of", def_of},
      {""}, {""}, {""},
#line 21 "KEYWORDS.txt"
      {"branch", branch},
#line 117 "KEYWORDS.txt"
      {"swoncat", def_swoncat},
      {""}, {""},
#line 116 "KEYWORDS.txt"
      {"swons", def_swons},
      {""},
#line 37 "KEYWORDS.txt"
      {"gt", def_gt},
      {""},
#line 32 "KEYWORDS.txt"
      {"rest", rest},
      {""}, {""}, {""}, {""}, {""},
#line 128 "KEYWORDS.txt"
      {"while", def_while},
      {""},
#line 41 "KEYWORDS.txt"
      {"ge", def_ge},
      {""},
#line 119 "KEYWORDS.txt"
      {"take", def_take},
#line 33 "KEYWORDS.txt"
      {"stack", stack},
      {""},
#line 100 "KEYWORDS.txt"
      {"reverse", def_reverse},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 28 "KEYWORDS.txt"
      {"first", first},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 101 "KEYWORDS.txt"
      {"rrest", def_rrest},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 73 "KEYWORDS.txt"
      {"grabN", def_grabN},
      {""}, {""}, {""},
#line 74 "KEYWORDS.txt"
      {"grba", def_grba},
      {""}, {""}, {""}, {""},
#line 80 "KEYWORDS.txt"
      {"make_generator", def_make_generator},
      {""},
#line 34 "KEYWORDS.txt"
      {"swaack", swaack},
      {""},
#line 110 "KEYWORDS.txt"
      {"sqr", def_sqr},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 69 "KEYWORDS.txt"
      {"fork", def_fork},
      {""}, {""}, {""},
#line 82 "KEYWORDS.txt"
      {"neg", def_neg}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register unsigned int key = keyword_hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return 0;
}
