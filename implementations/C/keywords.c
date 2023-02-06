/* ANSI-C code produced by gperf version 3.1 */
/* Command-line: gperf --output-file=keywords.c --readonly-tables --enum --includes --hash-function-name=keyword_hash KEYWORDS.txt  */
/* Computed positions: -k'2-3,$' */

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
/* maximum key range = 189, duplicates = 0 */

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
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190,  80, 190, 190,
      190, 190,  65,  40, 190,  10, 190,   5, 190, 105,
       90,  70, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190,  35, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190,  55,  20,  65,
        5,  35,  40,  30,  30,   0,  15,  90,  10,  10,
       15,   5,   0,  15,  65,  20,  45,  15,  65,  50,
       15,  75, 190,  25, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190, 190, 190, 190,
      190, 190, 190, 190, 190, 190, 190
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[2]+1];
      /*FALLTHROUGH*/
      case 2:
        hval += asso_values[(unsigned char)str[1]];
      /*FALLTHROUGH*/
      case 1:
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

const struct dict_entry *
in_word_set (register const char *str, register size_t len)
{
  enum
    {
      TOTAL_KEYWORDS = 109,
      MIN_WORD_LENGTH = 1,
      MAX_WORD_LENGTH = 14,
      MIN_HASH_VALUE = 1,
      MAX_HASH_VALUE = 189
    };

  static const struct dict_entry wordlist[] =
    {
      {""},
#line 29 "KEYWORDS.txt"
      {"i", i_joyfunc},
#line 71 "KEYWORDS.txt"
      {"ii", def_ii},
      {""}, {""}, {""},
#line 19 "KEYWORDS.txt"
      {"/", tdiv_q},
      {""}, {""},
#line 30 "KEYWORDS.txt"
      {"loop", loop},
      {""},
#line 18 "KEYWORDS.txt"
      {"-", sub},
      {""}, {""},
#line 49 "KEYWORDS.txt"
      {"clop", def_clop},
      {""},
#line 123 "KEYWORDS.txt"
      {"x", def_x},
      {""},
#line 26 "KEYWORDS.txt"
      {"dip", dip},
#line 20 "KEYWORDS.txt"
      {"bool", truthy},
      {""},
#line 44 "KEYWORDS.txt"
      {"b", def_b},
#line 82 "KEYWORDS.txt"
      {"pm", def_pm},
#line 31 "KEYWORDS.txt"
      {"pop", pop},
#line 53 "KEYWORDS.txt"
      {"dipd", def_dipd},
#line 85 "KEYWORDS.txt"
      {"popop", def_popop},
#line 90 "KEYWORDS.txt"
      {"quoted", def_quoted},
#line 86 "KEYWORDS.txt"
      {"popopop", def_popopop},
#line 23 "KEYWORDS.txt"
      {"cmp", cmp_joyfunc},
#line 83 "KEYWORDS.txt"
      {"popd", def_popd},
#line 84 "KEYWORDS.txt"
      {"popdd", def_popdd},
#line 87 "KEYWORDS.txt"
      {"popopd", def_popopd},
#line 88 "KEYWORDS.txt"
      {"popopdd", def_popopdd},
#line 27 "KEYWORDS.txt"
      {"dup", dup},
#line 24 "KEYWORDS.txt"
      {"cons", cons},
#line 78 "KEYWORDS.txt"
      {"nulco", def_nulco},
#line 59 "KEYWORDS.txt"
      {"dupdip", def_dupdip},
#line 55 "KEYWORDS.txt"
      {"down_to_zero", def_down_to_zero},
#line 96 "KEYWORDS.txt"
      {"run", def_run},
#line 57 "KEYWORDS.txt"
      {"dupd", def_dupd},
#line 58 "KEYWORDS.txt"
      {"dupdd", def_dupdd},
#line 17 "KEYWORDS.txt"
      {"+", add},
#line 60 "KEYWORDS.txt"
      {"dupdipd", def_dupdipd},
#line 108 "KEYWORDS.txt"
      {"sum", def_sum},
#line 50 "KEYWORDS.txt"
      {"codi", def_codi},
      {""},
#line 118 "KEYWORDS.txt"
      {"uncons", def_uncons},
      {""},
#line 75 "KEYWORDS.txt"
      {"mod", def_mod},
#line 93 "KEYWORDS.txt"
      {"reco", def_reco},
      {""},
#line 97 "KEYWORDS.txt"
      {"second", def_second},
#line 62 "KEYWORDS.txt"
      {"flatten", def_flatten},
#line 51 "KEYWORDS.txt"
      {"codireco", def_codireco},
#line 41 "KEYWORDS.txt"
      {"appN", def_appN},
#line 115 "KEYWORDS.txt"
      {"third", def_third},
#line 37 "KEYWORDS.txt"
      {"anamorphism", def_anamorphism},
      {""}, {""},
#line 52 "KEYWORDS.txt"
      {"dinfrirst", def_dinfrirst},
      {""},
#line 25 "KEYWORDS.txt"
      {"concat", concat},
      {""},
#line 102 "KEYWORDS.txt"
      {"split_at", def_split_at},
#line 100 "KEYWORDS.txt"
      {"size", def_size},
#line 103 "KEYWORDS.txt"
      {"split_list", def_split_list},
#line 16 "KEYWORDS.txt"
      {"*", mul},
      {""},
#line 77 "KEYWORDS.txt"
      {"not", def_not},
#line 56 "KEYWORDS.txt"
      {"drop", def_drop},
#line 28 "KEYWORDS.txt"
      {"first", first},
#line 101 "KEYWORDS.txt"
      {"spiral_next", def_spiral_next},
#line 54 "KEYWORDS.txt"
      {"disenstacken", def_disenstacken},
      {""},
#line 35 "KEYWORDS.txt"
      {"swap", swap},
#line 110 "KEYWORDS.txt"
      {"swons", def_swons},
#line 105 "KEYWORDS.txt"
      {"stackd", def_stackd},
      {""},
#line 92 "KEYWORDS.txt"
      {"range_to_zero", def_range_to_zero},
#line 119 "KEYWORDS.txt"
      {"unit", def_unit},
#line 109 "KEYWORDS.txt"
      {"swapd", def_swapd},
#line 15 "KEYWORDS.txt"
      {"%", tdiv_r},
#line 80 "KEYWORDS.txt"
      {"of", def_of},
#line 81 "KEYWORDS.txt"
      {"pam", def_pam},
#line 61 "KEYWORDS.txt"
      {"enstacken", def_enstacken},
#line 122 "KEYWORDS.txt"
      {"while", def_while},
#line 45 "KEYWORDS.txt"
      {"binary", def_binary},
#line 121 "KEYWORDS.txt"
      {"unswons", def_unswons},
#line 36 "KEYWORDS.txt"
      {"abs", def_abs},
#line 40 "KEYWORDS.txt"
      {"app3", def_app3},
#line 47 "KEYWORDS.txt"
      {"ccons", def_ccons},
#line 48 "KEYWORDS.txt"
      {"cleave", def_cleave},
#line 42 "KEYWORDS.txt"
      {"at", def_at},
#line 120 "KEYWORDS.txt"
      {"unquoted", def_unquoted},
#line 70 "KEYWORDS.txt"
      {"ifte", def_ifte},
#line 98 "KEYWORDS.txt"
      {"shift", def_shift},
#line 73 "KEYWORDS.txt"
      {"infrst", def_infrst},
#line 46 "KEYWORDS.txt"
      {"ccccons", def_ccccons},
#line 76 "KEYWORDS.txt"
      {"neg", def_neg},
#line 106 "KEYWORDS.txt"
      {"step_zero", def_step_zero},
#line 91 "KEYWORDS.txt"
      {"range", def_range},
      {""},
#line 111 "KEYWORDS.txt"
      {"swoncat", def_swoncat},
#line 104 "KEYWORDS.txt"
      {"sqr", def_sqr},
#line 113 "KEYWORDS.txt"
      {"take", def_take},
#line 72 "KEYWORDS.txt"
      {"infra", def_infra},
#line 64 "KEYWORDS.txt"
      {"fourth", def_fourth},
#line 79 "KEYWORDS.txt"
      {"nullary", def_nullary},
#line 65 "KEYWORDS.txt"
      {"gcd", def_gcd},
#line 39 "KEYWORDS.txt"
      {"app2", def_app2},
      {""},
#line 66 "KEYWORDS.txt"
      {"genrec", def_genrec},
      {""}, {""},
#line 116 "KEYWORDS.txt"
      {"tuck", def_tuck},
#line 117 "KEYWORDS.txt"
      {"unary", def_unary},
      {""},
#line 89 "KEYWORDS.txt"
      {"product", def_product},
      {""},
#line 63 "KEYWORDS.txt"
      {"fork", def_fork},
#line 22 "KEYWORDS.txt"
      {"clear", clear},
#line 21 "KEYWORDS.txt"
      {"branch", branch},
      {""}, {""},
#line 38 "KEYWORDS.txt"
      {"app1", def_app1},
#line 67 "KEYWORDS.txt"
      {"grabN", def_grabN},
      {""},
#line 94 "KEYWORDS.txt"
      {"reverse", def_reverse},
      {""},
#line 32 "KEYWORDS.txt"
      {"rest", rest},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 114 "KEYWORDS.txt"
      {"ternary", def_ternary},
#line 107 "KEYWORDS.txt"
      {"stuncons", def_stuncons},
      {""},
#line 69 "KEYWORDS.txt"
      {"hypot", def_hypot},
      {""},
#line 112 "KEYWORDS.txt"
      {"tailrec", def_tailrec},
      {""},
#line 74 "KEYWORDS.txt"
      {"make_generator", def_make_generator},
#line 99 "KEYWORDS.txt"
      {"shunt", def_shunt},
      {""},
#line 43 "KEYWORDS.txt"
      {"average", def_average},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 95 "KEYWORDS.txt"
      {"rrest", def_rrest},
      {""}, {""}, {""}, {""},
#line 33 "KEYWORDS.txt"
      {"stack", stack},
      {""}, {""}, {""}, {""}, {""},
#line 34 "KEYWORDS.txt"
      {"swaack", swaack},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 68 "KEYWORDS.txt"
      {"grba", def_grba}
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
