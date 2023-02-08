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
/* maximum key range = 294, duplicates = 0 */

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
  static const unsigned short asso_values[] =
    {
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295,  40, 295, 295, 295,  95,  60, 295,
      295, 295,  80,  10, 295,   0, 295,  60, 110,  35,
       20,  60, 295, 295, 295, 295, 295, 295, 295, 295,
       90,  20,  30,  85, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295,   0, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295,   5, 295,  60,  55,   0,
        5,  30,  40, 105,   5,  15, 295,  50,  40,  25,
        0,  15,   0, 110,  70,  30,  25,   0,  65,  95,
       25,  75,  25,  45,  45,   0, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295, 295, 295, 295, 295,
      295, 295, 295, 295, 295, 295
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
      TOTAL_KEYWORDS = 152,
      MIN_WORD_LENGTH = 1,
      MAX_WORD_LENGTH = 14,
      MIN_HASH_VALUE = 1,
      MAX_HASH_VALUE = 294
    };

  static const struct dict_entry wordlist[] =
    {
      {""},
#line 28 "KEYWORDS.txt"
      {"-", sub},
#line 54 "KEYWORDS.txt"
      {"--", def_HYPHEN_MINUS_HYPHEN_MINUS},
      {""}, {""}, {""}, {""}, {""},
#line 39 "KEYWORDS.txt"
      {"dup", dup},
      {""}, {""},
#line 85 "KEYWORDS.txt"
      {"dupdip", def_dupdip},
      {""}, {""},
#line 83 "KEYWORDS.txt"
      {"dupd", def_dupd},
#line 84 "KEYWORDS.txt"
      {"dupdd", def_dupdd},
      {""},
#line 86 "KEYWORDS.txt"
      {"dupdipd", def_dupdipd},
#line 43 "KEYWORDS.txt"
      {"pop", pop},
      {""},
#line 110 "KEYWORDS.txt"
      {"popop", def_popop},
#line 26 "KEYWORDS.txt"
      {"+", add},
#line 111 "KEYWORDS.txt"
      {"popopop", def_popopop},
#line 38 "KEYWORDS.txt"
      {"dip", dip},
#line 108 "KEYWORDS.txt"
      {"popd", def_popd},
#line 109 "KEYWORDS.txt"
      {"popdd", def_popdd},
#line 112 "KEYWORDS.txt"
      {"popopd", def_popopd},
#line 113 "KEYWORDS.txt"
      {"popopdd", def_popopdd},
#line 35 "KEYWORDS.txt"
      {"cmp", cmp_joyfunc},
#line 79 "KEYWORDS.txt"
      {"dipd", def_dipd},
      {""},
#line 41 "KEYWORDS.txt"
      {"i", i_joyfunc},
#line 57 "KEYWORDS.txt"
      {"++", def_PLUS_SIGN_PLUS_SIGN},
      {""}, {""}, {""},
#line 147 "KEYWORDS.txt"
      {"uncons", def_uncons},
#line 72 "KEYWORDS.txt"
      {"ccccons", def_ccccons},
      {""},
#line 76 "KEYWORDS.txt"
      {"codi", def_codi},
      {""},
#line 15 "KEYWORDS.txt"
      {"=", def_eq},
#line 59 "KEYWORDS.txt"
      {"!-", def_EXCLAMATION_MARK_HYPHEN_MINUS},
#line 77 "KEYWORDS.txt"
      {"codireco", def_codireco},
#line 148 "KEYWORDS.txt"
      {"unit", def_unit},
      {""},
#line 37 "KEYWORDS.txt"
      {"concat", concat},
#line 97 "KEYWORDS.txt"
      {"ii", def_ii},
      {""},
#line 36 "KEYWORDS.txt"
      {"cons", cons},
#line 73 "KEYWORDS.txt"
      {"ccons", def_ccons},
#line 152 "KEYWORDS.txt"
      {"x", def_x},
#line 107 "KEYWORDS.txt"
      {"pm", def_pm},
#line 23 "KEYWORDS.txt"
      {"mod", tdiv_r},
#line 78 "KEYWORDS.txt"
      {"dinfrirst", def_dinfrirst},
#line 144 "KEYWORDS.txt"
      {"third", def_third},
      {""}, {""}, {""},
#line 75 "KEYWORDS.txt"
      {"clop", def_clop},
#line 103 "KEYWORDS.txt"
      {"nulco", def_nulco},
#line 16 "KEYWORDS.txt"
      {">", def_gt},
#line 80 "KEYWORDS.txt"
      {"disenstacken", def_disenstacken},
      {""},
#line 67 "KEYWORDS.txt"
      {"appN", def_appN},
#line 128 "KEYWORDS.txt"
      {"shunt", def_shunt},
#line 90 "KEYWORDS.txt"
      {"fourth", def_fourth},
#line 150 "KEYWORDS.txt"
      {"unswons", def_unswons},
#line 102 "KEYWORDS.txt"
      {"not", def_not},
#line 87 "KEYWORDS.txt"
      {"enstacken", def_enstacken},
      {""},
#line 126 "KEYWORDS.txt"
      {"second", def_second},
#line 21 "KEYWORDS.txt"
      {">=", def_ge},
#line 125 "KEYWORDS.txt"
      {"run", def_run},
#line 42 "KEYWORDS.txt"
      {"loop", loop},
      {""}, {""},
#line 160 "KEYWORDS.txt"
      {"_timest", def__timest},
#line 27 "KEYWORDS.txt"
      {"add", add},
#line 145 "KEYWORDS.txt"
      {"tuck", def_tuck},
#line 127 "KEYWORDS.txt"
      {"shift", def_shift},
#line 130 "KEYWORDS.txt"
      {"spiral_next", def_spiral_next},
#line 18 "KEYWORDS.txt"
      {"!=", def_neq},
#line 137 "KEYWORDS.txt"
      {"sum", def_sum},
#line 65 "KEYWORDS.txt"
      {"app2", def_app2},
      {""},
#line 99 "KEYWORDS.txt"
      {"infrst", def_infrst},
#line 159 "KEYWORDS.txt"
      {"_times1", def__times1},
#line 161 "KEYWORDS.txt"
      {"map", def_map},
#line 153 "KEYWORDS.txt"
      {"step", def_step},
      {""},
#line 156 "KEYWORDS.txt"
      {"_stept", def__stept},
#line 50 "KEYWORDS.txt"
      {"lt", def_lt},
#line 136 "KEYWORDS.txt"
      {"stuncons", def_stuncons},
#line 82 "KEYWORDS.txt"
      {"drop", def_drop},
      {""}, {""},
#line 105 "KEYWORDS.txt"
      {"of", def_of},
      {""},
#line 64 "KEYWORDS.txt"
      {"app1", def_app1},
#line 157 "KEYWORDS.txt"
      {"times", def_times},
#line 155 "KEYWORDS.txt"
      {"_step1", def__step1},
#line 52 "KEYWORDS.txt"
      {"le", def_le},
#line 131 "KEYWORDS.txt"
      {"split_at", def_split_at},
#line 129 "KEYWORDS.txt"
      {"size", def_size},
#line 132 "KEYWORDS.txt"
      {"split_list", def_split_list},
#line 74 "KEYWORDS.txt"
      {"cleave", def_cleave},
#line 141 "KEYWORDS.txt"
      {"tailrec", def_tailrec},
#line 25 "KEYWORDS.txt"
      {"mul", mul},
#line 135 "KEYWORDS.txt"
      {"step_zero", def_step_zero},
#line 95 "KEYWORDS.txt"
      {"hypot", def_hypot},
#line 70 "KEYWORDS.txt"
      {"b", def_b},
#line 68 "KEYWORDS.txt"
      {"at", def_at},
#line 106 "KEYWORDS.txt"
      {"pam", def_pam},
#line 96 "KEYWORDS.txt"
      {"ifte", def_ifte},
#line 166 "KEYWORDS.txt"
      {"_map2", def__map2},
      {""},
#line 114 "KEYWORDS.txt"
      {"product", def_product},
#line 91 "KEYWORDS.txt"
      {"gcd", def_gcd},
#line 118 "KEYWORDS.txt"
      {"reco", def_reco},
#line 98 "KEYWORDS.txt"
      {"infra", def_infra},
#line 30 "KEYWORDS.txt"
      {"/", tdiv_q},
#line 104 "KEYWORDS.txt"
      {"nullary", def_nullary},
#line 149 "KEYWORDS.txt"
      {"unquoted", def_unquoted},
#line 66 "KEYWORDS.txt"
      {"app3", def_app3},
#line 163 "KEYWORDS.txt"
      {"_mape", def__mape},
#line 134 "KEYWORDS.txt"
      {"stackd", def_stackd},
      {""}, {""},
#line 32 "KEYWORDS.txt"
      {"bool", truthy},
#line 165 "KEYWORDS.txt"
      {"_map1", def__map1},
#line 122 "KEYWORDS.txt"
      {"rollup", def_rollup},
#line 20 "KEYWORDS.txt"
      {"<=", def_le},
#line 123 "KEYWORDS.txt"
      {"rolldown", def_rolldown},
      {""}, {""},
#line 115 "KEYWORDS.txt"
      {"quoted", def_quoted},
#line 58 "KEYWORDS.txt"
      {"||", def_VERTICAL_LINE_VERTICAL_LINE},
#line 60 "KEYWORDS.txt"
      {"<{}", def_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET},
      {""},
#line 146 "KEYWORDS.txt"
      {"unary", def_unary},
#line 92 "KEYWORDS.txt"
      {"genrec", def_genrec},
#line 81 "KEYWORDS.txt"
      {"down_to_zero", def_down_to_zero},
#line 29 "KEYWORDS.txt"
      {"sub", sub},
      {""},
#line 34 "KEYWORDS.txt"
      {"clear", clear},
      {""},
#line 88 "KEYWORDS.txt"
      {"flatten", def_flatten},
      {""}, {""},
#line 151 "KEYWORDS.txt"
      {"while", def_while},
#line 71 "KEYWORDS.txt"
      {"binary", def_binary},
#line 19 "KEYWORDS.txt"
      {"<>", def_neq},
#line 31 "KEYWORDS.txt"
      {"div", tdiv_q},
      {""},
#line 40 "KEYWORDS.txt"
      {"first", first},
#line 63 "KEYWORDS.txt"
      {"anamorphism", def_anamorphism},
#line 49 "KEYWORDS.txt"
      {"gt", def_gt},
#line 117 "KEYWORDS.txt"
      {"range_to_zero", def_range_to_zero},
#line 44 "KEYWORDS.txt"
      {"rest", rest},
#line 120 "KEYWORDS.txt"
      {"roll>", def_roll_GREATER_THAN_SIGN},
#line 24 "KEYWORDS.txt"
      {"*", mul},
#line 158 "KEYWORDS.txt"
      {"_times0", def__times0},
      {""}, {""},
#line 116 "KEYWORDS.txt"
      {"range", def_range},
      {""},
#line 53 "KEYWORDS.txt"
      {"ge", def_ge},
      {""},
#line 142 "KEYWORDS.txt"
      {"take", def_take},
#line 45 "KEYWORDS.txt"
      {"stack", stack},
#line 55 "KEYWORDS.txt"
      {"?", def_QUESTION_MARK},
#line 140 "KEYWORDS.txt"
      {"swoncat", def_swoncat},
      {""}, {""},
#line 139 "KEYWORDS.txt"
      {"swons", def_swons},
#line 154 "KEYWORDS.txt"
      {"_step0", def__step0},
      {""},
#line 62 "KEYWORDS.txt"
      {"abs", def_abs},
#line 89 "KEYWORDS.txt"
      {"fork", def_fork},
#line 162 "KEYWORDS.txt"
      {"_map?", def__map_QUESTION_MARK},
#line 17 "KEYWORDS.txt"
      {"<", def_lt},
#line 56 "KEYWORDS.txt"
      {"&&", def_AMPERSAND_AMPERSAND},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 47 "KEYWORDS.txt"
      {"swap", swap},
      {""},
#line 22 "KEYWORDS.txt"
      {"%", tdiv_r},
#line 69 "KEYWORDS.txt"
      {"average", def_average},
      {""}, {""},
#line 138 "KEYWORDS.txt"
      {"swapd", def_swapd},
#line 33 "KEYWORDS.txt"
      {"branch", branch},
      {""}, {""}, {""},
#line 124 "KEYWORDS.txt"
      {"rrest", def_rrest},
      {""},
#line 119 "KEYWORDS.txt"
      {"reverse", def_reverse},
      {""}, {""},
#line 164 "KEYWORDS.txt"
      {"_map0", def__map0},
      {""},
#line 143 "KEYWORDS.txt"
      {"ternary", def_ternary},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 100 "KEYWORDS.txt"
      {"make_generator", def_make_generator},
#line 121 "KEYWORDS.txt"
      {"roll<", def_roll_LESS_THAN_SIGN},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 61 "KEYWORDS.txt"
      {"<<{}", def_LESS_THAN_SIGN_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 93 "KEYWORDS.txt"
      {"grabN", def_grabN},
#line 46 "KEYWORDS.txt"
      {"swaack", swaack},
      {""},
#line 101 "KEYWORDS.txt"
      {"neg", def_neg},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 48 "KEYWORDS.txt"
      {"eq", def_eq},
#line 51 "KEYWORDS.txt"
      {"neq", def_neq},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 133 "KEYWORDS.txt"
      {"sqr", def_sqr},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 94 "KEYWORDS.txt"
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
