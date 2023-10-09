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
/* maximum key range = 323, duplicates = 0 */

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
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 110, 324, 324, 324, 105, 324, 324,
      324, 324, 100,  50, 324,  35, 324,  10,  35,  10,
       80,  35, 324, 324, 324, 324, 324, 324, 324, 324,
       95,  60,  15,  40, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324,  40, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324,  15, 324, 324, 100, 324,  20,  70,  15,
        5,  60,  55, 100,  55,   0, 324,  55,  35, 100,
        0,   0,   0, 110,  65,   0,  30,  15,  80, 160,
       55,  85,  15,   5, 324,   0, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324, 324, 324, 324, 324,
      324, 324, 324, 324, 324, 324
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
      TOTAL_KEYWORDS = 176,
      MIN_WORD_LENGTH = 1,
      MAX_WORD_LENGTH = 15,
      MIN_HASH_VALUE = 1,
      MAX_HASH_VALUE = 323
    };

  static const struct dict_entry wordlist[] =
    {
      {""},
#line 43 "KEYWORDS.txt"
      {"i", i_joyfunc},
#line 103 "KEYWORDS.txt"
      {"ii", def_ii},
#line 46 "KEYWORDS.txt"
      {"pop", pop},
      {""},
#line 123 "KEYWORDS.txt"
      {"popop", def_popop},
      {""},
#line 122 "KEYWORDS.txt"
      {"popopop", def_popopop},
#line 40 "KEYWORDS.txt"
      {"dip", dip},
#line 119 "KEYWORDS.txt"
      {"popd", def_popd},
#line 118 "KEYWORDS.txt"
      {"popdd", def_popdd},
#line 121 "KEYWORDS.txt"
      {"popopd", def_popopd},
#line 120 "KEYWORDS.txt"
      {"popopdd", def_popopdd},
      {""},
#line 82 "KEYWORDS.txt"
      {"dipd", def_dipd},
#line 83 "KEYWORDS.txt"
      {"dipdd", def_dipdd},
#line 84 "KEYWORDS.txt"
      {"dipddd", def_dipddd},
#line 85 "KEYWORDS.txt"
      {"disenstacken", def_disenstacken},
#line 187 "KEYWORDS.txt"
      {"zip", def_zip},
#line 38 "KEYWORDS.txt"
      {"cons", cons},
      {""},
#line 30 "KEYWORDS.txt"
      {"/", fdiv_q},
#line 164 "KEYWORDS.txt"
      {"unswons", def_unswons},
#line 41 "KEYWORDS.txt"
      {"dup", dup},
#line 79 "KEYWORDS.txt"
      {"codi", def_codi},
      {""},
#line 92 "KEYWORDS.txt"
      {"dupdip", def_dupdip},
      {""},
#line 80 "KEYWORDS.txt"
      {"codireco", def_codireco},
#line 90 "KEYWORDS.txt"
      {"dupd", def_dupd},
#line 89 "KEYWORDS.txt"
      {"dupdd", def_dupdd},
#line 16 "KEYWORDS.txt"
      {">", def_gt},
#line 91 "KEYWORDS.txt"
      {"dupdipd", def_dupdipd},
#line 66 "KEYWORDS.txt"
      {"and", def_and},
#line 67 "KEYWORDS.txt"
      {"app1", def_app1},
#line 76 "KEYWORDS.txt"
      {"ccons", def_ccons},
#line 160 "KEYWORDS.txt"
      {"uncons", def_uncons},
#line 185 "KEYWORDS.txt"
      {"\\/", def_REVERSE_SOLIDUS_SOLIDUS},
#line 27 "KEYWORDS.txt"
      {"add", add},
#line 45 "KEYWORDS.txt"
      {"loop", loop},
#line 189 "KEYWORDS.txt"
      {"uncons-two", def_uncons_HYPHEN_MINUS_two},
#line 143 "KEYWORDS.txt"
      {"spiral_next", def_spiral_next},
#line 184 "KEYWORDS.txt"
      {"/\\", def_SOLIDUS_REVERSE_SOLIDUS},
      {""},
#line 81 "KEYWORDS.txt"
      {"dinfrirst", def_dinfrirst},
      {""}, {""},
#line 136 "KEYWORDS.txt"
      {">>", def_GREATER_THAN_SIGN_GREATER_THAN_SIGN},
      {""},
#line 161 "KEYWORDS.txt"
      {"unit", def_unit},
      {""},
#line 39 "KEYWORDS.txt"
      {"concat", concat},
#line 75 "KEYWORDS.txt"
      {"ccccons", def_ccccons},
#line 149 "KEYWORDS.txt"
      {"stuncons", def_stuncons},
#line 78 "KEYWORDS.txt"
      {"clop", def_clop},
#line 110 "KEYWORDS.txt"
      {"nulco", def_nulco},
      {""},
#line 51 "KEYWORDS.txt"
      {"fn", fn},
      {""},
#line 69 "KEYWORDS.txt"
      {"app3", def_app3},
      {""},
#line 147 "KEYWORDS.txt"
      {"stackd", def_stackd},
      {""},
#line 109 "KEYWORDS.txt"
      {"not", def_not},
#line 70 "KEYWORDS.txt"
      {"appN", def_appN},
      {""}, {""}, {""},
#line 44 "KEYWORDS.txt"
      {"inscribe", inscribe},
#line 93 "KEYWORDS.txt"
      {"enstacken", def_enstacken},
      {""},
#line 28 "KEYWORDS.txt"
      {"-", sub},
#line 154 "KEYWORDS.txt"
      {"tailrec", def_tailrec},
#line 144 "KEYWORDS.txt"
      {"split_at", def_split_at},
#line 88 "KEYWORDS.txt"
      {"drop", def_drop},
#line 145 "KEYWORDS.txt"
      {"split_list", def_split_list},
      {""},
#line 163 "KEYWORDS.txt"
      {"unstack", def_unstack},
      {""},
#line 141 "KEYWORDS.txt"
      {"size", def_size},
#line 104 "KEYWORDS.txt"
      {"infra", def_infra},
#line 58 "KEYWORDS.txt"
      {"?", def_QUESTION_MARK},
#line 71 "KEYWORDS.txt"
      {"at", def_at},
#line 137 "KEYWORDS.txt"
      {"run", def_run},
      {""}, {""},
#line 138 "KEYWORDS.txt"
      {"second", def_second},
      {""}, {""},
#line 112 "KEYWORDS.txt"
      {"null", def_null},
#line 139 "KEYWORDS.txt"
      {"shift", def_shift},
#line 105 "KEYWORDS.txt"
      {"infrst", def_infrst},
      {""},
#line 64 "KEYWORDS.txt"
      {"abs", def_abs},
#line 167 "KEYWORDS.txt"
      {"step", def_step},
#line 157 "KEYWORDS.txt"
      {"third", def_third},
#line 86 "KEYWORDS.txt"
      {"divmod", def_divmod},
#line 54 "KEYWORDS.txt"
      {"lt", def_lt},
#line 128 "KEYWORDS.txt"
      {"range_to_zero", def_range_to_zero},
#line 148 "KEYWORDS.txt"
      {"step_zero", def_step_zero},
      {""},
#line 26 "KEYWORDS.txt"
      {"+", add},
#line 125 "KEYWORDS.txt"
      {"product", def_product},
#line 62 "KEYWORDS.txt"
      {"<{}", def_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET},
#line 68 "KEYWORDS.txt"
      {"app2", def_app2},
#line 140 "KEYWORDS.txt"
      {"shunt", def_shunt},
#line 134 "KEYWORDS.txt"
      {"rollup", def_rollup},
#line 61 "KEYWORDS.txt"
      {"--", def_HYPHEN_MINUS_HYPHEN_MINUS},
#line 131 "KEYWORDS.txt"
      {"rolldown", def_rolldown},
#line 34 "KEYWORDS.txt"
      {"bool", truthy},
#line 48 "KEYWORDS.txt"
      {"stack", stack},
#line 166 "KEYWORDS.txt"
      {"x", def_x},
#line 113 "KEYWORDS.txt"
      {"of", def_of},
#line 23 "KEYWORDS.txt"
      {"mod", fdiv_r},
      {""},
#line 182 "KEYWORDS.txt"
      {"_isnt_two_bools", def__isnt_two_bools},
      {""},
#line 95 "KEYWORDS.txt"
      {"flatten", def_flatten},
#line 37 "KEYWORDS.txt"
      {"cmp", cmp_joyfunc},
#line 158 "KEYWORDS.txt"
      {"tuck", def_tuck},
#line 133 "KEYWORDS.txt"
      {"roll>", def_roll_GREATER_THAN_SIGN},
#line 15 "KEYWORDS.txt"
      {"=", def_eq},
      {""},
#line 175 "KEYWORDS.txt"
      {"map", def_map},
      {""},
#line 159 "KEYWORDS.txt"
      {"unary", def_unary},
#line 32 "KEYWORDS.txt"
      {"lshift", lshift},
#line 19 "KEYWORDS.txt"
      {"<>", def_neq},
#line 98 "KEYWORDS.txt"
      {"gcd", def_gcd},
#line 94 "KEYWORDS.txt"
      {"first_two", def_first_two},
#line 188 "KEYWORDS.txt"
      {"shift-pair", def_shift_HYPHEN_MINUS_pair},
#line 97 "KEYWORDS.txt"
      {"fourth", def_fourth},
#line 114 "KEYWORDS.txt"
      {"or", def_or},
      {""},
#line 190 "KEYWORDS.txt"
      {"quote-two", def_quote_HYPHEN_MINUS_two},
#line 171 "KEYWORDS.txt"
      {"times", def_times},
#line 126 "KEYWORDS.txt"
      {"quoted", def_quoted},
#line 21 "KEYWORDS.txt"
      {">=", def_ge},
#line 162 "KEYWORDS.txt"
      {"unquoted", def_unquoted},
      {""},
#line 186 "KEYWORDS.txt"
      {"_isnt_list", def__isnt_list},
#line 73 "KEYWORDS.txt"
      {"b", def_b},
#line 111 "KEYWORDS.txt"
      {"nullary", def_nullary},
      {""},
#line 129 "KEYWORDS.txt"
      {"reco", def_reco},
#line 181 "KEYWORDS.txt"
      {"_isnt_bool", def__isnt_bool},
#line 169 "KEYWORDS.txt"
      {"_step1", def__step1},
#line 173 "KEYWORDS.txt"
      {"_times1", def__times1},
      {""},
#line 102 "KEYWORDS.txt"
      {"ifte", def_ifte},
#line 127 "KEYWORDS.txt"
      {"range", def_range},
#line 65 "KEYWORDS.txt"
      {"anamorphism", def_anamorphism},
#line 60 "KEYWORDS.txt"
      {"++", def_PLUS_SIGN_PLUS_SIGN},
      {""}, {""},
#line 42 "KEYWORDS.txt"
      {"first", first},
#line 33 "KEYWORDS.txt"
      {"rshift", rshift},
#line 56 "KEYWORDS.txt"
      {"le", def_le},
#line 29 "KEYWORDS.txt"
      {"sub", sub},
#line 47 "KEYWORDS.txt"
      {"rest", rest},
#line 142 "KEYWORDS.txt"
      {"small", def_small},
#line 74 "KEYWORDS.txt"
      {"binary", def_binary},
#line 53 "KEYWORDS.txt"
      {"gt", def_gt},
      {""}, {""},
#line 153 "KEYWORDS.txt"
      {"swons", def_swons},
#line 170 "KEYWORDS.txt"
      {"_stept", def__stept},
#line 174 "KEYWORDS.txt"
      {"_timest", def__timest},
#line 31 "KEYWORDS.txt"
      {"div", fdiv_q},
#line 155 "KEYWORDS.txt"
      {"take", def_take},
      {""},
#line 168 "KEYWORDS.txt"
      {"_step0", def__step0},
#line 172 "KEYWORDS.txt"
      {"_times0", def__times0},
      {""}, {""}, {""},
#line 77 "KEYWORDS.txt"
      {"cleave", def_cleave},
#line 87 "KEYWORDS.txt"
      {"down_to_zero", def_down_to_zero},
      {""},
#line 96 "KEYWORDS.txt"
      {"fork", def_fork},
#line 36 "KEYWORDS.txt"
      {"clear", clear},
#line 99 "KEYWORDS.txt"
      {"genrec", def_genrec},
#line 59 "KEYWORDS.txt"
      {"!-", def_EXCLAMATION_MARK_HYPHEN_MINUS},
      {""},
#line 50 "KEYWORDS.txt"
      {"swap", swap},
      {""}, {""}, {""},
#line 25 "KEYWORDS.txt"
      {"mul", mul},
      {""},
#line 151 "KEYWORDS.txt"
      {"swapd", def_swapd},
#line 17 "KEYWORDS.txt"
      {"<", def_lt},
      {""}, {""}, {""}, {""}, {""},
#line 152 "KEYWORDS.txt"
      {"swoncat", def_swoncat},
      {""},
#line 63 "KEYWORDS.txt"
      {"<<{}", def_LESS_THAN_SIGN_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET},
#line 132 "KEYWORDS.txt"
      {"roll<", def_roll_LESS_THAN_SIGN},
#line 24 "KEYWORDS.txt"
      {"*", mul},
#line 117 "KEYWORDS.txt"
      {"pm", def_pm},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 115 "KEYWORDS.txt"
      {"over", def_over},
      {""},
#line 22 "KEYWORDS.txt"
      {"%", fdiv_r},
      {""}, {""}, {""}, {""},
#line 35 "KEYWORDS.txt"
      {"branch", branch},
#line 20 "KEYWORDS.txt"
      {"<=", def_le},
#line 150 "KEYWORDS.txt"
      {"sum", def_sum},
      {""}, {""}, {""},
#line 57 "KEYWORDS.txt"
      {"ge", def_ge},
#line 116 "KEYWORDS.txt"
      {"pam", def_pam},
      {""},
#line 135 "KEYWORDS.txt"
      {"rrest", def_rrest},
      {""},
#line 72 "KEYWORDS.txt"
      {"average", def_average},
      {""},
#line 183 "KEYWORDS.txt"
      {"_\\/_", def___REVERSE_SOLIDUS_SOLIDUS__},
#line 100 "KEYWORDS.txt"
      {"grabN", def_grabN},
      {""},
#line 18 "KEYWORDS.txt"
      {"!=", def_neq},
      {""}, {""},
#line 179 "KEYWORDS.txt"
      {"_map1", def__map1},
      {""}, {""}, {""}, {""}, {""},
#line 49 "KEYWORDS.txt"
      {"swaack", swaack},
      {""},
#line 146 "KEYWORDS.txt"
      {"sqr", def_sqr},
      {""}, {""}, {""},
#line 156 "KEYWORDS.txt"
      {"ternary", def_ternary},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 107 "KEYWORDS.txt"
      {"make_generator", def_make_generator},
      {""}, {""}, {""}, {""},
#line 101 "KEYWORDS.txt"
      {"grba", def_grba},
#line 178 "KEYWORDS.txt"
      {"_map0", def__map0},
      {""}, {""},
#line 108 "KEYWORDS.txt"
      {"neg", def_neg},
      {""},
#line 176 "KEYWORDS.txt"
      {"_map?", def__map_QUESTION_MARK},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 130 "KEYWORDS.txt"
      {"reverse", def_reverse},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 165 "KEYWORDS.txt"
      {"while", def_while},
      {""},
#line 52 "KEYWORDS.txt"
      {"eq", def_eq},
#line 55 "KEYWORDS.txt"
      {"neq", def_neq},
      {""},
#line 177 "KEYWORDS.txt"
      {"_mape", def__mape},
      {""},
#line 106 "KEYWORDS.txt"
      {"<<", def_LESS_THAN_SIGN_LESS_THAN_SIGN},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 180 "KEYWORDS.txt"
      {"_map2", def__map2},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 124 "KEYWORDS.txt"
      {"pow", def_pow}
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
