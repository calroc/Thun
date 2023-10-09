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
/* maximum key range = 365, duplicates = 0 */

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
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366,  50, 366, 366, 366,  95, 366, 366,
      366, 366,  50,  45, 366,  85, 366,  20,  95,  85,
       35,  50, 366, 366, 366, 366, 366, 366, 366, 366,
       75, 115,  15,  55, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366,  15, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366,  80, 366, 366,  75, 366, 105,  70,  15,
        5,  30,  35, 115,  20,   0, 366,  95,  65,  85,
        0,   0,   0, 110,  55,   0,  40,  15,  30,  45,
       10, 105,  10,  30, 366,   0, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366, 366, 366, 366, 366,
      366, 366, 366, 366, 366, 366
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
      TOTAL_KEYWORDS = 188,
      MIN_WORD_LENGTH = 1,
      MAX_WORD_LENGTH = 17,
      MIN_HASH_VALUE = 1,
      MAX_HASH_VALUE = 365
    };

  static const struct dict_entry wordlist[] =
    {
      {""},
#line 43 "KEYWORDS.txt"
      {"i", i_joyfunc},
#line 105 "KEYWORDS.txt"
      {"ii", def_ii},
#line 46 "KEYWORDS.txt"
      {"pop", pop},
      {""},
#line 127 "KEYWORDS.txt"
      {"popop", def_popop},
      {""},
#line 126 "KEYWORDS.txt"
      {"popopop", def_popopop},
#line 40 "KEYWORDS.txt"
      {"dip", dip},
#line 123 "KEYWORDS.txt"
      {"popd", def_popd},
#line 122 "KEYWORDS.txt"
      {"popdd", def_popdd},
#line 125 "KEYWORDS.txt"
      {"popopd", def_popopd},
#line 124 "KEYWORDS.txt"
      {"popopdd", def_popopdd},
#line 193 "KEYWORDS.txt"
      {"zip", def_zip},
#line 83 "KEYWORDS.txt"
      {"dipd", def_dipd},
#line 84 "KEYWORDS.txt"
      {"dipdd", def_dipdd},
#line 85 "KEYWORDS.txt"
      {"dipddd", def_dipddd},
#line 86 "KEYWORDS.txt"
      {"disenstacken", def_disenstacken},
      {""},
#line 38 "KEYWORDS.txt"
      {"cons", cons},
      {""},
#line 172 "KEYWORDS.txt"
      {"x", def_x},
#line 170 "KEYWORDS.txt"
      {"unswons", def_unswons},
#line 41 "KEYWORDS.txt"
      {"dup", dup},
#line 80 "KEYWORDS.txt"
      {"codi", def_codi},
      {""},
#line 93 "KEYWORDS.txt"
      {"dupdip", def_dupdip},
      {""},
#line 81 "KEYWORDS.txt"
      {"codireco", def_codireco},
#line 91 "KEYWORDS.txt"
      {"dupd", def_dupd},
#line 90 "KEYWORDS.txt"
      {"dupdd", def_dupdd},
#line 16 "KEYWORDS.txt"
      {">", def_gt},
#line 92 "KEYWORDS.txt"
      {"dupdipd", def_dupdipd},
      {""}, {""},
#line 76 "KEYWORDS.txt"
      {"ccons", def_ccons},
#line 166 "KEYWORDS.txt"
      {"uncons", def_uncons},
#line 51 "KEYWORDS.txt"
      {"fn", fn},
#line 44 "KEYWORDS.txt"
      {"inscribe", inscribe},
#line 94 "KEYWORDS.txt"
      {"enstacken", def_enstacken},
#line 195 "KEYWORDS.txt"
      {"uncons-two", def_uncons_HYPHEN_MINUS_two},
#line 30 "KEYWORDS.txt"
      {"/", fdiv_q},
      {""}, {""},
#line 146 "KEYWORDS.txt"
      {"size", def_size},
      {""},
#line 87 "KEYWORDS.txt"
      {"divmod", def_divmod},
#line 141 "KEYWORDS.txt"
      {">>", def_GREATER_THAN_SIGN_GREATER_THAN_SIGN},
      {""},
#line 155 "KEYWORDS.txt"
      {"succ", def_succ},
#line 159 "KEYWORDS.txt"
      {"swons", def_swons},
#line 148 "KEYWORDS.txt"
      {"spiral_next", def_spiral_next},
#line 75 "KEYWORDS.txt"
      {"ccccons", def_ccccons},
      {""},
#line 82 "KEYWORDS.txt"
      {"dinfrirst", def_dinfrirst},
      {""},
#line 143 "KEYWORDS.txt"
      {"second", def_second},
      {""}, {""},
#line 167 "KEYWORDS.txt"
      {"unit", def_unit},
      {""},
#line 39 "KEYWORDS.txt"
      {"concat", concat},
#line 88 "KEYWORDS.txt"
      {"down_to_zero", def_down_to_zero},
#line 154 "KEYWORDS.txt"
      {"stuncons", def_stuncons},
#line 89 "KEYWORDS.txt"
      {"drop", def_drop},
#line 144 "KEYWORDS.txt"
      {"shift", def_shift},
      {""}, {""},
#line 31 "KEYWORDS.txt"
      {"div", fdiv_q},
#line 45 "KEYWORDS.txt"
      {"loop", loop},
#line 163 "KEYWORDS.txt"
      {"third", def_third},
#line 77 "KEYWORDS.txt"
      {"choice", def_choice},
#line 116 "KEYWORDS.txt"
      {"of", def_of},
#line 142 "KEYWORDS.txt"
      {"run", def_run},
#line 173 "KEYWORDS.txt"
      {"step", def_step},
      {""},
#line 98 "KEYWORDS.txt"
      {"fourth", def_fourth},
      {""}, {""},
#line 153 "KEYWORDS.txt"
      {"step_zero", def_step_zero},
#line 145 "KEYWORDS.txt"
      {"shunt", def_shunt},
#line 107 "KEYWORDS.txt"
      {"infrst", def_infrst},
      {""},
#line 112 "KEYWORDS.txt"
      {"not", def_not},
#line 79 "KEYWORDS.txt"
      {"clop", def_clop},
#line 113 "KEYWORDS.txt"
      {"nulco", def_nulco},
      {""}, {""},
#line 201 "KEYWORDS.txt"
      {"min", def_min},
      {""},
#line 188 "KEYWORDS.txt"
      {"_isnt_two_bools", def__isnt_two_bools},
#line 26 "KEYWORDS.txt"
      {"+", add},
#line 158 "KEYWORDS.txt"
      {"swoncat", def_swoncat},
#line 128 "KEYWORDS.txt"
      {"pow", def_pow},
#line 129 "KEYWORDS.txt"
      {"pred", def_pred},
#line 200 "KEYWORDS.txt"
      {"min-of-two", def_min_HYPHEN_MINUS_of_HYPHEN_MINUS_two},
#line 194 "KEYWORDS.txt"
      {"uncons-pair", def_uncons_HYPHEN_MINUS_pair},
#line 110 "KEYWORDS.txt"
      {"modulus", def_modulus},
#line 23 "KEYWORDS.txt"
      {"mod", fdiv_r},
#line 95 "KEYWORDS.txt"
      {"first_two", def_first_two},
#line 171 "KEYWORDS.txt"
      {"while", def_while},
#line 24 "KEYWORDS.txt"
      {"*", mul},
#line 130 "KEYWORDS.txt"
      {"product", def_product},
#line 37 "KEYWORDS.txt"
      {"cmp", cmp_joyfunc},
#line 134 "KEYWORDS.txt"
      {"reco", def_reco},
      {""}, {""},
#line 19 "KEYWORDS.txt"
      {"<>", def_neq},
#line 62 "KEYWORDS.txt"
      {"<{}", def_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET},
#line 104 "KEYWORDS.txt"
      {"ifte", def_ifte},
      {""},
#line 58 "KEYWORDS.txt"
      {"?", def_QUESTION_MARK},
#line 117 "KEYWORDS.txt"
      {"or", def_or},
#line 149 "KEYWORDS.txt"
      {"split_at", def_split_at},
#line 120 "KEYWORDS.txt"
      {"pick", def_pick},
#line 150 "KEYWORDS.txt"
      {"split_list", def_split_list},
      {""},
#line 169 "KEYWORDS.txt"
      {"unstack", def_unstack},
#line 66 "KEYWORDS.txt"
      {"and", def_and},
#line 118 "KEYWORDS.txt"
      {"over", def_over},
      {""},
#line 33 "KEYWORDS.txt"
      {"rshift", rshift},
#line 191 "KEYWORDS.txt"
      {"\\/", def_REVERSE_SOLIDUS_SOLIDUS},
#line 27 "KEYWORDS.txt"
      {"add", add},
#line 70 "KEYWORDS.txt"
      {"appN", def_appN},
#line 192 "KEYWORDS.txt"
      {"_isnt_list", def__isnt_list},
#line 139 "KEYWORDS.txt"
      {"rollup", def_rollup},
#line 56 "KEYWORDS.txt"
      {"le", def_le},
#line 136 "KEYWORDS.txt"
      {"rolldown", def_rolldown},
#line 47 "KEYWORDS.txt"
      {"rest", rest},
#line 177 "KEYWORDS.txt"
      {"times", def_times},
#line 32 "KEYWORDS.txt"
      {"lshift", lshift},
      {""}, {""},
#line 196 "KEYWORDS.txt"
      {"quote-two", def_quote_HYPHEN_MINUS_two},
#line 42 "KEYWORDS.txt"
      {"first", first},
#line 131 "KEYWORDS.txt"
      {"quoted", def_quoted},
#line 60 "KEYWORDS.txt"
      {"++", def_PLUS_SIGN_PLUS_SIGN},
#line 168 "KEYWORDS.txt"
      {"unquoted", def_unquoted},
#line 34 "KEYWORDS.txt"
      {"bool", truthy},
#line 138 "KEYWORDS.txt"
      {"roll>", def_roll_GREATER_THAN_SIGN},
#line 73 "KEYWORDS.txt"
      {"b", def_b},
#line 202 "KEYWORDS.txt"
      {"on-non-empty-list", def_on_HYPHEN_MINUS_non_HYPHEN_MINUS_empty_HYPHEN_MINUS_list},
#line 99 "KEYWORDS.txt"
      {"gcd", def_gcd},
#line 68 "KEYWORDS.txt"
      {"app2", def_app2},
#line 106 "KEYWORDS.txt"
      {"infra", def_infra},
#line 78 "KEYWORDS.txt"
      {"cleave", def_cleave},
#line 54 "KEYWORDS.txt"
      {"lt", def_lt},
      {""},
#line 115 "KEYWORDS.txt"
      {"null", def_null},
#line 187 "KEYWORDS.txt"
      {"_isnt_bool", def__isnt_bool},
#line 17 "KEYWORDS.txt"
      {"<", def_lt},
#line 135 "KEYWORDS.txt"
      {"reverse", def_reverse},
      {""},
#line 50 "KEYWORDS.txt"
      {"swap", swap},
      {""},
#line 152 "KEYWORDS.txt"
      {"stackd", def_stackd},
      {""},
#line 29 "KEYWORDS.txt"
      {"sub", sub},
#line 69 "KEYWORDS.txt"
      {"app3", def_app3},
#line 157 "KEYWORDS.txt"
      {"swapd", def_swapd},
#line 176 "KEYWORDS.txt"
      {"_stept", def__stept},
#line 180 "KEYWORDS.txt"
      {"_timest", def__timest},
      {""}, {""}, {""},
#line 100 "KEYWORDS.txt"
      {"genrec", def_genrec},
#line 160 "KEYWORDS.txt"
      {"tailrec", def_tailrec},
      {""},
#line 164 "KEYWORDS.txt"
      {"tuck", def_tuck},
#line 36 "KEYWORDS.txt"
      {"clear", clear},
#line 28 "KEYWORDS.txt"
      {"-", sub},
#line 121 "KEYWORDS.txt"
      {"pm", def_pm},
#line 133 "KEYWORDS.txt"
      {"range_to_zero", def_range_to_zero},
      {""}, {""},
#line 197 "KEYWORDS.txt"
      {"empty?", def_empty_QUESTION_MARK},
#line 57 "KEYWORDS.txt"
      {"ge", def_ge},
#line 64 "KEYWORDS.txt"
      {"abs", def_abs},
      {""}, {""},
#line 74 "KEYWORDS.txt"
      {"binary", def_binary},
#line 190 "KEYWORDS.txt"
      {"/\\", def_SOLIDUS_REVERSE_SOLIDUS},
      {""},
#line 63 "KEYWORDS.txt"
      {"<<{}", def_LESS_THAN_SIGN_LESS_THAN_SIGN_LEFT_CURLY_BRACKET_RIGHT_CURLY_BRACKET},
#line 140 "KEYWORDS.txt"
      {"rrest", def_rrest},
      {""},
#line 71 "KEYWORDS.txt"
      {"at", def_at},
#line 156 "KEYWORDS.txt"
      {"sum", def_sum},
#line 97 "KEYWORDS.txt"
      {"fork", def_fork},
      {""},
#line 22 "KEYWORDS.txt"
      {"%", fdiv_r},
#line 114 "KEYWORDS.txt"
      {"nullary", def_nullary},
#line 181 "KEYWORDS.txt"
      {"map", def_map},
#line 67 "KEYWORDS.txt"
      {"app1", def_app1},
#line 132 "KEYWORDS.txt"
      {"range", def_range},
      {""},
#line 53 "KEYWORDS.txt"
      {"gt", def_gt},
      {""}, {""},
#line 137 "KEYWORDS.txt"
      {"roll<", def_roll_LESS_THAN_SIGN},
      {""},
#line 72 "KEYWORDS.txt"
      {"average", def_average},
      {""}, {""}, {""},
#line 175 "KEYWORDS.txt"
      {"_step1", def__step1},
#line 179 "KEYWORDS.txt"
      {"_times1", def__times1},
      {""}, {""},
#line 198 "KEYWORDS.txt"
      {"max-of-two", def_max_HYPHEN_MINUS_of_HYPHEN_MINUS_two},
      {""},
#line 96 "KEYWORDS.txt"
      {"flatten", def_flatten},
#line 199 "KEYWORDS.txt"
      {"max", def_max},
      {""}, {""},
#line 174 "KEYWORDS.txt"
      {"_step0", def__step0},
#line 178 "KEYWORDS.txt"
      {"_times0", def__times0},
      {""}, {""}, {""}, {""},
#line 59 "KEYWORDS.txt"
      {"!-", def_EXCLAMATION_MARK_HYPHEN_MINUS},
#line 151 "KEYWORDS.txt"
      {"sqr", def_sqr},
      {""}, {""}, {""},
#line 108 "KEYWORDS.txt"
      {"<<", def_LESS_THAN_SIGN_LESS_THAN_SIGN},
      {""}, {""},
#line 165 "KEYWORDS.txt"
      {"unary", def_unary},
#line 15 "KEYWORDS.txt"
      {"=", def_eq},
      {""},
#line 25 "KEYWORDS.txt"
      {"mul", mul},
      {""}, {""}, {""},
#line 162 "KEYWORDS.txt"
      {"ternary", def_ternary},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 48 "KEYWORDS.txt"
      {"stack", stack},
      {""},
#line 21 "KEYWORDS.txt"
      {">=", def_ge},
      {""}, {""}, {""},
#line 49 "KEYWORDS.txt"
      {"swaack", swaack},
#line 52 "KEYWORDS.txt"
      {"eq", def_eq},
#line 55 "KEYWORDS.txt"
      {"neq", def_neq},
#line 189 "KEYWORDS.txt"
      {"_\\/_", def___REVERSE_SOLIDUS_SOLIDUS__},
      {""},
#line 35 "KEYWORDS.txt"
      {"branch", branch},
#line 61 "KEYWORDS.txt"
      {"--", def_HYPHEN_MINUS_HYPHEN_MINUS},
      {""}, {""},
#line 147 "KEYWORDS.txt"
      {"small", def_small},
      {""}, {""},
#line 111 "KEYWORDS.txt"
      {"neg", def_neg},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 161 "KEYWORDS.txt"
      {"take", def_take},
      {""}, {""},
#line 101 "KEYWORDS.txt"
      {"getitem", def_getitem},
#line 119 "KEYWORDS.txt"
      {"pam", def_pam},
      {""}, {""}, {""},
#line 18 "KEYWORDS.txt"
      {"!=", def_neq},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 102 "KEYWORDS.txt"
      {"grabN", def_grabN},
      {""}, {""}, {""}, {""},
#line 183 "KEYWORDS.txt"
      {"_mape", def__mape},
      {""}, {""}, {""}, {""},
#line 186 "KEYWORDS.txt"
      {"_map2", def__map2},
#line 65 "KEYWORDS.txt"
      {"anamorphism", def_anamorphism},
#line 20 "KEYWORDS.txt"
      {"<=", def_le},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 182 "KEYWORDS.txt"
      {"_map?", def__map_QUESTION_MARK},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""},
#line 103 "KEYWORDS.txt"
      {"grba", def_grba},
      {""}, {""}, {""}, {""},
#line 109 "KEYWORDS.txt"
      {"make_generator", def_make_generator},
#line 185 "KEYWORDS.txt"
      {"_map1", def__map1},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 184 "KEYWORDS.txt"
      {"_map0", def__map0}
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
