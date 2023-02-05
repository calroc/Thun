/* ANSI-C code produced by gperf version 3.1 */
/* Command-line: gperf --output-file=keywords.c --readonly-tables --enum --includes --hash-function-name=keyword_hash KEYWORDS.txt  */
/* Computed positions: -k'2,$' */

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

#line 4 "KEYWORDS.txt"

#include "joy.h"
#line 7 "KEYWORDS.txt"
struct dict_entry;
#include <string.h>
/* maximum key range = 56, duplicates = 0 */

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
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 55, 57, 57,
      57, 57, 50, 45, 57, 40, 57, 35, 57, 35,
      30, 25, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 20, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57,  0,  0,
      15,  5, 57, 57, 25,  5, 57,  5, 15, 10,
       0,  0, 10, 57,  0,  0,  0, 15,  0,  5,
      57,  0, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
      57, 57, 57, 57, 57, 57
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
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
      TOTAL_KEYWORDS = 34,
      MIN_WORD_LENGTH = 1,
      MAX_WORD_LENGTH = 12,
      MIN_HASH_VALUE = 1,
      MAX_HASH_VALUE = 56
    };

  static const struct dict_entry wordlist[] =
    {
      {""},
#line 32 "KEYWORDS.txt"
      {"b", def_b},
#line 30 "KEYWORDS.txt"
      {"at", def_at},
#line 24 "KEYWORDS.txt"
      {"abs", def_abs},
#line 18 "KEYWORDS.txt"
      {"cons", cons},
#line 35 "KEYWORDS.txt"
      {"ccons", def_ccons},
#line 21 "KEYWORDS.txt"
      {"i", i_joyfunc},
#line 34 "KEYWORDS.txt"
      {"ccccons", def_ccccons},
#line 39 "KEYWORDS.txt"
      {"codireco", def_codireco},
#line 38 "KEYWORDS.txt"
      {"codi", def_codi},
      {""},
#line 33 "KEYWORDS.txt"
      {"binary", def_binary},
#line 31 "KEYWORDS.txt"
      {"average", def_average},
#line 22 "KEYWORDS.txt"
      {"pop", pop},
#line 40 "KEYWORDS.txt"
      {"dinfrirst", def_dinfrirst},
      {""},
#line 23 "KEYWORDS.txt"
      {"swaack", swaack},
#line 42 "KEYWORDS.txt"
      {"disenstacken", def_disenstacken},
#line 19 "KEYWORDS.txt"
      {"dip", dip},
#line 14 "KEYWORDS.txt"
      {"bool", truthy},
#line 16 "KEYWORDS.txt"
      {"clear", clear},
#line 25 "KEYWORDS.txt"
      {"anamorphism", def_anamorphism},
      {""},
#line 17 "KEYWORDS.txt"
      {"cmp", cmp_joyfunc},
#line 41 "KEYWORDS.txt"
      {"dipd", def_dipd},
      {""},
#line 36 "KEYWORDS.txt"
      {"cleave", def_cleave},
      {""},
#line 20 "KEYWORDS.txt"
      {"dup", dup},
#line 37 "KEYWORDS.txt"
      {"clop", def_clop},
      {""},
#line 15 "KEYWORDS.txt"
      {"branch", branch},
      {""}, {""},
#line 29 "KEYWORDS.txt"
      {"appN", def_appN},
      {""},
#line 13 "KEYWORDS.txt"
      {"/", tdiv_q},
      {""}, {""},
#line 28 "KEYWORDS.txt"
      {"app3", def_app3},
      {""},
#line 12 "KEYWORDS.txt"
      {"-", sub},
      {""}, {""},
#line 27 "KEYWORDS.txt"
      {"app2", def_app2},
      {""},
#line 11 "KEYWORDS.txt"
      {"+", add},
      {""}, {""},
#line 26 "KEYWORDS.txt"
      {"app1", def_app1},
      {""},
#line 10 "KEYWORDS.txt"
      {"*", mul},
      {""}, {""}, {""}, {""},
#line 9 "KEYWORDS.txt"
      {"%", tdiv_r}
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
