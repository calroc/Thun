#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <gc.h>


const char *BLANKS = " \t";
const char *TEXT = " 23 [dup   *] i hi there  fr  [[]  ie]nd]  []    23      ";


char *
trim_leading_blanks(char *str)
{
	size_t offset = strspn(str, BLANKS);
	return (offset == strlen(str)) ? NULL : (str + offset);
}


int
main(void)
{
	char *rest, *text, *snip;
	ptrdiff_t diff;

	text = trim_leading_blanks((char *)TEXT);
	if (NULL == text) {
		/* All blanks. */
		return 1;
	}

	rest = strpbrk(text, " []");
	/*
	rest now points to a space or '[' or ']' after a term,
	-or- it is NULL if the rest of the string is a single term
	with no spaces nor brackets.
	*/

	while (NULL != rest) {

		/* How many chars have we got? */
		diff = rest - text;
		/*
		diff can be zero when there is more than one space in
		a sequence in the input string.  This won't happen on
		the first iteration but it can on later iterations.
		*/

		if (diff) {
			/* Allocate space and copy out the substring. */
			snip = (char *)GC_malloc(diff + 1);
			strncat(snip, text, diff);
			printf("%s\n", snip);
		}

		/* The next char is a space or '[' or ']'. */
		if ('[' == rest[0] || ']' == rest[0]) {
			printf("%c\n", rest[0]);
		}

		text = trim_leading_blanks(++rest);

		/* calling strpbrk on NULL caused segfault! */
		rest = (NULL != text) ? strpbrk(text, " []") : text;
	}
	if (text) {
		printf("%s\n", text);
	}
}
