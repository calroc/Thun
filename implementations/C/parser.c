#include <stddef.h>
#include <stdio.h>
#include <string.h>

const char *BLANKS = " \t";
const char *TEXT = " hi there friend";
/*                  01234567890123456789
                       ^     ^
*/

char *
trim_leading_blanks(char *str)
{
        size_t offset = strspn(str, BLANKS);
        if (offset == strlen(str)) {
                /* All blanks. */
                return NULL;
        }
        return str + offset;
}

int
main(void)
{
	char *rest, *text;
	ptrdiff_t diff;

        text = trim_leading_blanks((char *)TEXT);
	rest = strpbrk(text, " []");
	while (NULL != rest) {
		diff = rest - text;
                text = rest;
                printf("%ld\n", diff);
                rest = strpbrk(++rest, " []");
	}
}
