#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <gc.h>


const char *BLANKS = " \t";
const char *TEXT = " hi there fr[ie]nd";
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
	char *rest, *text, *snip;
	ptrdiff_t diff;

        text = trim_leading_blanks((char *)TEXT);
	rest = strpbrk(text, " []");
	while (NULL != rest) {
		diff = rest - text;
                printf("%ld\n", diff);
                snip = (char *)GC_malloc(diff + 1);
                strncat(snip, text, diff);
                printf("%s <%c>\n", snip, rest[0]);
                text = ++rest;
                rest = strpbrk(text, " []");
	}
        printf("%s\n", text);
}
