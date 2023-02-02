#include <stddef.h>
#include <stdio.h>
#include <string.h>

const char *text = "hi there friend";
/*                  01234567890123456789
                      ^     ^
*/

int
main(void)
{
	char *rest;
	ptrdiff_t diff;

	rest = strpbrk(text, " []");
	while (NULL != rest) {
		diff = rest - text;
                text = rest;
                printf("%ld\n", diff);
                rest = strpbrk(++rest, " []");
	}
}
