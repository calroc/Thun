/*
    Copyright © 2023 Simon Forman

    This file is part of Thun

    Thun is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Thun is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Thun.  If not see <http://www.gnu.org/licenses/>.


████████╗██╗  ██╗██╗   ██╗███╗   ██╗
╚══██╔══╝██║  ██║██║   ██║████╗  ██║
   ██║   ███████║██║   ██║██╔██╗ ██║
   ██║   ██╔══██║██║   ██║██║╚██╗██║
   ██║   ██║  ██║╚██████╔╝██║ ╚████║
   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

This program implements an interpreter for a dialect of Joy.

Joy is a programming language created by Manfred von Thun that is easy to
use and understand and has many other nice properties. This Python
package implements an interpreter for a dialect of Joy that attempts to
stay very close to the spirit of Joy but does not precisely match the
behaviour of the original version(s) written in C. The main difference
between Thun and the originals, other than being written in Python, is
that it works by the “Continuation-Passing Style”.

Here is an example of Joy code:


    [   [[abs] ii <=]
        [
            [<>] [pop !-] ||
        ] &&
    ]
    [[    !-] [[++]] [[--]] ifte dip]
    [[pop !-]  [--]   [++]  ifte    ]
    ifte

This function accepts two integers on the stack and increments or
decrements one of them such that the new pair of numbers is the next
coordinate pair in a square spiral (like the kind used to construct an
Ulam Spiral).

*/
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <gc.h>
#include <gmp.h>


const char *BLANKS = " \t";
const char *FALSE = "false";
const char *TRUE = "true";


enum JoyTypeType {
	joySymbol,
	joyTrue,
	joyFalse,
	joyInt,
	joyList
};


struct JoyType {
	enum JoyTypeType kind;
	union {
		int boolean;
		mpz_t i;
		struct list_node* el;
		char *symbol;
	} value;
} name ;


struct list_node {
	struct JoyType head;  /* Should this be a pointer? */
	struct list_node* tail;
} JoyList;


void*
reallocate_function (void *ptr, __attribute__((unused)) size_t old_size, size_t new_size) {
	return GC_REALLOC(ptr, new_size);
}


void
deallocate_function (void *ptr, __attribute__((unused)) size_t size) {
	GC_FREE(ptr);
}


void
my_callback(GC_PTR void_obj, __attribute__((unused)) GC_PTR void_environment) {
	/*MY_ENVIRONMENT *env = (MY_ENVIRONMENT)void_environment;*/
	mpz_t *obj = (mpz_t*)void_obj;
	mpz_clear(*obj);
}


struct list_node*
push_integer_from_str(char *str, struct list_node* tail)
{
	struct list_node* el;
	el = GC_malloc(sizeof(struct list_node));
	el->head.kind = joyInt;
	mpz_init_set_str(el->head.value.i, str, 10);
	GC_register_finalizer(el->head.value.i, my_callback, NULL, NULL, NULL);
	el->tail = tail;
	return el;
}



/* Pre-declare so we can use it in print_node(). */
void
print_list(struct list_node* el);


void
print_node(struct JoyType j)
{
	switch (j.kind) {
	case joyInt:
		gmp_printf("%Zd", j.value.i);
		break;
	case joySymbol:
		printf("%s", j.value.symbol);
		break;
	case joyTrue:
		printf("true");
		break;
	case joyFalse:
		printf("false");
		break;
	case joyList:
		printf("[");
		print_list(j.value.el);
		printf("]");
		break;
	default:
		printf("wtf");
	}
}


void
print_list(struct list_node* el)
{
	while (NULL != el) {
		print_node(el->head);
		el = el->tail;
		if (NULL != el) {
			printf(" ");
		}
	}
}


char *
trim_leading_blanks(char *str)
{
	size_t offset = strspn(str, BLANKS);
	return (offset == strlen(str)) ? NULL : (str + offset);
}


struct list_node*
make_non_list_node(char *text, size_t size)
{
	struct list_node *node;
	char *sym;

	sym = GC_malloc(size + 1);  /* one more for the zero, right? */
	strncat(sym, text, size);

	node = GC_malloc(sizeof(struct list_node));

	if (!strncmp(sym, FALSE, 6)) {  /* I know it's wrong to hardcode the length here.  Sorry. */
		/* If head was a pointer we could reuse Boolean singletons... */
		node->head.kind = joyFalse;
		node->head.value.boolean = 0;

	} else if (!strncmp(sym, TRUE, 5)) {  /* I know it's wrong to hardcode the length here.  Sorry. */
		node->head.kind = joyTrue;
		node->head.value.boolean = 1;

	} else if (mpz_init_set_str(node->head.value.i, sym, 10)) {
		/* Non-zero (-1) return value means the string is not an int. */
		mpz_clear(node->head.value.i);
		node->head.kind = joySymbol;
		node->head.value.symbol = sym;

	} else {
		node->head.kind = joyInt;
		GC_register_finalizer(node->head.value.i, my_callback, NULL, NULL, NULL);
	}

	return node;
}


/* Create a new list_node with a joyList head. */
struct list_node*
make_list_node(struct list_node *el)
{
	struct list_node *node;
	node = GC_malloc(sizeof(struct list_node));
	node->head.kind = joyList;
	node->head.value.el = el;
	return node;
}

#define EMPTY_LIST (struct list_node*)NULL

/*
Extract terms from the text until a closing bracket is found.
*/
struct list_node*
parse_list(char **text)
{
	char *rest;
	ptrdiff_t diff;
	struct list_node *result = NULL;

	/* NULL string input? */
	if (NULL == *text) {
		printf("Missing ']' bracket. A\n");
		exit(1);
	};

	*text = trim_leading_blanks(*text);

	if (NULL == *text) {
		printf("Missing ']' bracket. B\n");
		exit(1);
	};

	/* Look for blanks or brackets. */
	rest = strpbrk(*text, " []");
	/*
	rest now points to a space or '[' or ']' after a term,
	-or- it is NULL if the rest of the string is a single term
	with no spaces nor brackets.  If that's the case then we're
	missing a closing bracket!
	*/
	if (NULL == rest) {
		printf("Missing ']' bracket. C\n");
		exit(1);
	};

	/* How many chars have we got? */
	diff = rest - *text;

	if (diff) {
		result = make_non_list_node(*text, diff);
		*text = rest;
	} else if ('[' == rest[0]) {
		*text = ++rest;
		result = make_list_node(parse_list(text));
	} else if (']' == rest[0]) {
		*text = ++rest;
		return result;
	}
	result->tail = parse_list(text);
	return result;
}

/*
Get the next node from the text, updating text
to point to the rest of the, uh, text.
*/
struct list_node*
parse_node(char **text)
{
	char *rest;
	ptrdiff_t diff;
	struct list_node *thing;

	/* NULL string input? */
	if (NULL == *text) return EMPTY_LIST;

	*text = trim_leading_blanks(*text);

	/* All blanks? */
	if (NULL == *text) return EMPTY_LIST;

	/* Look for blanks or brackets. */
	rest = strpbrk(*text, " []");
	/*
	rest now points to a space or '[' or ']' after a term,
	-or- it is NULL if the rest of the string is a single term
	with no spaces nor brackets.  If that's the case then we're
	done, and we can just return a list with one symbol in it.
	*/
	if (NULL == rest) {
		thing = make_non_list_node(*text, strlen(*text));
		*text = rest;
		return thing;
	}

	/* How many chars have we got? */
	diff = rest - *text;

	if (diff) {
		thing = make_non_list_node(*text, diff);
		*text = rest;
		return thing;
	}
	if ('[' == rest[0]) {
		*text = ++rest;
		return make_list_node(parse_list(text));
	}
	if (']' == rest[0]) {
		printf("Extra ']' bracket.\n");
		exit(1);
	}
	printf("Should be unreachable.");
	exit(1);
}


struct list_node*
text_to_expression(char *text)
{
	struct list_node *result, *head, *tail;

	result = parse_node(&text);
	head = result;
	tail = parse_node(&text);
	while (NULL != tail) {
		head->tail = tail;
		head = tail;
		tail = parse_node(&text);
	}
	return result;
}

int
main(void)
{
	char *line;
	char *status;

	mp_set_memory_functions(
		&GC_malloc,
		&reallocate_function,
		&deallocate_function
	);

	line = (char *)GC_malloc(1025);

	while (1) {
		status = gets_s(line, 1025);
		if (NULL == status) {
			/*
			From the man page:

			> Upon successful completion, fgets(), gets_s(), and gets() return a
			pointer to the string.  If end-of-file occurs before any characters are
			read, they return NULL and the buffer contents remain unchanged.  If an
			error occurs, they return NULL and the buffer contents are indeterminate.
			The fgets(), gets_s(), and gets() functions do not distinguish between
			end-of-file and error, and callers must use feof(3) and ferror(3) to
			determine which occurred.

			TODO: "use feof(3) and ferror(3)"...

			*/
			printf("bye\n");
			break;
		}
		print_list(text_to_expression(line));
		printf("\n");
	}
	return 0;
}
