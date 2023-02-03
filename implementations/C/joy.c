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

*/
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <gc.h>
#include <gmp.h>

#include "joy.h"


const char *BLANKS = " \t";
const char *FALSE = "false";
const char *TRUE = "true";



/*
void
dup(JoyList *stack, JoyList *expression) {
}
*/


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


JoyList
push_integer_from_str(char *str, JoyList tail)
{
	JoyList el = newJoyList;
	el->head.kind = joyInt;
	mpz_init_set_str(el->head.value.i, str, 10);
	GC_register_finalizer(el->head.value.i, my_callback, NULL, NULL, NULL);
	el->tail = tail;
	return el;
}


/* Pre-declare so we can use it in print_node(). */
void
print_list(JoyList el);


void
print_node(JoyType j)
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
print_list(JoyList el)
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


JoyList
make_non_list_node(char *text, size_t size)
{
	char *sym;
	const struct dict_entry *interned;
	JoyList node = newJoyList;

	interned = in_word_set(text, size);
	if (interned) {
		node->head.kind = joySymbol;
		node->head.value.symbol = interned->name;
		return node;
	}

	sym = GC_malloc(size + 1);  /* one more for the zero, right? */
	strncat(sym, text, size);

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
JoyList
make_list_node(JoyList el)
{
	JoyList node = newJoyList;
	node->head.kind = joyList;
	node->head.value.el = el;
	return node;
}


/*
██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
*/


JoyList
parse_list(char **text)
{
/*
Extract terms from the text until a closing bracket is found.
*/
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
JoyList
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


JoyList
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


void add(JoyList *stack, JoyList *expression) {stack = expression;}
void branch(JoyList *stack, JoyList *expression) {stack = expression;}
void clear(JoyList *stack, JoyList *expression) {stack = expression;}
void div_joyfunc(JoyList *stack, JoyList *expression) {stack = expression;}
void eq(JoyList *stack, JoyList *expression) {stack = expression;}
void ge(JoyList *stack, JoyList *expression) {stack = expression;}
void gt(JoyList *stack, JoyList *expression) {stack = expression;}
void le(JoyList *stack, JoyList *expression) {stack = expression;}
void lt(JoyList *stack, JoyList *expression) {stack = expression;}
void mod(JoyList *stack, JoyList *expression) {stack = expression;}
void mul(JoyList *stack, JoyList *expression) {stack = expression;}
void neq(JoyList *stack, JoyList *expression) {stack = expression;}
void sub(JoyList *stack, JoyList *expression) {stack = expression;}
void truthy(JoyList *stack, JoyList *expression) {stack = expression;}


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
