/*

████████╗██╗  ██╗██╗   ██╗███╗   ██╗
╚══██╔══╝██║  ██║██║   ██║████╗  ██║
   ██║   ███████║██║   ██║██╔██╗ ██║
   ██║   ██╔══██║██║   ██║██║╚██╗██║
   ██║   ██║  ██║╚██████╔╝██║ ╚████║
   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

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
#include <setjmp.h>

#include <gc.h>
#include <gmp.h>

#include "joy.h"
#include "definitions.h"
#include "uthash.h"


static jmp_buf jbuf;

const char *BLANKS = " \t";
const char *FALSE = "false";
const char *TRUE = "true";

JoyType loop_symbol = {joySymbol, {"loop"}};
JoyType JoyTrueVal = {joyTrue, {NULL}};
JoyType JoyFalseVal = {joyFalse, {NULL}};

JoyTypePtr JoyTrue = &JoyTrueVal;
JoyTypePtr JoyFalse = &JoyFalseVal;


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
	mpz_t *obj = (mpz_t*)void_obj;
	mpz_clear(*obj);
}





struct user_def {
	char *name;/* key */
	JoyList body;
	UT_hash_handle hh;         /* makes this structure hashable */
};

struct user_def *user_defs = NULL;


/*
██╗   ██╗████████╗██╗██╗     ███████╗
██║   ██║╚══██╔══╝██║██║     ██╔════╝
██║   ██║   ██║   ██║██║     ███████╗
██║   ██║   ██║   ██║██║     ╚════██║
╚██████╔╝   ██║   ██║███████╗███████║
 ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝
*/


JoyList
make_non_list_node(char *text, size_t size)
{
	char *sym;
	const struct dict_entry *interned;
	JoyList node = newJoyList;

	interned = in_word_set(text, size);
	if (interned) {
		/* TODO: pre-compute and reuse whole JoyType in wordlist? */
		node->head = newJoyType;
		node->head->kind = joySymbol;
		node->head->value.symbol = interned->name;
		return node;
	}

	sym = GC_malloc(size + 1);  /* one more for the zero, right? */
	strncat(sym, text, size);

	if (!strcmp(sym, FALSE)) {
		node->head = JoyFalse;
	} else if (!strcmp(sym, TRUE)) {
		node->head = JoyTrue;
	} else {
		node->head = newJoyType;
		if (mpz_init_set_str(node->head->value.i, sym, 10)) {
			/* Non-zero (-1) return value means the string is not an int. */
			mpz_clear(node->head->value.i);
			node->head->kind = joySymbol;
			node->head->value.symbol = sym;

		} else {
			node->head->kind = joyInt;
			GC_register_finalizer(node->head->value.i, my_callback, NULL, NULL, NULL);
		}
	}

	return node;
}


/* Create a new list_node with a joyList head. */
JoyList
make_list_node(JoyList el)
{
	JoyList node = newJoyList;
	node->head = newJoyType;
	node->head->kind = joyList;
	node->head->value.el = el;
	return node;
}


void
push_quote(JoyList el, JoyListPtr stack)
{
	JoyList node = make_list_node(el);
	node->tail = *stack;
	*stack = node;
}


JoyList
pop_any(JoyListPtr stack)
{
	JoyList result;
	if (!(*stack)) {
		printf("Not enough values on stack.\n");
		longjmp(jbuf, 1);
	}
	result = *stack;
	*stack = (*stack)->tail;
	return result;
}


mpz_t *
pop_int(JoyListPtr stack)
{
	JoyList node = pop_any(stack);
	switch (node->head->kind) {
	case joyInt:
		return &(node->head->value.i);
	default:
		printf("Not an integer.\n");
		longjmp(jbuf, 1);
	}
}


int
pop_bool(JoyListPtr stack)
{
	JoyList node = pop_any(stack);
	/* TODO: Look for just the singletons? */
	switch (node->head->kind) {
	case joyTrue:
		return 1;
	case joyFalse:
		return 0;
	default:
		printf("Not a Boolean.\n");
		longjmp(jbuf, 1);
	}
}


JoyList
pop_list_node(JoyListPtr stack)
{
	JoyList node;
	node = pop_any(stack);
	switch (node->head->kind) {
	case joyList:
		return node;
	default:
		printf("Not a list.\n");
		longjmp(jbuf, 1);
	}
}


JoyList
pop_list(JoyListPtr stack)
{
	return pop_list_node(stack)->head->value.el;
}


JoyList
newIntNode(void) {
	JoyList node = newJoyList;
	node->head = newJoyType;
	node->head->kind = joyInt;
	mpz_init(node->head->value.i);
	GC_register_finalizer(node->head->value.i, my_callback, NULL, NULL, NULL);
	return node;
}


void
push_thing(JoyTypePtr term, JoyListPtr stack)
{
	JoyList node = newJoyList;
	node->head = term;
	node->tail = *stack;
	*stack = node;
}


JoyList
concat_lists(JoyList a, JoyList b)
{
	JoyList node;
	if (!a) return b;
	if (!b) return a;
	node = newJoyList;
	node->head = a->head;
	node->tail = concat_lists(a->tail, b);
	return node;
}


void
push_thing_in_unit_list(JoyTypePtr term, JoyListPtr expression)
{
	JoyList x = EMPTY_LIST;
	JoyListPtr xPtr = &x;
	push_thing(term, xPtr);
	push_quote_onto_expression(*xPtr, expression);
}


/*
██████╗ ██████╗ ██╗███╗   ██╗████████╗███████╗██████╗
██╔══██╗██╔══██╗██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗
██████╔╝██████╔╝██║██╔██╗ ██║   ██║   █████╗  ██████╔╝
██╔═══╝ ██╔══██╗██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗
██║     ██║  ██║██║██║ ╚████║   ██║   ███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
*/


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
		print_node(*(el->head));
		el = el->tail;
		if (NULL != el) {
			printf(" ");
		}
	}
}


void
print_stack(JoyList el)
{
	if (el) {
		if (el->tail) {
			print_stack(el->tail);
			printf(" ");
		}
		print_node(*(el->head));
	}
}


/*
██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
*/


char *
trim_leading_blanks(char *str)
{
	size_t offset = strspn(str, BLANKS);
	return (offset == strlen(str)) ? NULL : (str + offset);
}


JoyList
parse_list(char **text)
{
	/*
	 * Extract terms from the text until a closing bracket is found.
	 */
	char *rest;
	ptrdiff_t diff;
	JoyList result = EMPTY_LIST;

	/* NULL string input? */
	if (NULL == *text) {
		printf("Missing ']' bracket. A\n");
		longjmp(jbuf, 1);
	};

	*text = trim_leading_blanks(*text);

	if (NULL == *text) {
		printf("Missing ']' bracket. B\n");
		longjmp(jbuf, 1);
	};

	/* Look for blanks or brackets. */
	rest = strpbrk(*text, " []");
	/* rest now points to a space or '[' or ']' after a term, -or- it
	 * is NULL if the rest of the string is a single term with no
	 * spaces nor brackets.  If that's the case then we're missing a
	 * closing bracket!
	 */

	if (NULL == rest) {
		printf("Missing ']' bracket. C\n");
		longjmp(jbuf, 1);
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
	JoyList thing;

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
		longjmp(jbuf, 1);
	}
	printf("Should be unreachable.");
	exit(1);
}


JoyList
text_to_expression(char *text)
{
	JoyList result, head, tail;

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


/*
███████╗██╗  ██╗██████╗ ██████╗ ███████╗███████╗███████╗██╗ ██████╗ ███╗   ██╗
██╔════╝╚██╗██╔╝██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝██║██╔═══██╗████╗  ██║
█████╗   ╚███╔╝ ██████╔╝██████╔╝█████╗  ███████╗███████╗██║██║   ██║██╔██╗ ██║
██╔══╝   ██╔██╗ ██╔═══╝ ██╔══██╗██╔══╝  ╚════██║╚════██║██║██║   ██║██║╚██╗██║
███████╗██╔╝ ██╗██║     ██║  ██║███████╗███████║███████║██║╚██████╔╝██║ ╚████║
╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝

As elegant as it is to model the expression as a stack, it's not very
efficient, as concatenating definitions and other quoted programs to
the expression is a common and expensive operation.

Instead, let's keep a stack of sub-expressions, reading from them
one-by-one, and prepending new sub-expressions to the stack rather than
concatenating them.

*/


void
push_quote_onto_expression(JoyList el, JoyListPtr expression)
{
	JoyList node;
	if (!el) return;
	node = make_list_node(el);
	node->tail = *expression;
	*expression = node;
}


JoyTypePtr
next_term(JoyListPtr expression)
{
	JoyList quote;
	JoyTypePtr term;
	if (!(*expression)) {
		printf("Do not call next_term on an empty expression.\n");
		exit(1);
	}
	quote = pop_list(expression);
	if (!quote) {
		printf("How did an empty list get onto the expression!?\n");
		exit(1);
	}
	term = quote->head;
	quote = quote->tail;
	if (quote) {
		push_quote_onto_expression(quote, expression);
	}
	return term;
}


/*
 ██████╗ ██████╗ ██████╗ ███████╗    ██╗    ██╗ ██████╗ ██████╗ ██████╗ ███████╗
██╔════╝██╔═══██╗██╔══██╗██╔════╝    ██║    ██║██╔═══██╗██╔══██╗██╔══██╗██╔════╝
██║     ██║   ██║██████╔╝█████╗      ██║ █╗ ██║██║   ██║██████╔╝██║  ██║███████╗
██║     ██║   ██║██╔══██╗██╔══╝      ██║███╗██║██║   ██║██╔══██╗██║  ██║╚════██║
╚██████╗╚██████╔╝██║  ██║███████╗    ╚███╔███╔╝╚██████╔╝██║  ██║██████╔╝███████║
 ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚══╝╚══╝  ╚═════╝ ╚═╝  ╚═╝╚═════╝ ╚══════╝
*/


#define BINARY_MATH_OP(name) \
void \
name(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression) \
{ \
	mpz_t *a, *b; \
	JoyList node; \
	b = pop_int(stack); \
	a = pop_int(stack); \
	node = newIntNode(); \
	mpz_ ## name(node->head->value.i, *a, *b); \
	node->tail = *stack; \
	*stack = node; \
}

BINARY_MATH_OP(add)
BINARY_MATH_OP(sub)
BINARY_MATH_OP(mul)
BINARY_MATH_OP(fdiv_q)
BINARY_MATH_OP(fdiv_r)


/*
With mpz_cmp we can implement the rest of the comparison functions as definitions:

     G       E       L
 eq [false] [true] [false] cmp
 gt [true] [false] [false] cmp
 lt [false] [false] [true] cmp
neq [true] [false] [true] cmp
 le [false] [true] [true] cmp
 ge [true] [true] [false] cmp
*/
void
cmp_joyfunc(JoyListPtr stack, JoyListPtr expression)
{
	JoyList L = pop_list(stack);
	JoyList E = pop_list(stack);
	JoyList G = pop_list(stack);
	mpz_t *b = pop_int(stack);
	mpz_t *a = pop_int(stack);
	int hmm = mpz_cmp(*a, *b);
	push_quote_onto_expression(((hmm > 0) ? G : (hmm < 0) ? L : E), expression);
}


void
i_joyfunc(JoyListPtr stack, JoyListPtr expression)
{
	push_quote_onto_expression(pop_list(stack), expression);
}


void
branch(JoyListPtr stack, JoyListPtr expression)
{
	JoyList T = pop_list(stack);
	JoyList F = pop_list(stack);
	push_quote_onto_expression((pop_bool(stack) ? T : F), expression);
}


void
loop(JoyListPtr stack, JoyListPtr expression)
{
	JoyList body = pop_list(stack);
	JoyList x = EMPTY_LIST;

	if (pop_bool(stack)) {
		JoyListPtr xPtr = &x;
		push_thing(&loop_symbol, xPtr);
		push_quote(body, xPtr);
		push_quote_onto_expression(*xPtr, expression);
		push_quote_onto_expression(body, expression);
	}
}


void
clear(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	*stack = EMPTY_LIST;
}


void
cons(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	JoyList quote = pop_list(stack);
	JoyListPtr qPtr = &quote;
	JoyList node = pop_any(stack);
	push_thing(node->head, qPtr);
	push_quote(*qPtr, stack);
}


void
pop(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	if (!(*stack)) {
		printf("Cannot pop empty stack.\n");
		longjmp(jbuf, 1);
	}
	pop_any(stack);
}


void
swaack(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	JoyList quote = pop_list(stack);
	JoyListPtr qPtr = &quote;
	push_quote(*stack, qPtr);
	*stack = *qPtr;
}


void
stack(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	push_quote(*stack, stack);
}


void
swap(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	JoyList a = pop_any(stack);
	JoyList b = pop_any(stack);
	push_thing(a->head, stack);
	push_thing(b->head, stack);
}


void
concat(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	JoyList b = pop_list(stack);
	JoyList a = pop_list(stack);
	push_quote(concat_lists(a, b), stack);
}


void
first(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	JoyList quote = pop_list(stack);
	if (!quote) {
		printf("Cannot take first of empty list.\n");
		longjmp(jbuf, 1);
	}
	push_thing(quote->head, stack);
}


void
rest(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	JoyList quote = pop_list(stack);
	if (!quote) {
		printf("Cannot take rest of empty list.\n");
		longjmp(jbuf, 1);
	}
	push_quote(quote->tail, stack);
}


void
dip(JoyListPtr stack, JoyListPtr expression)
{
	JoyList quote = pop_list(stack);
	JoyList node = pop_any(stack);
	push_thing_in_unit_list(node->head, expression);
	push_quote_onto_expression(quote, expression);
}


void
dup(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	JoyList s = *stack;
	JoyList node = pop_any(stack);
	*stack = s;
	push_thing(node->head, stack);
}


void
truthy(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	/*
	Keep the original stack in case	the top item is already a Boolean value.
	*/
	JoyList s = *stack;
	JoyList node = pop_any(stack);
	switch (node->head->kind) {
	case joyTrue:
		*stack = s;
		break;
	case joyFalse:
		*stack = s;
		break;
	case joyInt:
		if mpz_cmp_si(node->head->value.i, 0) {
			push_thing(JoyTrue, stack);
		} else {
			push_thing(JoyFalse, stack);
		}
		break;
	case joyList:
		if (node->head->value.el) {
			push_thing(JoyTrue, stack);
		} else {
			push_thing(JoyFalse, stack);
		}
		break;
	default:
		printf("Cannot Boolify.\n");
		longjmp(jbuf, 1);
	}
}


/*
 *User definitions with inscribe command.
 */

void
add_user_def(char *name, JoyList body)
{
	struct user_def *s;
	HASH_FIND_STR(user_defs, name, s);
	if (!s) {
		s = GC_malloc(sizeof *s);
		s->name = name;
		HASH_ADD_KEYPTR(hh, user_defs, s->name, strlen(s->name), s);
	}
	s->body = body;
}


void
inscribe(JoyListPtr stack, __attribute__((unused)) JoyListPtr expression)
{
	JoyList quote = pop_list(stack);
	if (!quote) return;
	if (joySymbol != quote->head->kind) return;
	add_user_def(quote->head->value.symbol, quote->tail);
}


/*
██╗███╗   ██╗████████╗███████╗██████╗ ██████╗ ██████╗ ███████╗████████╗███████╗██████╗
██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗
██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██████╔╝██████╔╝█████╗     ██║   █████╗  ██████╔╝
██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██╔═══╝ ██╔══██╗██╔══╝     ██║   ██╔══╝  ██╔══██╗
██║██║ ╚████║   ██║   ███████╗██║  ██║██║     ██║  ██║███████╗   ██║   ███████╗██║  ██║
╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
*/


void
dispatch(char *sym, JoyListPtr stack, JoyListPtr expression)
{
	struct user_def *s;
	const struct dict_entry *word = in_word_set(sym, strlen(sym));
	if (word) {
		word->func(stack, expression);
		return;
	}
	HASH_FIND_STR(user_defs, sym, s);
	if (s) {
		push_quote_onto_expression(s->body, expression);
		return;
	}
	printf("Unknown: %s\n", sym);
	longjmp(jbuf, 1);
}


void
joy(JoyListPtr stack, JoyListPtr expression)
{
	JoyTypePtr term;
	JoyList e = EMPTY_LIST;
	JoyListPtr ePtr = &e;
	push_quote_onto_expression(*expression, ePtr);
	expression = ePtr;

	while (*expression) {
		term = next_term(expression);
		switch (term->kind) {
		case joyInt:
		case joyTrue:
		case joyFalse:
		case joyList:
			push_thing(term, stack);
			break;
		case joySymbol:
			dispatch(term->value.symbol, stack, expression);
		}
	}
}


/* Set quiet mode by "-q as only command line option. "*/
int quiet = 0;

#define SHH(message) \
if (!quiet) { \
	printf(message); \
}


int
main(int argc, char *argv[])
{
	char *line;
	char *status;
	JoyList stack = EMPTY_LIST;
	JoyList expression = EMPTY_LIST;
	JoyList s;

	mp_set_memory_functions(
		&GC_malloc,
		&reallocate_function,
		&deallocate_function
	);

	init_defs();

	quiet = ((2 == argc) && (!strcmp("-q", argv[1])));

	line = (char *)GC_malloc(1025);

	while (1) {
		SHH("\njoy? ")
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
			SHH("bye\n")
			break;
		}
		s = stack;
		if (!setjmp(jbuf)) {
			expression = text_to_expression(line);
			joy(&stack, &expression);
		} else {
			/* err */
			stack = s;
		}
		print_stack(stack);
		printf("\n");
	}
	return 0;
}
