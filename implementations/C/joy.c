#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <gc.h>
#include <gmp.h>


const char *BLANKS = " \t";
/*const char *TEXT = " 23 [dup   *] i hi there  fr  [[]  ie]nd  []    23      ";*/
/*const char *TEXT = " 23 33 []     ";*/
const char *TEXT = "";


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
	struct JoyType head;
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


/* Create a new list_node with a joySymbol head. */
struct list_node*
make_symbol_node(char *text, size_t size)
{
	struct list_node *node;
	node = GC_malloc(sizeof(struct list_node));
	node->head.kind = joySymbol;
	node->head.value.symbol = (char *)GC_malloc(size + 1);
	strncat(node->head.value.symbol, text, size);
	/*printf("%s\n", node->head.value.symbol);*/
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
		printf("Missing ']' bracket.");
		exit(1);
	};

	*text = trim_leading_blanks(*text);

	if (NULL == *text) {
		printf("Missing ']' bracket.");
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
		printf("Missing ']' bracket.");
		exit(1);
	};

	/* How many chars have we got? */
	diff = rest - *text;

	if (diff) {
		result = make_symbol_node(*text, diff);
		*text = rest;
	} else if ('[' == rest[0]) {
		*text = rest++;
		result = make_list_node(parse_list(text));
	} else if (']' == rest[0]) {
		*text = rest++;
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
	if (NULL == rest) return make_symbol_node(*text, strlen(*text));

	/* How many chars have we got? */
	diff = rest - *text;

	if (diff) {
		thing = make_symbol_node(*text, diff);
		*text = rest;
		return thing;
	}
	if ('[' == rest[0]) {
		*text = rest++;
		return make_list_node(parse_list(text));
	}
	if (']' == rest[0]) {
		printf("Extra ']' bracket.");
		exit(1);
	}
	printf("Should be unreachable.");
	exit(1);
}


struct list_node*
text_to_expression(char *text)
{
	struct list_node *result, *head, *tail;
	printf("1\n");
	result = parse_node(&text);
	printf("2\n");
	print_list(result);
	printf(" <- eh?\n");
	head = result;
	tail = parse_node(&text);
	while (NULL != tail) {
		print_list(tail);
		printf("<- ooh?\n");
		head->tail = tail;
		head = tail;
		tail = parse_node(&text);
	}
	return result;
}


int
main(void)
{
	mpz_t pi;
	struct list_node* el;
	char *text = (char *)TEXT;

	mp_set_memory_functions(
		&GC_malloc,
		&reallocate_function,
		&deallocate_function
	);
	mpz_init_set_str(pi, "3141592653589793238462643383279502884", 10);
	/*mpz_init_set_str(pi, "25d0c79fe247f31777d922627a74624", 16);*/
	GC_register_finalizer(pi, my_callback, NULL, NULL, NULL);

	el = push_integer_from_str("3141592653589793238462643383279502884", 0);
	printf("BEGIN\n");
	el->tail = text_to_expression(text);
	print_list(el);
	printf("\n");
	return 0;
}
