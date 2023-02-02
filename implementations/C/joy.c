#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include <gc.h>
#include <gmp.h>


const char *BLANKS = " \t";
const char *TEXT = " 23 [dup   *] i hi there  fr  [[]  ie]nd]  []    23      ";


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
	mpz_init_set_str((mpz_ptr)&(el->head.value), str, 10);
	GC_register_finalizer((mpz_ptr)&(el->head.value), my_callback, NULL, NULL, NULL);
	el->tail = tail;
	return el;
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
	printf("%s\n", node->head.value.symbol);
	return node;
}


struct list_node*
text_to_expression(char *text)
{
	char *rest;
	ptrdiff_t diff;
	struct list_node *current_list_node;
	struct list_node *result;
	struct list_node *head = NULL;

	if (NULL == text) {
		/* NULL string input. */
		return (struct list_node*)NULL;
	}
	text = trim_leading_blanks(text);
	if (NULL == text) {
		/* All blanks. */
		return (struct list_node*)NULL;
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
			current_list_node = make_symbol_node(text, diff);
			if (head) {
				head->tail = current_list_node;
			} else {
				/* There is no head now, so this must be the first
				   result, the head that we will eventually return. */
				result = current_list_node;
			}
			head = current_list_node;
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
                current_list_node = make_symbol_node(text, strlen(text));
                if (head) {
                        head->tail = current_list_node;
                } else {
                        result = current_list_node;
                }
	}
	return result;
}


void
print_node(struct JoyType j)
{
        switch (j.kind) {
        case joySymbol:
                printf("%s", j.value.symbol);
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
        printf("\n");
}


int
main(void)
{
	mpz_t pi;
	struct list_node* el;

	mp_set_memory_functions(
		&GC_malloc,
		&reallocate_function,
		&deallocate_function
	);
	mpz_init_set_str(pi, "3141592653589793238462643383279502884", 10);
	/*mpz_init_set_str(pi, "25d0c79fe247f31777d922627a74624", 16);*/
	GC_register_finalizer(pi, my_callback, NULL, NULL, NULL);

	gmp_printf("%Zd = %Zx\n", pi, pi);
	mpz_mul(pi, pi, pi);
	gmp_printf("%Zd = %Zx\n", pi, pi);

	el = push_integer_from_str("3141592653589793238462643383279502884", 0);
	el = text_to_expression("char *te xt");
        print_list(el);
	/*sexpr i = new_int();*/
	/*mpz_add(i.i, pi, pi);*/
	/*gmp_printf ("%Zd\n", i.i);*/
	return 0;

	/*return to_i(car(cons(from_i(0),from_i(1))));*/
}
