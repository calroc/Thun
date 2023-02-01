#include <gc.h>
#include <gmp.h>


enum JoyTypeType {
	joySymbol,
	joyTrue,
	joyFalse,
	joyInt,
	joyList
};

typedef struct list_node* JoyList;

struct JoyType {
	enum JoyTypeType kind;
	union {
		int boolean;
		mpz_t i;
		JoyList el;
	} value;
} name ;

struct list_node {
	struct JoyType head;
	struct list_node* tail;
};


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


int
main(void)
{
	mpz_t pi;
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


	/*sexpr i = new_int();*/
	/*mpz_add(i.i, pi, pi);*/
	/*gmp_printf ("%Zd\n", i.i);*/
	return 0;

	/*return to_i(car(cons(from_i(0),from_i(1))));*/
}
