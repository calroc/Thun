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

typedef struct JoyType {
	enum JoyTypeType kind;
	union {
		int b; // bool
		mpz_t i;
		JoyList el;
	};
} name ;

typedef struct list_node {
	struct JoyType head;
	struct list_node* tail;
} *JoyList;


// Example S-exprs
// https://www.hboehm.info/gc/04tutorial.pdf
/**/
/*typedef union se*/
/*{*/
/*  struct cons * cp;*/
/*  mpz_t i;*/
/*} sexpr;*/
/**/
/*struct cons*/
/*{*/
/*  union se head;*/
/*  union se tail;*/
/*};*/
/**/
/*#define car(s) (s).cp->head*/
/*#define cdr(s) (s).cp->tail*/
/*#define from_i(z) ({sexpr tmp; tmp.i=z; tmp;})*/
/*#define to_i(s) (s).i*/
/**/
/*sexpr cons(sexpr a, sexpr b) {*/
/*    sexpr tmp = {GC_MALLOC(sizeof(struct cons))};*/
/*    car(tmp) = a; cdr(tmp) = b;*/
/*    return (tmp);*/
/*};*/

void* reallocate_function (void *ptr, size_t old_size, size_t new_size) {
	return GC_REALLOC(ptr, new_size);
}

void deallocate_function (void *ptr, size_t size) {
	GC_FREE(ptr);
}

void my_callback(GC_PTR void_obj, GC_PTR void_environment) {
	//MY_ENVIRONMENT *env = (MY_ENVIRONMENT)void_environment;
	mpz_t *obj = (mpz_t*)void_obj;
	mpz_clear(*obj);
}

/*sexpr new_int(void) {*/
/*  sexpr node = {GC_MALLOC(sizeof(struct cons))};*/
/*  mpz_init(node.i);*/
/*  GC_register_finalizer(node.i, my_callback, NULL, NULL, NULL);*/
/*  return (node);*/
/*}*/


int
main(void)
{
	mp_set_memory_functions(
		&GC_malloc,
		&reallocate_function,
		&deallocate_function
	);
	mpz_t pi;
	mpz_init_set_str(pi, "3141592653589793238462643383279502884", 10);
	//mpz_init_set_str(pi, "25d0c79fe247f31777d922627a74624", 16);
	GC_register_finalizer(pi, my_callback, NULL, NULL, NULL);

	gmp_printf("%Zd = %Zx\n", pi, pi);
	mpz_mul(pi, pi, pi);
	gmp_printf("%Zd = %Zx\n", pi, pi);


	/*sexpr i = new_int();*/
	/*mpz_add(i.i, pi, pi);*/
	/*gmp_printf ("%Zd\n", i.i);*/
	return 0;

	//return to_i(car(cons(from_i(0),from_i(1))));
}
