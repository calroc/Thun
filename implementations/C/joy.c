#include <gc.h>
#include <gmp.h>

// Example S-exprs
// https://www.hboehm.info/gc/04tutorial.pdf

typedef union se
{
  struct cons * cp;
  mpz_t i;
} sexpr;

struct cons
{
  union se head;
  union se tail;
};

#define car(s) (s).cp->head
#define cdr(s) (s).cp->tail
#define from_i(z) ({sexpr tmp; tmp.i=z; tmp;})
#define to_i(s) (s).i

sexpr cons(sexpr a, sexpr b) {
    sexpr tmp = {GC_MALLOC(sizeof(struct cons))};
    car(tmp) = a; cdr(tmp) = b;
    return (tmp);
};

void* reallocate_function (void *ptr, size_t old_size, size_t new_size) {
    return GC_realloc(ptr, new_size);
}
void deallocate_function (void *ptr, size_t size) {
    GC_free(ptr);
}

void my_callback(GC_PTR void_obj, GC_PTR void_environment) {
    //MY_ENVIRONMENT *env = (MY_ENVIRONMENT)void_environment;
    mpz_t *obj = (mpz_t*)void_obj;
    mpz_clear(*obj);
}

int main(void)
{
    mp_set_memory_functions(
        &GC_malloc,
        &reallocate_function,
        &deallocate_function
        );
    mpz_t pie;
    //mpz_init_set_str (pie, "3141592653589793238462643383279502884", 10);
    mpz_init_set_str (pie, "25d0c79fe247f31777d922627a74624", 16);
    gmp_printf ("%Zd = %Zx\n", pie, pie);
    GC_register_finalizer(pie, my_callback, NULL, NULL, NULL);
    return 0;

    //return to_i(car(cons(from_i(0),from_i(1))));
}

