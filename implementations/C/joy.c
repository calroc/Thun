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
int main(void)
{
    mp_set_memory_functions(
        &GC_malloc,
        &reallocate_function,
        &deallocate_function
        );
    return 0;
    //return to_i(car(cons(from_i(0),from_i(1))));
}

