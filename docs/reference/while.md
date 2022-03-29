------------------------------------------------------------------------

## while

Combinator

A specialization of [loop] that accepts a quoted predicate program `P`
and runs it [nullary].

       [P] [F] while
    ------------------- P -> false

        [P] [F] while
    --------------------- P -> true
       F [P] [F] while

### Definition

> [swap] [nulco] [dupdipd] [concat] [loop]

### Crosslinks

[loop]

