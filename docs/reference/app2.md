------------------------------------------------------------------------

## app2

Combinator

Like [app1](#app1) with two items.

       ... y x [Q] . app2
    -----------------------------------
       ... [y ...] [Q] . infra first
           [x ...] [Q]   infra first

### Definition

> \[[grba] [swap] [grba] [swap]\] [dip] \[[infrst]\] [cons] [ii]

### Discussion

Unlike [app1](#app1), which is essentially an alias for [unary](#unary),
this function is not the same as [binary](#binary).  Instead of running
one program using exactly two items from the stack and pushing one
result (as [binary](#binary) does) this function takes two items from the
stack and runs the program twice, separately for each of the items, then
puts both results onto the stack.

This is not currently implemented as parallel processes but it can (and
should) be done.

### Crosslinks

[app1](#app1)
[app3](#app3)
[appN](#appN)
[unary](#unary)

