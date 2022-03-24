--------------------

## app1

"apply one"

Combinator

Given a quoted program on TOS and anything as the second stack item run
the program without disturbing the stack and replace the two args with
the first result of the program.

             ... x [Q] app1
    ---------------------------------
       ... [x ...] [Q] infra first

This is the same effect as the [unary](#unary) combinator.

### Definition

> [nullary](#nullary) [popd](#popd)

### Discussion

Just a specialization of `nullary` really.  Its parallelizable cousins
are more useful.

### Crosslinks

[app2](#app2)
[app3](#app3)
[appN](#appN)
[unary](#unary)

