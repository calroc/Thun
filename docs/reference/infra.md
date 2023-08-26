--------------------

## infra

Combinator

Accept a quoted program and a list on the stack and run the program with
the list as its stack.  Does not affect the stack (below the list.)

       ... x y z [a b c] [Q] infra
    ---------------------------------
        c b a Q [z y x ...] swaack

### Definition

> [swons] [swaack] \[[i]\] [dip] [swaack]


    ... [a b c] [F] swons swaack [i] dip swaack
    ... [[F] a b c]       swaack [i] dip swaack

    c b a [F]   [...] [i] dip swaack
    c b a [F] i [...]         swaack
    c b a  F    [...]         swaack
    d e         [...]         swaack
    ... [e d]


### Discussion

This is one of the more useful combinators.  It allows a quoted
expression to serve as a stack for a program, effectively running it in a
kind of "pocket universe".  If the list represents a datastructure then
`infra` lets you work on its internal structure.

### Crosslinks

[swaack](#swaack)

