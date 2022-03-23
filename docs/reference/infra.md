--------------------

## infra

(Combinator)

Accept a quoted program and a list on the stack and run the program with
the list as its stack.  Does not affect the stack (below the list.)

       ... [a b c] [Q] infra
    ---------------------------
        c b a Q [...] swaack

### Definition

    swons swaack [i] dip swaack


### Discussion

This is one of the more useful combinators.  It allows a quoted
expression to serve as a stack for a program, effectively running it in a
kind of "pocket universe".  If the list represents a datastructure then
`infra` lets you work on its internal structure.

### Crosslinks

[swaack](#swaack)

