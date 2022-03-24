------------------------------------------------------------------------

## branch

Basis Combinator

Use a Boolean value to select and run one of two quoted programs.


       false [F] [T] branch
    --------------------------
              F

       true [F] [T] branch
    -------------------------
                 T


### Definition

> [rolldown] [choice] [i]

### Discussion

This is one of the fundamental operations (although it can be defined in
terms of [choice] as above).  The more common "if..then..else" construct
[ifte] adds a predicate function that is evaluated [nullary].

### Crosslinks

[choice]
[ifte]
[select]

