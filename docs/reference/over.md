------------------------------------------------------------------------

## over

Function

[dup] the second item on the stack `over` the first.

       a b over
    --------------
        a b a

### Definition

There are many many ways to define this function.

> [swap] [tuck]

> \[[pop]\] [nullary]

> \[[dup]\] [dip] [swap]

> [unit] [dupdip]

> [unit] [dupdipd] [first]

And so on...

### Discussion

A fine old word from Forth.

### Crosslinks

[tuck]

