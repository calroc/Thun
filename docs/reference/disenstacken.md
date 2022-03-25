------------------------------------------------------------------------

## disenstacken

Function

The `disenstacken` function expects a list on top of the stack and makes
that the stack discarding the rest of the stack.

       1 2 3 [4 5 6] disenstacken
    --------------------------------
                6 5 4

### Definition

> \[[clear]\] [dip] [reverse] [unstack](#unstack)

### Discussion

Note that the order of the list is not changed, it just looks that way
because the stack is printed with the top on the right while lists are
printed with the top or head on the left.

### Crosslinks

[enstacken]
[stack]
[unstack](#unstack)

