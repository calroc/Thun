------------------------------------------------------------------------

## split_at

Function

Split a list (second on the stack) at the position given by the number on
the top of the stack.

### Example

       [1 2 3 4 5 6 7] 4 split_at
    --------------------------------
           [5 6 7] [4 3 2 1]

### Definition

> \[[drop]\] \[[take]\] [clop]

### Discussion

Take a list and a number `n` from the stack, take `n` items from the top
of the list and [shunt] them onto a new list that replaces the number `n`
on the top of the stack.

### Crosslinks

[split_list]

