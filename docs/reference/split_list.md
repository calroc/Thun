------------------------------------------------------------------------

## split_list

Function

Split a list (second on the stack) at the position given by the number on
the top of the stack such that [concat] would reconstruct the original
list.

       [1 2 3 4 5 6 7] 4 split_list
    ----------------------------------
            [1 2 3 4] [5 6 7]

### Definition

> \[[take] [reverse]\] \[[drop]\] [clop]

### Discussion

Compare with [split_at].  This function does extra work to ensure that
[concat] would reconstruct the original list.

### Crosslinks

[split_at]

