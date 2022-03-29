------------------------------------------------------------------------

## swaack

Basis Function

Swap stack.  Take a list from the top of the stack, replace the stack
with the list, and put the old stack onto it.

### Example

       1 2 3 [4 5 6] swaack
    --------------------------
       6 5 4 [3 2 1]

### Discussion

This function works as a kind of "context switch".  It's used in the
definition of [infra].

### Crosslinks

[infra]

