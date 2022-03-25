------------------------------------------------------------------------

## grba

Function

A weird function used in [app2] that does this:

          ... 1 2 3 4 5 grba
    -------------------------------
       ... 1 2 3 [4 3 2 1 ...] 5

It grabs the stack under the top item, and substitutes it for the second item down on the stack.

### Definition

> \[[stack] [popd]\] [dip]

### Discussion

This function "grabs" an item from the stack along with a copy of the stack.
It's part of the [app2] definition.

### Crosslinks

[app2]
