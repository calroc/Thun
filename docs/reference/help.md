------------------------------------------------------------------------

## help

Function

Accepts a quoted symbol on the top of the stack and prints its
documentation.

       [foo] help
    ----------------

### Discussion

Technically this is equivalent to `pop`, but it will only work if the
item on the top of the stack is a quoted symbol.

