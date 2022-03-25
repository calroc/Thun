------------------------------------------------------------------------

## drop

Function

Expects an integer and a quote on the stack and returns the quote with n
items removed off the top.

### Example

       [a b c d] 2 drop
    ----------------------
           [c d]

### Definition

> \[[rest]\] [times]

### Crosslinks

[take]

