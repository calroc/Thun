------------------------------------------------------------------------

## range

Function

Expect a number `n` on the stack and replace it with a list:
`[(n-1)...0]`.

### Example

         5 range
    -----------------
       [4 3 2 1 0]

       -5 range
    --------------
          []

### Definition

> \[0 \<=\] \[1 - [dup]\] [anamorphism]

### Discussion

If `n` is less than 1 the resulting list is empty.

### Crosslinks

[range_to_zero]

