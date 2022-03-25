------------------------------------------------------------------------

## getitem

Function

Expects an integer and a quote on the stack and returns the item at the
nth position in the quote counting from 0.

### Example

       [a b c d] 2 getitem
    -------------------------
            c

### Definition

> [drop] [first]

### Discussion

If the number isn't a valid index into the quote `getitem` will cause
some sort of problem (the exact nature of which is
implementation-dependant.)

### Crosslinks

[concat]
[first]
[first_two]
[flatten]
[fourth]
[remove]
[rest]
[reverse]
[rrest]
[second]
[shift]
[shunt]
[size]
[sort]
[split_at]
[split_list]
[swaack]
[third]
[zip]
