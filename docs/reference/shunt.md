------------------------------------------------------------------------

## shunt

Function

Like [concat] but [reverse] the top list into the second.

### Example

       [a b c] [d e f] shunt
    ---------------------------
           [f e d a b c] 

### Definition

> \[[swons]\] [step]

### Discussion

This is more efficient than [concat] so prefer it if you don't need to
preserve order.

### Crosslinks

[concat]
[reverse]
[shift]

