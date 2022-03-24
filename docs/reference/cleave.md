------------------------------------------------------------------------

## cleave

Combinator

Run two programs in parallel, consuming one additional item, and put their
results on the stack.

       ... x [A] [B] cleave
    ------------------------
            ... a b

### Derivation

> [fork] [popdd]

### Example

       1 2 3 [+] [-] cleave
    --------------------------
             1 2 5 -1

### Discussion

One of a handful of useful parallel combinators.

### Crosslinks

[clop]
[fork]
[map]

