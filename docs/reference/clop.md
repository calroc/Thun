------------------------------------------------------------------------

## clop

Combinator

Run two programs in parallel, consuming two additional items, and put their results on the stack.

       ... x y [A] [B] clop
    --------------------------
            ... a b

### Definition

> [cleave] [popdd]

### Discussion

Like [cleave] but consumes an additional item from the stack.

       1 2 3 4 [+] [-] clop
    --------------------------
             1 2 7 -1

### Crosslinks

[cleave]
[fork]
[map]

