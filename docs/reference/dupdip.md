------------------------------------------------------------------------

## dupdip

Combinator

Apply a function `F` and [dup] the item under it on the stack.

       a [F] dupdip
    ------------------
          a F a

### Definition

> [dupd] [dip]

### Derivation

    a [F] dupdip
    a [F] dupd dip
    a [F] [dup] dip dip
    a dup [F] dip
    a a [F] dip
    a F a

### Discussion

A very common and useful combinator.

### Crosslinks

[dupdipd]

