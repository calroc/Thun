------------------------------------------------------------------------

## &&

Combinator

Short-circuiting Boolean AND

Accept two quoted programs, run the first and expect a Boolean value, if
it's `true` pop it and run the second program (which should also return a
Boolean value) otherwise pop the second program (leaving `false` on the
stack.)


       [A] [B] &&
    ---------------- true
            B


       [A] [B] &&
    ---------------- false
         false


### Definition

    nulco [nullary [false]] dip branch

### Derivation

TODO: this is derived in one of the notebooks I think, look it up and
link to it, or copy the content here.

### Discussion

This is seldom useful, I suspect, but this way you have it.

### Crosslinks

[||](#section-25)

