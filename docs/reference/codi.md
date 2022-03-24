------------------------------------------------------------------------

## codi

Combinator

Take a quoted program from the stack, [cons] the next item onto it, then
[dip] the whole thing under what was the third item on the stack.
 
       a b [F] . codi
    --------------------
             b . F a

### Definition

> [cons] [dip]

### Discussion

This is one of those weirdly specific functions that turns out to be
useful in a few places.

### Crosslinks

[appN]
[codireco]

