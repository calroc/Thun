------------------------------------------------------------------------

## \|\|

Combinator

Short-circuiting Boolean OR


### Definition

> [nulco](#nulco) \[[nullary](#nullary)\] [dip](#dip) \[true\] [branch](#branch)

### Discussion

Accept two quoted programs, run the first and expect a Boolean value, if
it’s `false` pop it and run the second program (which should also return a
Boolean value) otherwise pop the second program (leaving `true` on the
stack.)

       [A] [B] ||
    ---------------- A -> false
            B


       [A] [B] ||
    ---------------- A -> true
          true

### Crosslinks

[&&](#section-1)
