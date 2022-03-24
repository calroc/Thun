------------------------------------------------------------------------

## anamorphism

Combinator

Build a list of values from a generator program `G` and a stopping
predicate `P`.

               [P] [G] anamorphism
    -----------------------------------------
       [P] [pop []] [G] [dip swons] genrec

### Definition

> \[[pop](#pop) \[\]\] [swap](#swap) \[[dip](#dip) [swons](#swons)\] [genrec](#genrec)

### Example

The `range` function generates a list of the integers from 0 to n - 1:

> \[0 <=\] \[\-\- dup\] anamorphism

### Discussion

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).

