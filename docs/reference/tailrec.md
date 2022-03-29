------------------------------------------------------------------------

## tailrec

Combinator

A specialization of the [genrec] combinator.

### Definition

> \[[i]\] [genrec]

### Discussion

Some recursive functions do not need to store additional data or pending
actions per-call.  These are called ["tail recursive" functions](https://en.wikipedia.org/wiki/Tail_recursive).  In Joy,
they appear as [genrec] definitions that have [i] for the second half of
their recursive branch.

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).

### Crosslinks

[genrec]

