--------------------

## x

Combinator

Take a quoted function `F` and run it with itself as the first item on
the stack.

       [F] x
    -----------
       [F] F

### Definition

    dup i

### Discussion

The simplest recursive pattern.

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).
as well as
[Recursion Theory and Joy](https://www.kevinalbrecht.com/code/joy-mirror/j05cmp.html) by Manfred von


