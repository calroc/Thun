------------------------------------------------------------------------

## step

Combinator

Run a quoted program on each item in a sequence.

       ... [] [Q] step
    ---------------------
             ...


       ... [a] [Q] step
    ----------------------
          ... a Q


       ... [a b c] [Q] . step
    ----------------------------------------
                 ... a . Q [b c] [Q] step

### Discussion

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).

### Crosslinks

[step_zero]

