------------------------------------------------------------------------

## pow

Basis Function

Take two numbers `a` and `b` from the stack and raise `a` to the `n`th
power.  (`b` is on the top of the stack.)

       a n pow
    -------------
        (aⁿ)

### Example

       2 [2 3 4 5 6 7 8 9] [pow] map
    -----------------------------------
        2 [4 8 16 32 64 128 256 512]

