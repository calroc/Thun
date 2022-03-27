------------------------------------------------------------------------

## make_generator

Function

Given an initial state value and a quoted generator function build a
generator quote.

       state [generator function] make_generator
    -----------------------------------------------
         [state [generator function] codireco]

### Example

       230 [dup ++] make_generator
    ---------------------------------
         [230 [dup ++] codireco]

And then:

       [230 [dup ++] codireco] 5 [x] times pop
    ---------------------------------------------
                 230 231 232 233 234

### Definition

> \[[codireco]\] [ccons]

### Discussion

See the ["Using `x` to Generate Values" notebook](https://joypy.osdn.io/notebooks/Generator_Programs.html#an-interesting-variation).

### Crosslinks

[codireco]

