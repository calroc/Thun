--------------------

## unary

(Combinator)

Run a quoted program using exactly one stack value and leave the first
item of the result on the stack.

       ... x [P] unary
    ---------------------
           ... a

### Definition

> [nullary] [popd]

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly one item from the stack.

### Crosslinks

[binary](#binary)
[nullary](#nullary)
[ternary](#ternary)

