--------------------

## ternary

Combinator

Run a quoted program using exactly three stack values and leave the first
item of the result on the stack.

       ... z y x [P] ternary
    -------------------------
             ... a

### Definition

> [binary] [popd]

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly three items from the stack.

### Crosslinks

[binary](#binary)
[nullary](#nullary)
[unary](#unary)

