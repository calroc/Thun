--------------------

## binary

(Combinator)

Run a quoted program using exactly two stack values and leave the first
item of the result on the stack.

       ... y x [P] binary
    -----------------------
            ... A

### Definition

    unary popd

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly two items from the stack.

### Crosslinks

[nullary](#nullary)
[ternary](#ternary)
[unary](#unary)

