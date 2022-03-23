--------------------

## nullary

(Combinator)

Run a quoted program without using any stack values and leave the first item of the result on the stack.

       ... [P] nullary
    ---------------------
            ... A

### Definition

    [stack] dip infra first

### Derivation

    ... [P] nullary
    ... [P] [stack] dip infra first
    ... stack [P] infra first
    ... [...] [P] infra first
    ... [A ...] first
    ...  A

### Discussion

A very useful function that runs any other quoted function and returns
it's first result without disturbing the stack (under the quoted
program.)

### Crosslinks

[unary](#unary)
[binary](#binary)
[ternary](#ternary)

