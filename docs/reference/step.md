------------------------------------------------------------------------

## step

Basis Function Combinator

Run a quoted program on each item in a sequence. :

    ... [] [Q] . step
    -----------------------
       ... .


    ... [a] [Q] . step
    ------------------------
      ... a . Q


    ... [a b c] [Q] . step
    ----------------------------------------
          ... a . Q [b c] [Q] step

The step combinator executes the quotation on each member of the list on
top of the stack.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.
