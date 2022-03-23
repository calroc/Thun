------------------------------------------------------------------------

## select

Basis Function Combinator

Use a Boolean value to select one of two items from a sequence. :

    [A B] false select
    ------------------------
     A


    [A B] true select
    -----------------------
       B

The sequence can contain more than two items but not fewer. Currently
Python semantics are used to evaluate the \"truthiness\" of the Boolean
value (so empty string, zero, etc. are counted as false, etc.)

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
