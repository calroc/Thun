------------------------------------------------------------------------

# cmp

Basis Function Combinator

cmp takes two values and three quoted programs on the stack and runs one
of the three depending on the results of comparing the two values: :

    a b [G] [E] [L] cmp
    ------------------------- a > b
     G

    a b [G] [E] [L] cmp
    ------------------------- a = b
         E

    a b [G] [E] [L] cmp
    ------------------------- a < b
         L

Gentzen diagram.

## Definition

if not basis.

## Derivation

if not basis.

## Source

if basis

## Discussion

Lorem ipsum.

## Crosslinks

Lorem ipsum.
