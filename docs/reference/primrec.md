------------------------------------------------------------------------

# primrec

Basis Function Combinator

From the \"Overview of the language JOY\":

\> The primrec combinator expects two quoted programs in addition to a
data parameter. For an integer data parameter it works like this: If the
data parameter is zero, then the first quotation has to produce the
value to be returned. If the data parameter is positive then the second
has to combine the data parameter with the result of applying the
function to its predecessor.:

    5  [1]  [*]  primrec

\> Then primrec tests whether the top element on the stack (initially
the 5) is equal to zero. If it is, it pops it off and executes one of
the quotations, the \[1\] which leaves 1 on the stack as the result.
Otherwise it pushes a decremented copy of the top element and recurses.
On the way back from the recursion it uses the other quotation, \[\*\],
to multiply what is now a factorial on top of the stack by the second
element on the stack.:

    n [Base] [Recur] primrec

       0 [Base] [Recur] primrec
    ------------------------------
          Base

         n [Base] [Recur] primrec
    ------------------------------------------ n > 0
       n (n-1) [Base] [Recur] primrec Recur

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
