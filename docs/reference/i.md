--------------------

## i

Basis Combinator

Append a quoted expression onto the pending expression.


       [Q] . i
    -------------
           . Q

### Discussion

This is a fundamental combinator.  It is used in all kinds of places.  For
example, the [x] combinator can be defined as `dup i`.

