--------------------

## i

(Basis Combinator)

Append a quoted expression onto the pending expression.


       [Q] i
    -----------
        Q

### Source

    combo(i, [list(P)|S], S, Ei, Eo) :- append(P, Ei, Eo).

### Discussion

This is probably the fundamental combinator.  You wind up using it in all
kinds of places (for example, the `x` combinator can be defined as `dup i`.)

