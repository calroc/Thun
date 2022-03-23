--------------------

## cons

(Basis Function)

Given an item and a list, append the item to the list to make a new list.

       A [...] cons
    ------------------
         [A ...]

### Source

    func(cons, [list(A), B|S], [list([B|A])|S]).

### Discussion

Cons is a venerable old function from Lisp.  It doesn't inspect the item
but it will not cons onto a non-list.  It's inverse operation is called
`uncons`.

### Crosslinks

[ccons](#ccons)
[uncons](#uncons)

