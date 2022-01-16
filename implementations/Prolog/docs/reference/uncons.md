--------------------

## uncons

(Basis Function)

Removes an item from a list and leaves it on the stack under the rest of
the list.  You cannot `uncons` an item from an empty list.

       [A ...] uncons
    --------------------
          A [...]

### Source

    func(uncons, Si, So) :- func(cons, So, Si).

### Discussion

This is the inverse of `cons`.

### Crosslinks

[cons](#cons)

