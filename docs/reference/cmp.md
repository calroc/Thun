------------------------------------------------------------------------

## cmp

Combinator

Take two values and three quoted programs on the stack and run one
of the three depending on the results of comparing the two values.

       a b [G] [E] [L] cmp
    ------------------------- a > b
            G

       a b [G] [E] [L] cmp
    ------------------------- a = b
                E

       a b [G] [E] [L] cmp
    ------------------------- a < b
                    L
### Discussion

This is useful sometimes, and you can [dup] or [dupd] with two quoted
programs to handle the cases when you just want to deal with [<=] or [>=]
and not all three possibilities, e.g.:

    [G] [LE] dup cmp

    [GE] [L] dupd cmp

Or even:

    [GL] [E] over cmp

### Crosslinks

TODO: link to tree notebooks where this was used.

