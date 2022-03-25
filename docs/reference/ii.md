------------------------------------------------------------------------

## ii

Combinator

Take a quoted program from the stack and run it twice, first under the
top item, then again with the top item.

    ... a [Q] ii
    ------------------
     ... Q a Q

### Definition

> \[[dip]\] [dupdip] [i]

### Example

It's a little tricky to understand how this works so here's an example trace:

          1 2 3 4 [++] • [dip] dupdip i
    1 2 3 4 [++] [dip] • dupdip i
          1 2 3 4 [++] • dip [++] i
                 1 2 3 • ++ 4 [++] i
                 1 2 4 • 4 [++] i
               1 2 4 4 • [++] i
          1 2 4 4 [++] • i
               1 2 4 4 • ++
               1 2 4 5 •

### Discussion

In some cases (like the example above) this is the same effect as using [app2] but most of the time it's not:

       1 2 3 4 [+] ii
    --------------------
            1 9

       1 2 3 4 [+] app2
    ----------------------
           1 2 5 6

### Crosslinks

[app2]
[b]
