------------------------------------------------------------------------

## map

Combinator

Given a list of items and a quoted program run the program for each item
in the list (with the rest of the stack) and replace the old list and the
program with a list of the results.

### Example

       5 [1 2 3] [++ *] map
    --------------------------
           5 [10 15 20]

### Discussion

This is a common operation in many languages.  In Joy it can be a
parallelism combinator due to the "pure" nature of the language.

### Crosslinks

[app1]
[app2]
[app3]
[appN](#appn)
[fork]

