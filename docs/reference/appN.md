------------------------------------------------------------------------

## appN

Combinator

Like [app1] with any number of items.

       ... xN ... x2 x1 x0 [Q] n . appN
    --------------------------------------
       ... [xN ...] [Q] . infra first
                       ...
           [x2 ...] [Q]   infra first
           [x1 ...] [Q]   infra first
           [x0 ...] [Q]   infra first

### Definition

> \[[grabN]\] [codi] [map] [disenstacken]

### Discussion

This function takes a quoted function `Q` and an integer and runs the
function that many times on that many stack items.  See also [app2].

### Crosslinks

[app1](#app1)
[app2](#app2)
[app3](#app3)
[unary](#unary)

