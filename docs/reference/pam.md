------------------------------------------------------------------------

## pam

Combinator

Take a list of quoted functions from the stack and replace it with a list
of the [first] results from running those functions (on copies of the
rest of the stack.)

### Example

       5 7 [[+][-][*][/][%]] pam
    -------------------------------
          5 7 [12 -2 35 0 5]

### Definition

> \[[i]\] [map]

### Discussion

A specialization of [map] that runs a list of functions in parallel (if
the underlying [map] function is so implemented, of course.)

### Crosslinks

[map]

