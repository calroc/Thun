--------------------

## app1

"apply one"

(Combinator)

Given a quoted program on TOS and anything as the second stack item run
the program without disturbing the stack and replace the two args with
the first result of the program.

             ... x [Q] app1
    ---------------------------------
       ... [x ...] [Q] infra first

### Definition

    nullary popd

### Discussion

Just a specialization of `nullary` really.  Its parallelizable cousins
are more useful.


