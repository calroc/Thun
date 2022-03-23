
# Functor Reference

Version -10.0.0

Each function, combinator, or definition should be documented here.

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


--------------------

## b

(Combinator)

Run two quoted programs

       [P] [Q] b
    ---------------
          P Q

### Definition

    [i] dip i

### Derivation

    [P] [Q] b
    [P] [Q] [i] dip i
    [P] i [Q] i
     P    [Q] i
     P     Q

### Discussion

This combinator comes in handy.

### Crosslinks

[dupdip](#dupdip)
[ii](#ii)

--------------------

## binary

(Combinator)

Run a quoted program using exactly two stack values and leave the first
item of the result on the stack.

       ... y x [P] binary
    -----------------------
            ... A

### Definition

    unary popd

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly two items from the stack.

### Crosslinks

[nullary](#nullary)
[ternary](#ternary)
[unary](#unary)

--------------------

## ccons

(Function)

Given two items and a list, append the items to the list to make a new list.

       B A [...] ccons
    ---------------------
          [B A ...]

### Definition

    cons cons

### Discussion

Does `cons` twice.

### Crosslinks

[cons](#cons)

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

--------------------

## infra

(Combinator)

Accept a quoted program and a list on the stack and run the program with
the list as its stack.  Does not affect the stack (below the list.)

       ... [a b c] [Q] infra
    ---------------------------
        c b a Q [...] swaack

### Definition

    swons swaack [i] dip swaack


### Discussion

This is one of the more useful combinators.  It allows a quoted
expression to serve as a stack for a program, effectively running it in a
kind of "pocket universe".  If the list represents a datastructure then
`infra` lets you work on its internal structure.

### Crosslinks

[swaack](#swaack)

--------------------

## !-

"not negative"

(Function, Boolean Predicate)

Integer on top of stack is replaced by Boolean value indicating whether
it is non-negative.

        N !-
    -----------  N < 0
       false

       N !-
    ----------  N >= 0
       true


### Definition

    0 >=

--------------------

## nullary

(Combinator)

Run a quoted program without using any stack values and leave the first item of the result on the stack.

       ... [P] nullary
    ---------------------
            ... A

### Definition

    [stack] dip infra first

### Derivation

    ... [P] nullary
    ... [P] [stack] dip infra first
    ... stack [P] infra first
    ... [...] [P] infra first
    ... [A ...] first
    ...  A

### Discussion

A very useful function that runs any other quoted function and returns
it's first result without disturbing the stack (under the quoted
program.)

### Crosslinks

[unary](#unary)
[binary](#binary)
[ternary](#ternary)

--------------------

## ternary

(Combinator)


Run a quoted program using exactly three stack values and leave the first
item of the result on the stack.

       ... z y x [P] unary
    -------------------------
             ... A

### Definition

    binary popd

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly three items from the stack.

### Crosslinks

[binary](#binary)
[nullary](#nullary)
[unary](#unary)

--------------------

## unary

(Combinator)

Run a quoted program using exactly one stack value and leave the first item of the result on the stack.

       ... x [P] unary
    ---------------------
           ... A

### Definition

    nullary popd

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly one item from the stack.

### Crosslinks

[binary](#binary)
[nullary](#nullary)
[ternary](#ternary)

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

--------------------

## x

(Combinator)

       [F] x
    -----------
       [F] F

### Definition

    dup i

### Discussion

The `x` combinator ...

