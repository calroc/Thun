
Version -10.0.0

Each function, combinator, or definition should be documented here.


--------------

## &

See [and](#and).


------------------------------------------------------------------------

## &&

Combinator

Short-circuiting Boolean AND

Accept two quoted programs, run the first and expect a Boolean value, if
it's `true` pop it and run the second program (which should also return a
Boolean value) otherwise pop the second program (leaving `false` on the
stack.)


       [A] [B] &&
    ---------------- true
            B


       [A] [B] &&
    ---------------- false
         false


### Definition

    nulco [nullary [false]] dip branch

### Derivation

TODO: this is derived in one of the notebooks I think, look it up and
link to it, or copy the content here.

### Discussion

This is seldom useful, I suspect, but this way you have it.

### Crosslinks

[||](#section-25)


--------------

## *

See [mul](#mul).


--------------

## •

See [id](#id).


--------------

## ^

See [xor](#xor).


--------------

## =

See [eq](#eq).


--------------

## !=

See [ne](#ne).


------------------------------------------------------------------------

## !-

Function

Not negative.


        n !-
    ----------- n < 0
       false


       n !-
    ---------- n >= 0
       true


### Definition

    0 \>=

### Discussion

Return a Boolean value indicating if a number is greater than or equal to
zero.


--------------

## >

See [gt](#gt).


--------------

## >=

See [ge](#ge).


--------------

## >>

See [rshift](#rshift).


--------------

## -

See [sub](#sub).


--------------

## --

See [pred](#pred).


--------------

## <

See [lt](#lt).


--------------

## <=

See [le](#le).


--------------

## <>

See [ne](#ne).


------------------------------------------------------------------------

## \<\{\}

Function


       ... a <{}
    ----------------
       ... [] a


### Definition

    [] swap

### Discussion

Tuck an empty list just under the first item on the stack.

### Crosslinks

[<<{}](#section-18)


--------------

## <<

See [lshift](#lshift).


------------------------------------------------------------------------

## \<\<\{\}

Function


       ... b a <{}
    -----------------
       ... [] b a


### Definition

    [] rollup


### Discussion

Tuck an empty list just under the first two items on the stack.

### Crosslinks

[<{}](#section-16)


--------------

## %

See [mod](#mod).


--------------

## +

See [add](#add).


--------------

## ++

See [succ](#succ).


------------------------------------------------------------------------

## ?

Function

Is the item on the top of the stack "truthy"?

### Definition

> [dup](#dup) [bool](#bool)

### Discussion

You often want to test the truth value of an item on the stack without
consuming the item.

### Crosslinks

[bool](#bool)


--------------

## /

See [floordiv](#floordiv).


--------------

## //

See [floordiv](#floordiv).


--------------

## /floor

See [floordiv](#floordiv).


------------------------------------------------------------------------

## \|\|

Combinator

Short-circuiting Boolean OR


### Definition

> [nulco](#nulco) \[[nullary](#nullary)\] [dip](#dip) \[true\] [branch](#branch)

### Discussion

Accept two quoted programs, run the first and expect a Boolean value, if
it’s `false` pop it and run the second program (which should also return a
Boolean value) otherwise pop the second program (leaving `true` on the
stack.)

       [A] [B] ||
    ---------------- A -> false
            B


       [A] [B] ||
    ---------------- A -> true
          true

### Crosslinks

[&&](#section-1)

------------------------------------------------------------------------

## abs

Function

Return the absolute value of the argument.

### Definition

> [dup](#dup) 0 < [] \[[neg](#neg)\] [branch](#branch)


------------------------------------------------------------------------

## add

Basis Function

Add two numbers together: a + b.


------------------------------------------------------------------------

## anamorphism

Combinator

Build a list of values from a generator program `G` and a stopping
predicate `P`.

               [P] [G] anamorphism
    -----------------------------------------
       [P] [pop []] [G] [dip swons] genrec

### Definition

> \[[pop](#pop) \[\]\] [swap](#swap) \[[dip](#dip) [swons](#swons)\] [genrec](#genrec)

### Example

The `range` function generates a list of the integers from 0 to n - 1:

> \[0 <=\] \[\-\- dup\] anamorphism

### Discussion

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).


------------------------------------------------------------------------

## and

Basis Function

Logical bit-wise AND.

### Crosslinks

[or](#or)
[xor](#xor)


--------------------

## app1

"apply one"

Combinator

Given a quoted program on TOS and anything as the second stack item run
the program without disturbing the stack and replace the two args with
the first result of the program.

             ... x [Q] app1
    ---------------------------------
       ... [x ...] [Q] infra first

This is the same effect as the [unary](#unary) combinator.

### Definition

> [nullary](#nullary) [popd](#popd)

### Discussion

Just a specialization of `nullary` really.  Its parallelizable cousins
are more useful.

### Crosslinks

[app2](#app2)
[app3](#app3)
[appN](#appN)
[unary](#unary)


------------------------------------------------------------------------

## app2

Combinator

Like [app1](#app1) with two items.

       ... y x [Q] . app2
    -----------------------------------
       ... [y ...] [Q] . infra first
           [x ...] [Q]   infra first

### Definition

> \[[grba] [swap] [grba] [swap]\] [dip] \[[infrst]\] [cons] [ii]

### Discussion

Unlike [app1](#app1), which is essentially an alias for [unary](#unary),
this function is not the same as [binary](#binary).  Instead of running
one program using exactly two items from the stack and pushing one
result (as [binary](#binary) does) this function takes two items from the
stack and runs the program twice, separately for each of the items, then
puts both results onto the stack.

This is not currently implemented as parallel processes but it can (and
should) be done.

### Crosslinks

[app1](#app1)
[app3](#app3)
[appN](#appN)
[unary](#unary)


------------------------------------------------------------------------

## app3

Combinator

Like [app1] with three items.

         ... z y x [Q] . app3
    -----------------------------------
       ... [z ...] [Q] . infra first
           [y ...] [Q]   infra first
           [x ...] [Q]   infra first

### Definition

> 3 [appN]

### Discussion

See [app2].

### Crosslinks

[app1](#app1)
[app2](#app2)
[appN](#appN)
[unary](#unary)


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


--------------

## at

See [getitem](#getitem).


------------------------------------------------------------------------

## average

Function

Compute the average of a list of numbers.
(Currently broken until I can figure out what to do about "numeric tower"
in Thun.)

### Definition

> \[[sum]\] \[[size]\] [cleave] [/]

### Discussion

Theoretically this function would compute the sum and the size in two
separate threads, then divide.  This works but a compiled version would
probably do better to sum and count the list once, in one thread, eh?

As an exercise in Functional Programming in Joy it would be fun to
convert this into a catamorphism.
See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).


--------------------

## b

Combinator

Run two quoted programs

       [P] [Q] b
    ---------------
          P Q

### Definition

> \[[i]\] [dip] [i]

### Discussion

This combinator may seem trivial but it comes in handy.

### Crosslinks

[dupdip](#dupdip)
[ii](#ii)


--------------------

## binary

Combinator

Run a quoted program using exactly two stack values and leave the first
item of the result on the stack.

       ... y x [P] binary
    -----------------------
            ... a

### Definition

> [unary] [popd]

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly two items from the stack.

### Crosslinks

[nullary](#nullary)
[ternary](#ternary)
[unary](#unary)


------------------------------------------------------------------------

## bool

Basis Function

Convert the item on the top of the stack to a Boolean value.

### Discussion

For integers 0 is `false` and any other number is `true`; for lists the
empty list is `false` and all other lists are `true`.

### Crosslinks

[not]


------------------------------------------------------------------------

## branch

Basis Combinator

Use a Boolean value to select and run one of two quoted programs.


       false [F] [T] branch
    --------------------------
              F

       true [F] [T] branch
    -------------------------
                 T


### Definition

> [rolldown] [choice] [i]

### Discussion

This is one of the fundamental operations (although it can be defined in
terms of [choice] as above).  The more common "if..then..else" construct
[ifte] adds a predicate function that is evaluated [nullary].

### Crosslinks

[choice]
[ifte]
[select]


------------------------------------------------------------------------

## ccccons

Function

       a b c d [...] ccccons
    ---------------------------
           [a b c d ...]

Do [cons] four times.

### Definition

> [ccons] [ccons]

### Crosslinks

[ccons] [cons] [times]


--------------------

## ccons

Function

       a b [...] ccons
    ---------------------
          [a b ...]

Do [cons] two times.

### Definition

> [cons] [cons]

### Crosslinks

[cons]
[ccons]


------------------------------------------------------------------------

## choice

Basis Function

Use a Boolean value to select one of two items.

       a b false choice
    ----------------------
              a

       a b true choice
    ---------------------
              b

### Definition

> \[[pop]\] \[[popd]\] [branch]

### Discussion

It's a matter of taste whether you implement this in terms of [branch] or
the other way around.

### Crosslinks

[branch]
[select]


------------------------------------------------------------------------

## clear

Basis Function

Clear everything from the stack.

### Definition

> [stack] [bool] \[[pop] [stack] [bool]\] [loop]

### Crosslinks

[stack]
[swaack]


------------------------------------------------------------------------

## cleave

Combinator

Run two programs in parallel, consuming one additional item, and put their
results on the stack.

       ... x [A] [B] cleave
    ------------------------
            ... a b

### Derivation

> [fork] [popdd]

### Example

       1 2 3 [+] [-] cleave
    --------------------------
             1 2 5 -1

### Discussion

One of a handful of useful parallel combinators.

### Crosslinks

[clop]
[fork]
[map]


------------------------------------------------------------------------

## clop

Combinator

Run two programs in parallel, consuming two additional items, and put their results on the stack.

       ... x y [A] [B] clop
    --------------------------
            ... a b

### Definition

> [cleave] [popdd]

### Discussion

Like [cleave] but consumes an additional item from the stack.

       1 2 3 4 [+] [-] clop
    --------------------------
             1 2 7 -1

### Crosslinks

[cleave]
[fork]
[map]


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


------------------------------------------------------------------------

## codi

Combinator

Take a quoted program from the stack, [cons] the next item onto it, then
[dip] the whole thing under what was the third item on the stack.
 
       a b [F] . codi
    --------------------
             b . F a

### Definition

> [cons] [dip]

### Discussion

This is one of those weirdly specific functions that turns out to be
useful in a few places.

### Crosslinks

[appN]
[codireco]


------------------------------------------------------------------------

## codireco

Basis Function Combinator

codi reco

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## concat

Basis Function Combinator

Concatinate the two lists on the top of the stack. :

    [a b c] [d e f] concat
    ----------------------------
        [a b c d e f]

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## cond

Basis Function Combinator

This combinator works like a case statement. It expects a single quote
on the stack that must contain zero or more condition quotes and a
default quote. Each condition clause should contain a quoted predicate
followed by the function expression to run if that predicate returns
true. If no predicates return true the default function runs.

It works by rewriting into a chain of nested [ifte]{.title-ref}
expressions, e.g.:

    [[[B0] T0] [[B1] T1] [D]] cond
    -----------------------------------------
    [B0] [T0] [[B1] [T1] [D] ifte] ifte

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

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


------------------------------------------------------------------------

## dinfrirst

Basis Function Combinator

dip infrst

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## dip

Basis Function Combinator

The dip combinator expects a quoted program on the stack and below it
some item, it hoists the item into the expression and runs the program
on the rest of the stack. :

    ... x [Q] dip
    -------------------
      ... Q x

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## dipd

Basis Function Combinator

Like dip but expects two items. :

    ... y x [Q] dip
    ---------------------
      ... Q y x

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## dipdd

Basis Function Combinator

Like dip but expects three items. :

    ... z y x [Q] dip
    -----------------------
      ... Q z y x

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## disenstacken

Basis Function Combinator

The disenstacken operator expects a list on top of the stack and makes
that the stack discarding the rest of the stack.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

--------------

## div

See [floordiv](#floordiv).


------------------------------------------------------------------------

## divmod

Basis Function Combinator

divmod(x, y) -\> (quotient, remainder)

Return the tuple (x//y, x%y). Invariant: q \* y + r == x.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## down_to_zero

Basis Function Combinator

\[0 \>\] \[dup \--\] while

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## drop

Basis Function Combinator

    drop == [rest] times

Expects an integer and a quote on the stack and returns the quote with n
items removed off the top. :

    [a b c d] 2 drop
    ----------------------
        [c d]

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## dup

Basis Function Combinator

    (a1 -- a1 a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## dupd

Basis Function Combinator

    (a2 a1 -- a2 a2 a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## dupdd

Basis Function Combinator

    (a3 a2 a1 -- a3 a3 a2 a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## dupdip

Basis Function Combinator

    [F] dupdip == dup [F] dip

    ... a [F] dupdip
    ... a dup [F] dip
    ... a a   [F] dip
    ... a F a

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## dupdipd

Basis Function Combinator

dup dipd

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## enstacken

Basis Function Combinator

stack \[clear\] dip

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## eq

Basis Function Combinator

Same as a == b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## first

Basis Function Combinator

    ([a1 ...1] -- a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## first_two

Basis Function Combinator

    ([a1 a2 ...1] -- a1 a2)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## flatten

Basis Function Combinator

\<{} \[concat\] step

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## floor

Basis Function Combinator

Return the floor of x as an Integral.

This is the largest integer \<= x.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## floordiv

Basis Function Combinator

Same as a // b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## fork

Basis Function Combinator

\[i\] app2

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## fourth

Basis Function Combinator

    ([a1 a2 a3 a4 ...1] -- a4)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## gcd

Basis Function Combinator

true \[tuck mod dup 0 \>\] loop pop

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## gcd2

Basis Function Combinator

Compiled GCD function.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## ge

Basis Function Combinator

Same as a \>= b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## genrec

Basis Function Combinator

General Recursion Combinator. :

    [if] [then] [rec1] [rec2] genrec
    ---------------------------------------------------------------------
    [if] [then] [rec1 [[if] [then] [rec1] [rec2] genrec] rec2] ifte

From \"Recursion Theory and Joy\" (j05cmp.html) by Manfred von Thun:
\"The genrec combinator takes four program parameters in addition to
whatever data parameters it needs. Fourth from the top is an if-part,
followed by a then-part. If the if-part yields true, then the then-part
is executed and the combinator terminates. The other two parameters are
the rec1-part and the rec2-part. If the if-part yields false, the
rec1-part is executed. Following that the four program parameters and
the combinator are again pushed onto the stack bundled up in a quoted
form. Then the rec2-part is executed, where it will find the bundled
form. Typically it will then execute the bundled form, either with i or
with app2, or some other combinator.\"

The way to design one of these is to fix your base case \[then\] and the
test \[if\], and then treat rec1 and rec2 as an else-part
\"sandwiching\" a quotation of the whole function.

For example, given a (general recursive) function \'F\': :

    F == [I] [T] [R1] [R2] genrec

If the \[I\] if-part fails you must derive R1 and R2 from: :

    ... R1 [F] R2

Just set the stack arguments in front, and figure out what R1 and R2
have to do to apply the quoted \[F\] in the proper way. In effect, the
genrec combinator turns into an ifte combinator with a quoted copy of
the original definition in the else-part: :

    F == [I] [T] [R1]   [R2] genrec
      == [I] [T] [R1 [F] R2] ifte

Primitive recursive functions are those where R2 == i. :

    P == [I] [T] [R] tailrec
      == [I] [T] [R [P] i] ifte
      == [I] [T] [R P] ifte

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## getitem

Basis Function Combinator

    getitem == drop first

Expects an integer and a quote on the stack and returns the item at the
nth position in the quote counting from 0. :

    [a b c d] 0 getitem
    -------------------------
     a

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## grabN

Basis Function Combinator

\<{} \[cons\] times

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## grba

Basis Function Combinator

\[stack popd\] dip

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## gt

Basis Function Combinator

Same as a \> b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## help

Basis Function Combinator

Accepts a quoted symbol on the top of the stack and prints its docs.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## hypot

Basis Function Combinator

\[sqr\] ii + sqrt

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

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


------------------------------------------------------------------------

## id

Basis Function Combinator

The identity function.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## ifte

Basis Function Combinator

If-Then-Else Combinator :

    ... [if] [then] [else] ifte
    ---------------------------------------------------
    ... [[else] [then]] [...] [if] infra select i




    ... [if] [then] [else] ifte
    -------------------------------------------------------
    ... [else] [then] [...] [if] infra first choice i

Has the effect of grabbing a copy of the stack on which to run the
if-part using infra.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## ii

Basis Function Combinator

    ... a [Q] ii
    ------------------
     ... Q a Q

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

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


------------------------------------------------------------------------

## infrst

Basis Function Combinator

infra first

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## inscribe

Basis Function Combinator

Create a new Joy function definition in the Joy dictionary. A definition
is given as a quote with a name followed by a Joy expression. for
example:

> \[sqr dup mul\] inscribe

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## le

Basis Function Combinator

Same as a \<= b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## loop

Basis Function Combinator

Basic loop combinator. :

    ... True [Q] loop
    -----------------------
       ... Q [Q] loop

    ... False [Q] loop
    ------------------------
       ...

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## lshift

Basis Function Combinator

Same as a \<\< b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## lt

Basis Function Combinator

Same as a \< b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## make_generator

Basis Function Combinator

\[codireco\] ccons

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## map

Basis Function Combinator

Run the quoted program on TOS on the items in the list under it, push a
new list with the results in place of the program and original list.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## max

Basis Function Combinator

Given a list find the maximum.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## min

Basis Function Combinator

Given a list find the minimum.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## mod

Basis Function Combinator

Same as a % b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

--------------

## modulus

See [mod](#mod).


------------------------------------------------------------------------

## mul

Basis Function Combinator

Same as a \* b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## ne

Basis Function Combinator

Same as a != b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## neg

Basis Function Combinator

Same as -a.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## not

Basis Function Combinator

Same as not a.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

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


------------------------------------------------------------------------

## nulco

Basis Function Combinator

\[nullary\] cons

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

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


------------------------------------------------------------------------

## of

Basis Function Combinator

swap at

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## or

Basis Function Combinator

Same as a \| b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## over

Basis Function Combinator

    (a2 a1 -- a2 a1 a2)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## pam

Basis Function Combinator

\[i\] map

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

--------------

## pick

See [getitem](#getitem).


------------------------------------------------------------------------

## pm

Basis Function Combinator

Plus or minus :

    a b pm
    -------------
    a+b a-b

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## pop

Basis Function Combinator

    (a1 --)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## popd

Basis Function Combinator

    (a2 a1 -- a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## popdd

Basis Function Combinator

    (a3 a2 a1 -- a2 a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## popop

Basis Function Combinator

    (a2 a1 --)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## popopd

Basis Function Combinator

    (a3 a2 a1 -- a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## popopdd

Basis Function Combinator

    (a4 a3 a2 a1 -- a2 a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## popopop

Basis Function Combinator

pop popop

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## pow

Basis Function Combinator

Same as a \*\* b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## pred

Basis Function Combinator

Decrement TOS.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## primrec

Basis Function Combinator

From the \"Overview of the language JOY\":

\> The primrec combinator expects two quoted programs in addition to a
data parameter. For an integer data parameter it works like this: If the
data parameter is zero, then the first quotation has to produce the
value to be returned. If the data parameter is positive then the second
has to combine the data parameter with the result of applying the
function to its predecessor.:

    5  [1]  [*]  primrec

\> Then primrec tests whether the top element on the stack (initially
the 5) is equal to zero. If it is, it pops it off and executes one of
the quotations, the \[1\] which leaves 1 on the stack as the result.
Otherwise it pushes a decremented copy of the top element and recurses.
On the way back from the recursion it uses the other quotation, \[\*\],
to multiply what is now a factorial on top of the stack by the second
element on the stack.:

    n [Base] [Recur] primrec

       0 [Base] [Recur] primrec
    ------------------------------
          Base

         n [Base] [Recur] primrec
    ------------------------------------------ n > 0
       n (n-1) [Base] [Recur] primrec Recur

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## product

Basis Function Combinator

1 swap \[\*\] step

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## quoted

Basis Function Combinator

\[unit\] dip

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## range

Basis Function Combinator

\[0 \<=\] \[1 - dup\] anamorphism

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## range_to_zero

Basis Function Combinator

unit \[down_to_zero\] infra

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## reco

Basis Function Combinator

rest cons

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

--------------

## rem

See [mod](#mod).


--------------

## remainder

See [mod](#mod).


------------------------------------------------------------------------

## remove

Basis Function Combinator

Expects an item on the stack and a quote under it and removes that item
from the the quote. The item is only removed once. If the list is empty
or the item isn\'t in the list then the list is unchanged. :

    [1 2 3 1] 1 remove
    ------------------------
      [2 3 1]

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## rest

Basis Function Combinator

    ([a1 ...0] -- [...0])

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## reverse

Basis Function Combinator

Reverse the list on the top of the stack. :

    reverse == [] swap shunt

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## rolldown

Basis Function Combinator

    (a1 a2 a3 -- a2 a3 a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## rollup

Basis Function Combinator

    (a1 a2 a3 -- a3 a1 a2)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

--------------

## roll>

See [rollup](#rollup).


--------------

## roll<

See [rolldown](#rolldown).


------------------------------------------------------------------------

## round

Basis Function Combinator

Round a number to a given precision in decimal digits.

The return value is an integer if ndigits is omitted or None. Otherwise
the return value has the same type as the number. ndigits may be
negative.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## rrest

Basis Function Combinator

    ([a1 a2 ...1] -- [...1])

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## rshift

Basis Function Combinator

Same as a \>\> b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## run

Basis Function Combinator

\<{} infra

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## second

Basis Function Combinator

    ([a1 a2 ...1] -- a2)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## select

Basis Function Combinator

Use a Boolean value to select one of two items from a sequence. :

    [A B] false select
    ------------------------
     A


    [A B] true select
    -----------------------
       B

The sequence can contain more than two items but not fewer. Currently
Python semantics are used to evaluate the \"truthiness\" of the Boolean
value (so empty string, zero, etc. are counted as false, etc.)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## sharing

Basis Function Combinator

Print redistribution information.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## shift

Basis Function Combinator

uncons \[swons\] dip

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## shunt

Basis Function Combinator

Like concat but reverses the top list into the second. :

    shunt == [swons] step == reverse swap concat

       [a b c] [d e f] shunt
    ---------------------------
           [f e d a b c] 

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## size

Basis Function Combinator

\[pop ++\] step_zero

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## sort

Basis Function Combinator

Given a list return it sorted.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## spiral_next

Basis Function Combinator

\[\[\[abs\] ii \<=\] \[\[\<\>\] \[pop !-\] \|\|\] &&\] \[\[!-\]
\[\[++\]\] \[\[\--\]\] ifte dip\] \[\[pop !-\] \[\--\] \[++\] ifte\]
ifte

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## split_at

Basis Function Combinator

\[drop\] \[take\] clop

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## split_list

Basis Function Combinator

\[take reverse\] \[drop\] clop

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## sqr

Basis Function Combinator

dup \*

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## sqrt

Basis Function Combinator

Return the square root of the number a. Negative numbers return complex
roots.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## stack

Basis Function Combinator

    (... -- ... [...])

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## stackd

Basis Function Combinator

\[stack\] dip

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## step

Basis Function Combinator

Run a quoted program on each item in a sequence. :

    ... [] [Q] . step
    -----------------------
       ... .


    ... [a] [Q] . step
    ------------------------
      ... a . Q


    ... [a b c] [Q] . step
    ----------------------------------------
          ... a . Q [b c] [Q] step

The step combinator executes the quotation on each member of the list on
top of the stack.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## step_zero

Basis Function Combinator

0 roll> step

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## stuncons

Basis Function Combinator

    (... a1 -- ... a1 a1 [...])

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## stununcons

Basis Function Combinator

    (... a2 a1 -- ... a2 a1 a1 a2 [...])

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## sub

Basis Function Combinator

Same as a - b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## succ

Basis Function Combinator

Increment TOS.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## sum

Basis Function Combinator

Given a quoted sequence of numbers return the sum. :

    sum == 0 swap [+] step

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## swaack

Basis Function Combinator

    ([...1] -- [...0])

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## swap

Basis Function Combinator

    (a1 a2 -- a2 a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## swapd

Basis Function Combinator

\[swap\] dip

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## swoncat

Basis Function Combinator

swap concat

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## swons

Basis Function Combinator

    ([...1] a1 -- [a1 ...1])

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## tailrec

Basis Function Combinator

\[i\] genrec

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## take

Basis Function Combinator

Expects an integer and a quote on the stack and returns the quote with
just the top n items in reverse order (because that\'s easier and you
can use reverse if needed.) :

    [a b c d] 2 take
    ----------------------
        [b a]

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

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


------------------------------------------------------------------------

## third

Basis Function Combinator

    ([a1 a2 a3 ...1] -- a3)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## times

Basis Function Combinator

times == \[\-- dip\] cons \[swap\] infra \[0 \>\] swap while pop :

    ... n [Q] . times
    ---------------------  w/ n <= 0
      ... .


    ... 1 [Q] . times
    -----------------------
      ... . Q


    ... n [Q] . times
    -------------------------------------  w/ n > 1
      ... . Q (n - 1) [Q] times

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

--------------

## truthy

See [bool](#bool).


------------------------------------------------------------------------

## tuck

Basis Function Combinator

    (a2 a1 -- a1 a2 a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

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


------------------------------------------------------------------------

## unique

Basis Function Combinator

Given a list remove duplicate items.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## unit

Basis Function Combinator

    (a1 -- [a1 ])

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## unquoted

Basis Function Combinator

\[i\] dip

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## unswons

Basis Function Combinator

    ([a1 ...1] -- [...1] a1)

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## void

Basis Function Combinator

True if the form on TOS is void otherwise False.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## warranty

Basis Function Combinator

Print warranty information.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## while

Basis Function Combinator

swap nulco dupdipd concat loop

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## words

Basis Function Combinator

Print all the words in alphabetical order.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

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


------------------------------------------------------------------------

## xor

Basis Function Combinator

Same as a \^ b.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.

------------------------------------------------------------------------

## zip

Basis Function Combinator

Replace the two lists on the top of the stack with a list of the pairs
from each list. The smallest list sets the length of the result list.

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.
