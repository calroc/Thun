
[Home](/)

Version -10.0.0

Each function, combinator, or definition should be documented here.


------------------------------------------------------------------------

## /\

Binary Boolean *and*.

### Crosslinks

[bool]
[not]
[\/]

------------------------------------------------------------------------

## \/

Binary Boolean *or*.

### Crosslinks

[bool]
[not]
[/\]

------------------------------------------------------------------------

## abs

Take an integer from the stack and replace it with its absolute value.

------------------------------------------------------------------------

## add

Take two integers from the stack and replace them with their sum.

------------------------------------------------------------------------

## and

Combinator

Short-circuiting Boolean AND

Accept two quoted programs, run the first and expect a Boolean value, if
it's `true` pop it and run the second program (which should also return a
Boolean value) otherwise pop the second program (leaving `false` on the
stack.)  The quoted programs are run with [nullary].


       [A] [B] and
    ----------------- A -> true
            B


       [A] [B] and
    ----------------- A -> false
         false

TODO: this is derived in one of the notebooks I think, look it up and
link to it, or copy the content here.

### Crosslinks

[or](#or)

------------------------------------------------------------------------

## anamorphism

Combinator

Build a list of values from a generator program `G` and a stopping
predicate `P`.

               [P] [G] anamorphism
    -----------------------------------------
       [P] [pop []] [G] [dip swons] genrec

### Example

The `range` function generates a list of the integers from 0 to n - 1:

    [0 <=] [-- dup] anamorphism

> joy? 5 
> 
> 5
> 
> joy? [0 <=] [-- dup]
> 
> 5 [0 <=] [-- dup]
> 
> joy? anamorphism
> 
> [4 3 2 1 0]

Note that the last value generated (0) is at the bottom of the list.
See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).


------------------------------------------------------------------------

## app1

"apply one"

Combinator

Given a quoted program on TOS and anything as the second stack item run
the program without disturbing the rest of the stack and replace the two args with
the first result of the program.

             ... x [Q] app1
    ---------------------------------
       ... [x ...] [Q] infra first

This is the same effect as the [unary](#unary) combinator.

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

### Discussion

This function takes a quoted function `Q` and an integer and runs the
function that many times on that many stack items.  See also [app2].

### Crosslinks

[app1](#app1)
[app2](#app2)
[app3](#app3)
[unary](#unary)


--------------

## *

See [mul](#mul).


--------------

## at

See [getitem](#getitem).


------------------------------------------------------------------------

## average

Compute the average of a list of numbers.
(Currently broken until I can figure out what to do about "numeric tower"
in Thun.)

### Discussion

Theoretically this function would compute the sum and the size in two
separate threads, then divide.  This works but a compiled version would
probably do better to sum and count the list once, in one thread, eh?

As an exercise in Functional Programming in Joy it would be fun to
convert this into a catamorphism.
See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).


--------------------

## binary

Combinator

Run a quoted program using exactly two stack values and leave the first
item of the result on the stack.

       ... y x [P] binary
    -----------------------
            ... a

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly two items from the stack.

### Crosslinks

[nullary](#nullary)
[ternary](#ternary)
[unary](#unary)


--------------------

## b

Combinator

Run two quoted programs

       [P] [Q] b
    ---------------
          P Q

### Discussion

This combinator may seem trivial but it comes in handy.

### Crosslinks

[dupdip](#dupdip)
[ii](#ii)


------------------------------------------------------------------------

## bool

Convert the item on the top of the stack to a Boolean value.

### Discussion

For integers 0 is `false` and any other number is `true`; for lists the
empty list is `false` and all other lists are `true`.

### Crosslinks

[not]


------------------------------------------------------------------------

## branch

Combinator

Use a Boolean value to select and run one of two quoted programs.


       false [F] [T] branch
    --------------------------
              F

       true [F] [T] branch
    -------------------------
                 T


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

       a b c d [...] ccccons
    ---------------------------
           [a b c d ...]

Do [cons] four times.

### Crosslinks

[ccons]
[cons]
[times]


--------------------

## ccons

       a b [...] ccons
    ---------------------
          [a b ...]

Do [cons] two times.

### Crosslinks

[cons]
[ccons]


------------------------------------------------------------------------

## choice

Use a Boolean value to select one of two items.

       a b false choice
    ----------------------
              a

       a b true choice
    ---------------------
              b

### Discussion

It's a matter of taste whether you implement this in terms of [branch] or
the other way around.

### Crosslinks

[branch]
[select]


------------------------------------------------------------------------

## clear

Clear everything from the stack.

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

    [G] [EL] dup cmp

    [GE] [L] dupd cmp

Or even:

    [GL] [E] over cmp

------------------------------------------------------------------------

## codi

Combinator

Take a quoted program from the stack, [cons] the next item onto it, then
[dip] the whole thing under what was the third item on the stack.
 
       a b [F] . codi
    --------------------
             b . F a

### Discussion

This is one of those weirdly specific functions that turns out to be
useful in a few places.

### Crosslinks

[appN]
[codireco]


------------------------------------------------------------------------

## codireco

Combinator

This is part of the [make_generator] function.  You would not use this
combinator directly.

### Discussion

See [make_generator] and the 
["Using `x` to Generate Values" notebook](https://joypy.osdn.io/notebooks/Generator_Programs.html#an-interesting-variation)
as well as
[Recursion Theory and Joy](https://www.kevinalbrecht.com/code/joy-mirror/j05cmp.html) by Manfred von Thun.

### Crosslinks

[make_generator]


------------------------------------------------------------------------

## concat

Concatinate two lists.

       [a b c] [d e f] concat
    ----------------------------
           [a b c d e f]

### Crosslinks

[first]
[first_two]
[flatten]
[fourth]
[getitem]
[rest]
[reverse]
[rrest]
[second]
[shift]
[shunt]
[size]
[split_at]
[split_list]
[swaack]
[third]
[zip]


------------------------------------------------------------------------

## cond

Combinator

This combinator works like a case statement. It expects a single quote
on the stack that must contain zero or more condition quotes and a
default quote. Each condition quote should contain a quoted predicate
followed by the function expression to run if that predicate returns
`true`. If no predicates return `true` the default function runs.

    [
        [ [Predicate0] Function0 ]
        [ [Predicate1] Function1 ]
        ...
        [ [PredicateN] FunctionN ]
        [Default]
    ]
    cond

### Discussion

It works by rewriting into a chain of nested [ifte]{.title-ref}
expressions, e.g.:

          [[[B0] T0] [[B1] T1] [D]] cond
    -----------------------------------------
       [B0] [T0] [[B1] [T1] [D] ifte] ifte


### Crosslinks

[ifte]


--------------------

## cons

Given an item and a list, append the item to the list to make a new list.

       a [...] cons
    ------------------
         [a ...]

### Discussion

Cons is a [venerable old function from Lisp](https://en.wikipedia.org/wiki/Cons#Lists).
Its inverse operation is [uncons].

### Crosslinks

[uncons]


------------------------------------------------------------------------

## dinfrirst

Combinator

Specialist function (that means I forgot what it does and why.)

------------------------------------------------------------------------

## dipddd

Combinator

Like [dip] but expects four items. :

       ... z y x w [Q] . dipddd
    -------------------------------
                 ... . Q z y x w

### Discussion

See [dip].

### Crosslinks

[dip]
[dipd]
[dipdd]
[dupdip]
[dupdipd]
[infra]

------------------------------------------------------------------------

## dipdd

Combinator

Like [dip] but expects three items. :

       ... z y x [Q] . dipdd
    -----------------------------
                 ... . Q z y x
### Discussion

See [dip].

### Crosslinks

[dip]
[dipd]
[dipddd]
[dupdip]
[dupdipd]
[infra]

------------------------------------------------------------------------

## dipd

Combinator

Like [dip] but expects two items.

       ... y x [Q] . dipd
    -------------------------
               ... . Q y x

### Discussion

See [dip].

### Crosslinks

[dip]
[dipdd]
[dipddd]
[dupdip]
[dupdipd]
[infra]

------------------------------------------------------------------------

## dip

Combinator

The `dip` combinator expects a quoted program on the stack and below it
some item, it hoists the item into the expression and runs the program
on the rest of the stack. 

       ... x [Q] . dip
    ---------------------
             ... . Q x

### Discussion

This along with [infra] are enough to update any datastructure.
See the ["Traversing Datastructures with Zippers" notebook](https://joypy.osdn.io/notebooks/Zipper.html).

Note that the item that was on the top of the stack (`x` in the example above)
will not be treated specially by the interpreter when it is reached
again.  This is something of a footgun.  My advice is to avoid putting
bare unquoted symbols onto the stack, but then you can't use symbols as
"atoms" and also use `dip` and `infra` to operate on compound
datastructures with atoms in them.  This is a kind of side-effect of the
Continuation-Passing Style.  The `dip` combinator could "set aside" the
item and replace it after running `Q` but that means that there is an
"extra space" where the item resides while `Q` runs.  One of the nice
things about CPS is that the whole state is recorded in the stack and
pending expression (not counting modifications to the dictionary.)

### Crosslinks

[dipd]
[dipdd]
[dupdip]
[dupdipd]
[infra]


------------------------------------------------------------------------

## disenstacken

The `disenstacken` function expects a list on top of the stack and makes
that the stack discarding the rest of the stack.

       1 2 3 [4 5 6] disenstacken
    --------------------------------
                6 5 4

### Discussion

Note that the order of the list is not changed, it just looks that way
because the stack is printed with the top on the right while lists are
printed with the top or head on the left.

### Crosslinks

[enstacken]
[stack]
[unstack]

--------------------------------------------

## div

Divide.


------------------------------------------------------------------------

## divmod

        x y divmod
    ------------------
         q      r
       (x/y)  (x%y)

Invariant: `qy + r = x`.


------------------------------------------------------------------------

## down_to_zero

Given a number greater than zero put all the Natural numbers (including
zero) less than that onto the stack.

### Example

       3 down_to_zero
    --------------------
          3 2 1 0

### Crosslinks

[range]


------------------------------------------------------------------------

## drop

Expects an integer and a quote on the stack and returns the quote with n
items removed off the top.

### Example

       [a b c d] 2 drop
    ----------------------
           [c d]

### Crosslinks

[take]


------------------------------------------------------------------------

## dupdd

[dup] the third item down on the stack.

       a b c dupdd
    -----------------
         a a b c

### Crosslinks

[dup]
[dupd]
[dupdip]
[dupdipd]


------------------------------------------------------------------------

## dupdipd

Combinator

Run a copy of program `F` under the next item down on the stack.

       a [F] dupdipd
    -------------------
          F a [F]

### Crosslinks

[dupdip]


------------------------------------------------------------------------

## dupdip

Combinator

Apply a function `F` and [dup] the item under it on the stack.

       a [F] dupdip
    ------------------
          a F a

### Derivation

    a [F] dupdip
    a [F] dupd dip
    a [F] [dup] dip dip
    a dup [F] dip
    a a [F] dip
    a F a

### Discussion

A very common and useful combinator.

### Crosslinks

[dupdipd]


------------------------------------------------------------------------

## dupd

[dup] the second item down on the stack.

       a b dupd
    --------------
        a a b

### Crosslinks

[dup]
[dupdd]
[dupdip]
[dupdipd]


------------------------------------------------------------------------

## dup

"Dup"licate the top item on the stack.

       a dup
    -----------
        a a

### Crosslinks

[dupd]
[dupdd]
[dupdip]
[dupdipd]


------------------------------------------------------------------------

## enstacken

Put the stack onto the stack replacing the contents of the stack.

       ... a b c enstacken
    -------------------------
           [c b a ...]


### Discussion

This is a destructive version of [stack].  See the note under
[disenstacken] about the apparent but illusory reversal of the stack.

### Crosslinks

[stack]
[disenstacken]
[unstack]


------------------------------------------------------------------------

## eq

Compare the two items on the top of the stack for equality and replace
them with a Boolean value.

       a b eq
    -------------
       Boolean
       (a = b)

### Crosslinks

[cmp]
[ge]
[gt]
[le]
[lt]
[neq]


--------------

## =

See [eq](#eq).


--------------

## !=

See [neq](#neq).


------------------------------------------------------------------------

## !-

Not negative.


        n !-
    ----------- n < 0
       false


       n !-
    ---------- n >= 0
       true


### Discussion

Return a Boolean value indicating if a number is greater than or equal to
zero.


------------------------------------------------------------------------

## first

Replace a list with its first item.

       [a ...]
    --------------
          a

### Crosslinks

[second]
[third]
[fourth]
[rest]


------------------------------------------------------------------------

## first_two

Replace a list with its first two items.

       [a b ...] first_two
    -------------------------
               a b

### Crosslinks

[first]
[second]
[third]
[fourth]
[rest]


------------------------------------------------------------------------

## flatten

Given a list of lists, concatinate them.

### Example

       [[1 2] [3 [4] 5] [6 7]] flatten
    -------------------------------------
              [1 2 3 [4] 5 6 7]

### Discussion

Note that only one "level" of lists is flattened.  In the example above
`[4]` is not unquoted.

### Crosslinks

[concat]
[first]
[first_two]
[fourth]
[getitem]
[rest]
[reverse]
[rrest]
[second]
[shift]
[shunt]
[size]
[split_at]
[split_list]
[swaack]
[third]
[zip]


------------------------------------------------------------------------

## fork

Combinator

Run two quoted programs in parallel and replace them with their results.

       ... [F] [G] fork
    ----------------------
           ... f g

### Discussion

The basic parallelism combinator, the two programs are run independently.

### Crosslinks

[cleave]
[clop]
[map]


------------------------------------------------------------------------

## fourth

Replace a list with its fourth item.

       [a b c d ...] fourth
    --------------------------
              d

### Crosslinks

[first]
[second]
[third]
[rest]


------------------------------------------------------------------------

## gcd

Take two integers from the stack and replace them with their Greatest
Common Denominator.

### Discussion

Euclid's Algorithm


------------------------------------------------------------------------

## ge

Greater-than-or-equal-to comparison of two numbers.

       a b ge
    --------------
       Boolean
       (a >= b)

### Crosslinks

[cmp]
[eq]
[gt]
[le]
[lt]
[neq]


------------------------------------------------------------------------

## genrec

Combinator

**Gen**eral **Rec**ursion Combinator. 

                          [if] [then] [rec1] [rec2] genrec
    ---------------------------------------------------------------------
       [if] [then] [rec1 [[if] [then] [rec1] [rec2] genrec] rec2] ifte

### Discussion

Note that this definition includes the `genrec` symbol itself, it is
self-referential.  This is possible because the definition machinery does
not check that symbols in defs are in the dictionary.  `genrec` is the
only self-referential definition.

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).

From ["Recursion Theory and Joy"](https://www.kevinalbrecht.com/code/joy-mirror/j05cmp.html)
by Manfred von Thun:

> "The genrec combinator takes four program parameters in addition to
> whatever data parameters it needs. Fourth from the top is an if-part,
> followed by a then-part. If the if-part yields true, then the then-part
> is executed and the combinator terminates. The other two parameters are
> the rec1-part and the rec2-part. If the if-part yields false, the
> rec1-part is executed. Following that the four program parameters and
> the combinator are again pushed onto the stack bundled up in a quoted
> form.  Then the rec2-part is executed, where it will find the bundled
> form.  Typically it will then execute the bundled form, either with i
> or with app2, or some other combinator."

The way to design one of these is to fix your base case `[then]` and the
test `[if]`, and then treat `rec1` and `rec2` as an else-part
"sandwiching" a quotation of the whole function.

For example, given a (general recursive) function `F`:

    F == [I] [T] [R1] [R2] genrec

If the `[I]` if-part fails you must derive `R1` and `R2` from: :

    ... R1 [F] R2

Just set the stack arguments in front, and figure out what `R1` and `R2`
have to do to apply the quoted `[F]` in the proper way. In effect, the
`genrec` combinator turns into an [ifte] combinator with a quoted copy of
the original definition in the else-part:

    F == [I] [T] [R1]   [R2] genrec
      == [I] [T] [R1 [F] R2] ifte

Tail recursive functions are those where `R2` is the `i` combinator:

    P == [I] [T] [R] tailrec
      == [I] [T] [R [P] i] ifte
      == [I] [T] [R P] ifte

### Crosslinks

[anamorphism]
[tailrec]
[x]


------------------------------------------------------------------------

## getitem

Expects an integer and a quote on the stack and returns the item at the
nth position in the quote counting from 0.

### Example

       [a b c d] 2 getitem
    -------------------------
            c

### Discussion

If the number isn't a valid index into the quote `getitem` will cause
some sort of problem (the exact nature of which is
implementation-dependant.)

### Crosslinks

[concat]
[first]
[first_two]
[flatten]
[fourth]
[rest]
[reverse]
[rrest]
[second]
[shift]
[shunt]
[size]
[split_at]
[split_list]
[swaack]
[third]
[zip]

------------------------------------------------------------------------

## grabN

Expect a number on the top of the stack and [cons] that many items from under it onto a new list.

### Example

       a b c d e 3 grabN
    -----------------------
          a b [c d e]

------------------------------------------------------------------------

## grba

A weird function used in [app2] that does this:

          ... 1 2 3 4 5 grba
    -------------------------------
       ... 1 2 3 [4 3 2 1 ...] 5

It grabs the stack under the top item, and substitutes it for the second item down on the stack.

### Discussion

This function "grabs" an item from the stack along with a copy of the stack.
It's part of the [app2] definition.

### Crosslinks

[app2]

--------------

## >=

See [ge](#ge).


--------------

## >>

See [rshift](#rshift).


--------------

## >

See [gt](#gt).


------------------------------------------------------------------------

## gt

Greater-than comparison of two numbers.

       a b gt
    --------------
       Boolean
       (a > b)

### Crosslinks

[cmp]
[eq]
[ge]
[le]
[lt]
[neq]

------------------------------------------------------------------------

## --

See [pred](#pred).


--------------

## -

See [sub](#sub).


------------------------------------------------------------------------

## ifte

Combinator

If-Then-Else combinator, a common and convenient specialization of [branch].

            [if] [then] [else] ifte
    ---------------------------------------
       [if] nullary [else] [then] branch

### Crosslinks

[branch]
[loop]
[while]

------------------------------------------------------------------------

## ii

Combinator

Take a quoted program from the stack and run it twice, first under the
top item, then again with the top item.

    ... a [Q] ii
    ------------------
     ... Q a Q

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

--------------------

## i

Combinator

Append a quoted expression onto the pending expression.


       [Q] . i
    -------------
           . Q

### Discussion

This is a fundamental combinator.  It is used in all kinds of places.  For
example, the [x] combinator can be defined as `dup i`.


--------------------

## infra

Combinator

Accept a quoted program and a list on the stack and run the program with
the list as its stack.  Does not affect the stack (below the list.)

       ... x y z [a b c] [Q] infra
    ---------------------------------
        c b a Q [z y x ...] swaack


    ... [a b c] [F] swons swaack [i] dip swaack
    ... [[F] a b c]       swaack [i] dip swaack

    c b a [F]   [...] [i] dip swaack
    c b a [F] i [...]         swaack
    c b a  F    [...]         swaack
    d e         [...]         swaack
    ... [e d]


### Discussion

This is one of the more useful combinators.  It allows a quoted
expression to serve as a stack for a program, effectively running it in a
kind of "pocket universe".  If the list represents a datastructure then
`infra` lets you work on its internal structure.

### Crosslinks

[swaack](#swaack)


------------------------------------------------------------------------

## infrst

Combinator

Does [infra] and then extracts the [first] item from the resulting list.

------------------------------------------------------------------------

## inscribe

Create a new Joy function definition in the Joy dictionary. A definition
is given as a quote with a name followed by a Joy expression.

### Example

    [sqr dup mul] inscribe

### Discussion

This is the only function that modifies the dictionary.  It's provided as a 
convenience, for tinkering with new definitions before entering them into
the `defs.txt` file.  It can be abused, which you should avoid unless you
know what you're doing.

------------------------------------------------------------------------

## le

Less-Than-or-Equal-to comparison of the two items on the top of the
stack, replacing them with a Boolean value.

       a b le
    -------------
       Boolean
       (a <= b)

### Crosslinks

[cmp]
[eq]
[ge]
[gt]
[lt]
[neq]

--------------

## <=

See [le](#le).


--------------

## <>

See [neq](#neq).


------------------------------------------------------------------------

## <{}


       ... a <{}
    ----------------
       ... [] a


### Discussion

Tuck an empty list just under the first item on the stack.

### Crosslinks

[<<{}](#section-18)


------------------------------------------------------------------------

## <<{}


       ... b a <{}
    -----------------
       ... [] b a


### Discussion

Tuck an empty list just under the first two items on the stack.

### Crosslinks

[<{}](#section-16)


--------------

## <<

See [lshift](#lshift).


--------------

## <

See [lt](#lt).


------------------------------------------------------------------------

## loop

Combinator

Expect a quoted program `Q` and a Boolean value on the stack.  If the value is false
discard the quoted program, otherwise run a copy of `Q` and `loop` again.

       false [Q] loop
    --------------------


       true [Q] . loop
    --------------------------
                . Q [Q] loop

### Discussion

This, along with [branch] and [fork], is one of the four main combinators
of all programming.  The fourth, sequence, is implied by juxtaposition.
That is to say, in Joy `F G` is like `G(F(...))` in a language bassed on
function application.  Or again, to quote the [Joy Wikipedia
entry](https://en.wikipedia.org/wiki/Joy_(programming_language)#Mathematical_purity),

> In Joy, the meaning function is a homomorphism from the syntactic monoid onto the semantic monoid. That is, the syntactic relation of concatenation of symbols maps directly onto the semantic relation of composition of functions.

Anyway, [branch], [fork], amd [loop] are the fundamental combinators in Joy.
Just as [branch] has it's more common and convenient form [ifte],
[loop] has [while].

### Crosslinks

[branch]
[fork]
[while]

------------------------------------------------------------------------

## lshift

[Logical Left-Shift](https://en.wikipedia.org/wiki/Logical_shift)

       a n lshift
    ----------------
         (a×2ⁿ)

### Crosslinks

[rshift]

------------------------------------------------------------------------

## lt

Less-Than comparison of the two items on the top of the
stack, replacing them with a Boolean value.

       a b lt
    -------------
       Boolean
       (a < b)

### Crosslinks

[cmp]
[eq]
[ge]
[gt]
[le]
[neq]

------------------------------------------------------------------------

## make_generator

Given an initial state value and a quoted generator function build a
generator quote.

       state [generator function] make_generator
    -----------------------------------------------
         [state [generator function] codireco]

### Example

       230 [dup ++] make_generator
    ---------------------------------
         [230 [dup ++] codireco]

And then:

       [230 [dup ++] codireco] 5 [x] times pop
    ---------------------------------------------
                 230 231 232 233 234

### Discussion

See the ["Using `x` to Generate Values" notebook](https://joypy.osdn.io/notebooks/Generator_Programs.html#an-interesting-variation).

### Crosslinks

[codireco]


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


------------------------------------------------------------------------

## max

Given a list find the maximum.

### Example

       [1 2 3 4] max
    -------------------
             4

### Crosslinks

[min]
[size]
[sum]


------------------------------------------------------------------------

## min

Given a list find the minimum.

### Example

       [1 2 3 4] min
    -------------------
             1 

### Crosslinks

[max]
[size]
[sum]


------------------------------------------------------------------------

## mod

Return the remainder of `a` divided by `b`.

       a b mod
    -------------
        (a%b)

### Crosslinks

[divmod]
[mul]


--------------

## modulus

See [mod](#mod).


------------------------------------------------------------------------

## mul

Multiply two numbers.

       a b mul
    -------------
        (a×b)

### Crosslinks

[div]
[product]


------------------------------------------------------------------------

## neg

Invert the sign of a number.

       a neg
    -----------
        -a

------------------------------------------------------------------------

## neq

Not-Equal comparison of the two items on the top of the
stack, replacing them with a Boolean value.

       a b neq
    -------------
       Boolean
       (a = b)

### Crosslinks

[cmp]
[eq]
[ge]
[gt]
[le]
[lt]

------------------------------------------------------------------------

## not

Invert the Boolean value on the top of the stack.

       true not
    --------------
        false

       false not
    ---------------
         true
 
### Crosslinks

[bool]

------------------------------------------------------------------------

## nulco

Take the item on the top of the stack and [cons] it onto `[nullary]`.

         [F] nulco
    -------------------
       [[F] nullary]

### Discussion

Helper function for [or] and [and].

### Crosslinks

[and]
[or]


--------------------

## null

True if the item on the top of the stack is an empty list,
false if it's a list but not empty,
and an error if it's not a list.

--------------------

## nullary

Combinator

Run a quoted program without using any stack values and leave the first
item of the result on the stack.

       ... [P] nullary
    ---------------------
            ... a

### Example

    ... [P] nullary
    ... [P] [stack] dip infra first
    ... stack [P] infra first
    ... [...] [P] infra first
    ... [a ...] first
    ...  a

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

Like [getitem] but [swap]s the order of arguments.

### Example

       2 [a b c d] of
    --------------------
             c

### Crosslinks

[getitem]

------------------------------------------------------------------------

## over

[dup] the second item on the stack `over` the first.

       a b over
    --------------
        a b a

### Definition

There are many many ways to define this function.

> [swap] [tuck]

> \[[pop]\] [nullary]

> \[[dup]\] [dip] [swap]

> [unit] [dupdip]

> [unit] [dupdipd] [first]

And so on...

### Discussion

A fine old word from Forth.

### Crosslinks

[tuck]


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

### Discussion

A specialization of [map] that runs a list of functions in parallel (if
the underlying [map] function is so implemented, of course.)

### Crosslinks

[map]


--------------

## %

See [mod](#mod).


--------------

## pick

See [getitem](#getitem).


--------------

## +

See [add](#add).


--------------

## ++

See [succ](#succ).


------------------------------------------------------------------------

## pm

Plus or minus.  Replace two numbers with their sum and difference.

          a b pm
    -----------------
       (a+b) (a-b)

------------------------------------------------------------------------

## popdd

[pop] the third item on the stack.

       a b c popdd
    -----------------
           b c

### Crosslinks

[pop]
[popd]
[popop]
[popopd]
[popopdd]
[popopop]


------------------------------------------------------------------------

## popd

[pop] the second item down on the stack.

       a b popd
    --------------
          b

### Crosslinks

[pop]
[popdd]
[popop]
[popopd]
[popopdd]
[popopop]


------------------------------------------------------------------------

## pop

Pop the top item from the stack and discard it.

       a pop
    -----------

### Crosslinks

[popd]
[popdd]
[popop]
[popopd]
[popopdd]
[popopop]


------------------------------------------------------------------------

## popopdd

       a b c d popopdd
    ---------------------
            c d

### Crosslinks

[pop]
[popd]
[popdd]
[popop]
[popopd]
[popopop]


------------------------------------------------------------------------

## popopd

[pop] the second and third items from the stack.

       a b c popopd
    ------------------
            c

### Crosslinks

[pop]
[popd]
[popdd]
[popop]
[popopdd]
[popopop]


------------------------------------------------------------------------

## popop

[pop] two items from the stack.

       a b popop
    ---------------

### Crosslinks

[pop]
[popd]
[popdd]
[popopd]
[popopdd]
[popopop]


------------------------------------------------------------------------

## popopop

[pop] three items from the stack.

       a b c popopop
    -------------------

### Crosslinks

[pop]
[popd]
[popdd]
[popop]
[popopd]
[popopdd]


------------------------------------------------------------------------

## pow

Take two numbers `a` and `n` from the stack and raise `a` to the `n`th
power.  (`n` is on the top of the stack.)

       a n pow
    -------------
        (aⁿ)

### Example

       2 [2 3 4 5 6 7 8 9] [pow] map
    -----------------------------------
        2 [4 8 16 32 64 128 256 512]


------------------------------------------------------------------------

## pred

Predecessor. Decrement TOS.

### Crosslinks

[succ]


------------------------------------------------------------------------

## primrec

Combinator

From the ["Overview of the language JOY"](https://www.kevinalbrecht.com/code/joy-mirror/j00ovr.html)

> The primrec combinator expects two quoted programs in addition to a
> data parameter. For an integer data parameter it works like this: If
> the data parameter is zero, then the first quotation has to produce the
> value to be returned. If the data parameter is positive then the second
> has to combine the data parameter with the result of applying the
> function to its predecessor.

> 5  \[1\]  \[\*\]  primrec

> Then primrec tests whether the top element on the stack (initially the
> 5) is equal to zero. If it is, it pops it off and executes one of the
> quotations, the \[1\] which leaves 1 on the stack as the result.
> Otherwise it pushes a decremented copy of the top element and recurses.
> On the way back from the recursion it uses the other quotation, \[\*\],
> to multiply what is now a factorial on top of the stack by the second
> element on the stack.


       0 [Base] [Recur] primrec
    ------------------------------
          Base

             n [Base] [Recur] primrec
    ------------------------------------------ n > 0
       n (n-1) [Base] [Recur] primrec Recur

### Discussion

Simple and useful specialization of the [genrec] combinator from the
[original Joy system](https://www.kevinalbrecht.com/code/joy-mirror/index.html).

### Crosslinks

[genrec]
[tailrec]


------------------------------------------------------------------------

## product

Just as [sum] sums a list of numbers, this function multiplies them
together.

### Definition

> 1 [swap] \[[mul]\] [step]

Or,

> \[1\] \[[mul]\] [primrec]



------------------------------------------------------------------------

## ?

Is the item on the top of the stack "truthy"?

### Discussion

You often want to test the truth value of an item on the stack without
consuming the item.

### Crosslinks

[bool](#bool)


------------------------------------------------------------------------

## quoted

"Quote D" Wrap the second item on the stack in a list.

       a b quoted
    ----------------
         [a] b

### Discussion

This comes from the original Joy stuff.

### Crosslinks

[quote-two]
[unit]


------------------------------------------------------------------------

## range

Expect a number `n` on the stack and replace it with a list:
`[(n-1)...0]`.

### Example

         5 range
    -----------------
       [4 3 2 1 0]

       -5 range
    --------------
          []

### Discussion

If `n` is less than 1 the resulting list is empty.

### Crosslinks

[range_to_zero]


------------------------------------------------------------------------

## range_to_zero

Take a number `n` from the stack and replace it with a list
`[0...n]`.

### Example

       5 range_to_zero
    ---------------------
        [0 1 2 3 4 5]

### Discussion

Note that the order is reversed compared to [range].

### Crosslinks

[down_to_zero]
[range]


------------------------------------------------------------------------

## reco

Replace the first item in a list with the item under it.

       a [b ...] reco
    --------------------
         [a ...]

### Crosslinks

[codireco]
[make_generator]


--------------

## remainder

See [mod](#mod).


--------------

## rem

See [mod](#mod).


------------------------------------------------------------------------

## rest

       [a ...] rest
    ------------------
          [...]

### Crosslinks

[first]
[uncons]


------------------------------------------------------------------------

## reverse

Reverse the list on the top of the stack.

### Example

       [1 2 3] reverse
    ---------------------
           [3 2 1]


------------------------------------------------------------------------

## rolldown

       a b c rolldown
    --------------------
           b c a

### Crosslinks

[rollup]


--------------

## roll>

See [rollup](#rollup).


--------------

## roll<

See [rolldown](#rolldown).


------------------------------------------------------------------------

## rollup

       a b c rollup
    ------------------
          c a b

### Crosslinks

[rolldown]


------------------------------------------------------------------------

## rrest

       [a b ...] rrest
    ---------------------
            [...]

### Crosslinks

[rest]


------------------------------------------------------------------------

## rshift

[Logical Right-Shift](https://en.wikipedia.org/wiki/Logical_shift)

       a n rshift
    ----------------
         (a∕2ⁿ)

### Crosslinks

[lshift]

------------------------------------------------------------------------

## run

Run a quoted program in a list.

### Example

       [1 2 +] run
    -----------------
           [3]

------------------------------------------------------------------------

## second

       [a b ...] second
    ----------------------
              b

### Crosslinks

[first]
[third]
[fourth]


------------------------------------------------------------------------

## select

Use a Boolean value to select one of two items from a sequence. :

       [a b] false select
    ------------------------
               a

       [a b] true select
    -----------------------
               b

### Discussion

The sequence can contain more than two items but not fewer.

### Crosslinks

[choice]


------------------------------------------------------------------------

## sharing

Print redistribution information.

### Discussion

Mathematically this is a form of [id], but it has the side-effect of
printing out the GPL notice.

### Crosslinks

[warranty]


------------------------------------------------------------------------

## shift

Move the top item from one list to another.

### Example

       [x y z] [a b c] shift
    ---------------------------
          [a x y z] [b c]

### Crosslinks

[shunt]


------------------------------------------------------------------------

## shunt

Like [concat] but [reverse] the top list into the second.

### Example

       [a b c] [d e f] shunt
    ---------------------------
           [f e d a b c] 

### Discussion

This is more efficient than [concat] so prefer it if you don't need to
preserve order.

### Crosslinks

[concat]
[reverse]
[shift]


------------------------------------------------------------------------

## size

Replace a list with its size.

### Example

       [23 [cats] 4] size
    ------------------------
               3


--------------

## /

See [div](#div).


------------------------------------------------------------------------

## small

Return `true` if the item on the top of the stack is a list with zero or one item in it,
`false` if it is a list with more than one item in it,
and an error if it is not a list.

### Crosslinks

[null]

------------------------------------------------------------------------

## spiral_next

Example code.

### Discussion

See the ["Square Spiral Example Joy Code" notebook](https://joypy.osdn.io/notebooks/Square_Spiral.html).


------------------------------------------------------------------------

## split_at

Split a list (second on the stack) at the position given by the number on
the top of the stack.

### Example

       [1 2 3 4 5 6 7] 4 split_at
    --------------------------------
           [5 6 7] [4 3 2 1]

### Discussion

Take a list and a number `n` from the stack, take `n` items from the top
of the list and [shunt] them onto a new list that replaces the number `n`
on the top of the stack.

### Crosslinks

[split_list]


------------------------------------------------------------------------

## split_list

Split a list (second on the stack) at the position given by the number on
the top of the stack such that [concat] would reconstruct the original
list.

       [1 2 3 4 5 6 7] 4 split_list
    ----------------------------------
            [1 2 3 4] [5 6 7]

### Discussion

Compare with [split_at].  This function does extra work to ensure that
[concat] would reconstruct the original list.

### Crosslinks

[split_at]


------------------------------------------------------------------------

## sqr

Square the number on the top of the stack.

       n  sqr
    ------------
         n²

------------------------------------------------------------------------

## stackd

Grab the stack under the top item and put it onto the stack.

### Example

       ... 1 2 3 stackd
    ------------------------
      ... 1 2 [2 1 ...] 3

### Crosslinks

[stack]

------------------------------------------------------------------------

## stack

Put the stack onto the stack.

          ... c b a stack
    ---------------------------
       ... c b a [a b c ...]

### Discussion

This function forms a pair with [unstack], and together they form the
complement to the "destructive" pair [enstacken] and [disenstacken].

### Crosslinks

[enstacken]
[disenstacken]
[stackd]
[unstack]


------------------------------------------------------------------------

## step

Combinator

Run a quoted program on each item in a sequence.

       ... [] [Q] step
    ---------------------
             ...


       ... [a] [Q] step
    ----------------------
          ... a Q


       ... [a b c] [Q] . step
    ----------------------------------------
                 ... a . Q [b c] [Q] step

### Discussion

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).

### Crosslinks

[step_zero]


------------------------------------------------------------------------

## step_zero

Combinator

Like [step] but with 0 as the initial value.

       [...] [F] step_zero
    -------------------------
         0 [...] [F] step

### Discussion

[size] and [sum] can both be defined in terms of this specialization of
[step].

### Crosslinks

[step]


------------------------------------------------------------------------

## stuncons

Take the [stack] and [uncons] the top item.

### Example

       1 2 3 stuncons
    --------------------
       1 2 3 3 [2 1]

------------------------------------------------------------------------

## sub

Subtract the number on the top of the stack from the number below it.

       a b sub
    -------------
        (a-b)

### Crosslinks

[add]


------------------------------------------------------------------------

## succ

Successor. Increment TOS.

### Crosslinks

[pred]


------------------------------------------------------------------------

## sum

Combinator

Given a quoted sequence of numbers return the sum.

### Example

       [1 2 3 4 5] sum
    ---------------------
             15

### Crosslinks

[size]


------------------------------------------------------------------------

## swaack

Swap stack.  Take a list from the top of the stack, replace the stack
with the list, and put the old stack onto it.

### Example

       1 2 3 [4 5 6] swaack
    --------------------------
       6 5 4 [3 2 1]

### Discussion

This function works as a kind of "context switch".  It's used in the
definition of [infra].

### Crosslinks

[infra]


------------------------------------------------------------------------

## swapd

Swap the second and third items on the stack.

       a b c swapd
    -----------------
          b a c

### Crosslinks

[over]
[tuck]


------------------------------------------------------------------------

## swap

Swap the top two items on the stack.

       a b swap
    --------------
         b a

### Crosslinks

[swapd]


------------------------------------------------------------------------

## swoncat

[concat] two lists, but [swap] the lists first.

### Crosslinks

[concat]


------------------------------------------------------------------------

## swons

Like [cons] but [swap] the item and list.

       [...] a swons
    -------------------
          [a ...]


------------------------------------------------------------------------

## tailrec

Combinator

A specialization of the [genrec] combinator.

### Discussion

Some recursive functions do not need to store additional data or pending
actions per-call.  These are called ["tail recursive" functions](https://en.wikipedia.org/wiki/Tail_recursive).  In Joy,
they appear as [genrec] definitions that have [i] for the second half of
their recursive branch.

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).

### Crosslinks

[genrec]


------------------------------------------------------------------------

## take

Expects an integer `n` and a list on the stack and replace them with a list
with just the top `n` items in reverse order.

       [a b c d] 2 take
    ----------------------
            [b a]

--------------------

## ternary

Combinator

Run a quoted program using exactly three stack values and leave the first
item of the result on the stack.

       ... z y x [P] ternary
    -------------------------
             ... a

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly three items from the stack.

### Crosslinks

[binary](#binary)
[nullary](#nullary)
[unary](#unary)


------------------------------------------------------------------------

## third

       [a b c ...] third
    -----------------------
               c

### Crosslinks

[first]
[second]
[fourth]
[rest]


------------------------------------------------------------------------

## times

Combinator

Expect a quoted program and an integer `n` on the stack and do the
program `n` times.

       ... n [Q] . times
    -----------------------  w/ n <= 0
             ... .

       ... 1 [Q] . times
    -----------------------
             ... . Q

       ... n [Q] . times
    -------------------------------------  w/ n > 1
             ... . Q (n-1) [Q] times


### Discussion

This works by building a little [while] program and running it:

                     1 3 [++] • [-- dip] cons [swap] infra [0 >] swap while pop                                                                                                                 
            1 3 [++] [-- dip] • cons [swap] infra [0 >] swap while pop                                                                                                                          
            1 3 [[++] -- dip] • [swap] infra [0 >] swap while pop                                                                                                                               
     1 3 [[++] -- dip] [swap] • infra [0 >] swap while pop
                  dip -- [++] • swap [3 1] swaack [0 >] swap while pop                                                                                                                          
                  dip [++] -- • [3 1] swaack [0 >] swap while pop                                                                                                                               
            dip [++] -- [3 1] • swaack [0 >] swap while pop                                                                                                                                     
            1 3 [-- [++] dip] • [0 >] swap while pop                                                                                                                                            
      1 3 [-- [++] dip] [0 >] • swap while pop
      1 3 [0 >] [-- [++] dip] • while pop

This is a common pattern in Joy.  You accept some parameters from the
stack which typically include qouted programs and use them to build
another program which does the actual work.  This is kind of like macros
in Lisp, or preprocessor directives in C.


------------------------------------------------------------------------

## tuck

[dup] the item on the top of the stack under the second item on the
stack.

       a b tuck
    --------------
        b a b

### Crosslinks

[over]


--------------------

## unary

(Combinator)

Run a quoted program using exactly one stack value and leave the first
item of the result on the stack.

       ... x [P] unary
    ---------------------
           ... a

### Discussion

Runs any other quoted function and returns its first result while
consuming exactly one item from the stack.

### Crosslinks

[binary](#binary)
[nullary](#nullary)
[ternary](#ternary)


--------------------

## uncons

Removes an item from a list and leaves it on the stack under the rest of
the list.  You cannot `uncons` an item from an empty list.

       [a ...] uncons
    --------------------
          a [...]

### Discussion

This is the inverse of [cons].

### Crosslinks

[cons]


------------------------------------------------------------------------

## unit

       a unit
    ------------
        [a]

------------------------------------------------------------------------

## unstack

Take a list from the top of the stack and `concat` it to the stack.

    joy? 1 2 3 [4 5 6]
    
    1 2 3 [4 5 6]
    
    joy? unstack
    
    1 2 3 6 5 4

### Crosslinks

[stack]
[disenstacken]
[enstacken]

------------------------------------------------------------------------

## unquoted

Combinator

Unquote (using [i]) the list that is second on the stack.

### Example

       1 2 [3 4] 5 unquoted
    --------------------------
             1 2 3 4 5

### Crosslinks

[unit]


------------------------------------------------------------------------

## unswons

       [a ...] unswons
    ---------------------
           [...] a


------------------------------------------------------------------------

## or

Combinator

Short-circuiting Boolean OR

Accept two quoted programs, run the first and expect a Boolean value, if
it’s `false` pop it and run the second program (which should also return a
Boolean value) otherwise pop the second program (leaving `true` on the
stack.)  The quoted programs are run with [nullary].

       [A] [B] or
    ---------------- A -> false
            B


       [A] [B] or
    ---------------- A -> true
          true

### Crosslinks

[and]

------------------------------------------------------------------------

## warranty

Print warranty information.


------------------------------------------------------------------------

## while

Combinator

A specialization of [loop] that accepts a quoted predicate program `P`
and runs it [nullary].

       [P] [F] while
    ------------------- P -> false

        [P] [F] while
    --------------------- P -> true
       F [P] [F] while

### Crosslinks

[loop]

--------------------

## x

Combinator

Take a quoted function `F` and run it with itself as the first item on
the stack.

       [F] x
    -----------
       [F] F

### Discussion

The simplest recursive pattern.

See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).
as well as
[Recursion Theory and Joy](https://www.kevinalbrecht.com/code/joy-mirror/j05cmp.html) by Manfred von

------------------------------------------------------------------------

## zip

Replace the two lists on the top of the stack with a list of the pairs
from each list. The smallest list sets the length of the result list.

### Example

       [1 2 3] [4 5 6] zip
    -------------------------
       [[1 4] [2 5] [3 6]]



------------------------------------------------------------------------

## empty?

Expects a list on the stack and pushes `true` if it's empty and `false` otherwise.
It doesn't consume the list.

### Crosslinks

[null]

------------------------------------------------------------------------

## max-of-two

Expects two integers on the stack and removes the lesser of them, if they are equal just remove one.


------------------------------------------------------------------------

## min-of-two

Expects two integers on the stack and removes the greater of them, if they are equal just remove one.

------------------------------------------------------------------------

## quote-two

Take two items from the stack and put them into a new list.

    joy? 1 2 3 4
    
    1 2 3 4
    
    joy? quote-two
    
    1 2 [3 4]


### Crosslinks

[quoted]

------------------------------------------------------------------------

## uncons-two

Expect two non-empty lists on the stack and `uncons` the first item from each.

    joy? [1 2] [3 4] uncons-two
    
    1 3 [2] [4]

------------------------------------------------------------------------

## uncons-pair

Expect two non-empty lists on the stack and `uncons` the first item from each and put them in a new list.

    joy? [1 2] [3 4] uncons-pair
    
    [1 3] [2] [4]







