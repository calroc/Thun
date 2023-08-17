# BigNums in Joy

Most of the implementations of Thun support 
[BigNums](https://en.wikipedia.org/wiki/BigNum), either built-in or as
libraries, but some host languages and systems do not.  In those cases it
would be well to have a pure-Joy implementation.

We can model bignums as a pair of a Boolean value for the sign and a list
of integers for the digits.  The bool will be the first item on a list
followed by zero or more integer digits, with the Least Significant digit
at the top (closest to the head of the list.)  E.g.:

    [true 1]

Our *base* for the digits will be dictated by the size of the integers
supported by the host system.  Let's imagine we're using 32-bit signed
ints, so our base will be not 10, but 2³¹.  (We're ignoring the sign
bit.)

    joy? 2 31 pow
    2147483648

So our digits are not 0..9, but 0..2147483647

### ≡ `base`

We can `inscribe` a constant function `base` to keep this value handy.

    2147483648
    joy? unit [base] swoncat
    [base 2147483648]
    joy? inscribe

It's a little "wrong" to use the
dictionary to store values like this, however, this is how Forth does it
and if your design is good it works fine.  Just be careful, and wash
your hands afterward.

This also permits a kind of parameterization.  E.g. let's say we wanted
to use base 10 for our digits, maybe during debugging.  All that requires
is to rebind the symbol `base` to 10.

    [base 10] inscribe

## Converting Between Host BigNums and Joy BigNums

We will work with one of the Joy interpreters that has bignums already so
we can convert "native" ints to our Joy bignums and vice versa.  This
will be helpful to check our work.  Later we can deal with converting to
and from strings (which this Joy doesn't have anyway, so it's probably
fine to defer.)

To get the sign bool we can just use `!-` ("not negative") and to get the
list of digits we repeatedly `divmod` the number by our `base`:

### ≡ `moddiv`

We will want the results in the opposite order, so let's define a little
helper function to do that:

    [moddiv divmod swap] inscribe

### ≡ `get-digit`

    [get-digit base moddiv] inscribe

We keep it up until we get to zero.  This suggests a `while` loop:

    [0 >] [get-digit] while

Let's try it:

    joy? 1234567890123456789012345678901234567890
    1234567890123456789012345678901234567890
    
    joy? [0 >] [get-digit] while 
    1312754386 1501085485 57659106 105448366 58 0

We need to `pop` at the end to ditch that zero.

    [0 >] [get-digit] while pop

But we want these numbers in a list.  The naive way using `infra`
generates them in the reverse order of what we would like.

    joy? [1234567890123456789012345678901234567890]
    [1234567890123456789012345678901234567890]
    
    joy? [[0 >] [get-digit] while pop]
    [1234567890123456789012345678901234567890] [[0 >] [get-digit] while pop]
    
    joy? infra
    [58 105448366 57659106 1501085485 1312754386]

We could just reverse the list, but it's more efficient to build the
result list in the order we want. We construct a simple recursive
function.  (TODO: link to the recursion combinators notebook.)

The predicate will check that our number is yet positive:

    [0 <=]

When we find the zero we will discard it and start a list:

    [pop []]

But until we do find the zero, get digits:

    [get-digit]

Once we have found all the digits and ditched the zero and put our
initial empty list on the stack we `cons` up the digits we have found:

    [i cons] genrec

Let's try it:

    joy? 1234567890123456789012345678901234567890
    1234567890123456789012345678901234567890
    
    joy? [0 <=] [pop []] [get-digit] [i cons] genrec
    [1312754386 1501085485 57659106 105448366 58]

Okay.

### Representing Zero

This will return the empty list for zero:

    joy? 0 [0 <=] [pop []] [get-digit] [i cons] genrec
    []

I think this is better than returning `[0]` because that amounts to a
single leading zero.

    [true]   is "0"
    [true 0] is "00"

Eh?

### ≡ `digitalize`

Let's `inscribe` this function under the name `digitalize`:

    [digitalize [0 <=] [pop []] [get-digit] [i cons] genrec] inscribe

Putting it all together we have `!-` for the sign and `abs digitalize`
for the digits, followed by `cons`:

    [!-] [abs digitalize] cleave cons

### ≡ `to-bignum`

    [to-bignum [!-] [abs digitalize] cleave cons] inscribe

### Converting from Joy BigNums to Host BigNums

To convert a bignum into a host integer we need to keep a "power" value
on the stack, setting it up and discarding it at the end, as well as an
accumulator value starting at zero. We will deal with the sign bit later.

    rest 1 0 rolldown

So the problem is to derive:

       1 0 [digits...] [F] step
    ------------------------------
              result

Where `F` is:

              power acc digit F
    ---------------------------------------
       (power*base) (acc + (power*digit)

Now this is an interesting function. The first thing I noticed is that it
has two results that can be computed independently, suggesting a form
like:

    [G] [H] clop popdd

(Then I noticed that `power *` is a sub-function of both `G` and `H`, but
let's not overthink it, eh?)

So for the first result (the next power) we want:

    G == popop base *

And for the result:

    H == rolldown * +

### ≡ `add-digit`

Let's call this `add-digit`:

    [add-digit [popop base *] [rolldown * +] clop popdd] inscribe

Try it out:

    [true 1312754386 1501085485 57659106 105448366 58]
    joy? rest 1 0 rolldown

    1 0 [1312754386 1501085485 57659106 105448366 58]

    joy? [add-digit] step
    45671926166590716193865151022383844364247891968 1234567890123456789012345678901234567890

    joy? popd
    1234567890123456789012345678901234567890

### ≡ `from-bignum′`

    [from-bignum′ rest 1 0 rolldown [add-digit] step popd] inscribe

Try it out:

    joy? 1234567890123456789012345678901234567890 to-bignum
    [true 1312754386 1501085485 57659106 105448366 58]

    joy? from-bignum′
    1234567890123456789012345678901234567890

Not bad.

### What about that sign bit?

Time to deal with that.

Consider a Joy bignum:

    [true 1312754386 1501085485 57659106 105448366 58]

To get the sign bit would just be `first`.

    [true 1312754386 1501085485 57659106 105448366 58]

    joy? [from-bignum′] [first] cleave
    1234567890123456789012345678901234567890 true

Then use the sign flag to negate the int if the bignum was negative:

    [neg] [] branch

### ≡ `from-bignum`

This gives:

    [from-bignum [from-bignum′] [first] cleave [neg] [] branch] inscribe


## Our Source Code So Far

    [base 2147483648] inscribe
    [moddiv divmod swap] inscribe
    [get-digit base moddiv] inscribe
    [digitalize [0 <=] [pop []] [get-digit] [i cons] genrec] inscribe
    [to-bignum [!-] [abs digitalize] cleave cons] inscribe

    [add-digit [popop base *] [rolldown * +] clop popdd] inscribe
    [from-bignum′.prep rest 1 0 rolldown] inscribe
    [from-bignum′ from-bignum′.prep [add-digit] step popd] inscribe
    [from-bignum [from-bignum′] [first] cleave [neg] [] branch] inscribe


## Addition of Like Signs

### `add-digits`

Let's figure out how to add two lists of digits.  We will assume that the
signs are the same (both lists of digits represent numbers of the same
sign, both positive or both negative.) We're going to want a recursive
function, of course, but it's not quite a standard *hylomorphism* for (at
least) two reasons:

- We're tearing down two lists simultaneously.
- They might not be the same length.

There are two base cases: two empty lists or one empty list, the
recursive branch is taken only if both lists are non-empty.

We will also need an inital `false` value for a carry flag.  This implies
the following structure:

    false rollup [add-digits.P] [add-digits.THEN] [add-digits.R0] [add-digits.R1] genrec

### The predicate

The situation will be like this, a Boolean flag followed by two lists of
digits:

    bool [a ...] [b ...] add-digits.P

The predicate must evaluate to `false` *iff* both lists are non-`null`:

    add-digits.P == [null] ii \/

### The base cases

On the non-recursive branch of the `genrec` we have to decide between
three cases, but because addition is commutative we can lump together the
first two:

    bool [] [b ...] add-digits.THEN
    bool [a ...] [] add-digits.THEN
    
    bool [] [] add-digits.THEN

So we have an `ifte` expression:

    add-digits.THEN == [add-digits.THEN.P] [add-digits.THEN.THEN] [add-digits.THEN.ELSE] ifte

Let's define the predicate:

    add-digits.THEN.P == [null] ii /\

So `add-digits.THEN.THEN` deals with the case of both lists being empty,
and the `add-digits.THEN.ELSE` branch deals with one list of digits being
longer than the other.

### One list empty

In the cases where one of the two lists (but not both) is empty:

    carry [a ...] [] add-digits.THEN.ELSE
    carry [] [b ...] add-digits.THEN.ELSE

We first get rid of the empty list:

    [null] [pop] [popd] ifte

### ≡ `ditch-empty-list`

    [ditch-empty-list [null] [pop] [popd] ifte] inscribe

    add-digits.THEN.ELSE == ditch-empty-list add-digits.THEN.ELSE′

Now we have:

    carry [n ...] add-digits.THEN.ELSE′

This is just `add-carry-to-digits` which we will derive in a moment, but
first a side-quest...

### `add-with-carry`

To get ahead of ourselves a bit, we will want some function
`add-with-carry` that accepts a bool and two ints and leaves behind a new
int and a new Boolean carry flag. With some abuse of notation we can
treat bools as ints (type punning as in Python) and write:

          carry a b add-with-carry
    ---------------------------------
            (a+b+carry) carry′

(I find it interesting that this function accepts the carry from below
the int args but returns it above the result.  Hmm...)

### ≡ `bool-to-int`

    [bool-to-int [0] [1] branch] inscribe

We can use this function to convert the carry flag to an integer and then
add it to the sum of the two digits:

    [bool-to-int] dipd + +

So the first part of `add-with-carry` is `[bool-to-int] dipd + +` to get
the total, then we need to do `base mod` to get the new digit and `base >=`
to get the new carry flag.  Factoring give us:

    base [mod] [>=] clop

Put it all together and we have:

    [add-with-carry.0 [bool-to-int] dipd + +] inscribe
    [add-with-carry.1 base [mod] [>=] clop] inscribe
    [add-with-carry add-with-carry.0 add-with-carry.1] inscribe

### Now back to `add-carry-to-digits`

This should be a very simple recursive function.  It accepts a Boolean
`carry` flag and a non-empty list of digits (the list is only going to be
non-empty on the first iteration, after that we have to check it
ourselves because we may have emptied it of digits and still have a
`true` `carry` flag) and it returns a list of digits, consuming the carry
flag.

    add-carry-to-digits == [actd.P] [actd.THEN] [actd.R0] [actd.R1] genrec

The predicate is the carry flag itself inverted:

    actd.P == pop not

The base case simply discards the carry flag:

    actd.THEN == popd

So:

    add-carry-to-digits == [pop not] [popd] [actd.R0] [actd.R1] genrec

That leaves the recursive branch:

    true [n ...] actd.R0 [add-carry-to-digits] actd.R1

-or-

    true [] actd.R0 [add-carry-to-digits] actd.R1

We know that the Boolean value is `true`. We also know that the list will
be non-empty, but only on the first iteration of the `genrec`. It may be
that the list is empty on a later iteration.

The `actd.R0` function should check the list.

    actd.R0 == [null] [actd.R0.THEN] [actd.R0.ELSE] ifte

### If it's empty...

       true [] actd.R0.THEN [add-carry-to-digits] actd.R1
    --------------------------------------------------------
                 1 false [] [add-carry-to-digits] i cons

What we're seeing here is that `actd.R0.THEN` leaves the empty list of
digits on the stack, converts the carry flag to `false` and leave 1 on
the stack to be picked up by `actd.R1` and `cons`'d onto the list of
digits (e.g.: 999 -> 1000, it's the new 1.)

This implies:

    actd.R1 == i cons

And:

    actd.R0.THEN == popd 1 false rolldown

We have the results in this order `1 false []` rather than some other
arrangement to be compatible (same types and order) with the result of
the other branch, which we now derive.

### If the list of digits isn't empty...

With `actd.R1 == i cons` as above we have:

    true [a ...] actd.R0.ELSE [add-carry-to-digits] i cons

We want to get out that `a` value and use `add-with-carry` here:

       true 0 a add-with-carry [...] [add-carry-to-digits] i cons
    ----------------------------------------------------------------
           (a+1) carry         [...] [add-carry-to-digits] i cons

This leaves behind the new digit (a+1) for `actd.R1` and the new carry
flag for the next iteration.

So here is the specification of `actd.R0.ELSE`:

         true [a ...] actd.R0.ELSE
    -----------------------------------
       true 0 a add-with-carry [...]

It accepts a Boolean value and a non-empty list on the stack and is
responsible for `uncons`'ing `a` and `add-with-carry` and the initial 0:

                     true [a ...] . 0 swap
                   true 0 [a ...] . uncons
                   true 0 a [...] . [add-with-carry] dip
    true 0 a add-with-carry [...] .

### ≡ `actd.R0.ELSE`

    [actd.R0.ELSE 0 swap uncons [add-with-carry] dip] inscribe

Putting it all together:

    [bool-to-int [0] [1] branch] inscribe
    [ditch-empty-list [null] [pop] [popd] ifte] inscribe

    [add-with-carry.0 [bool-to-int] dipd + +] inscribe
    [add-with-carry.1 base [mod] [>=] clop] inscribe
    [add-with-carry add-with-carry.0 add-with-carry.1] inscribe

    [actd.R0.THEN popd 1 false rolldown] inscribe
    [actd.R0.ELSE 0 swap uncons [add-with-carry] dip] inscribe
    [actd.R0 [null] [actd.R0.THEN] [actd.R0.ELSE] ifte] inscribe

    [add-carry-to-digits [pop not] [popd] [actd.R0] [i cons] genrec] inscribe


We can set `base` to 10 to see it in action with familiar decimal digits:

    joy? [base 10] inscribe

Let's add a carry to 999:

    joy? true [9 9 9]
    true [9 9 9]

    joy? add-carry-to-digits
    [0 0 0 1]

Not bad!  Recall that our digits are stored in with the Most Significant
Digit at the bottom of the list.

Let's add another carry:

    joy? true swap
    true [0 0 0 1]
    
    joy? add-carry-to-digits
    [1 0 0 1]

What if we make the just the first digit into 9?
    
    joy? 9 swons
    [9 1 0 0 1]
    
    joy? true swap
    true [9 1 0 0 1]
    
    joy? add-carry-to-digits
    [0 2 0 0 1]

Excellent!

And adding `false` does nothing, yes?

    joy? false swap
    false [0 2 0 0 1]
    
    joy? add-carry-to-digits
    [0 2 0 0 1]

Wonderful!

So that handles the cases where one of the two lists (but not both) is
empty.

    add-digits.THEN.ELSE == ditch-empty-list add-carry-to-digits

### Both lists empty

If both lists are empty we discard one list and check the carry to
determine our result as described above:

    bool [] [] add-digits.THEN.THEN

Simple enough:

    bool [] [] . pop
    bool [] . swap
    [] bool . [] [1 swons] branch

True branch:

    [] true . [] [1 swons] branch
    [] .

False branch:

    [] false . [] [1 swons] branch
    [] . 1 swons
    [1] .

So:

    add-digits.THEN.THEN == pop swap [] [1 swons] branch

Here are the definitions, ready to `inscribe`:

    [add-digits.THEN.THEN pop swap [] [1 swons] branch] inscribe
    [add-digits.THEN.ELSE ditch-empty-list add-carry-to-digits] inscribe
    [add-digits.THEN [[null] ii /\] [add-digits.THEN.THEN] [add-digits.THEN.ELSE] ifte] inscribe

## And recur...

Now we go back and derive the recursive branch that is taken only if both
lists are non-empty.

    bool [a ...] [b ...] add-digits.R0 [add-digits′] add-digits.R1

We just need to knock out those recursive branch functions
`add-digits.R0` and `add-digits.R1` and we're done.

First we will want to `uncons` the digits.  Let's write a function that
just does that:

    [uncons] ii swapd

Try it:

    joy? [1 2 3] [4 5 6]
    [1 2 3] [4 5 6]
    
    joy? [uncons] ii swapd
    1 4 [2 3] [5 6]

### ≡ `uncons-two`

We could call this `uncons-two`:

    [uncons-two [uncons] ii swapd] inscribe

This brings us to:

    bool a b [...] [...] add-digits.R0′ [add-digits′] add-digits.R1

It's at this point that we'll want to employ the `add-with-carry`
function:

    bool a b [...] [...] [add-with-carry] dipd add-digits.R0″ [add-digits'] add-digits.R1

    bool a b add-with-carry [...] [...] add-digits.R0″ [add-digits'] add-digits.R1

    (a+b) bool [...] [...] add-digits.R0″ [add-digits'] add-digits.R1

If we postulate a `cons` in our `add-digits.R1` function...

    (a+b) bool [...] [...] add-digits.R0″ [add-digits'] i cons

Then it seems like we're done?  `add-digits.R0″` is nothing?

    add-digits.R0 == uncons-two [add-with-carry] dipd
    
    add-digits.R1 == i cons

### `add-digits`

    add-digits == false rollup [add-digits.P] [add-digits.THEN] [add-digits.R0] [i cons] genrec

The source code so far is now:

    [bool-to-int [0] [1] branch] inscribe
    [ditch-empty-list [null] [pop] [popd] ifte] inscribe
    [uncons-two [uncons] ii swapd] inscribe

    [add-with-carry.0 [bool-to-int] dipd + +] inscribe
    [add-with-carry.1 base [mod] [>=] clop] inscribe
    [add-with-carry add-with-carry.0 add-with-carry.1] inscribe

    [actd.R0.THEN popd 1 false rolldown] inscribe
    [actd.R0.ELSE 0 swap uncons [add-with-carry] dip] inscribe
    [actd.R0 [null] [actd.R0.THEN] [actd.R0.ELSE] ifte] inscribe

    [add-carry-to-digits [pop not] [popd] [actd.R0] [i cons] genrec] inscribe

    [add-digits.R0 uncons-two [add-with-carry] dipd] inscribe

    [add-digits.THEN.THEN pop swap [] [1 swons] branch] inscribe
    [add-digits.THEN.ELSE ditch-empty-list add-carry-to-digits] inscribe
    [add-digits.THEN [[null] ii /\] [add-digits.THEN.THEN] [add-digits.THEN.ELSE] ifte] inscribe

    [add-digits′ [[null] ii \/] [add-digits.THEN] [add-digits.R0] [i cons] genrec] inscribe
    [add-digits false rollup add-digits′] inscribe

Let's set `base` to 10 and try it out:

    joy? [base 10] inscribe

    joy? 12345 to-bignum
    [true 5 4 3 2 1]
    
    joy? rest
    [5 4 3 2 1]
    
    joy? 999 to-bignum
    [5 4 3 2 1] [true 9 9 9]
    
    joy? rest
    [5 4 3 2 1] [9 9 9]
    
    joy? add-digits
    [4 4 3 3 1]
    
    joy? true swons
    [true 4 4 3 3 1]
    
    joy? from-bignum
    13344

    joy? 12345 999 +
    13344 13344

Neat!

### `add-bignums`

There is one more thing we have to do to use this: we have to deal with
the signs.

    add-bignums [add-bignums.P] [add-bignums.THEN] [add-bignums.ELSE] ifte

To check are they the same sign?

With:

    [xor [] [not] branch] inscribe
    [nxor xor not] inscribe

We have:

    add-bignums.P == [first] ii nxor

If they are the same sign (both positive or both negative) we can use
`uncons` to keep one of the sign Boolean flags around and reuse it at the
end, and `rest` to discard the other, then `add-digits` to add the
digits, then `cons` that flag we saved onto the result digits list:

    add-bignums.THEN == [uncons] dip rest add-digits cons

If they are not both positive or both negative then we negate one of them
and subtract instead (adding unlikes is actually subtraction):

    add-bignums.ELSE == neg-bignum sub-bignums

So here we go:

    [same-sign [first] ii xor not] inscribe
    [add-like-bignums [uncons] dip rest add-digits cons] inscribe

    [add-bignums [same-sign] [add-like-bignums] [neg-bignum sub-bignums] ifte] inscribe

But we haven't implemented `neg-bignum` or `sub-bignums` yet...

We'll get to those in a moment, but first an interlude.

## Interlude: `list-combiner`

Let's review the form of our function `add-digits` (eliding the preamble
`false rollup`) and `add-digits.THEN`:

    add-digits′ == [add-digits.P] [add-digits.THEN] [add-digits.R0] [add-digits.R1] genrec

    add-digits.THEN == [add-digits.THEN.P] [add-digits.THEN.THEN] [add-digits.THEN.ELSE] ifte

Recall also:

         add-digits.P == [null] ii \/
    add-digits.THEN.P == [null] ii /\

Generalizing the names:

       F == [P] [THEN] [R0] [R1] genrec
    THEN == [THEN.P] [THEN.THEN] [THEN.ELSE] ifte

With auxiliary definitions:

    null-two == [null] ii
    both-null == null-two /\
    either-or-both-null == null-two \/

Rename predicates:

       F == [either-or-both-null] [THEN] [R0] [R1] genrec
    THEN == [both-null] [THEN.THEN] [THEN.ELSE] ifte

Substitute `THEN`:

       F == [either-or-both-null] [[both-null] [THEN.THEN] [THEN.ELSE] ifte] [R0] [R1] genrec

This is a little awkward, so let's pretend that we have a new combinator
`two-list-genrec` that accepts four quotes and does `F`:

    F == [THEN.THEN] [THEN.ELSE] [R0] [R1] two-list-genrec

So `THEN.THEN` handles the (non-recursive) case of both lists being
empty, `THEN.ELSE` handles the (non-recursive) case of one or the other
list being empty, and `R0 [F] R1` handles the (recursive) case of both
lists being non-empty.

Recall that our `R1` is just `i cons`, we can fold that in to the
definition of another new combinator that combines two lists into one:

    list-combiner-genrec == [i cons] two-list-genrec

So:

    F == [both-empty] [one-empty] [both-non-empty] list-combiner-genrec

Then for `add-digits′` we would have:

        both-empty == pop swap [] [1 swons] branch
         one-empty == ditch-empty-list add-carry-to-digits
    both-non-empty == uncons-two [add-with-carry] dipd

    add-digits′ == [both-empty] [one-empty] [both-non-empty] list-combiner-genrec

Which would expand into:

    add-digits′ == [either-or-both-null]
                   [[both-null] [both-empty] [one-empty] ifte]
                   [both-non-empty]
                   [i cons]
                   genrec


It's pretty straight forward to make a functions that converts the three
quotes into the expanded form (a kind of "macro") but you might want to
separate that from the actual `genrec` evaluation.  It would be better to
run the "macro" once, append the `[genrec]` quote to the resulting form,
and `inscribe` that, rather than putting the "macro" into the definition.
That way you avoid re-evaluating the "macro" on each iteration.

The simplification of the expanded form to the simpler version by coining
the `list-combiner-genrec` function is the "semantic compression" aspect
of factoring.  If you choose your seams and names well, the code is
(relatively) self-descriptive.

In any event, now that we know what's going on, we don't actually need
the "macro", we can just write out the expanded version directly.

Source code:

    [null-two [null] ii] inscribe
    [both-null null-two /\] inscribe
    [either-or-both-null null-two \/] inscribe

    [add-digits.both-empty pop swap [] [1 swons] branch] inscribe
    [add-digits.one-empty ditch-empty-list add-carry-to-digits] inscribe
    [add-digits.both-non-empty uncons-two [add-with-carry] dipd] inscribe

    [add-digits′ [either-or-both-null] [[both-null] [add-digits.both-empty] [add-digits.one-empty] ifte] [add-digits.both-non-empty] [i cons] genrec] inscribe



# ===================================================================================


## ≡ `neg-bignum`

Well, that was fun!  And we'll reuse it in a moment when we derive `sub-bignums`.
But for now, let's clear our palate with a nice simple function: `neg-bignum`.

To negate a Joy bignum you just invert the Boolean value at the head of the list.

    neg-bignum == [not] infra



## Subtraction of Like Signs `sub-digits`

Subtraction is similar to addition in that it's a simple recursive algorithm that works digit-by-digit.  It has the same four cases as well, we can reuse `P` and `P'`.

    initial-carry == false rollup
      sub-digits' == [P] [sub.THEN] [sub.R0] [sub.R1] genrec
       sub-digits == initial-carry add-digits'
         sub.THEN == [P'] [sub.THEN'] [sub.ELSE] ifte

### Refactoring For The Win


We noted above that the algorithm for subtraction is similar to that for addition.  Maybe we can reuse *more* than just `P` and `P'`?  In fact, I think we could refactor (prematurely, two cases is one too few) something like this?

                 [sub.THEN'] [sub.ELSE] [sub.R0] [sub.R1] foo
    ---------------------------------------------------------------------
       [P] [[P'] [sub.THEN'] [sub.ELSE] ifte] [sub.R0] [sub.R1] genrec

or just

                 [THEN] [ELSE] [R0] [R1] foo
    ----------------------------------------------------
       [P] [[P'] [THEN] [ELSE] ifte] [R0] [R1] genrec

eh?

`foo` is something like:

    F == [ifte] ccons [P'] swons
    G == [F] dipdd

    [THEN] [ELSE] [R0] [R1] [F] dipdd foo'
    [THEN] [ELSE] F [R0] [R1] foo'
    [THEN] [ELSE] [ifte] ccons [P'] swons [R0] [R1] foo'
    [[THEN] [ELSE] ifte] [P'] swons [R0] [R1] foo'
    [[P'] [THEN] [ELSE] ifte] [R0] [R1] foo'

That leaves `[P]`...

    F == [ifte] ccons [P'] swons [P] swap
    G == [F] dipdd

    [THEN] [ELSE] [ifte] ccons [P'] swons [P] swap [R0] [R1] foo'
    [[THEN] [ELSE] ifte] [P'] swons [P] swap [R0] [R1] foo'
    [[P'] [THEN] [ELSE] ifte] [P] swap [R0] [R1] foo'
    [P] [[P'] [THEN] [ELSE] ifte] [R0] [R1] genrec

Ergo:

      F == [ifte] ccons [P'] swons [P] swap
    foo == [F] dipdd genrec
    combine-two-lists == [i cons] foo

-and-

    add-digits' == [one-empty-list]
                   [both-empty]
                   [both-full]
                   combine-two-lists

    one-empty-list == ditch-empty-list add-carry-to-digits
    both-empty == pop swap carry
    both-full == uncons-two [add-with-carry] dipd

This illustrates how refactoring creates denser yet more readable code.

But this doesn't go quite far enough, I think.

    R0 == uncons-two [add-with-carry] dipd

I think `R0` will pretty much always do:

    uncons-two [combine-two-values] dipd

And so it should be refactored further to something like:

             [F] R0
    -------------------------
       uncons-two [F] dipd

And then `add-digits'` becomes just:


    add-digits' == [one-empty-list]
                   [both-empty]
                   [add-with-carry]
                   combine-two-lists

If we factor `ditch-empty-list` out of `one-empty-list`, and `pop` from `both-empty`:

    add-digits' == [add-carry-to-digits]
                   [swap carry]
                   [add-with-carry]
                   combine-two-lists


Let's figure out the new form.


       [ONE-EMPTY] [BOTH-EMPTY] [COMBINE-VALUES] foo
    ---------------------------------------------------
       [P]
       [
           [P']
           [ditch-empty-list ONE-EMPTY]
           [pop BOTH-EMPTY]
           ifte
       ]
       [uncons-two [COMBINE-VALUES] dipd]
       [i cons] genrec

eh?

Let's not over think it.

       [ONE-EMPTY] [ditch-empty-list] swoncat [BOTH-EMPTY] [pop] swoncat [COMBINE-VALUES] 
       [ditch-empty-list ONE-EMPTY] [pop BOTH-EMPTY] [COMBINE-VALUES]

With:

       [C] [A] [B] sandwich
    --------------------------
            [A [C] B]


```Joy
[sandwich swap [cons] dip swoncat] inscribe
```

    


```Joy
clear [B] [A] [C]
```

    [B] [A] [C]


```Joy
sandwich
```

    [A [B] C]

So to get from 

    [A] [B] [C]

to:

    [ditch-empty-list A] [pop B] [uncons-two [C] dipd]

we use:

    [[[ditch-empty-list] swoncat] dip [pop] swoncat] dip [uncons-two] [dipd] sandwich

It's gnarly, but simple:


```Joy
clear
[_foo0.0 [[ditch-empty-list] swoncat] dip] inscribe
[_foo0.1 [pop] swoncat] inscribe
[_foo0.3 [_foo0.0 _foo0.1] dip] inscribe
[_foo0.4 [uncons-two] [dipd] sandwich] inscribe
[_foo0 _foo0.3 _foo0.4] inscribe

[_foo1 [
  [ifte] ccons
  [P'] swons 
  [P] swap 
  ] dip
] inscribe
```

    


```Joy
[A] [B] [C] _foo0
```

    [ditch-empty-list A] [pop B] [uncons-two [C] dipd]


```Joy
_foo1
```

    [P] [[P'] [ditch-empty-list A] [pop B] ifte] [uncons-two [C] dipd]


```Joy
clear
[add-carry-to-digits]
[swap carry]
[add-with-carry]
_foo0 _foo1
```

    [P] [[P'] [ditch-empty-list add-carry-to-digits] [pop swap carry] ifte] [uncons-two [add-with-carry] dipd]

Compare the above with what we wanted:

    [P]
    [
       [P']
       [ditch-empty-list ONE-EMPTY]
       [pop BOTH-EMPTY]
       ifte
    ]
    [uncons-two [COMBINE-VALUES] dipd]

Allwe need to do is add:

    [i cons] genrec


```Joy
clear

[3 2 1] [6 5 4] initial-carry

[add-carry-to-digits]
[swap carry]
[add-with-carry]
_foo0 _foo1
```

    false [3 2 1] [6 5 4] [P] [[P'] [ditch-empty-list add-carry-to-digits] [pop swap carry] ifte] [uncons-two [add-with-carry] dipd]


```Joy
[i cons] genrec
```

    [9 7 5]


```Joy
clear
[build-two-list-combiner _foo0 _foo1 [i cons]] inscribe
[combine-two-lists [add-carry-to-digits] [swap carry] [add-with-carry] build-two-list-combiner] inscribe
```

    


```Joy
clear

[3 2 1] [6 5 4] initial-carry
combine-two-lists
```

    false [3 2 1] [6 5 4] [P] [[P'] [ditch-empty-list add-carry-to-digits] [pop swap carry] ifte] [uncons-two [add-with-carry] dipd] [i cons]


```Joy
genrec
```

    [9 7 5]


```Joy
[base 10] inscribe
```

    [9 7 5]


```Joy
clear

123456 to-bignum




```

    [true 6 5 4 3 2 1]


```Joy
clear
```

    

So that's nice.

In order to avoid the overhead of rebuilding the whole thing each time we could pre-compute the function and store it in the dictionary.


```Joy
[add-carry-to-digits]
[swap carry]
[add-with-carry]
build-two-list-combiner
```

    [P] [[P'] [ditch-empty-list add-carry-to-digits] [pop swap carry] ifte] [uncons-two [add-with-carry] dipd] [i cons]

Now grab the definition, add the `genrec` and symbol (name) and inscribe it:


```Joy
[genrec] ccons ccons [add-digits'] swoncat
```

    [add-digits' [P] [[P'] [ditch-empty-list add-carry-to-digits] [pop swap carry] ifte] [uncons-two [add-with-carry] dipd] [i cons] genrec]


```Joy
 inscribe
```

    

Try it out...


```Joy
false [3 2 1] [4 3 2] add-digits'
```

    [7 5 3]


```Joy
false swap base -- unit
```

    false [7 5 3] [9]


```Joy
add-digits'
```

    [6 6 3]


```Joy
clear
```

    

#### Demonstrate `add-bignums`


```Joy
1234 999 [to-bignum] ii 
```

    [true 4 3 2 1] [true 9 9 9]


```Joy
add-bignums
```

    [true 3 3 2 2]


```Joy
from-bignum
```

    2233


```Joy
1234 999 +
```

    2233 2233


```Joy
clear
```

    

### Subtracting

Okay, we're almost ready to implement subtraction, but there's a wrinkle!  When we subtract a smaller (absolute) value from a larger (absolute) value there's no problem:

    10 - 5 = 5

But I don't know the algorithm to subtract a larger number from a smaller one:

    5 - 10 = ???

The answer is -5, of course, but what's the algorithm?  How to make the computer figure that out?  We make use of the simple algebraic identity:

    a - b = -(b - a)

So if we want to subtract a larger number `a` from a smaller one `b` we can instead subtract the smaller from the larger and invert the sign:

    5 - 10 = -(10 - 5)

To do this we need a function `gt-digits` that will tell us which of two digit lists represents the larger integer.

#### `gt-digits`



I just realized I don't have a list length function yet!


```Joy
[length [pop ++] step_zero] inscribe
```

    


```Joy
clear
[] length
```

    0


```Joy
clear
[this is a list] length
```

    4


```Joy
clear
[1 2 3] [4 5] over over [length] app2
```

    [1 2 3] [4 5] 3 2


```Joy
[swap][6][7]cmp
```

    [4 5] [1 2 3]

what about a function that iterates through two lists until one or the other ends, or they end at the same time (same length) and we walk back through comparing the digits?


```Joy
clear
```

    


```Joy
[1 2 3] [4 5 6] [bool] ii &
```

    true


```Joy
clear
[1 2 3] [4 5 6]
[[bool] ii | not]
[pop]
[uncons-two]
[i [unit cons] dip cons]
genrec
```

    [[1 4] [2 5] [3 6]]


```Joy
clear
[1 2 3] [4 5 6]
[[bool] ii | not]
[pop]
[uncons-two]
[i [unit cons] dip cons]
genrec
```

    [[1 4] [2 5] [3 6]]


```Joy
clear
```

    

So I guess that's `zip`?

But we want something a little different.

It's a weird function: compare lengths, if they are the same length then compare contents pairwise from the end.

if the first list is empty and the second list isn't then the whole function should return false

if the first list is non-empty and the second list is empty then the whole function should return true

if both lists are non-empty we uncons some digits for later comparison?  Where to put them?  Leave them on the stack?  What about short-circuits?

if both lists are empty we start comparing uncons'd pairs until we find an un-equal pair or run out of pairs.

if we run out of pairs before we find an unequal pair then the function returns true (the numbers are identical, we should try to shortcut the actual subtraction here, but let's just get it working first, eh?)

if we find an unequal pair we return a>b and discard the rest of the pairs.  Or maybe this all happens in some sort of `infra first` situation?



So the predicate will be `[bool] ii & not`, if one list is longer than the other we are done.
We postulate a third list to contain the pairs:

    [] [3 2 1] [4 5 6] [P] [BASE] [R0] [R1] genrec

The recursive branch seems simpler to figure out:

    [] [3 2 1] [4 5 6] R0 [F] R1

    uncons-two [unit cons swons] dipd [F] i

    [] [3 2 1] [4 5 6] [P] [BASE] [uncons-two [unit cons swons] dipd] tailrec


```Joy
 [xR1 uncons-two [unit cons swons] dipd] inscribe
```

    


```Joy
clear

[] [3 2 1] [4 5 6]
```

    [] [3 2 1] [4 5 6]


```Joy
xR1 xR1 xR1
```

    [[1 6] [2 5] [3 4]] [] []


```Joy
clear
[xP [bool] ii & not] inscribe
```

    


```Joy
clear

[] [3 2 1] [5 4] [xP] [] [xR1] tailrec
```

    [[2 4] [3 5]] [1] []


```Joy
clear

[] [3 2] [4 5 1] [xP] [] [xR1] tailrec
```

    [[2 5] [3 4]] [] [1]


```Joy
clear

[] [3 2 1] [5 4 3] [xP] [] [xR1] tailrec
```

    [[1 3] [2 4] [3 5]] [] []

Now comes the tricky part, that base case:

we have three lists.  The first is a possibly-empty list of pairs to compare.

The second two are the tails of the original lists.

If the top list is non-empty then the second list must be empty so the whole function should return true

If the top list is empty and the second list isn't then the whole function should return false

If both lists are empty we start comparing uncons'd pairs until we find an un-equal pair or run out of pairs.

    [bool]  # if the first list is non-empty
    [popop pop true]
    [
        [pop bool]  # the second list is non-empty (the first list is empty)
        [popop pop false]
        [
            # both lists are empty
            popop
            compare-pairs
        ]
        ifte
    ]
    ifte




```Joy
clear
[][][1]

[bool]
[popop pop true]
[
    [pop bool]
    [popop pop false]
    [popop 23 swons]
    ifte
]
ifte
```

    true


```Joy
clear
[][1][]

[bool]
[popop pop true]
[
    [pop bool]
    [popop pop false]
    [popop 23 swons]
    ifte
]
ifte
```

    false


```Joy
clear
[1][][]

[bool]
[popop pop true]
[
    [pop bool]
    [popop pop false]
    [popop 23 swons]
    ifte
]
ifte
```

    [23 1]

#### `compare-pairs`

This should be a pretty simple recursive function

    [P] [THEN] [R0] [R1] genrec

If the list is empty we return `false`

    P == bool not
    THEN == pop false

On the recursive branch we have an `ifte` expression:

                pairs R0 [compare-pairs] R1
    ---------------------------------------------------
       pairs [P.rec] [THEN.rec] [compare-pairs] ifte

We must compare the pair from the top of the list:

    P.rec == first [>] infrst


```Joy
clear

[[1 3] [2 4] [3 5]] first [>] infrst
```

    true


```Joy
clear

[[1 3] [2 4] [3 5]] [[>] infrst] map
```

    [true true true]

    THEN.rec == pop true


```Joy
clear

[compare-pairs
  [bool not]
  [pop false]
  [
    [first [>] infrst]
    [pop true]
  ]
  [ifte]
  genrec
] inscribe
```

    


```Joy
clear [[1 3] [2 4] [3 5]] compare-pairs
```

    true


```Joy
clear [[1 3] [3 3] [3 5]] compare-pairs
```

    true

Whoops!  I forgot to remove the already-checked pair from the list of pairs!  (Later on I discover that the logic is inverted here: `>=` not `<` d'oh!)


```Joy
clear

[compare-pairs
  [bool not]
  [pop false]
  [
    [first [>=] infrst]
    [pop true]
  ]
  [[rest] swoncat ifte]
  genrec
] inscribe
```

    

This is clunky and inefficient but it works.


```Joy
clear [[1 0] [2 2] [3 3]] compare-pairs
```

    true


```Joy
clear [[1 1] [2 2] [3 3]] compare-pairs
```

    true


```Joy
clear [[1 2] [2 2] [3 3]] compare-pairs
```

    true


```Joy
clear
```

    


```Joy
clear [[1 1] [2 1] [3 3]] compare-pairs
```

    true


```Joy
clear [[1 1] [2 2] [3 3]] compare-pairs
```

    true


```Joy
clear [[1 1] [2 3] [3 3]] compare-pairs
```

    true


```Joy

```


```Joy
clear
[[1 1] [2 1] [3 3]] [] []

[bool]
[popop pop true]
[
    [pop bool]
    [popop pop false]
    [popop compare-pairs]
    ifte
]
ifte
```

    true


```Joy
[BASE
  [bool]
  [popop pop true]
  [
    [pop bool]
    [popop pop false]
    [popop compare-pairs]
    ifte
  ]
  ifte
] inscribe
```

    true


```Joy
clear

[] [3 2 1] [4 5 6]
```

    [] [3 2 1] [4 5 6]


```Joy
[xP] [BASE] [xR1] tailrec
```

    true


```Joy
clear

[] [3 2 1] [4 5 6] swap
```

    [] [4 5 6] [3 2 1]


```Joy
[xP] [BASE] [xR1] tailrec
```

    false


```Joy
clear

[] [3 2 1] dup
```

    [] [3 2 1] [3 2 1]


```Joy
[xP] [BASE] [xR1] tailrec
```

    true


```Joy
clear
```

    


```Joy
[gt-bignum <<{} [xP] [BASE] [xR1] tailrec] inscribe
```

    


```Joy
clear [3 2 1] [4 5 6] gt-bignum
```

    true


```Joy
clear [3 2 1] [4 5 6] swap gt-bignum
```

    false


```Joy
clear [3 2 1] dup gt-bignum
```

    true


```Joy

```


```Joy
clear [3 2 1] [4 5 6] [gt-bignum] [swap] [] ifte
```

    [4 5 6] [3 2 1]


```Joy
clear [4 5 6] [3 2 1] [gt-bignum] [swap] [] ifte
```

    [4 5 6] [3 2 1]

And so it goes.

Now we can subtract, we just have to remember to invert the sign bit if we swap the digit lists.

Maybe something like:

    check-gt == [gt-bignum] [swap true rollup] [false rollup] ifte

To keep the decision around as a Boolean flag?  We can `xor` it with the sign bit?


```Joy
clear
[check-gt [gt-bignum] [swap [not] dipd] [] ifte] inscribe
```

    


```Joy
false [4 5 6] [3 2 1]
```

    false [4 5 6] [3 2 1]


```Joy
check-gt
```

    false [4 5 6] [3 2 1]


```Joy
clear
```

    

### Subtraction, at last...

So now that we can compare digit lists to see if one is larger than the other we can subtract (inverting the sign if necessary) much like we did addition:

    sub-bignums == [same-sign] [sub-like-bignums] [1 0 /] ifte

    sub-like-bignums == [uncons] dip rest   sub-digits cons
                                          ^
                                          |

At this point we would have the sign bit then the two digit lists.

    sign [c b a] [z y x]

We want to use `check-gt` here:

    sign [c b a] [z y x] check-gt
    sign swapped? [c b a] [z y x] check-gt

It seems we should just flip the sign bit if we swap, eh?

    check-gt == [gt-bignum] [swap [not] dipd] [] ifte

Now we subtract the digits:

    sign [c b a] [z y x] sub-digits cons

So:

    sub-like-bignums == [uncons] dip rest check-gt sub-digits cons

    sub-digits == initial-carry sub-digits'

    sub-digits' ==
        [sub-carry-from-digits]
        [swap sub-carry]
        [sub-with-carry]
        build-two-list-combiner
        genrec

We just need to define the pieces.

#### `sub-with-carry`

We know we will never be subtracting a larger (absolute) number from a smaller (absolute) number (they might be equal) so the carry flag will never be true *at the end of a digit list subtraction.*

       carry a b sub-with-carry
    ------------------------------
         (a-b-carry)  new-carry

    _sub-with-carry0 ≡ [bool-to-int] dipd - -
    _sub-with-carry1 ≡ [base + base mod] [0 <] clop

    sub-with-carry ≡ _sub-with-carry0 _sub-with-carry1



```Joy
[_sub-with-carry0 rolldown bool-to-int [-] ii] inscribe
[_sub-with-carry1 [base + base mod] [0 <] cleave] inscribe
[sub-with-carry _sub-with-carry0 _sub-with-carry1] inscribe
```

    


```Joy
clear false 3 base --
```

    false 3 9


```Joy
sub-with-carry
```

    4 true


```Joy
clear
```

    

#### `sub-carry-from-digits`

Should be easy to make modeled on `add-carry-to-digits`, another very simple recursive function.  The predicate, base case, and `R1` are the same:

    carry [n ...] sub-carry-from-digits
    carry [n ...] [pop not] [popd] [_scfd_R0] [i cons] genrec

That leaves the recursive branch:

    true [n ...] _scfd_R0 [sub-carry-from-digits] i cons

-or-

    true [] _scfd_R0 [sub-carry-from-digits] i cons

**Except** that this should should never happen when subtracting, because we already made sure that we're only ever subtracting a number less than or equal to the, uh, number we are subtracting from (TODO rewrite this trainwreck of a sentence).

             true [a ...] _scfd_R0 [sub-carry-from-digits] i cons
    ----------------------------------------------------------------
     true 0 a add-with-carry [...] [sub-carry-from-digits] i cons
    ------------------------------------------------------------------
                 (a+1) carry [...] [sub-carry-from-digits] i cons

    
    true [a ...] _scfd_R0
    true [a ...] 0 swap uncons [sub-with-carry] dip
    true 0 [a ...] uncons [sub-with-carry] dip
    true 0 a [...] [sub-with-carry] dip
    true 0 a sub-with-carry [...]

    _scfd_R0 == 0 swap uncons [sub-with-carry] dip

But there's a problem!  This winds up subtracting `a` from 0 rather than the other way around:

    _scfd_R0 == uncons 0 swap [sub-with-carry] dip


```Joy
[sub-carry-from-digits
  [pop not]
  [popd]
  [_scfd_R0]
  [i cons]
  genrec
] inscribe
[_scfd_R0 uncons 0 swap [sub-with-carry] dip] inscribe
```

    

Try it out:


```Joy
clear

false [3 2 1] sub-carry-from-digits
```

    [3 2 1]


```Joy
clear

true [0 1] sub-carry-from-digits
```

    [9 0]


```Joy
clear

true [3 2 1] sub-carry-from-digits
```

    [2 2 1]


```Joy
clear

true [0 0 1] sub-carry-from-digits
```

    [9 9 0]


```Joy
clear
```

    

But what about those leading zeroes?

We could use a version of `cons` that refuses to put 0 onto an empty list?

    cons-but-not-leading-zeroes == [[bool] ii | not] [popd] [cons] ifte


```Joy
[cons-but-not-leading-zeroes [[bool] ii | not] [popd] [cons] ifte] inscribe
```

    


```Joy
[sub-carry-from-digits
  [pop not]
  [popd]
  [_scfd_R0]
  [i cons-but-not-leading-zeroes]
  genrec
] inscribe
```

    


```Joy
[_scfd_R0 uncons 0 swap [sub-with-carry] dip] inscribe
```

    


```Joy
clear

true [0 0 1] sub-carry-from-digits
```

    [9 9]


```Joy
clear
```

    

#### `sub-carry`

    sub-carry == pop


```Joy
[sub-like-bignums [uncons] dip rest check-gt sub-digits cons] inscribe
[sub-digits initial-carry sub-digits'] inscribe
[sub-digits'
    [sub-carry-from-digits]
    [swap pop]
    [sub-with-carry]
    build-two-list-combiner
    genrec
] inscribe
```

    


```Joy
clear
true [3 2 1] [6 5 4]
```

    true [3 2 1] [6 5 4]


```Joy
check-gt initial-carry
```

    false false [6 5 4] [3 2 1]


```Joy
sub-digits'
```

    false [3 3 3]


```Joy
clear
12345 to-bignum 109 to-bignum
```

    [true 5 4 3 2 1] [true 9 0 1]


```Joy
sub-like-bignums
```

    [true 6 3 2 2 1]


```Joy
from-bignum
```

    12236


```Joy
clear
```

    

#### `neg-bignum`


```Joy
[neg-bignum [not] infra] inscribe
```

    


```Joy
123 
```

    123


```Joy
to-bignum neg-bignum from-bignum
```

    -123


```Joy
to-bignum neg-bignum from-bignum
```

    123


```Joy
clear
[sub-bignums [same-sign] [sub-like-bignums] [neg-bignum add-like-bignums] ifte] inscribe
[add-bignums [same-sign] [add-like-bignums] [neg-bignum sub-like-bignums] ifte] inscribe
```

    

## Multiplication




```Joy

```

## Appendix: Source Code
    clear
    [base 2147483648]
    [ditch-empty-list [bool] [popd] [pop] ifte]
    [bool-to-int [0] [1] branch]
    [uncons-two [uncons] ii swapd]
    [sandwich swap [cons] dip swoncat]

    [digitalize [0 <=] [pop []] [base divmod swap] [i cons] genrec]
    [to-bignum [!-] [abs digitalize] cleave cons]

    [prep rest 1 0 rolldown]
    [from-bignum′ [next-digit] step popd]
    [next-digit [increase-power] [accumulate-digit] clop popdd]
    [increase-power popop base *]
    [accumulate-digit rolldown * +]

    [sign-int [first] [prep from-bignum′] cleave]
    [neg-if-necessary swap [neg] [] branch]
    [from-bignum sign-int neg-if-necessary]

    [add-with-carry _add-with-carry0 _add-with-carry1]
    [_add-with-carry0 [bool-to-int] dipd + +]
    [_add-with-carry1 base [mod] [>=] clop]

    [add-carry-to-digits [pop not] [popd] [actd.R0] [i cons] genrec]
    [actd.R0 [bool] [actd.R0.then] [actd.R0.else] ifte]
    [actd.R0.else popd 1 false rolldown]
    [actd.R0.then 0 swap uncons [add-with-carry] dip]

    [add-digits initial-carry add-digits']
    [initial-carry false rollup]

    [add-digits' [P] [THEN] [R0] [R1] genrec]
    [P [bool] ii & not]
    [THEN [P'] [THEN'] [ELSE] ifte]
    [R0 uncons-two [add-with-carry] dipd]
    [R1 i cons]
    [P' [bool] ii |]
    [THEN' ditch-empty-list add-carry-to-digits]
    [ELSE pop swap [] [1 swons] branch]

    [same-sign [first] ii xor not]
    [add-like-bignums [uncons] dip rest add-digits cons]
    [add-bignums [same-sign] [add-like-bignums] [neg-bignum sub-like-bignums] ifte]

    [build-two-list-combiner _btlc0 _btlc1 [i cons]]
    [_btlc0.0 [[ditch-empty-list] swoncat] dip]
    [_btlc0.1 [pop] swoncat]
    [_btlc0.3 [_btlc0.0 _btlc0.1] dip]
    [_btlc0.4 [uncons-two] [dipd] sandwich]
    [_btlc0 _btlc0.3 _btlc0.4]
    [_btlc1 [[ifte] ccons [P'] swons [P] swap] dip]

    [carry [] [1 swons] branch]

    [compare-pairs [bool not] [pop false] [[first [>=] infrst] [pop true]] [[rest] swoncat ifte] genrec]
    [xR1 uncons-two [unit cons swons] dipd]
    [xP [bool] ii & not]
    [BASE [bool] [popop pop true] [[pop bool] [popop pop false] [popop compare-pairs] ifte] ifte]
    [gt-bignum <<{} [xP] [BASE] [xR1] tailrec]
    [check-gt [gt-bignum] [swap [not] dipd] [] ifte]

    [sub-carry pop]

    [sub-carry-from-digits [pop not] [popd] [_scfd_R0] [i cons-but-not-leading-zeroes] genrec] inscribe
    [_scfd_R0 uncons 0 swap [sub-with-carry] dip] inscribe
    [cons-but-not-leading-zeroes [P'] [cons] [popd] ifte]

    [sub-with-carry _sub-with-carry0 _sub-with-carry1]
    [_sub-with-carry0 rolldown bool-to-int [-] ii]
    [_sub-with-carry1 [base + base mod] [0 <] cleave]

    [sub-like-bignums [uncons] dip rest check-gt sub-digits cons]
    [sub-digits initial-carry sub-digits']

    enstacken [inscribe] step

    [add-carry-to-digits]
    [swap carry]
    [add-with-carry]
    build-two-list-combiner
    [genrec] ccons ccons
    [add-digits'] swoncat
    inscribe

    [sub-carry-from-digits]
    [swap sub-carry]
    [sub-with-carry]
    build-two-list-combiner
    [genrec] ccons ccons
    [sub-digits'] swoncat
    inscribe


### notes

So far I have three formats for Joy source:

- `def.txt` is a list of definitions (UTF-8), one per line, with no special marks.
- `foo ≡ bar baz...` lines in the `joy.py` embedded definition text, because why not?  (Sometimes I use `==` instead of `≡` mostly because some tools can't handle the Unicode glyph.  Like converting this notebook to PDF via LaTeX just omitted them.)
- `[name body] inscribe` Joy source code that literally defines new words in the dictionary at runtime.  A text of those commands can be fed to the interpreter to customize it without any special processing (like the other two formats require.)

So far I prefer the `def.txt` style but that makes it tricky to embed them automatically into the `joy.py` file.

#### Refactoring

We have `i cons` but that's pretty tight already, eh?

However, `[i cons] genrec` is an interesting combinator.  It's almost `tailrec` with that `i` combinator for the recursion, but then `cons` means it's a list-builder (an *anamorphism* if you go for that sort of thing.)

    simple-list-builder == [i cons] genrec

And maybe:

    boolii == [bool] ii

       both? == boolii &
     one-of? == boolii |

