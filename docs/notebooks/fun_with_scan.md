```python
from notebook_preamble import D, DefinitionWrapper, J, V, define
```

# On "Two Exercises Found in a Book on Algorithmics"

Bird & Meertens

[PDF paper available here](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.694.2614)

## Define `scan` in terms of a reduction.

> Problem I. The reduction operator `/` of APL takes some binary operator `⨁` on its left and a vector `x` of values on its right. The meaning of `⨁/x` for `x = [a b ... z]` is the value `a⨁b⨁...⨁z`.  For this to be well-defined in the absence of brackets, the operation `⨁` has to be associative.  Now there is another operator `\` of APL called `scan`.  Its effect is closely related to reduction in that we have:

    ⨁\x = [a a⨁b a⨁b⨁c ... a⨁b⨁...⨁z]

> The problem is to find some definition of `scan` as a reduction.  In other words, we have to find some function `f` and an operator `⨂` so that

    ⨁\x = f(a)⨂f(b)⨂...⨂f(z)

## Designing the Recursive Function
Ignoring the exact requirements (finding `f` and `⨂`) can we implement `scan` as a hylomorphism in Joy?

Looking at the forms of hylomorphism, `H3` is the one to use:

### `H3`
If the combiner and the generator both need to work on the current value then `dup` must be used, and the generator must produce one item instead of two (the b is instead the duplicate of a.)


    H3 == [P] [pop c] [[G] dupdip] [dip F] genrec

    ... a [G] dupdip [H3] dip F
    ... a  G  a      [H3] dip F
    ... a′    a      [H3] dip F
    ... a′ H3 a               F
    ... a′ [G] dupdip [H3] dip F a F
    ... a′  G  a′     [H3] dip F a F
    ... a″     a′     [H3] dip F a F
    ... a″ H3  a′              F a F
    ... a″ [G] dupdip [H3] dip F a′ F a F
    ... a″  G    a″   [H3] dip F a′ F a F
    ... a‴       a″   [H3] dip F a′ F a F
    ... a‴ H3    a″            F a′ F a F
    ... a‴ pop c a″ F a′ F a F
    ...        c a″ F a′ F a F
    ...        d      a′ F a F
    ...        d′          a F
    ...        d″

### Initial Definition
We're building a list of values so this is an "anamorphism".  (An anamorphism uses `[]` for `c` and `swons` for `F`.)

    scan == [P] [pop []] [[G] dupdip]      [dip swons] genrec

Convert to `ifte`:

    scan == [P] [pop []] [[G] dupdip [scan] dip swons] ifte

On the recursive branch `[G] dupdip` doesn't cut it:

    [1 2 3] [G] dupdip [scan] dip swons
    [1 2 3]  G [1 2 3] [scan] dip swons

### Use `first`
At this point, we want the copy of `[1 2 3]` to just be `1`, so we use `first`.

    scan == [P] [pop []] [[G] dupdip first] [dip swons] genrec

    [1 2 3] [G] dupdip first [scan] dip swons
    [1 2 3]  G [1 2 3] first [scan] dip swons
    [1 2 3]  G  1            [scan] dip swons

### `G` applies `⨁`
Now what does `G` have to do?  Just apply `⨁` to the first two terms in the list.

    [1 2 3] G
    [1 2 3] [⨁] infra
    [1 2 3] [+] infra
    [3 3]

### Predicate `P`
Which tells us that the predicate `[P]` must guard against lists with less that two items in them:

    P == size 1 <=

Let's see what we've got so far:

    scan == [P        ] [pop []] [[G]         dupdip first] [dip swons] genrec
    scan == [size 1 <=] [pop []] [[[F] infra] dupdip first] [dip swons] genrec

### Handling the Last Term
This works to a point, but it throws away the last term:


```python
J('[1 2 3] [size 1 <=] [pop []] [[[+] infra] dupdip first] [dip swons] genrec')
```

    [1 3]


Hmm... Let's take out the `pop` for a sec...


```python
J('[1 2 3] [size 1 <=] [[]] [[[+] infra] dupdip first] [dip swons] genrec')
```

    [6] [1 3]


That leaves the last item in our list, then it puts an empty list on the stack and `swons`'s the new terms onto that.  If we leave out that empty list, they will be `swons`'d onto that list that already has the last item.


```python
J('[1 2 3] [size 1 <=] [] [[[+] infra] dupdip first] [dip swons] genrec')
```

    [1 3 6]


### Parameterize `⨁`
So we have:

    [⨁] scan == [size 1 <=] [] [[[⨁] infra] dupdip first] [dip swons] genrec

Trivially:

     == [size 1 <=] [] [[[⨁] infra] dupdip first]                 [dip swons] genrec
     == [[[⨁] infra] dupdip first]           [size 1 <=] [] roll< [dip swons] genrec
     == [[⨁] infra]      [dupdip first] cons [size 1 <=] [] roll< [dip swons] genrec
     == [⨁] [infra] cons [dupdip first] cons [size 1 <=] [] roll< [dip swons] genrec

And so:

    scan == [infra] cons [dupdip first] cons [size 1 <=] [] roll< [dip swons] genrec


```python
define('scan [infra] cons [dupdip first] cons [size 1 <=] [] roll< [dip swons] genrec')
```


```python
J('[1 2 3 4] [+] scan')
```

    [1 3 6 10]



```python
J('[1 2 3 4] [*] scan')
```

    [1 2 6 24]



```python
J('[1 2 3 4 5 6 7] [neg +] scan')
```

    [1 1 2 2 3 3 4]


## Problem 2.
> Define a line to be a sequence of characters not containing the newline character.  It is easy to define a function `Unlines` that converts a non-empty sequence of lines into a sequence of characters by inserting newline characters between every two lines.
>
> Since `Unlines` is injective, the function `Lines`, which converts a sequence of characters into a sequence of lines by splitting on newline characters, can be specified as the inverse of `Unlines`.
>
> The problem, just as in Problem 1. is to find a definition by reduction of the function `Lines`.


    Unlines = uncons ['\n' swap + +] step



```python
J('["hello" "world"] uncons ["\n" swap + +] step')
```

    'hello\nworld'


Again ignoring the actual task let's just derive `Lines`:

       "abc\nefg\nhij" Lines
    ---------------------------
        ["abc" "efg" "hij"]

Instead of `P == [size 1 <=]` we want `["\n" in]`, and for the base-case of a string with no newlines in it we want to use `unit`:

    Lines == ["\n" in] [unit] [R0]       [dip swons] genrec
    Lines == ["\n" in] [unit] [R0 [Lines] dip swons] ifte

Derive `R0`:

    "a \n b" R0                    [Lines] dip swons
    "a \n b" split-at-newline swap [Lines] dip swons
    "a " " b"                 swap [Lines] dip swons
    " b" "a "                      [Lines] dip swons
    " b" Lines "a " swons
    [" b"]     "a " swons
    ["a " " b"]

So:

    R0 == split-at-newline swap

    Lines == ["\n" in] [unit] [split-at-newline swap] [dip swons] genrec

## Missing the Point?
This is all good and well, but in the paper many interesting laws and properties are discussed.  Am I missing the point?

    0 [a b c d] [F] step == 0 [a b] [F] step 0 [c d] [F] step concat

For associative function `F` and a "unit" element for that function, here represented by `0`.

For functions that don't have a "unit" we can fake it (the example is given of infinity for the `min(a, b)` function.) We can also use:

    safe_step == [size 1 <=] [] [uncons [F] step] ifte

Or:

    safe_step == [pop size 1 <=] [pop] [[uncons] dip step] ifte

       [a b c] [F] safe_step
    ---------------------------
       a [b c] [F] step

To limit `F` to working on pairs of terms from its domain.


