

```python
from notebook_preamble import D, DefinitionWrapper, J, V, define
```

# Recursive Combinators

This article describes the `genrec` combinator, how to use it, and several generic specializations.

                          [if] [then] [rec1] [rec2] genrec
    ---------------------------------------------------------------------
       [if] [then] [rec1 [[if] [then] [rec1] [rec2] genrec] rec2] ifte


From "Recursion Theory and Joy" (j05cmp.html) by Manfred von Thun:

> "The genrec combinator takes four program parameters in addition to
whatever data parameters it needs. Fourth from the top is an if-part,
followed by a then-part. If the if-part yields true, then the then-part
is executed and the combinator terminates. The other two parameters are
the rec1-part and the rec2-part. If the if-part yields false, the
rec1-part is executed. Following that the four program parameters and
the combinator are again pushed onto the stack bundled up in a quoted
form. Then the rec2-part is executed, where it will find the bundled
form. Typically it will then execute the bundled form, either with i or
with app2, or some other combinator."

## Designing Recursive Functions
The way to design one of these is to fix your base case and 
test and then treat `R1` and `R2` as an else-part "sandwiching"
a quotation of the whole function.

For example, given a (general recursive) function `F`:

    F == [I] [T] [R1]   [R2] genrec
      == [I] [T] [R1 [F] R2] ifte

If the `[I]` predicate is false you must derive `R1` and `R2` from:

    ... R1 [F] R2

Set the stack arguments in front and figure out what `R1` and `R2`
have to do to apply the quoted `[F]` in the proper way.

## Primitive Recursive Functions
Primitive recursive functions are those where `R2 == i`.

    P == [I] [T] [R] primrec
      == [I] [T] [R [P] i] ifte
      == [I] [T] [R P] ifte

## [Hylomorphism](https://en.wikipedia.org/wiki/Hylomorphism_%28computer_science%29)
A [hylomorphism](https://en.wikipedia.org/wiki/Hylomorphism_%28computer_science%29) is a recursive function `H :: A -> C` that converts a value of type `A` into a value of type `C` by means of:

- A generator `G :: A -> (B, A)`
- A combiner `F :: (B, C) -> C`
- A predicate `P :: A -> Bool` to detect the base case
- A base case value `c :: C`
- Recursive calls (zero or more); it has a "call stack in the form of a cons list".

It may be helpful to see this function implemented in imperative Python code.


```python
def hylomorphism(c, F, P, G):
    '''Return a hylomorphism function H.'''

    def H(a):
        if P(a):
            result = c
        else:
            b, aa = G(a)
            result = F(b, H(aa))  # b is stored in the stack frame during recursive call to H().
        return result

    return H
```

Cf. ["Bananas, Lenses, & Barbed Wire"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.41.125)

Note that during evaluation of `H()` the intermediate `b` values are stored in the Python call stack.  This is what is meant by "call stack in the form of a cons list".

## Hylomorphism in Joy
We can define a combinator `hylomorphism` that will make a hylomorphism combinator `H` from constituent parts.

    H == [P] c [G] [F] hylomorphism

The function `H` is recursive, so we start with `ifte` and set the else-part to
some function `J` that will contain a quoted copy of `H`.  (The then-part just
discards the leftover `a` and replaces it with the base case value `c`.)

    H == [P] [pop c] [J] ifte

The else-part `J` gets just the argument `a` on the stack.

    a J
    a G              The first thing to do is use the generator G
    aa b             which produces b and a new aa
    aa b [H] dip     we recur with H on the new aa
    aa H b F         and run F on the result.

This gives us a definition for `J`.

    J == G [H] dip F

Plug it in and convert to genrec.

    H == [P] [pop c] [G [H] dip F] ifte
    H == [P] [pop c] [G]   [dip F] genrec

This is the form of a hylomorphism in Joy, which nicely illustrates that
it is a simple specialization of the general recursion combinator.

    H == [P] c [G] [F] hylomorphism == [P] [pop c] [G] [dip F] genrec

## Derivation of `hylomorphism` combinator

Now we just need to derive a definition that builds the `genrec` arguments
out of the pieces given to the `hylomorphism` combinator.

       [P]      c  [G]     [F] hylomorphism
    ------------------------------------------
       [P] [pop c] [G] [dip F] genrec

Working in reverse:

- Use `swoncat` twice to decouple `[c]` and `[F]`.
- Use `unit` to dequote `c`.
- Use `dipd` to untangle `[unit [pop] swoncat]` from the givens.

So:

    H == [P] [pop c]              [G]                  [dip F] genrec
         [P] [c]    [pop] swoncat [G]        [F] [dip] swoncat genrec
         [P] c unit [pop] swoncat [G]        [F] [dip] swoncat genrec
         [P] c [G] [F] [unit [pop] swoncat] dipd [dip] swoncat genrec

At this point all of the arguments (givens) to the hylomorphism are to the left so we have
a definition for `hylomorphism`:

    hylomorphism == [unit [pop] swoncat] dipd [dip] swoncat genrec


```python
define('hylomorphism == [unit [pop] swoncat] dipd [dip] swoncat genrec')
```

### Example: Finding [Triangular Numbers](https://en.wikipedia.org/wiki/Triangular_number)
Let's write a function that, given a positive integer, returns the sum of all positive integers less than that one.  (In this case the types `A`, `B` and `C` are all `int`.)

To sum a range of integers from 0 to *n* - 1:

- `[P]` is `[1 <=]`
- `c` is `0`
- `[G]` is `[-- dup]`
- `[F]` is `[+]`


```python
define('triangular_number == [1 <=] 0 [-- dup] [+] hylomorphism')
```

Let's try it:


```python
J('5 triangular_number')
```

    10



```python
J('[0 1 2 3 4 5 6] [triangular_number] map')
```

    [0 0 1 3 6 10 15]


## Four Specializations
There are at least four kinds of recursive combinator, depending on two choices.  The first choice is whether the combiner function `F` should be evaluated during the recursion or pushed into the pending expression to be "collapsed" at the end.  The second choice is whether the combiner needs to operate on the current value of the datastructure or the generator's output, in other words, whether `F` or `G` should run first in the recursive branch.

    H1 ==        [P] [pop c] [G             ] [dip F] genrec
    H2 == c swap [P] [pop]   [G [F]    dip  ] [i]     genrec
    H3 ==        [P] [pop c] [  [G] dupdip  ] [dip F] genrec
    H4 == c swap [P] [pop]   [  [F] dupdip G] [i]     genrec

The working of the generator function `G` differs slightly for each.  Consider the recursive branches:

    ... a G [H1] dip F                w/ a G == a′ b
    
    ... c a G [F] dip H2                 a G == b  a′
    
    ... a [G] dupdip [H3] dip F          a G == a′
    
    ... c a [F] dupdip G H4              a G == a′

The following four sections illustrate how these work, omitting the predicate evaluation.

### `H1`

    H1 == [P] [pop c] [G] [dip F] genrec

Iterate n times.

    ... a  G [H1] dip F
    ... a′ b [H1] dip F
    ... a′ H1 b F
    ... a′ G [H1] dip F b F
    ... a″ b′ [H1] dip F b F
    ... a″ H1 b′ F b F
    ... a″ G [H1] dip F b′ F b F
    ... a‴ b″ [H1] dip F b′ F b F
    ... a‴ H1 b″ F b′ F b F
    ... a‴ pop c b″ F b′ F b F
    ... c b″ F b′ F b F
    ... d      b′ F b F
    ... d′          b F
    ... d″

This form builds up a pending expression (continuation) that contains the intermediate results along with the pending combiner functions.  When the base case is reached the last term is replaced by the identity value `c` and the continuation "collapses" into the final result using the combiner `F`.

### `H2`
When you can start with the identity value `c` on the stack and the combiner `F` can operate as you go using the intermediate results immediately rather than queuing them up, use this form.  An important difference is that the generator function must return its results in the reverse order.

    H2 == c swap [P] [pop] [G [F] dip] primrec

    ... c a G  [F] dip H2
    ... c b a′ [F] dip H2
    ... c b F a′ H2
    ... d     a′ H2
    ... d a′ G  [F] dip H2
    ... d b′ a″ [F] dip H2
    ... d b′ F a″ H2
    ... d′     a″ H2
    ... d′ a″ G  [F] dip H2
    ... d′ b″ a‴ [F] dip H2
    ... d′ b″ F a‴ H2
    ... d″      a‴ H2
    ... d″ a‴ pop
    ... d″


### `H3`
If you examine the traces above you'll see that the combiner `F` only gets to operate on the results of `G`, it never "sees" the first value `a`.  If the combiner and the generator both need to work on the current value then `dup` must be used, and the generator must produce one item instead of two (the b is instead the duplicate of a.)


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

### `H4`
And, last but not least, if you can combine as you go, starting with `c`, and the combiner `F` needs to work on the current item, this is the form:

    H4 == c swap [P] [pop] [[F] dupdip G] primrec

    ... c  a  [F] dupdip G H4
    ... c  a   F  a      G H4
    ... d         a      G H4
    ... d  a′              H4
    ... d  a′ [F] dupdip G H4
    ... d  a′  F  a′     G H4
    ... d′        a′     G H4
    ... d′ a″              H4
    ... d′ a″ [F] dupdip G H4
    ... d′ a″  F  a″     G H4
    ... d″        a″     G H4
    ... d″ a‴              H4
    ... d″ a‴ pop
    ... d″

## Anamorphism
An anamorphism can be defined as a hylomorphism that uses `[]` for `c` and
`swons` for `F`.  An anamorphic function builds a list of values.

    A == [P] [] [G] [swons] hylomorphism

### `range` et. al.
An example of an anamorphism is the `range` function which generates the list of integers from 0 to *n* - 1 given *n*.

Each of the above variations can be used to make four slightly different `range` functions.

#### `range` with `H1`
    H1 == [P]    [pop c]  [G]      [dip F]     genrec
       == [0 <=] [pop []] [-- dup] [dip swons] genrec


```python
define('range == [0 <=] [] [-- dup] [swons] hylomorphism')
```


```python
J('5 range')
```

    [4 3 2 1 0]


#### `range` with `H2`
    H2 == c  swap [P]    [pop] [G      [F]     dip] primrec
       == [] swap [0 <=] [pop] [-- dup [swons] dip] primrec


```python
define('range_reverse == [] swap [0 <=] [pop] [-- dup [swons] dip] primrec')
```


```python
J('5 range_reverse')
```

    [0 1 2 3 4]


#### `range` with `H3`
    H3 == [P]    [pop c]  [[G]  dupdip] [dip F]     genrec
       == [0 <=] [pop []] [[--] dupdip] [dip swons] genrec


```python
define('ranger == [0 <=] [pop []] [[--] dupdip] [dip swons] genrec')
```


```python
J('5 ranger')
```

    [5 4 3 2 1]


#### `range` with `H4`
    H4 == c  swap [P]    [pop] [[F]     dupdip G ] primrec
       == [] swap [0 <=] [pop] [[swons] dupdip --] primrec


```python
define('ranger_reverse == [] swap [0 <=] [pop] [[swons] dupdip --] primrec')
```


```python
J('5 ranger_reverse')
```

    [1 2 3 4 5]


Hopefully this illustrates the workings of the variations.  For more insight you can run the cells using the `V()` function instead of the `J()` function to get a trace of the Joy evaluation.

## Catamorphism
A catamorphism can be defined as a hylomorphism that uses `[uncons swap]` for `[G]`
and `[[] =]` (or just `[not]`) for the predicate `[P]`.  A catamorphic function tears down a list term-by-term and makes some new value.

    C == [not] c [uncons swap] [F] hylomorphism


```python
define('swuncons == uncons swap')  # Awkward name.
```

An example of a catamorphism is the sum function.

    sum == [not] 0 [swuncons] [+] hylomorphism


```python
define('sum == [not] 0 [swuncons] [+] hylomorphism')
```


```python
J('[5 4 3 2 1] sum')
```

    15


### The `step` combinator
The `step` combinator will usually be better to use than `catamorphism`.


```python
J('[step] help')
```

    Run a quoted program on each item in a sequence.
    ::
    
            ... [] [Q] . step
         -----------------------
                   ... .
    
    
           ... [a] [Q] . step
        ------------------------
                 ... a . Q
    
    
         ... [a b c] [Q] . step
      ----------------------------------------
                   ... a . Q [b c] [Q] step
    
    The step combinator executes the quotation on each member of the list
    on top of the stack.
    



```python
define('sum == 0 swap [+] step')
```


```python
J('[5 4 3 2 1] sum')
```

    15


## Example: Factorial Function

For the Factorial function:

    H4 == c swap [P] [pop] [[F] dupdip G] primrec

With:

    c == 1
    F == *
    G == --
    P == 1 <=


```python
define('factorial == 1 swap [1 <=] [pop] [[*] dupdip --] primrec')
```


```python
J('5 factorial')
```

    120


## Example: `tails`
An example of a paramorphism for lists given in the ["Bananas..." paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.41.125) is `tails` which returns the list of "tails" of a list.

        [1 2 3] tails
    --------------------
       [[] [3] [2 3]]
    

We can build as we go, and we want `F` to run after `G`, so we use pattern `H2`:

    H2 == c swap [P] [pop] [G [F] dip] primrec

We would use:

    c == []
    F == swons
    G == rest dup
    P == not


```python
define('tails == [] swap [not] [pop] [rest dup [swons] dip] primrec')
```


```python
J('[1 2 3] tails')
```

    [[] [3] [2 3]]


## Conclusion: Patterns of Recursion
Our story so far...


### Hylo-, Ana-, Cata-

    H == [P  ] [pop c ] [G          ] [dip F        ] genrec
    A == [P  ] [pop []] [G          ] [dip swap cons] genrec
    C == [not] [pop c ] [uncons swap] [dip F        ] genrec

### Para-, ?-, ?-

    P == c  swap [P  ] [pop] [[F        ] dupdip G          ] primrec
    ? == [] swap [P  ] [pop] [[swap cons] dupdip G          ] primrec
    ? == c  swap [not] [pop] [[F        ] dupdip uncons swap] primrec


## Appendix: Fun with Symbols

    |[ (c, F), (G, P) ]| == (|c, F|) • [(G, P)]

["Bananas, Lenses, & Barbed Wire"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.41.125)

    (|...|)  [(...)]  [<...>]

I think they are having slightly too much fun with the symbols.  However, "Too much is always better than not enough."
