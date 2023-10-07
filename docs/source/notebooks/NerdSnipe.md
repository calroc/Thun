# Nerd Sniped

There's [an interesting video by Conor Hoekstra](https://youtu.be/JELcdZLre3s?t=2748)
wherein he presents an example function implemented in BQN.

It's the Sum of Squares of a list of numbers filtered by whether or not the index of each item in the list modulus the length of list.  It's a weird thing to do, unlikely to come up in practice, but that's alright, it's just to exercise the language.

I figure it might be fun and informative to derive a similar function in Joy (Thun).

let's start with an example list on the stack:

    joy? [2 7 1 19 18 3]
    
    [2 7 1 19 18 3]

Following the BQN style, let's get the length (`size` in Thun) and create a list of indicies:

    [2 7 1 19 18 3]
    
    joy? dup size range
    
    [2 7 1 19 18 3] [5 4 3 2 1 0]

We want to increment each index:

    [2 7 1 19 18 3] [5 4 3 2 1 0]
    
    joy? [++] map
    
    [2 7 1 19 18 3] [6 5 4 3 2 1]

But we will also want the length in a function that calculates an index `mod` the length:

    [2 7 1 19 18 3]
    
    joy? dup size [swap mod] cons
    
    [2 7 1 19 18 3] [6 swap mod]

Let's do both:

    joy? clear [2 7 1 19 18 3]
    
    [2 7 1 19 18 3]
    
    joy? dup size [range [++] map] [[swap mod] cons] cleave
    
    [2 7 1 19 18 3] [6 5 4 3 2 1] [6 swap mod]

Okay!  Then `map` the one over the other:

    joy? map
    
    [2 7 1 19 18 3] [0 1 2 0 0 0]

Now we want to make these results into Booleans, but represented as integers (because we are going to multiply by them later):

    [2 7 1 19 18 3] [0 1 2 0 0 0]
    
    joy? [0 = [0] [1] branch] map
    
    [2 7 1 19 18 3] [1 0 0 1 1 1]

Clunky, but we now have our list of filter integers. `zip` to transpose the two lists to a list of pairs:

    [2 7 1 19 18 3] [1 0 0 1 1 1]
    
    joy? zip
    
    [[1 2] [0 7] [0 1] [1 19] [1 18] [1 3]]

Now, for each pair we want to multiply the pairs (using the duality of Booleans as integers to convert the numbers we want to work on to themselves and the numbers we do not want to work on to zero which is the --I don't recall the jargon!--  zero times anything is zero, and zero plus anything is that thing, and we use that in a moment to get around actually filtering our list!)  As I was saying we multiply the pairs, then square the result, then sum all the results:

    [[1 2] [0 7] [0 1] [1 19] [1 18] [1 3]]
    
    joy? [i * sqr] map
    
    [4 0 0 361 324 9]
    
    joy? sum
    
    698

Oops!  I got the wrong result!  I must have inverted the logic on the mapping to ints above:

    [2 7 1 19 18 3] [0 1 2 0 0 0]
    
    joy? [0 = [1] [0] branch] map
    
    [2 7 1 19 18 3] [0 1 1 0 0 0]

    joy? zip
    
    [[0 2] [1 7] [1 1] [0 19] [0 18] [0 3]]
    
    joy? [i * sqr] map
    
    [0 49 1 0 0 0]
    
    joy? sum
    
    50

Hmm, that's not it.  Maybe I'm indexing the items backwards?

    [2 7 1 19 18 3] [0 1 2 0 0 0]
    
    joy? reverse
    
    [2 7 1 19 18 3] [0 0 0 2 1 0]
    
    joy? [0 = [0] [1] branch] map
    
    [2 7 1 19 18 3] [1 1 1 0 0 1]
    
    joy? zip
    
    [[1 2] [1 7] [1 1] [0 19] [0 18] [1 3]]
    
    joy? [i * sqr] map
    
    [4 49 1 0 0 9]
    
    joy? sum
    
    63

Ah!  That was it.  How do you like my debugging strategy here?  Rather than reviewing the original to see that I was getting the right result at each step I just guessed at the "polarity" or "chirality" of the two operations that were (mildly, perhaps) ambiguous.  It had to be one or the other, the function is too simple to hide many opportunities for confusion.)

Let's examine this monster in all it's glory:

    dup size [range [++] map] [[swap mod] cons] cleave map reverse [0 = [0] [1] branch] map zip [i * sqr] map sum

Gnarly, eh?

Let's reformat:

    dup size
    [range [++] map] [[swap mod] cons] cleave
    map
    reverse
    [0 = [0] [1] branch] map
    zip
    [i * sqr] map
    sum


### ≡ `reverse-range-++`

Let's golf it a little.  There is a version of `range` that generates its result list in reverse order, which would allow us to get rid of that `reverse` in the expression, and I bet we could modify that to generate them already incremented too.  Let's assume we've done that already and call it `reverse-range-++`, why not?

    [reverse-range-++ range [++] map reverse] inscribe

Then:

    dup size
    [reverse-range-++] [[swap mod] cons] cleave
    map
    [0 = [0] [1] branch] map
    zip
    [i * sqr] map
    sum

We can also collapse two of the `map` functions into one:

    dup size
    [reverse-range-++] [[swap mod 0 = [0] [1] branch] cons] cleave
    map
    zip
    [i * sqr] map
    sum

We could extract sub-functions, e.g. `[0] [1] branch` converts Booleans to integers, we might wind up using that again somewhere, eh?

But really, following the BQN style, this is about as good as it gets.


## What's it Look Like in Thun?

That's BQN in Thun, what's it look like in Thun?

One way to approach it is to simplify the desired function in terms of another function, and then to derive that other function.  We know that the desired function is a "catamorphism" (roughly, it accepts a list and returns a single value) and we know that the "combining function" will be `sqr +` with some filter.  So let's start with a specialization of `step`:

    0 swap [sqr +] step-indicies-mod-length

E.g.:

    0 [2 7 1 19 18 3] [sqr +] step-indicies-mod-length

So now the problem is to derive an efficient form of `step-indicies-mod-length`

Let's assume for the moment that we have a function that gives us a predicate for the indicies:

    size [swap mod 0 =] cons

We run that on the list and then we can make something like this (where `n` is the `size` of the list):

    n swap mod 0 = [pop] [sqr +] branch

So:

    0 [2 7 1 19 18 3] [F] step

Except we need a counter for the index!

### ≡ `step-enumerate`

How about a version of `step` that also enumerates the list items?

    [...] [F] step-enumerate

If the list is empty, it does nothing:

       [] [F] step-enumerate
    ---------------------------

But if the list has members it starts another function with an index counter initialized to zero.  Since we know the list has at least one element we can set up the first evaluation of `F`:

          [a ...] [F] step-enumerate
    ---------------------------------------
       a 0 F [...] [F] 0 step-enumerate′


so

    step-enumerate == over truthy [popop] [FOO] branch

`FOO`

    [a ...] [F] FOO
    [a ...] [F] [uncons] dip
    a [...] [F] 0 roll<
    a 0 [...] [F] dupdipd
    a 0 F [...] [F] 0 step-enumerate′

Okay, so:

    FOO == [uncons] dip 0 roll< dupdipd 0 step-enumerate′

And therefore:

    [step-enumerate over truthy [popop] [[uncons] dip 0 roll< dupdipd 0 step-enumerate′] branch] inscribe



This function is like `step` but it first increments the counter before each evaluation of `F`.

If the list has become empty, do nothing:

       ... [] [F] n step-enumerate′
    ----------------------------------

If there are terms yet in the list it increments the counter and runs `F` with (a copy of) it:

             ... [b ...] [F] n step-enumerate′
    ---------------------------------------------------
       ... b (n+1) F [...] [F] (n+1) step-enumerate′


### A simple General Recursive Function

                ... [] [F] n step-enumerate′
        -------------------------------------------
           ... [] [F] n [P] [T] [R0] [R1] genrec
    ---------------------------------------------------------
       ... [] [F] n [P] [T] [R0 [step-enumerate′] R1] ifte

Where:

    P == popop not
    
    E == popopop

Now then:

    ... [b ...] [F] n R0 [step-enumerate′] R1

Here's what we're trying to accomplish:

        ... [b ...]          [F]   n   R0 [step-enumerate′] R1
        ...  b (n+1) F [...] [F] (n+1)     step-enumerate′

`R1` is trivially `i` (so this is a `tailrec` function.)  Let's derive `R0`:

    ... [b ...] [F] n R0
    ... [b ...] [F] n ++
    ... [b ...] [F] (n+1)

    ... [b ...] [F] (n+1) [swons] nullary
    ... [b ...] [F] (n+1) [(n+1) F] 


### - - - - -

    ... [b ...] [F] (n+1)
    ... [b ...] [F] (n+1) [uncons] dipd
    ... [b ...] uncons [F] (n+1)
    ... b [...] [F] (n+1)

    ... b [...] [F] (n+1) [swons] nullary
    ... b [...] [F] (n+1) [(n+1) F] dipddd
    ... b (n+1) F [...] [F] (n+1)

It looks like we're done:

    R0 == ++ [uncons] dipd [swons] nullary dipddd

I don't like it, but it should work (provided you write `dipddd`, eh?)


