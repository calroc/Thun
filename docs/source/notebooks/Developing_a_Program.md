# Developing a Joy Program

As a first attempt at writing software in Joy let's tackle
[Project Euler, first problem: "Multiples of 3 and 5"](https://projecteuler.net/problem=1):

> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

## Filter

Let's create a predicate that returns `true` if a number is a multiple of 3 or 5 and `false` otherwise.

### `multiple-of`

First we write `multiple-of` which take two numbers and return true if the first is a multiple of the second.  We use the `mod` operator, convert the remainder to a Boolean value, then invert it to get our answer:

    [multiple-of mod bool not] inscribe

### `multiple-of-3-or-5`

Next we can use that with `app2` to get both Boolean values (for 3 and 5) and then use the logical OR function `\/` on them.  (We use `popd` to get rid of the original number):

    [multiple-of-3-or-5 3 5 [multiple-of] app2 \/ popd] inscribe

Here it is in action:

    joy? [6 7 8 9 10] [multiple-of-3-or-5] map
    
    [true false false true true]

Given the predicate function `multiple-of-3-or-5` a suitable program would be:

    1000 range [multiple-of-3-or-5] filter sum

This function generates a list of the integers from 0 to 999, filters
that list by `multiple-of-3-or-5`, and then sums the result.  (I should
mention that the `filter` function has not yet been implemented.)

Logically this is fine, but pragmatically we are doing more work than we
should.  We generate one thousand integers but actually use less than
half of them.  A better solution would be to generate just the multiples
we want to sum, and to add them as we go rather than storing them and
adding summing them at the end.

At first I had the idea to use two counters and increase them by three
and five, respectively.  This way we only generate the terms that we
actually want to sum.  We have to proceed by incrementing the counter
that is lower, or if they are equal, the three counter, and we have to
take care not to double add numbers like 15 that are multiples of both
three and five.

This seemed a little clunky, so I tried a different approach.


## Looking for Pattern

Consider the first few terms in the series:

    3 5 6 9 10 12 15 18 20 21 ...

Subtract each number from the one after it (subtracting 0 from 3):

    3 5 6 9 10 12 15 18 20 21 24 25 27 30 ...
    0 3 5 6  9 10 12 15 18 20 21 24 25 27 ...
    -------------------------------------------
    3 2 1 3  1  2  3  3  2  1  3  1  2  3 ...

You get this lovely repeating palindromic sequence:

    3 2 1 3 1 2 3

To make a counter that increments by factors of 3 and 5 you just add
these differences to the counter one-by-one in a loop.

### Increment a Counter

To make use of this sequence to increment a counter and sum terms as we
go we need a function that will accept the sum, the counter, and the next
term to add, and that adds the term to the counter and a copy of the
counter to the running sum.  This function will do that:

    + [+] dupdip

We start with a sum, the counter, and a term to add:

    joy? 0 0 3
    0 0 3

Here is our function, quoted to let it be run with the `trace` combinator
(which is like `i` but it also prints a trace of evaluation to `stdout`.)

    joy? [+ [+] dupdip]
    0 0 3 [+ [+] dupdip]

And here we go:

    joy? trace
    
      0 0 3 • + [+] dupdip
        0 3 • [+] dupdip
    0 3 [+] • dupdip
        0 3 • + 3
          3 • 3
        3 3 • 
    
    3 3


### `PE1.1`

We can `inscribe` it for use in later definitions:

    [PE1.1 + [+] dupdip] inscribe

Let's try it out on our palindromic sequence:

    joy? 0 0 [3 2 1 3 1 2 3] [PE1.1] step
    60 15

So one `step` through all seven terms brings the counter to 15 and the total to 60.


### How Many Times?

We want all the terms less than 1000, and each pass through the palindromic sequence
will count off 15 so how many is that?

    joy? 1000 15 /
    66

So 66 × 15 bring us to...

    66
    joy? 15 *
    990

That means we want to run the full list of numbers sixty-six times to get to 990 and then, obviously, the first four terms of the palindromic sequence, 3 2 1 3, to get to 999.

Start with the sum and counter:

    joy? 0 0
    0 0

We will run a program sixty-six times:

    joy? 66
    0 0 66

This is the program we will run sixty-six times, it steps through the
palindromic sequence and sums up the terms:

    joy? [[3 2 1 3 1 2 3] [PE1.1] step]
    0 0 66 [[3 2 1 3 1 2 3] [PE1.1] step]

Runing that brings us to the sum of the numbers less than 991:

    joy? times
    229185 990

We need to count 9 more to reach the sum of the numbers less than 1000:

    joy? [3 2 1 3] [PE1.1] step
    233168 999

All that remains is the counter, which we can discard:

    joy? pop
    233168

And so we have our answer: **233168**

This form uses no extra storage and produces no unused summands.  It's
good but there's one more trick we can apply.


## A Slight Increase of Efficiency

The list of seven terms
takes up at least seven bytes for themselves and a few bytes for their list.
But notice that all of the terms are less
than four, and so each can fit in just two bits.  We could store all
seven terms in just fourteen bits and use masking and shifts to pick out
each term as we go.  This will use less space and save time loading whole
integer terms from the list.

Let's encode the term in 7 × 2 = 14 bits:

    Decimal:    3  2  1  3  1  2  3
    Binary:    11 10 01 11 01 10 11

The number 11100111011011 in binary is 14811 in decimal notation.

### Recovering the Terms

We can recover the terms from this number by `4 divmod`:

    joy? 14811
    14811

    joy? 4 divmod
    3702 3

We want the term below the rest of the terms:

    joy? swap
    3 3702


### `PE1.2`

Giving us `4 divmod swap`:

    [PE1.2 4 divmod swap] inscribe

    joy? 14811
    14811
    joy? PE1.2
    3 3702
    joy? PE1.2
    3 2 925
    joy? PE1.2
    3 2 1 231
    joy? PE1.2
    3 2 1 3 57
    joy? PE1.2
    3 2 1 3 1 14
    joy? PE1.2
    3 2 1 3 1 2 3
    joy? PE1.2
    3 2 1 3 1 2 3 0

So we want:

    joy? 0 0 14811 PE1.2
    0 0 3 3702

And then:

    joy? [PE1.1] dip
    3 3 3702

Continuing:

    joy? PE1.2 [PE1.1] dip
    8 5 925
    
    joy? PE1.2 [PE1.1] dip
    14 6 231
    
    joy? PE1.2 [PE1.1] dip
    23 9 57
    
    joy? PE1.2 [PE1.1] dip
    33 10 14
    
    joy? PE1.2 [PE1.1] dip
    45 12 3
    
    joy? PE1.2 [PE1.1] dip
    60 15 0


### `PE1.3`

Let's define:

    [PE1.3 PE1.2 [PE1.1] dip] inscribe

Now:

    14811 7 [PE1.3] times pop

Will add up one set of the palindromic sequence of terms:

    joy? 0 0 
    0 0
    
    joy? 14811 7 [PE1.3] times pop
    60 15

And we want to do that sixty-six times:

    joy? 0 0 
    0 0
    
    joy? 66 [14811 7 [PE1.3] times pop] times
    229185 990

And then four more:

    joy? 14811 4 [PE1.3] times pop
    233168 999

And discard the counter:

    joy? pop
    233168

*Violà!*


### Let's refactor.

From these two:

    14811 7 [PE1.3] times pop
    14811 4 [PE1.3] times pop

We can generalize the loop counter:

    14811 n [PE1.3] times pop

And use `swap` to put it to the left...

    n 14811 swap [PE1.3] times pop

### `PE1.4`

...and so we have a new definition:

    [PE1.4 14811 swap [PE1.3] times pop] inscribe

Now we can simplify the program from:

    0 0 66 [14811 7 [PE1.3] times pop] times 14811 4 [PE1.3] times pop pop

To:

    0 0 66 [7 PE1.4] times 4 PE1.4 pop

Let's run it and see:

    joy? 0 0 66 [7 PE1.4] times 4 PE1.4 pop
    233168

### `PE1`

    [PE1 0 0 66 [7 PE1.4] times 4 PE1.4 pop] inscribe

    joy? PE1
    233168

Here's our joy program all in one place, as it might appear in `def.txt`:

    PE1.1 + [+] dupdip
    PE1.2 4 divmod swap
    PE1.3 PE1.2 [PE1.1] dip
    PE1.4 14811 swap [PE1.3] times pop
    PE1   0 0 66 [7 PE1.4] times 4 PE1.4 pop


## Generator Version

It's a little clunky iterating sixty-six times though the seven numbers then four more.  In the _Generator Programs_ notebook we derive a generator that can be repeatedly driven by the `x` combinator to produce a stream of the seven numbers repeating over and over again.

Here it is again:

    [0 swap [? [pop 14811] [] branch PE1.2] dip rest cons]

This self-referential quote contains a bit of state (the initial 0) and a "step"
function (the rest of the quote) and can be "forced" by the `x` combinator to
produce successive terms of our palindromic sequence.  The `branch` sub-expression
resets the integer that encodes the terms when it reaches 0.

Let's `inscribe` this generator quote to keep it handy.

### `PE1.terms`

    [PE1.terms [0 swap [? [pop 14811] [] branch PE1.2] dip rest cons]] inscribe

Let's try it out:

    joy? PE1.terms 21 [x] times pop
    3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3

Pretty neat, eh?

### How Many Terms?

We know from above that we need sixty-six times seven then four more terms to reach up to but not over one thousand.

    joy? 7 66 * 4 +
    466


### Here they are...

    joy? PE1.terms 466 [x] times pop

> 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3

### ...and they do sum to 999.

    joy? [PE1.terms 466 [x] times pop] run sum
    999

Now we can use `PE1.1` to accumulate the terms as we go, and then `pop` the
generator and the counter from the stack when we're done, leaving just the sum.

    joy? 0 0 PE1.terms 466 [x [PE1.1] dip] times popop
    233168


## A Little Further Analysis...

A little further analysis renders iteration unnecessary.
Consider finding the sum of the positive integers less than or equal to ten.

    joy? [10 9 8 7 6 5 4 3 2 1] sum
    55

Gauss famously showed that you can find the sum directly with a simple equation.

[Observe](https://en.wikipedia.org/wiki/File:Animated_proof_for_the_formula_giving_the_sum_of_the_first_integers_1%2B2%2B...%2Bn.gif):

      10  9  8  7  6
    +  1  2  3  4  5
    ---- -- -- -- --
      11 11 11 11 11
      
      11 × 5 = 55

The sum of the first N positive integers is:

    (𝑛 + 1) × 𝑛 / 2 

In Joy this equation could be expressed as:

    dup ++ * 2 /

(Note that `(𝑛 + 1) × 𝑛` will always be an even number.)

### Generalizing to Blocks of Terms

We can apply the same reasoning to the `PE1` problem.

Recall that between 1 and 990 inclusive there are sixty-six "blocks" of seven terms each, starting with:

    3 5 6 9 10 12 15
    
And ending with:

    978 980 981 984 985 987 990
    
If we reverse one of these two blocks and sum pairs...

    joy? [3 5 6 9 10 12 15] reverse
    [15 12 10 9 6 5 3]
    
    joy? [978 980 981 984 985 987 990] 
    [15 12 10 9 6 5 3] [978 980 981 984 985 987 990]
    
    joy? zip
    [[978 15] [980 12] [981 10] [984 9] [985 6] [987 5] [990 3]]
    
    joy? [sum] map
    [993 992 991 993 991 992 993]

(Interesting that the sequence of seven numbers appears again in the rightmost digit of each term.)

...and then sum the sums...
    
    joy? sum
    6945

We arrive at 6945.

### Pair Up the Blocks

Since there are sixty-six blocks and we are pairing them up, there must be thirty-three pairs, each of which sums to 6945.

    6945
    joy? 33 *
    229185

We also have those four additional terms between 990 and 1000, they are unpaired:

    993 995 996 999

### A Simple Solution

I think we can just use those as is, so we can give the "sum of all the multiples of 3 or 5 below 1000" like so:

    joy? 6945 33 * [993 995 996 999] cons sum
    233168


### Generalizing

It's worth noting, I think, that this same reasoning holds for any two numbers 𝑛 and 𝑚 the multiples of which we hope to sum.  The multiples would have a cycle of differences of length 𝑘 and so we could compute the sum of 𝑁𝑘 multiples as above.

The sequence of differences will always be a palindrome.  Consider an interval spanning the least common multiple of 𝑛 and 𝑚:

    |   |   |   |   |   |   |   |
    |      |      |      |      |
    
Here we have 4 and 7, and you can read off the sequence of differences directly from the diagram: 4 3 1 4 2 2 4 1 3 4.
    
Geometrically, the actual values of 𝑛 and 𝑚 and their *least common multiple* don't matter, the pattern they make will always be symmetrical around its midpoint.  The same reasoning holds for multiples of more than two numbers.

# The Simplest Program

Of course, having done all that, the simplest joy program for the first Project Euler problem is just:

    [PE1 233168] inscribe

Fin.
