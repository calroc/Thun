# Generator Programs

## Using `x` to Generate Values

Cf. [Self-reproducing and reproducing programs](https://www.kevinalbrecht.com/code/joy-mirror/jp-reprod.html) by Manfred von Thun

Consider the `x` combinator:

    x == dup i

We can apply it to a quoted program consisting of some value `a` and some function `B`:

    [a B] x
    [a B] a B

Let `B` function `swap` the `a` with the quote and run some function `C` on it to generate a new value `b`:

    B == swap [C] dip

    [a B] a B
    [a B] a swap [C] dip
    a [a B]      [C] dip
    a C [a B]
    b [a B]

Now discard the quoted `a` with `rest` then `cons` `b`:

    b [a B] rest cons
    b [B]        cons
    [b B]

Altogether, this is the definition of `B`:

    B == swap [C] dip rest cons


## An Example

We can make a generator for the Natural numbers (0, 1, 2, ...) by using
`0` for the initial state `a` and `[dup ++]` for `[C]`.
We need the `dup` to leave the old state value behind on the stack.
Putting it together:

    [0 swap [dup ++] dip rest cons]

Let's try it:

    joy? [0 swap [dup ++] dip rest cons]
    [0 swap [dup ++] dip rest cons]
    
    joy? [x]
    [0 swap [dup ++] dip rest cons] [x]
    
    joy? trace
               [0 swap [dup ++] dip rest cons] • x
               [0 swap [dup ++] dip rest cons] • 0 swap [dup ++] dip rest cons
             [0 swap [dup ++] dip rest cons] 0 • swap [dup ++] dip rest cons
             0 [0 swap [dup ++] dip rest cons] • [dup ++] dip rest cons
    0 [0 swap [dup ++] dip rest cons] [dup ++] • dip rest cons
                                             0 • dup ++ [0 swap [dup ++] dip rest cons] rest cons
                                           0 0 • ++ [0 swap [dup ++] dip rest cons] rest cons
                                           0 0 • 1 + [0 swap [dup ++] dip rest cons] rest cons
                                         0 0 1 • + [0 swap [dup ++] dip rest cons] rest cons
                                           0 1 • [0 swap [dup ++] dip rest cons] rest cons
           0 1 [0 swap [dup ++] dip rest cons] • rest cons
             0 1 [swap [dup ++] dip rest cons] • cons
             0 [1 swap [dup ++] dip rest cons] • 

After one application of `x` the quoted program contains 1 and 0 is below it on the stack.

    0 [1 swap [dup ++] dip rest cons]

We can use `x` as many times as we like to get as many terms as we like:

    joy? x x x x x pop
    0 1 2 3 4 5

### `direco`

Let's define a helper function:

    [direco dip rest cons] inscribe

That makes our generator quote into:

    [0 swap [dup ++] direco]


## Making Generators

We want to define a function that accepts `a` and `[C]` and builds our quoted program:

             a [C] G
    -------------------------
       [a swap [C] direco]

Working in reverse:

    [a swap   [C] direco] cons
    a [swap   [C] direco] concat
    a [swap] [[C] direco] swap
    a [[C] direco] [swap]
    a [C] [direco] cons [swap]

Reading from the bottom up:

    [direco] cons [swap] swap concat cons

Or:

    [direco] cons [swap] swoncat cons

### make-generator

    [make-generator [direco] cons [swap] swoncat cons] inscribe

Let's try it out:

    joy? 0 [dup ++] make-generator
    [0 swap [dup ++] direco]

And generate some values:

    joy? x x x pop
    0 1 2

### Powers of Two

Let's generate powers of two:

    joy? 1 [dup 1 <<] make-generator
    [1 swap [dup 1 <<] direco]

We can drive it using `times` with the `x` combinator.

    joy? 10 [x] times pop
    1 2 4 8 16 32 64 128 256 512

## Generating Multiples of Three and Five

Look at the treatment of the Project Euler Problem One in the
[Developing a Program](/notebooks/Developing_a_Program.html)
notebook and you'll see that we might be interested in generating an endless cycle of:

    3 2 1 3 1 2 3

To do this we want to encode the numbers as pairs of bits in a single integer:

    Decimal:    3  2  1  3  1  2  3
    Binary:    11 10 01 11 01 10 11

The number 11100111011011 in binary is 14811 in decimal notation.
We can recover the terms from this number by using `4 divmod`.

    joy? 14811 [4 divmod swap] make-generator
    [14811 swap [4 divmod swap] direco]
    
    joy? x
    3 [3702 swap [4 divmod swap] direco]
    
    joy? x
    3 2 [925 swap [4 divmod swap] direco]
    
    joy? x
    3 2 1 [231 swap [4 divmod swap] direco]
    
    joy? x
    3 2 1 3 [57 swap [4 divmod swap] direco]
    
    joy? x
    3 2 1 3 1 [14 swap [4 divmod swap] direco]
    
    joy? x
    3 2 1 3 1 2 [3 swap [4 divmod swap] direco]
    
    joy? x
    3 2 1 3 1 2 3 [0 swap [4 divmod swap] direco]
    
    joy? x
    3 2 1 3 1 2 3 0 [0 swap [4 divmod swap] direco]
    
    joy? x
    3 2 1 3 1 2 3 0 0 [0 swap [4 divmod swap] direco]
    
    joy? x
    3 2 1 3 1 2 3 0 0 0 [0 swap [4 divmod swap] direco]


...we get a generator that works for seven cycles before it reaches zero.

### Reset at Zero

We need a function that checks if the int has reached zero and resets it if so.
That's easy enough to write:

    ? [pop 14811] [] branch

I don't like that we're checking every time even though we know we
only need to reset the integer every seventh time, but this way we
can include this function in the generator (rather than wrapping the
generator in something to do it only every seventh iteration.) So
the "forcing" function is just `x`.

### `PE1.1.check`

    [PE1.1.check ? [pop 14811] [] branch] inscribe

### `PE1.1`

    [PE1.1 4 divmod swap] inscribe

Now we can `make-generator`:

    joy? 14811 [PE1.1.check PE1.1] make-generator
    [14811 swap [PE1.1.check PE1.1] direco]

We can then "force" the generator with `x` to get as many terms as we like:

    joy? 21 [x] times pop
    3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3

### Run 466 times

In the PE1 problem [PE1 problem](/notebooks/Developing_a_Program.html)
we are asked to sum all the multiples of three and five
less than 1000.  It's worked out that we need to use our cycle of seven numbers
sixty-six times and then four more.

    joy? 7 66 * 4 +
    466

If we drive our generator 466 times and sum the stack we get 999:

    joy? 14811 [PE1.1.check PE1.1] make-generator
    [14811 swap [PE1.1.check PE1.1] direco]

    joy? 466 [x] times pop enstacken sum
    999

If you want to see how this is used read the
[Developing a Program](/notebooks/Developing_a_Program.html) notebook.


## A generator for the Fibonacci Sequence.

Consider:

    [b a F] x
    [b a F] b a F

The obvious first thing to do is just add `b` and `a`:

    [b a F] b a +
    [b a F] b+a

From here we want to arrive at:

    b [b+a b F]

Let's start with `swons`:

    [b a F] b+a swons
    [b+a b a F]

Considering this quote as a stack:

    F a b b+a

We want to get it to:

    F b b+a b

So:

    F a b b+a popdd over
    F b b+a b

And therefore:

    [b+a b a F] [popdd over] infra
    [b b+a b F]

But we can just use `cons` to carry `b+a` into the quote:

    [b a F] b+a [popdd over] cons infra
    [b a F] [b+a popdd over]      infra
    [b b+a b F]

Lastly:

    [b b+a b F] uncons
    b [b+a b F]

Putting it all together:

    F == + [popdd over] cons infra uncons

### `fib-gen`

Let's call `F` `fib-gen`:

    [fib-gen + [popdd over] cons infra uncons] inscribe

We can just write the initial quote and then "force" it with `x`:

    joy? [1 1 fib-gen] 10 [x] times
    1 2 3 5 8 13 21 34 55 89 [144 89 fib-gen]

It skips the first term (1) but if that bothers you you can just prepend it to the program:

    1 [1 1 fib-gen] 10 [x] times


## Project Euler Problem Two

> By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

Now that we have a generator for the Fibonacci sequence, we need a function that adds
a term in the sequence to a sum if it is even, and `pop`s it otherwise.

### `even`

    [even 2 % bool] inscribe

### `PE2.1`

    [PE2.1 dup even [+] [pop] branch] inscribe

And a predicate function that detects when the terms in the series "exceed four million".

### `>4M`

    [>4M 4000000 >] inscribe

Now it's straightforward to define `PE2` as a recursive function that generates terms
in the Fibonacci sequence until they exceed four million and sums the even ones.

    joy? 0 [1 1 fib-gen] x [pop >4M] [popop] [[PE2.1] dip x] tailrec
    4613732

### `PE2`

    [PE2 0 [1 1 fib-gen] x [pop >4M] [popop] [[PE2.1] dip x] tailrec] inscribe

Here's the collected program definitions (with a little editorializing):

    fib-gen + [popdd over] cons infra uncons
    even 2 % bool
    >4M 4000000 >
    PE2.1 dup even [+] [pop] branch
    PE2.2 [PE2.1] dip x
    PE2.init 0 [1 1 fib-gen] x
    PE2.rec [pop >4M] [popop] [PE2.2] tailrec
    PE2 PE2.init PE2.rec


### Hmm...

    fib-gen + swons [popdd over] infra uncons


## Even-valued Fibonacci Terms

Using `o` for odd and `e` for even:

    o + o = e
    e + e = e
    o + e = o

So the Fibonacci sequence considered in terms of just parity would be:

    o o e o o e  o  o  e  o  o   e . . .
    1 1 2 3 5 8 13 21 34 55 89 144 . . .

Every third term is even.

So what if we drive the generator three times and discard the odd terms?
We would have to initialize our `fib` generator with 1 0:

    [1 0 fib-gen]

### `third-term`

    [third-term x x x [popop] dipd] inscribe

So:

    joy? [1 0 fib-gen]
    [1 0 fib-gen]
    
    joy? third-term
    2 [3 2 fib-gen]
    
    joy? third-term
    2 8 [13 8 fib-gen]
    
    joy? third-term
    2 8 34 [55 34 fib-gen]
    
    joy? third-term
    2 8 34 144 [233 144 fib-gen]

So now we need a sum:

    joy? 0
    0

And our Fibonacci generator:

    joy? [1 0 fib-gen]
    0 [1 0 fib-gen]

We want to generate the initial term:

    joy? third-term
    0 2 [3 2 fib-gen]

Now we check if the term is less than four million,
if so we add it and recur,
otherwise we discard the term and the generator leaving the sum on the stack:

    joy? [pop >4M] [popop] [[PE2.1] dip third-term] tailrec
    4613732

## Math

    a      b
    b      a+b
    a+b    a+b+b
    a+b+b  a+a+b+b+b

So if (a,b) and a is even then the next even term pair is (a+2b, 2a+3b)

Reconsider:

    [b a F] x
    [b a F] b a F

From here we want to arrive at:

    (a+2b) [(2a+3b) (a+2b) F]

    b a F
    b a [F0] [F1] fork

       b a over [+] ii
    ---------------------
            a+2b

And:

       b a over [dup + +] ii
    ---------------------------
              2a+3b


    [over [dup + +] ii] [over [+] ii] clop
    roll< rrest [tuck] dip ccons


    [b a F] b a F

    [b a F] (2a+3b) (a+2b) roll<
    (2a+3b) (a+2b) [b a F] rrest
    (2a+3b) (a+2b) [F] [tuck] dip ccons


    joy? [1 0 [over [dup + +] ii] [over [+] ii] clop roll< rrest [tuck] dip ccons]
    [1 0 [over [dup + +] ii] [over [+] ii] clop roll< rrest [tuck] dip ccons]
    
    joy? x
    2 [3 2 [over [dup + +] ii] [over [+] ii] clop roll< rrest [tuck] dip ccons]
    
    joy? x
    2 8 [13 8 [over [dup + +] ii] [over [+] ii] clop roll< rrest [tuck] dip ccons]
    
    joy? x
    2 8 34 [55 34 [over [dup + +] ii] [over [+] ii] clop roll< rrest [tuck] dip ccons]
    
    joy? x
    2 8 34 144 [233 144 [over [dup + +] ii] [over [+] ii] clop roll< rrest [tuck] dip ccons]

And so it goes...

