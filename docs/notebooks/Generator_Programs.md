# Using `x` to Generate Values

Cf. jp-reprod.html


```python
from notebook_preamble import J, V, define
```

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

We can make a generator for the Natural numbers (0, 1, 2, ...) by using `0` for `a` and `[dup ++]` for `[C]`:

    [0 swap [dup ++] dip rest cons]

Let's try it:


```python
V('[0 swap [dup ++] dip rest cons] x')
```

                                               . [0 swap [dup ++] dip rest cons] x
               [0 swap [dup ++] dip rest cons] . x
               [0 swap [dup ++] dip rest cons] . 0 swap [dup ++] dip rest cons
             [0 swap [dup ++] dip rest cons] 0 . swap [dup ++] dip rest cons
             0 [0 swap [dup ++] dip rest cons] . [dup ++] dip rest cons
    0 [0 swap [dup ++] dip rest cons] [dup ++] . dip rest cons
                                             0 . dup ++ [0 swap [dup ++] dip rest cons] rest cons
                                           0 0 . ++ [0 swap [dup ++] dip rest cons] rest cons
                                           0 1 . [0 swap [dup ++] dip rest cons] rest cons
           0 1 [0 swap [dup ++] dip rest cons] . rest cons
             0 1 [swap [dup ++] dip rest cons] . cons
             0 [1 swap [dup ++] dip rest cons] . 


After one application of `x` the quoted program contains `1` and `0` is below it on the stack.


```python
J('[0 swap [dup ++] dip rest cons] x x x x x pop')
```

    0 1 2 3 4


## `direco`


```python
define('direco == dip rest cons')
```


```python
V('[0 swap [dup ++] direco] x')
```

                                        . [0 swap [dup ++] direco] x
               [0 swap [dup ++] direco] . x
               [0 swap [dup ++] direco] . 0 swap [dup ++] direco
             [0 swap [dup ++] direco] 0 . swap [dup ++] direco
             0 [0 swap [dup ++] direco] . [dup ++] direco
    0 [0 swap [dup ++] direco] [dup ++] . direco
    0 [0 swap [dup ++] direco] [dup ++] . dip rest cons
                                      0 . dup ++ [0 swap [dup ++] direco] rest cons
                                    0 0 . ++ [0 swap [dup ++] direco] rest cons
                                    0 1 . [0 swap [dup ++] direco] rest cons
           0 1 [0 swap [dup ++] direco] . rest cons
             0 1 [swap [dup ++] direco] . cons
             0 [1 swap [dup ++] direco] . 


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

    G == [direco] cons [swap] swap concat cons
    G == [direco] cons [swap] swoncat cons


```python
define('G == [direco] cons [swap] swoncat cons')
```

Let's try it out:


```python
J('0 [dup ++] G')
```

    [0 swap [dup ++] direco]



```python
J('0 [dup ++] G x x x pop')
```

    0 1 2


### Powers of 2


```python
J('1 [dup 1 <<] G x x x x x x x x x pop')
```

    1 2 4 8 16 32 64 128 256


### `[x] times`
If we have one of these quoted programs we can drive it using `times` with the `x` combinator.


```python
J('23 [dup ++] G 5 [x] times')
```

    23 24 25 26 27 [28 swap [dup ++] direco]


## Generating Multiples of Three and Five
Look at the treatment of the Project Euler Problem One in the "Developing a Program" notebook and you'll see that we might be interested in generating an endless cycle of:

    3 2 1 3 1 2 3

To do this we want to encode the numbers as pairs of bits in a single int:

        3  2  1  3  1  2  3
    0b 11 10 01 11 01 10 11 == 14811

And pick them off by masking with 3 (binary 11) and then shifting the int right two bits.


```python
define('PE1.1 == dup [3 &] dip 2 >>')
```


```python
V('14811 PE1.1')
```

                      . 14811 PE1.1
                14811 . PE1.1
                14811 . dup [3 &] dip 2 >>
          14811 14811 . [3 &] dip 2 >>
    14811 14811 [3 &] . dip 2 >>
                14811 . 3 & 14811 2 >>
              14811 3 . & 14811 2 >>
                    3 . 14811 2 >>
              3 14811 . 2 >>
            3 14811 2 . >>
               3 3702 . 


If we plug `14811` and `[PE1.1]` into our generator form...


```python
J('14811 [PE1.1] G')
```

    [14811 swap [PE1.1] direco]


...we get a generator that works for seven cycles before it reaches zero:


```python
J('[14811 swap [PE1.1] direco] 7 [x] times')
```

    3 2 1 3 1 2 3 [0 swap [PE1.1] direco]


### Reset at Zero
We need a function that checks if the int has reached zero and resets it if so.


```python
define('PE1.1.check == dup [pop 14811] [] branch')
```


```python
J('14811 [PE1.1.check PE1.1] G')
```

    [14811 swap [PE1.1.check PE1.1] direco]



```python
J('[14811 swap [PE1.1.check PE1.1] direco] 21 [x] times')
```

    3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 [0 swap [PE1.1.check PE1.1] direco]


(It would be more efficient to reset the int every seven cycles but that's a little beyond the scope of this article.  This solution does extra work, but not much, and we're not using it "in production" as they say.)

### Run 466 times
In the PE1 problem we are asked to sum all the multiples of three and five less than 1000.  It's worked out that we need to use all seven numbers sixty-six times and then four more.


```python
J('7 66 * 4 +')
```

    466


If we drive our generator 466 times and sum the stack we get 999.


```python
J('[14811 swap [PE1.1.check PE1.1] direco] 466 [x] times')
```

    3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 1 2 3 3 2 1 3 [57 swap [PE1.1.check PE1.1] direco]



```python
J('[14811 swap [PE1.1.check PE1.1] direco] 466 [x] times pop enstacken sum')
```

    999


## Project Euler Problem One


```python
define('PE1.2 == + dup [+] dip')
```

Now we can add `PE1.2` to the quoted program given to `G`.


```python
J('0 0 0 [PE1.1.check PE1.1] G 466 [x [PE1.2] dip] times popop')
```

    233168


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
    fib_gen == [1 1 F]


```python
define('fib == + [popdd over] cons infra uncons')
```


```python
define('fib_gen == [1 1 fib]')
```


```python
J('fib_gen 10 [x] times')
```

    1 2 3 5 8 13 21 34 55 89 [144 89 fib]


## Project Euler Problem Two
> By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

Now that we have a generator for the Fibonacci sequence, we need a function that adds a term in the sequence to a sum if it is even, and `pop`s it otherwise.


```python
define('PE2.1 == dup 2 % [+] [pop] branch')
```

And a predicate function that detects when the terms in the series "exceed four million".


```python
define('>4M == 4000000 >')
```

Now it's straightforward to define `PE2` as a recursive function that generates terms in the Fibonacci sequence until they exceed four million and sums the even ones.


```python
define('PE2 == 0 fib_gen x [pop >4M] [popop] [[PE2.1] dip x] primrec')
```


```python
J('PE2')
```

    4613732


Here's the collected program definitions:

    fib == + swons [popdd over] infra uncons
    fib_gen == [1 1 fib]

    even == dup 2 %
    >4M == 4000000 >

    PE2.1 == even [+] [pop] branch
    PE2 == 0 fib_gen x [pop >4M] [popop] [[PE2.1] dip x] primrec

### Even-valued Fibonacci Terms

Using `o` for odd and `e` for even:

    o + o = e
    e + e = e
    o + e = o

So the Fibonacci sequence considered in terms of just parity would be:

    o o e o o e o o e o o e o o e o o e
    1 1 2 3 5 8 . . .

Every third term is even.



```python
J('[1 0 fib] x x x')  # To start the sequence with 1 1 2 3 instead of 1 2 3.
```

    1 1 2 [3 2 fib]


Drive the generator three times and `popop` the two odd terms.


```python
J('[1 0 fib] x x x [popop] dipd')
```

    2 [3 2 fib]



```python
define('PE2.2 == x x x [popop] dipd')
```


```python
J('[1 0 fib] 10 [PE2.2] times')
```

    2 8 34 144 610 2584 10946 46368 196418 832040 [1346269 832040 fib]


Replace `x` with our new driver function `PE2.2` and start our `fib` generator at `1 0`.


```python
J('0 [1 0 fib] PE2.2 [pop >4M] [popop] [[PE2.1] dip PE2.2] primrec')
```

    4613732


## How to compile these?
You would probably start with a special version of `G`, and perhaps modifications to the default `x`?

## An Interesting Variation


```python
define('codireco == cons dip rest cons')
```


```python
V('[0 [dup ++] codireco] x')
```

                                     . [0 [dup ++] codireco] x
               [0 [dup ++] codireco] . x
               [0 [dup ++] codireco] . 0 [dup ++] codireco
             [0 [dup ++] codireco] 0 . [dup ++] codireco
    [0 [dup ++] codireco] 0 [dup ++] . codireco
    [0 [dup ++] codireco] 0 [dup ++] . cons dip rest cons
    [0 [dup ++] codireco] [0 dup ++] . dip rest cons
                                     . 0 dup ++ [0 [dup ++] codireco] rest cons
                                   0 . dup ++ [0 [dup ++] codireco] rest cons
                                 0 0 . ++ [0 [dup ++] codireco] rest cons
                                 0 1 . [0 [dup ++] codireco] rest cons
           0 1 [0 [dup ++] codireco] . rest cons
             0 1 [[dup ++] codireco] . cons
             0 [1 [dup ++] codireco] . 



```python
define('G == [codireco] cons cons')
```


```python
J('230 [dup ++] G 5 [x] times pop')
```

    230 231 232 233 234

