
# [Newton's method](https://en.wikipedia.org/wiki/Newton%27s_method)
Let's use the Newton-Raphson method for finding the root of an equation to write a function that can compute the square root of a number.

Cf. ["Why Functional Programming Matters" by John Hughes](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)


```python
from notebook_preamble import J, V, define
```

## A Generator for Approximations

To make a generator that generates successive approximations let’s start by assuming an initial approximation and then derive the function that computes the next approximation:

       a F
    ---------
        a'

### A Function to Compute the Next Approximation

This is the equation for computing the next approximate value of the square root:

$a_{i+1} = \frac{(a_i+\frac{n}{a_i})}{2}$

    a n over / + 2 /
    a n a    / + 2 /
    a n/a      + 2 /
    a+n/a        2 /
    (a+n/a)/2

The function we want has the argument `n` in it:

    F == n over / + 2 /

### Make it into a Generator

Our generator would be created by:

    a [dup F] make_generator

With n as part of the function F, but n is the input to the sqrt function we’re writing. If we let 1 be the initial approximation:

    1 n 1 / + 2 /
    1 n/1   + 2 /
    1 n     + 2 /
    n+1       2 /
    (n+1)/2

The generator can be written as:

    23 1 swap  [over / + 2 /] cons [dup] swoncat make_generator
    1 23       [over / + 2 /] cons [dup] swoncat make_generator
    1       [23 over / + 2 /]      [dup] swoncat make_generator
    1   [dup 23 over / + 2 /]                    make_generator


```python
define('codireco == cons dip rest cons')
define('make_generator == [codireco] ccons')
define('ccons == cons cons')
```


```python
define('gsra == 1 swap [over / + 2 /] cons [dup] swoncat make_generator')
```


```python
J('23 gsra')
```

    [1 [dup 23 over / + 2 /] codireco]


Let's drive the generator a few time (with the `x` combinator) and square the approximation to see how well it works...


```python
J('23 gsra 6 [x popd] times first sqr')
```

    23.0000000001585


## Finding Consecutive Approximations within a Tolerance


> The remainder of a square root finder is a function _within_, which takes a tolerance and a list of approximations and looks down the list for two successive approximations that differ by no more than the given tolerance.

From ["Why Functional Programming Matters" by John Hughes](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)

(And note that by “list” he means a lazily-evaluated list.)

Using the _output_ `[a G]` of the above generator for square root approximations, and further assuming that the first term a has been generated already and epsilon ε is handy on the stack...

       a [b G] ε within
    ---------------------- a b - abs ε <=
          b


       a [b G] ε within
    ---------------------- a b - abs ε >
       b [c G] ε within



### Predicate

    a [b G]             ε [first - abs] dip <=
    a [b G] first - abs ε                   <=
    a b           - abs ε                   <=
    a-b             abs ε                   <=
    abs(a-b)            ε                   <=
    (abs(a-b)<=ε)


```python
define('_within_P == [first - abs] dip <=')
```

### Base-Case

    a [b G] ε roll< popop first
      [b G] ε a     popop first
      [b G]               first
       b


```python
define('_within_B == roll< popop first')
```

### Recur

    a [b G] ε R0 [within] R1

1. Discard a.
2. Use x combinator to generate next term from G.
3. Run within with `i` (it is a `primrec` function.)

Pretty straightforward:

    a [b G]        ε R0           [within] R1
    a [b G]        ε [popd x] dip [within] i
    a [b G] popd x ε              [within] i
      [b G]      x ε              [within] i
    b [c G]        ε              [within] i
    b [c G]        ε               within

    b [c G] ε within


```python
define('_within_R == [popd x] dip')
```

### Setting up

The recursive function we have defined so far needs a slight preamble: `x` to prime the generator and the epsilon value to use:

    [a G] x ε ...
    a [b G] ε ...


```python
define('within == x 0.000000001 [_within_P] [_within_B] [_within_R] primrec')
define('sqrt == gsra within')
```


```python
J('23 sqrt')
```

    4.795831523312719



```python
4.795831523312719**2
```




    22.999999999999996


