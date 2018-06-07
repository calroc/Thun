
# [Quadratic formula](https://en.wikipedia.org/wiki/Quadratic_formula)


```python
from notebook_preamble import J, V, define
```

Cf. [jp-quadratic.html](http://www.kevinalbrecht.com/code/joy-mirror/jp-quadratic.html)

             -b  +/- sqrt(b^2 - 4 * a * c)
             -----------------------------
                        2 * a

$\frac{-b  \pm \sqrt{b^2 - 4ac}}{2a}$

## Write a straightforward program with variable names.

    b neg b sqr 4 a c * * - sqrt [+] [-] cleave a 2 * [truediv] cons app2

We use `cleave` to compute the sum and difference and then `app2` to finish computing both roots using a quoted program `[2a truediv]` built with `cons`.

### Check it.
Evaluating by hand:

     b neg b sqr 4 a c * * - sqrt [+] [-] cleave a 2 * [truediv] cons app2
    -b     b sqr 4 a c * * - sqrt [+] [-] cleave a 2 * [truediv] cons app2
    -b     b^2   4 a c * * - sqrt [+] [-] cleave a 2 * [truediv] cons app2
    -b     b^2 4ac         - sqrt [+] [-] cleave a 2 * [truediv] cons app2
    -b     b^2-4ac           sqrt [+] [-] cleave a 2 * [truediv] cons app2
    -b sqrt(b^2-4ac)              [+] [-] cleave a 2 * [truediv] cons app2

    -b -b+sqrt(b^2-4ac)    -b-sqrt(b^2-4ac)    a 2 * [truediv] cons app2
    -b -b+sqrt(b^2-4ac)    -b-sqrt(b^2-4ac)    2a    [truediv] cons app2
    -b -b+sqrt(b^2-4ac)    -b-sqrt(b^2-4ac)    [2a truediv]         app2
    -b -b+sqrt(b^2-4ac)/2a -b-sqrt(b^2-4ac)/2a

(Eventually we’ll be able to use e.g. Sympy versions of the Joy commands to do this sort of thing symbolically. This is part of what is meant by a “categorical” language.)

### Cleanup
    -b -b+sqrt(b^2-4ac)/2a -b-sqrt(b^2-4ac)/2a                          roll< pop
       -b+sqrt(b^2-4ac)/2a -b-sqrt(b^2-4ac)/2a -b                             pop
       -b+sqrt(b^2-4ac)/2a -b-sqrt(b^2-4ac)/2a

## Derive a definition.

    b neg b           sqr 4 a c        * * - sqrt [+] [-] cleave a       2 * [truediv] cons app2 roll< pop
    b    [neg] dupdip sqr 4 a c        * * - sqrt [+] [-] cleave a       2 * [truediv] cons app2 roll< pop
    b a c    [[neg] dupdip sqr 4] dipd * * - sqrt [+] [-] cleave a       2 * [truediv] cons app2 roll< pop
    b a c a    [[[neg] dupdip sqr 4] dipd * * - sqrt [+] [-] cleave] dip 2 * [truediv] cons app2 roll< pop
    b a c over [[[neg] dupdip sqr 4] dipd * * - sqrt [+] [-] cleave] dip 2 * [truediv] cons app2 roll< pop


```python
define('quadratic == over [[[neg] dupdip sqr 4] dipd * * - sqrt [+] [-] cleave] dip 2 * [truediv] cons app2 roll< pop')
```


```python
J('3 1 1 quadratic')
```

    -0.3819660112501051 -2.618033988749895


## Simplify
We can define a `pm` plus-or-minus function:

    pm == [+] [-] cleave popdd

Then `quadratic` becomes:


```python
define('quadratic == over [[[neg] dupdip sqr 4] dipd * * - sqrt pm] dip 2 * [truediv] cons app2')
```


```python
J('3 1 1 quadratic')
```

    -0.3819660112501051 -2.618033988749895


### Define a "native" `pm` function.
The definition of `pm` above is pretty elegant, but the implementation takes a lot of steps relative to what it's accomplishing.  Since we are likely to use `pm` more than once in the future, let's write a primitive in Python and add it to the dictionary.  (This has been done already.)


```python
def pm(stack):
    a, (b, stack) = stack
    p, m, = b + a, b - a
    return m, (p, stack)
```

The resulting trace is short enough to fit on a page.


```python
V('3 1 1 quadratic')
```

                                                        . 3 1 1 quadratic
                                                      3 . 1 1 quadratic
                                                    3 1 . 1 quadratic
                                                  3 1 1 . quadratic
                                                  3 1 1 . over [[[neg] dupdip sqr 4] dipd * * - sqrt pm] dip 2 * [truediv] cons app2
                                                3 1 1 1 . [[[neg] dupdip sqr 4] dipd * * - sqrt pm] dip 2 * [truediv] cons app2
      3 1 1 1 [[[neg] dupdip sqr 4] dipd * * - sqrt pm] . dip 2 * [truediv] cons app2
                                                  3 1 1 . [[neg] dupdip sqr 4] dipd * * - sqrt pm 1 2 * [truediv] cons app2
                             3 1 1 [[neg] dupdip sqr 4] . dipd * * - sqrt pm 1 2 * [truediv] cons app2
                                                      3 . [neg] dupdip sqr 4 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                                3 [neg] . dupdip sqr 4 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                                      3 . neg 3 sqr 4 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                                     -3 . 3 sqr 4 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                                   -3 3 . sqr 4 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                                   -3 3 . dup mul 4 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                                 -3 3 3 . mul 4 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                                   -3 9 . 4 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                                 -3 9 4 . 1 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                               -3 9 4 1 . 1 * * - sqrt pm 1 2 * [truediv] cons app2
                                             -3 9 4 1 1 . * * - sqrt pm 1 2 * [truediv] cons app2
                                               -3 9 4 1 . * - sqrt pm 1 2 * [truediv] cons app2
                                                 -3 9 4 . - sqrt pm 1 2 * [truediv] cons app2
                                                   -3 5 . sqrt pm 1 2 * [truediv] cons app2
                                    -3 2.23606797749979 . pm 1 2 * [truediv] cons app2
                  -0.7639320225002102 -5.23606797749979 . 1 2 * [truediv] cons app2
                -0.7639320225002102 -5.23606797749979 1 . 2 * [truediv] cons app2
              -0.7639320225002102 -5.23606797749979 1 2 . * [truediv] cons app2
                -0.7639320225002102 -5.23606797749979 2 . [truediv] cons app2
      -0.7639320225002102 -5.23606797749979 2 [truediv] . cons app2
      -0.7639320225002102 -5.23606797749979 [2 truediv] . app2
                      [-0.7639320225002102] [2 truediv] . infra first [-5.23606797749979] [2 truediv] infra first
                                    -0.7639320225002102 . 2 truediv [] swaack first [-5.23606797749979] [2 truediv] infra first
                                  -0.7639320225002102 2 . truediv [] swaack first [-5.23606797749979] [2 truediv] infra first
                                    -0.3819660112501051 . [] swaack first [-5.23606797749979] [2 truediv] infra first
                                 -0.3819660112501051 [] . swaack first [-5.23606797749979] [2 truediv] infra first
                                  [-0.3819660112501051] . first [-5.23606797749979] [2 truediv] infra first
                                    -0.3819660112501051 . [-5.23606797749979] [2 truediv] infra first
                -0.3819660112501051 [-5.23606797749979] . [2 truediv] infra first
    -0.3819660112501051 [-5.23606797749979] [2 truediv] . infra first
                                      -5.23606797749979 . 2 truediv [-0.3819660112501051] swaack first
                                    -5.23606797749979 2 . truediv [-0.3819660112501051] swaack first
                                     -2.618033988749895 . [-0.3819660112501051] swaack first
               -2.618033988749895 [-0.3819660112501051] . swaack first
               -0.3819660112501051 [-2.618033988749895] . first
                 -0.3819660112501051 -2.618033988749895 . 

