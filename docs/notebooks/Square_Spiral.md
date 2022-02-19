# Square Spiral Example Joy Code


Here is the example of Joy code from the `README` file:

    [[[abs]ii <=][[<>][pop !-]||]&&][[!-][[++]][[--]]ifte dip][[pop !-][--][++]ifte]ifte

It might seem unreadable but with a little familiarity it becomes just as
legible as any other notation.  Some layout helps:

    [   [[abs] ii <=]
        [
            [<>] [pop !-] ||
        ] &&
    ]
    [[    !-] [[++]] [[--]] ifte dip]
    [[pop !-]  [--]   [++]  ifte    ]
    ifte

This function accepts two integers on the stack and increments or
decrements one of them such that the new pair of numbers is the next
coordinate pair in a square spiral (like the kind used to construct an
Ulam Spiral).  



## Original Form

It's adapted from [the original code on StackOverflow](https://stackoverflow.com/questions/398299/looping-in-a-spiral/31864777#31864777):


> If all you're trying to do is generate the first N points in the spiral
> (without the original problem's constraint of masking to an N x M
> region), the code becomes very simple:

    void spiral(const int N)
    {
        int x = 0;
        int y = 0;
        for(int i = 0; i < N; ++i)
        {
            cout << x << '\t' << y << '\n';
            if(abs(x) <= abs(y) && (x != y || x >= 0))
                x += ((y >= 0) ? 1 : -1);
            else
                y += ((x >= 0) ? -1 : 1);
        }
    }

## Translation to Joy

I'm going to make a function that take two ints (`x` and `y`) and
generates the next pair, we'll turn it into a generator later using the
`x` combinator.

### First Boolean Predicate

We need a function that computes `abs(x) <= abs(y)`, we can use `ii` to
apply `abs` to both values and then compare them
with `<=`:

    [abs] ii <=


```Joy
[_p [abs] ii <=] inscribe
```

    


```Joy
clear 23 -18
```

    23 -18


```Joy
[_p] trace
```

          23 -18 • _p
          23 -18 • [abs] ii <=
    23 -18 [abs] • ii <=
              23 • abs -18 abs <=
              23 • -18 abs <=
          23 -18 • abs <=
           23 18 • <=
           false • 
    
    false


```Joy
clear
```

    

### Short-Circuiting Boolean Combinators

I've defined two short-circuiting Boolean combinators `&&` and `||` that
each accept two quoted predicate programs, run the first, and
conditionally run the second only if required (to compute the final
Boolean value).  They run their predicate arguments `nullary`.


```Joy
[&& [nullary] cons [nullary [false]] dip branch] inscribe
[|| [nullary] cons [nullary] dip [true] branch] inscribe
```

    


```Joy
clear 
[true] [false] &&
```

    false


```Joy
clear 
[false] [true] &&
```

    false


```Joy
clear 
[true] [false] ||
```

    true


```Joy
clear 
[false] [true] ||
```

    true


```Joy
clear
```

    

### Translating the Conditionals

Given those, we can define `x != y || x >= 0` as:

    _a == [!=] [pop 0 >=] ||


```Joy
[_a [!=] [pop 0 >=] ||] inscribe
```

    

And `(abs(x) <= abs(y) && (x != y || x >= 0))` as:

    _b == [_p] [_a] &&


```Joy
[_b [_p] [_a] &&] inscribe
```

    

It's a little rough, but, as I say, with a little familiarity it becomes
legible.


```Joy
clear 23 -18
```

    23 -18


```Joy
[_b] trace
```

                                          23 -18 • _b
                                          23 -18 • [_p] [_a] &&
                                     23 -18 [_p] • [_a] &&
                                23 -18 [_p] [_a] • &&
                                23 -18 [_p] [_a] • [nullary] cons [nullary [false]] dip branch
                      23 -18 [_p] [_a] [nullary] • cons [nullary [false]] dip branch
                      23 -18 [_p] [[_a] nullary] • [nullary [false]] dip branch
    23 -18 [_p] [[_a] nullary] [nullary [false]] • dip branch
                                     23 -18 [_p] • nullary [false] [[_a] nullary] branch
                                     23 -18 [_p] • [stack] dinfrirst [false] [[_a] nullary] branch
                             23 -18 [_p] [stack] • dinfrirst [false] [[_a] nullary] branch
                             23 -18 [_p] [stack] • dip infrst [false] [[_a] nullary] branch
                                          23 -18 • stack [_p] infrst [false] [[_a] nullary] branch
                                 23 -18 [-18 23] • [_p] infrst [false] [[_a] nullary] branch
                            23 -18 [-18 23] [_p] • infrst [false] [[_a] nullary] branch
                            23 -18 [-18 23] [_p] • infra first [false] [[_a] nullary] branch
                                          23 -18 • _p [-18 23] swaack first [false] [[_a] nullary] branch
                                          23 -18 • [abs] ii <= [-18 23] swaack first [false] [[_a] nullary] branch
                                    23 -18 [abs] • ii <= [-18 23] swaack first [false] [[_a] nullary] branch
                                              23 • abs -18 abs <= [-18 23] swaack first [false] [[_a] nullary] branch
                                              23 • -18 abs <= [-18 23] swaack first [false] [[_a] nullary] branch
                                          23 -18 • abs <= [-18 23] swaack first [false] [[_a] nullary] branch
                                           23 18 • <= [-18 23] swaack first [false] [[_a] nullary] branch
                                           false • [-18 23] swaack first [false] [[_a] nullary] branch
                                  false [-18 23] • swaack first [false] [[_a] nullary] branch
                                  23 -18 [false] • first [false] [[_a] nullary] branch
                                    23 -18 false • [false] [[_a] nullary] branch
                            23 -18 false [false] • [[_a] nullary] branch
             23 -18 false [false] [[_a] nullary] • branch
                                          23 -18 • false
                                    23 -18 false • 
    
    23 -18 false


```Joy
clear
```

    

### The Increment / Decrement Branches

Turning to the branches of the main `if` statement:

    x += ((y >= 0) ? 1 : -1);

Rewrite as a hybrid (pseudo-code) `ifte` expression:

    [y >= 0] [x += 1] [X -= 1] ifte

Change each C phrase to Joy code:

    [0 >=] [[++] dip] [[--] dip] ifte

Factor out the dip from each branch:

    [0 >=] [[++]] [[--]] ifte dip

Similar logic applies to the other branch:

    y += ((x >= 0) ? -1 : 1);

    [x >= 0] [y -= 1] [y += 1] ifte

    [pop 0 >=] [--] [++] ifte

## Putting the Pieces Together

We can assemble the three functions we just defined in quotes and give
them them to the `ifte` combinator.  With some arrangement to show off
the symmetry of the two branches, we have:

    [[[abs] ii <=] [[<>] [pop !-] ||] &&]
    [[    !-] [[++]] [[--]] ifte dip]
    [[pop !-]  [--]   [++]  ifte    ]
    ifte


```Joy
[spiral_next

[_b]
[[    !-] [[++]] [[--]] ifte dip]
[[pop !-]  [--]   [++]  ifte    ]
ifte

] inscribe
```

    

As I was writing this up I realized that, since the `&&` combinator
doesn't consume the stack (below its quoted args), I can unquote the
predicate, swap the branches, and use the `branch` combinator instead of
`ifte`:

    [[abs] ii <=] [[<>] [pop !-] ||] &&
    [[pop !-]  [--]   [++]  ifte    ]
    [[    !-] [[++]] [[--]] ifte dip]
    branch

Let's try it out:


```Joy
clear 0 0
```

    0 0


```Joy
spiral_next
```

    1 0


```Joy
spiral_next
```

    1 -1


```Joy
spiral_next
```

    0 -1


```Joy
spiral_next
```

    -1 -1


```Joy
spiral_next
```

    -1 0


```Joy
spiral_next
```

    -1 1


```Joy
spiral_next
```

    0 1


```Joy
spiral_next
```

    1 1


```Joy
spiral_next
```

    2 1


```Joy
spiral_next
```

    2 0


```Joy
spiral_next
```

    2 -1


```Joy
spiral_next
```

    2 -2


```Joy
spiral_next
```

    1 -2


```Joy
spiral_next
```

    0 -2


```Joy
spiral_next
```

    -1 -2

## Turning it into a Generator with `x`

It can be used with the x combinator to make a kind of generator for
spiral square coordinates.


We can use `codireco` to make a generator

    codireco == cons dip rest cons

It will look like this:

    [value [F] codireco]

Here's a trace of how it works:


```Joy
clear

[0 [dup ++] codireco] [x] trace
```

               [0 [dup ++] codireco] • x
               [0 [dup ++] codireco] • 0 [dup ++] codireco
             [0 [dup ++] codireco] 0 • [dup ++] codireco
    [0 [dup ++] codireco] 0 [dup ++] • codireco
    [0 [dup ++] codireco] 0 [dup ++] • codi reco
    [0 [dup ++] codireco] 0 [dup ++] • cons dip reco
    [0 [dup ++] codireco] [0 dup ++] • dip reco
                                     • 0 dup ++ [0 [dup ++] codireco] reco
                                   0 • dup ++ [0 [dup ++] codireco] reco
                                 0 0 • ++ [0 [dup ++] codireco] reco
                                 0 1 • [0 [dup ++] codireco] reco
           0 1 [0 [dup ++] codireco] • reco
           0 1 [0 [dup ++] codireco] • rest cons
             0 1 [[dup ++] codireco] • cons
             0 [1 [dup ++] codireco] • 
    
    0 [1 [dup ++] codireco]


```Joy
clear
```

    

But first we have to change the `spiral_next` function to work on a
quoted pair of integers, and leave a copy of the pair on the stack.
From:

       y x spiral_next
    ---------------------
            y' x'

to:

       [x y] [spiral_next] infra
    -------------------------------
               [x' y']


```Joy
[0 0] [spiral_next] infra
```

    [0 1]

So our generator is:

    [[x y] [dup [spiral_next] infra] codireco]

Or rather:

    [[0 0] [dup [spiral_next] infra] codireco]

There is a function `make_generator` that will build the generator for us
out of the value and stepper function:

       [0 0] [dup [spiral_next] infra] make_generator
    ----------------------------------------------------
         [[0 0] [dup [spiral_next] infra] codireco]


```Joy
clear
```

    

Here it is in action:


```Joy
[0 0] [dup [spiral_next] infra] make_generator x x x x pop
```

    [0 0] [0 1] [-1 1] [-1 0]

Four `x` combinators, four pairs of coordinates.

Or you can leave out `dup` and let the value stay in the generator until you want it:


```Joy
clear

[0 0] [[spiral_next] infra] make_generator 50 [x] times first
```

    [2 4]

## Conclusion

So that's an example of Joy code.  It's a straightforward translation of
the original.  It's a little long for a single definition, you might
break it up like so:

    _spn_Pa == [abs] ii <=
    _spn_Pb == [!=] [pop 0 >=] ||
    _spn_P  == [_spn_Pa] [_spn_Pb] &&
    
    _spn_T == [    !-] [[++]] [[--]] ifte dip
    _spn_E == [pop !-]  [--]   [++]  ifte

    spiral_next == _spn_P [_spn_E] [_spn_T] branch

This way it's easy to see that the function is a branch with two
quasi-symmetrical paths.

We then used this function to make a simple generator of coordinate
pairs, where the next pair in the series can be generated at any time by
using the `x` combinator on the generator (which is just a quoted
expression containing a copy of the current pair and the "stepper
function" to generate the next pair from that.)
