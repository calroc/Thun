     ___                     _        ___         _     
    | __|_ ____ _ _ __  _ __| |___   / __|___  __| |___ 
    | _|\ \ / _` | '  \| '_ \ / -_) | (__/ _ \/ _` / -_)
    |___/_\_\__,_|_|_|_| .__/_\___|  \___\___/\__,_\___|
                        |_|
# On the Square Spiral Example Code

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

It's adapted from the [original code on StackOverflow](https://stackoverflow.com/questions/398299/looping-in-a-spiral/31864777#31864777):

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

> The trick is that you can compare x and y to determine what side of the
> square you're on, and that tells you what direction to move in.


## Translation to Joy

I'm going to make a function that take two ints (`x` and `y`) and
generates the next pair, we'll turn it into a generator later using the
`x` combinator.

### First Boolean Predicate

We need a function that computes `abs(x) <= abs(y)`, we can use `ii` to
apply `abs` in parallel (eventually) to both values and then compare them
with `<=`:

    [abs] ii <=

I've defined two short-circuiting Boolean combinators `&&` and `||` that
each accept two quoted predicate programs, run the first, and
conditionally run the second only if required (to compute the final
Boolean value).  They run their predicate arguments `nullary`.  Given
those, we can define `x != y || x >= 0` as:

    [<>] [pop 0 >=] ||

And `(abs(x) <= abs(y) && (x != y || x >= 0))` as:

    [[abs] ii <=] [[<>] [pop 0 >=] ||] &&

It's a little rough, but, as I say, with a little familiarity it becomes
legible.

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

As I was writing this up I realized that, since the `&&` combinator
doesn't consume the stack (below its quoted args), I can unquote the
predicate, swap the branches, and use the `branch` combinator instead of
`ifte`:

    [[abs] ii <=] [[<>] [pop !-] ||] &&
    [[pop !-]  [--]   [++]  ifte    ]
    [[    !-] [[++]] [[--]] ifte dip]
    branch


## Turning it into a Generator with `x`

It can be used with the x combinator to make a kind of generator for
spiral square coordinates.


We can use `codireco` to make a generator

    codireco ::= cons dip rest cons

It will look like this:

    [value [F] codireco]

Here's a trace of how it works:

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

So our generator is:

    [[x y] [dup [spiral_next] infra] codireco]

Or rather:

    [[0 0] [dup [spiral_next] infra] codireco]

There is a function `make_generator` that will build the generator for us
out of the value and stepper function:

       [0 0] [dup [spiral_next] infra] make_generator
    ----------------------------------------------------
         [[0 0] [dup [spiral_next] infra] codireco]

Here it is in action:

    ?- joy(`[[0 0] [dup [spiral_next] infra] codireco] x x x x pop`, [], _So),
    |    joy_terms_to_string(_So, S).

    _So = [list([int(-1), int(0)]), list([int(-1), int(1)]), list([int(0), int(1)]), list([int(0), int(0)])],

    S = "[-1 0] [-1 1] [0 1] [0 0]" .

Four `x` combinators, four pairs of coordinates.


## Conclusion

So that's an example of Joy code.  It's a straightforward translation of
the original.  It's a little long for a single definition, you might
break it up like so:

         _spn_P ::= [[abs] ii <=] [[<>] [pop !-] ||] &&

         _spn_T ::= [    !-] [[++]] [[--]] ifte dip
         _spn_E ::= [pop !-]  [--]   [++]  ifte

    spiral_next ::= _spn_P [_spn_E] [_spn_T] branch

This way it's easy to see that the function is a branch with two
quasi-symmetrical paths.

We then used this function to make a simple generator of coordinate
pairs, where the next pair in the series can be generated at any time by
using the `x` combinator on the generator (which is just a quoted
expression containing a copy of the current pair and the "stepper
function" to generate the next pair from that.)