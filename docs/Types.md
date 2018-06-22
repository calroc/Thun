
# Type Inference

## Pöial's Rules

["Typing Tools for Typeless Stack Languages" by Jaanus Pöial
](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.212.6026)

    @INPROCEEDINGS{Pöial06typingtools,
        author = {Jaanus Pöial},
        title = {Typing tools for typeless stack languages},
        booktitle = {In 23rd Euro-Forth Conference},
        year = {2006},
        pages = {40--46}
    }

### First Rule
This rule deals with functions (and literals) that put items on the stack `(-- d)`:


       (a -- b)∘(-- d)
    ---------------------
         (a -- b d)

### Second Rule
This rule deals with functions that consume items from the stack `(a --)`:

       (a --)∘(c -- d)
    ---------------------
         (c a -- d)

### Third Rule
The third rule is actually two rules.  These two rules deal with composing functions when the second one will consume one of items the first one produces.  The two types must be *unified* or a type conflict declared.

       (a -- b t[i])∘(c u[j] -- d)   t <= u (t is subtype of u)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == t[k] == u[j]
                                             ^

       (a -- b t[i])∘(c u[j] -- d)   u <= t (u is subtype of t)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == u[k] == u[j]

## Examples
Let's work through some examples by hand to develop an intuition for the algorithm.

There's a function in one of the other notebooks.

    F == pop swap roll< rest rest cons cons

It's all "stack chatter" and list manipulation so we should be able to deduce its type.

### Stack Effect Comments
Joy function types will be represented by Forth-style stack effect comments.  I'm going to use numbers instead of names to keep track of the stack arguments.  (A little bit like [De Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index), at least it reminds me of them):

    pop (1 --)

    swap (1 2 -- 2 1)

    roll< (1 2 3 -- 2 3 1)

These commands alter the stack but don't "look at" the values so these numbers represent an "Any type".

### `pop swap`

    (1 --) (1 2 -- 2 1)
    
Here we encounter a complication. The argument numbers need to be made unique among both sides.   For this let's change `pop` to use 0:

    (0 --) (1 2 -- 2 1)

Following the second rule:
    
    (1 2 0 -- 2 1)

### `pop∘swap roll<`

    (1 2 0 -- 2 1) (1 2 3 -- 2 3 1)

Let's re-label them:

    (1a 2a 0a -- 2a 1a) (1b 2b 3b -- 2b 3b 1b)

Now we follow the rules.

We must unify `1a` and `3b`, and `2a` and `2b`, replacing the terms in the forms:

    (1a 2a 0a -- 2a 1a) (1b 2b 3b -- 2b 3b 1b)
                                                w/  {1a: 3b}
    (3b 2a 0a -- 2a   ) (1b 2b    -- 2b 3b 1b)
                                                w/  {2a: 2b}
    (3b 2b 0a --      ) (1b       -- 2b 3b 1b)

Here we must apply the second rule:

       (3b 2b 0a --) (1b -- 2b 3b 1b)
    -----------------------------------
         (1b 3b 2b 0a -- 2b 3b 1b)

Now we de-label the type, uh, labels:

    (1b 3b 2b 0a -- 2b 3b 1b)

    w/ {
        1b: 1,
        3b: 2,
        2b: 3,
        0a: 0,
        }

    (1 2 3 0 -- 3 2 1)

And now we have the stack effect comment for `pop∘swap∘roll<`.

### Compiling `pop∘swap∘roll<`
The simplest way to "compile" this function would be something like:


```python
def poswrd(s, e, d):
    return roll_down(*swap(*pop(s, e, d)))
```

However, internally this function would still be allocating tuples (stack cells) and doing other unnecesssary work.

Looking ahead for a moment, from the stack effect comment:

    (1 2 3 0 -- 3 2 1)

We should be able to directly write out a Python function like:


```python
def poswrd(stack):
    (_, (a, (b, (c, stack)))) = stack
    return (c, (b, (a, stack)))
```

This eliminates the internal work of the first version.  Because this function only rearranges the stack and doesn't do any actual processing on the stack items themselves all the information needed to implement it is in the stack effect comment.

### Functions on Lists
These are slightly tricky.

    rest ( [1 ...] -- [...] )

    cons ( 1 [...] -- [1 ...] )

### `pop∘swap∘roll< rest`

    (1 2 3 0 -- 3 2 1) ([1 ...] -- [...])

Re-label (instead of adding left and right tags I'm just taking the next available index number for the right-side stack effect comment):

    (1 2 3 0 -- 3 2 1) ([4 ...] -- [...])

Unify and update:

    (1       2 3 0 -- 3 2 1) ([4 ...] -- [...])
                                                 w/ {1: [4 ...]}
    ([4 ...] 2 3 0 -- 3 2  ) (        -- [...])

Apply the first rule:

       ([4 ...] 2 3 0 -- 3 2) (-- [...])
    ---------------------------------------
         ([4 ...] 2 3 0 -- 3 2 [...])

And there we are.

### `pop∘swap∘roll<∘rest rest`

Let's do it again.

    ([4 ...] 2 3 0 -- 3 2 [...]) ([1 ...] -- [...])

Re-label (the tails of the lists on each side each get their own label):

    ([4 .0.] 2 3 0 -- 3 2 [.0.]) ([5 .1.] -- [.1.])

Unify and update (note the opening square brackets have been omited in the substitution dict, this is deliberate and I'll explain below):

    ([4 .0.]   2 3 0 -- 3 2 [.0.]  ) ([5 .1.] -- [.1.])
                                                        w/ { .0.] : 5 .1.] }
    ([4 5 .1.] 2 3 0 -- 3 2 [5 .1.]) ([5 .1.] -- [.1.])

How do we find `.0.]` in `[4 .0.]` and replace it with `5 .1.]` getting the result `[4 5 .1.]`?  This might seem hard, but because the underlying structure of the Joy list is a cons-list in Python it's actually pretty easy.  I'll explain below.

Next we unify and find our two terms are the same already: `[5 .1.]`:

    ([4 5 .1.] 2 3 0 -- 3 2 [5 .1.]) ([5 .1.] -- [.1.])

Giving us:

    ([4 5 .1.] 2 3 0 -- 3 2) (-- [.1.])

From here we apply the first rule and get:

    ([4 5 .1.] 2 3 0 -- 3 2 [.1.])

Cleaning up the labels:

    ([4 5 ...] 2 3 1 -- 3 2 [...])

This is the stack effect of `pop∘swap∘roll<∘rest∘rest`.

### `pop∘swap∘roll<∘rest∘rest cons`

    ([4 5 ...] 2 3 1 -- 3 2 [...]) (1 [...] -- [1 ...])

Re-label:

    ([4 5 .1.] 2 3 1 -- 3 2 [.1.]) (6 [.2.] -- [6 .2.])

Unify:

    ([4 5 .1.] 2 3 1 -- 3 2 [.1.]) (6 [.2.] -- [6 .2.])
                                                         w/ { .1.] : .2.] }
    ([4 5 .2.] 2 3 1 -- 3 2      ) (6       -- [6 .2.])
                                                         w/ {2: 6}
    ([4 5 .2.] 6 3 1 -- 3        ) (        -- [6 .2.])

First rule:

    ([4 5 .2.] 6 3 1 -- 3 [6 .2.])

Re-label:

    ([4 5 ...] 2 3 1 -- 3 [2 ...])

Done.

### `pop∘swap∘roll<∘rest∘rest∘cons cons`
One more time.

    ([4 5 ...] 2 3 1 -- 3 [2 ...]) (1 [...] -- [1 ...])

Re-label:

    ([4 5 .1.] 2 3 1 -- 3 [2 .1.]) (6 [.2.] -- [6 .2.])

Unify:

    ([4 5 .1.] 2 3 1 -- 3 [2 .1.]) (6 [.2.] -- [6 .2.]  )
                                                           w/ { .2.] : 2 .1.] }
    ([4 5 .1.] 2 3 1 -- 3        ) (6       -- [6 2 .1.])
                                                           w/ {3: 6}
    ([4 5 .1.] 2 6 1 --          ) (        -- [6 2 .1.])

First or second rule:

    ([4 5 .1.] 2 6 1 -- [6 2 .1.])

Clean up the labels:

    ([4 5 ...] 2 3 1 -- [3 2 ...])

And there you have it, the stack effect for `pop∘swap∘roll<∘rest∘rest∘cons∘cons`.

    ([4 5 ...] 2 3 1 -- [3 2 ...])

From this stack effect comment it should be possible to construct the following Python code:


```python
def F(stack):
    (_, (d, (c, ((a, (b, S0)), stack)))) = stack
    return (d, (c, S0)), stack
```

## Implementation

### Representing Stack Effect Comments in Python

I'm going to use pairs of tuples of type descriptors, which will be integers or tuples of type descriptors:


```python
roll_dn = (1, 2, 3), (2, 3, 1)

pop = (1,), ()

swap = (1, 2), (2, 1)
```

### `compose()`


```python
def compose(f, g):

    (f_in, f_out), (g_in, g_out) = f, g

    # First rule.
    #
    #       (a -- b) (-- d)
    #    ---------------------
    #         (a -- b d)

    if not g_in:

        fg_in, fg_out = f_in, f_out + g_out

    # Second rule.
    #
    #       (a --) (c -- d)
    #    ---------------------
    #         (c a -- d)

    elif not f_out:

        fg_in, fg_out = g_in + f_in, g_out

    else: # Unify, update, recur.

        fo, gi = f_out[-1], g_in[-1]

        s = unify(gi, fo)

        if s == False:  # s can also be the empty dict, which is ok.
            raise TypeError('Cannot unify %r and %r.' % (fo, gi))

        f_g = (f_in, f_out[:-1]), (g_in[:-1], g_out)

        if s: f_g = update(s, f_g)

        fg_in, fg_out = compose(*f_g)

    return fg_in, fg_out
```

### `unify()`


```python
def unify(u, v, s=None):
    if s is None:
        s = {}

    if u == v:
        return s

    if isinstance(u, int):
        s[u] = v
        return s

    if isinstance(v, int):
        s[v] = u
        return s

    return False
```

### `update()`


```python
def update(s, term):
    if not isinstance(term, tuple):
        return s.get(term, term)
    return tuple(update(s, inner) for inner in term)
```

### `relabel()`


```python
def relabel(left, right):
    return left, _1000(right)

def _1000(right):
    if not isinstance(right, tuple):
        return 1000 + right
    return tuple(_1000(n) for n in right)

relabel(pop, swap)
```




    (((1,), ()), ((1001, 1002), (1002, 1001)))



### `delabel()`


```python
def delabel(f):
    s = {u: i for i, u in enumerate(sorted(_unique(f)))}
    return update(s, f)

def _unique(f, seen=None):
    if seen is None:
        seen = set()
    if not isinstance(f, tuple):
        seen.add(f)
    else:
        for inner in f:
            _unique(inner, seen)
    return seen

delabel(relabel(pop, swap))
```




    (((0,), ()), ((1, 2), (2, 1)))



### `C()`

At last we put it all together in a function `C()` that accepts two stack effect comments and returns their composition (or raises and exception if they can't be composed due to type conflicts.)


```python
def C(f, g):
    f, g = relabel(f, g)
    fg = compose(f, g)
    return delabel(fg)
```

Let's try it out.


```python
C(pop, swap)
```




    ((1, 2, 0), (2, 1))




```python
C(C(pop, swap), roll_dn)
```




    ((3, 1, 2, 0), (2, 1, 3))




```python
C(swap, roll_dn)
```




    ((2, 0, 1), (1, 0, 2))




```python
C(pop, C(swap, roll_dn))
```




    ((3, 1, 2, 0), (2, 1, 3))




```python
poswrd = reduce(C, (pop, swap, roll_dn))
poswrd
```




    ((3, 1, 2, 0), (2, 1, 3))



### List Functions
Here's that trick to represent functions like `rest` and `cons` that manipulate lists.  We use a cons-list of tuples and give the tails their own numbers.  Then everything above already works. 


```python
rest = ((1, 2),), (2,)

cons = (1, 2), ((1, 2),)
```


```python
C(poswrd, rest)
```




    (((3, 4), 1, 2, 0), (2, 1, 4))



Compare this to the stack effect comment we wrote above:

    ((  (3, 4), 1, 2, 0 ), ( 2, 1,   4  ))
    (   [4 ...] 2  3  0  --  3  2  [...])

The translation table, if you will, would be:

    {
    3: 4,
    4: ...],
    1: 2,
    2: 3,
    0: 0,
    }


```python
F = reduce(C, (pop, swap, roll_dn, rest, rest, cons, cons))

F
```




    (((3, (4, 5)), 1, 2, 0), ((2, (1, 5)),))



Compare with the stack effect comment and you can see it works fine:

    ([4 5 ...] 2 3 1 -- [3 2 ...])

### Dealing with `cons` and `uncons`
However, if we try to compose e.g. `cons` and `uncons` it won't work:


```python
uncons = ((1, 2),), (1, 2)
```


```python
try:
    C(cons, uncons)
except Exception, e:
    print e
```

    Cannot unify (1, 2) and (1001, 1002).


#### `unify()` version 2
The problem is that the `unify()` function as written doesn't handle the case when both terms are tuples.  We just have to add a clause to deal with this recursively:


```python
def unify(u, v, s=None):
    if s is None:
        s = {}
    else:
        u = update(s, u)
        v = update(s, v)

    if u == v:
        return s

    if isinstance(u, int):
        s[u] = v
        return s

    if isinstance(v, int):
        s[v] = u
        return s

    if isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != len(v) != 2:
            raise ValueError(repr((u, v)))
        for uu, vv in zip(u, v):
            s = unify(uu, vv, s)
            if s == False: # (instead of a substitution dict.)
                break
        return s
 
    return False
```


```python
C(cons, uncons)
```




    ((0, 1), (0, 1))



## Compiling
Now consider the Python function we would like to derive:


```python
def F_python(stack):
    (_, (d, (c, ((a, (b, S0)), stack)))) = stack
    return (d, (c, S0)), stack
```

And compare it to the input stack effect comment tuple we just computed:


```python
F[0]
```




    ((3, (4, 5)), 1, 2, 0)



The stack-de-structuring tuple has nearly the same form as our input stack effect comment tuple, just in the reverse order:

    (_, (d, (c, ((a, (b, S0)), stack))))

Remove the punctuation:

     _   d   c   (a, (b, S0))

Reverse the order and compare:

     (a, (b, S0))   c   d   _
    ((3, (4, 5 )),  1,  2,  0)

Eh?

And the return tuple 


```python
F[1]
```




    ((2, (1, 5)),)



is similar to the output stack effect comment tuple:

    ((d, (c, S0)), stack)
    ((2, (1, 5 )),      )

This should make it pretty easy to write a Python function that accepts the stack effect comment tuples and returns a new Python function (either as a string of code or a function object ready to use) that performs the semantics of that Joy function (described by the stack effect.)

### Python Identifiers
We want to substitute Python identifiers for the integers.  I'm going to repurpose `joy.parser.Symbol` class for this:


```python
from collections import defaultdict
from joy.parser import Symbol


def _names_for():
    I = iter(xrange(1000))
    return lambda: Symbol('a%i' % next(I))


def identifiers(term, s=None):
    if s is None:
        s = defaultdict(_names_for())
    if isinstance(term, int):
        return s[term]
    return tuple(identifiers(inner, s) for inner in term)
```

### `doc_from_stack_effect()`
As a convenience I've implemented a function to convert the Python stack effect comment tuples to reasonable text format.  There are some details in how this code works that related to stuff later in the notebook, so you should skip it for now and read it later if you're interested.


```python
def doc_from_stack_effect(inputs, outputs):
    return '(%s--%s)' % (
        ' '.join(map(_to_str, inputs + ('',))),
        ' '.join(map(_to_str, ('',) + outputs))
    )


def _to_str(term):
    if not isinstance(term, tuple):
        try:
            t = term.prefix == 's'
        except AttributeError:
            return str(term)
        return '[.%i.]' % term.number if t else str(term)

    a = []
    while term and isinstance(term, tuple):
        item, term = term
        a.append(_to_str(item))

    try:
        n = term.number
    except AttributeError:
        n = term
    else:
        if term.prefix != 's':
            raise ValueError('Stack label: %s' % (term,))

    a.append('.%s.' % (n,))
    return '[%s]' % ' '.join(a)
```

### `compile_()`
Now we can write a compiler function to emit Python source code.  (The underscore suffix distiguishes it from the built-in `compile()` function.)


```python
def compile_(name, f, doc=None):
    if doc is None:
        doc = doc_from_stack_effect(*f)
    inputs, outputs = identifiers(f)
    i = o = Symbol('stack')
    for term in inputs:
        i = term, i
    for term in outputs:
        o = term, o
    return '''def %s(stack):
    """%s"""
    %s = stack
    return %s''' % (name, doc, i, o)
```

Here it is in action:


```python
source = compile_('F', F)

print source
```

    def F(stack):
        """([3 4 .5.] 1 2 0 -- [2 1 .5.])"""
        (a5, (a4, (a3, ((a0, (a1, a2)), stack)))) = stack
        return ((a4, (a3, a2)), stack)


Compare:


```python
def F_python(stack):
    (_, (d, (c, ((a, (b, S0)), stack)))) = stack
    return ((d, (c, S0)), stack)
```

Next steps:


```python
L = {}

eval(compile(source, '__main__', 'single'), {}, L)

L['F']
```




    <function F>



Let's try it out:


```python
from notebook_preamble import D, J, V
from joy.library import SimpleFunctionWrapper
```


```python
D['F'] = SimpleFunctionWrapper(L['F'])
```


```python
J('[4 5 ...] 2 3 1 F')
```

    [3 2 ...]


With this, we have a partial Joy compiler that works on the subset of Joy functions that manipulate stacks (both what I call "stack chatter" and the ones that manipulate stacks on the stack.)

I'm probably going to modify the definition wrapper code to detect definitions that can be compiled by this partial compiler and do it automatically.  It might be a reasonable idea to detect sequences of compilable functions in definitions that have uncompilable functions in them and just compile those.  However, if your library is well-factored this might be less helpful.

### Compiling Library Functions
We can use `compile_()` to generate many primitives in the library from their stack effect comments:


```python
def defs():

    roll_down = (1, 2, 3), (2, 3, 1)

    roll_up = (1, 2, 3), (3, 1, 2)

    pop = (1,), ()

    swap = (1, 2), (2, 1)

    rest = ((1, 2),), (2,)
    
    rrest = C(rest, rest)

    cons = (1, 2), ((1, 2),)

    uncons = ((1, 2),), (1, 2)
    
    swons = C(swap, cons)

    return locals()
```


```python
for name, stack_effect_comment in sorted(defs().items()):
    print
    print compile_(name, stack_effect_comment)
    print
```

    
    def cons(stack):
        """(1 2 -- [1 .2.])"""
        (a1, (a0, stack)) = stack
        return ((a0, a1), stack)
    
    
    def pop(stack):
        """(1 --)"""
        (a0, stack) = stack
        return stack
    
    
    def rest(stack):
        """([1 .2.] -- 2)"""
        ((a0, a1), stack) = stack
        return (a1, stack)
    
    
    def roll_down(stack):
        """(1 2 3 -- 2 3 1)"""
        (a2, (a1, (a0, stack))) = stack
        return (a0, (a2, (a1, stack)))
    
    
    def roll_up(stack):
        """(1 2 3 -- 3 1 2)"""
        (a2, (a1, (a0, stack))) = stack
        return (a1, (a0, (a2, stack)))
    
    
    def rrest(stack):
        """([0 1 .2.] -- 2)"""
        ((a0, (a1, a2)), stack) = stack
        return (a2, stack)
    
    
    def swap(stack):
        """(1 2 -- 2 1)"""
        (a1, (a0, stack)) = stack
        return (a0, (a1, stack))
    
    
    def swons(stack):
        """(0 1 -- [1 .0.])"""
        (a1, (a0, stack)) = stack
        return ((a1, a0), stack)
    
    
    def uncons(stack):
        """([1 .2.] -- 1 2)"""
        ((a0, a1), stack) = stack
        return (a1, (a0, stack))
    


## Types and Subtypes of Arguments
So far we have dealt with types of functions, those dealing with simple stack manipulation.  Let's extend our machinery to deal with types of arguments.

### "Number" Type

Consider the definition of `sqr`:

    sqr == dup mul


The `dup` function accepts one *anything* and returns two of that:

    dup (1 -- 1 1)

And `mul` accepts two "numbers" (we're ignoring ints vs. floats vs. complex, etc., for now) and returns just one:

    mul (n n -- n)

So we're composing:

    (1 -- 1 1)∘(n n -- n)

The rules say we unify 1 with `n`:

       (1 -- 1 1)∘(n n -- n)
    ---------------------------  w/  {1: n}
       (1 -- 1  )∘(n   -- n)

This involves detecting that "Any type" arguments can accept "numbers".  If we were composing these functions the other way round this is still the case:

       (n n -- n)∘(1 -- 1 1)
    ---------------------------  w/  {1: n}
       (n n --  )∘(  -- n n) 

The important thing here is that the mapping is going the same way in both cases, from the "any" integer to the number

### Distinguishing Numbers
We should also mind that the number that `mul` produces is not (necessarily) the same as either of its inputs, which are not (necessarily) the same as each other:

    mul (n2 n1 -- n3)


       (1  -- 1  1)∘(n2 n1 -- n3)
    --------------------------------  w/  {1: n2}
       (n2 -- n2  )∘(n2    -- n3)


       (n2 n1 -- n3)∘(1 -- 1  1 )
    --------------------------------  w/  {1: n3}
       (n2 n1 --   )∘(  -- n3 n3) 



### Distinguishing Types
So we need separate domains of "any" numbers and "number" numbers, and we need to be able to ask the order of these domains.  Now the notes on the right side of rule three make more sense, eh?

       (a -- b t[i])∘(c u[j] -- d)   t <= u (t is subtype of u)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == t[k] == u[j]
                                             ^

       (a -- b t[i])∘(c u[j] -- d)   u <= t (u is subtype of t)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == u[k] == u[j]

The indices `i`, `k`, and `j` are the number part of our labels and `t` and `u` are the domains.

By creative use of Python's "double underscore" methods we can define a Python class hierarchy of Joy types and use the `issubclass()` method to establish domain ordering, as well as other handy behaviour that will make it fairly easy to reuse most of the code above.


```python
class AnyJoyType(object):

    prefix = 'a'

    def __init__(self, number):
        self.number = number

    def __repr__(self):
        return self.prefix + str(self.number)

    def __eq__(self, other):
        return (
            isinstance(other, self.__class__)
            and other.prefix == self.prefix
            and other.number == self.number
        )

    def __ge__(self, other):
        return issubclass(other.__class__, self.__class__)

    def __add__(self, other):
        return self.__class__(self.number + other)
    __radd__ = __add__
    
    def __hash__(self):
        return hash(repr(self))


class NumberJoyType(AnyJoyType): prefix = 'n'
class FloatJoyType(NumberJoyType): prefix = 'f'
class IntJoyType(FloatJoyType): prefix = 'i'


class StackJoyType(AnyJoyType):
    prefix = 's'


_R = range(10)
A = map(AnyJoyType, _R)
N = map(NumberJoyType, _R)
S = map(StackJoyType, _R)
```

Mess with it a little:


```python
from itertools import permutations
```

"Any" types can be specialized to numbers and stacks, but not vice versa:


```python
for a, b in permutations((A[0], N[0], S[0]), 2):
    print a, '>=', b, '->', a >= b
```

    a0 >= n0 -> True
    a0 >= s0 -> True
    n0 >= a0 -> False
    n0 >= s0 -> False
    s0 >= a0 -> False
    s0 >= n0 -> False


Our crude [Numerical Tower](https://en.wikipedia.org/wiki/Numerical_tower) of *numbers* > *floats* > *integers* works as well (but we're not going to use it yet):


```python
for a, b in permutations((A[0], N[0], FloatJoyType(0), IntJoyType(0)), 2):
    print a, '>=', b, '->', a >= b
```

    a0 >= n0 -> True
    a0 >= f0 -> True
    a0 >= i0 -> True
    n0 >= a0 -> False
    n0 >= f0 -> True
    n0 >= i0 -> True
    f0 >= a0 -> False
    f0 >= n0 -> False
    f0 >= i0 -> True
    i0 >= a0 -> False
    i0 >= n0 -> False
    i0 >= f0 -> False


### Typing `sqr`


```python
dup = (A[1],), (A[1], A[1])

mul = (N[1], N[2]), (N[3],)
```


```python
dup
```




    ((a1,), (a1, a1))




```python
mul
```




    ((n1, n2), (n3,))



### Modifying the Inferencer
Re-labeling still works fine:


```python
foo = relabel(dup, mul)

foo
```




    (((a1,), (a1, a1)), ((n1001, n1002), (n1003,)))



#### `delabel()` version 2
The `delabel()` function needs an overhaul.  It now has to keep track of how many labels of each domain it has "seen".


```python
from collections import Counter


def delabel(f, seen=None, c=None):
    if seen is None:
        assert c is None
        seen, c = {}, Counter()

    try:
        return seen[f]
    except KeyError:
        pass

    if not isinstance(f, tuple):
        seen[f] = f.__class__(c[f.prefix])
        c[f.prefix] += 1
        return seen[f]

    return tuple(delabel(inner, seen, c) for inner in f)
```


```python
delabel(foo)
```




    (((a0,), (a0, a0)), ((n0, n1), (n2,)))



#### `unify()` version 3


```python
def unify(u, v, s=None):
    if s is None:
        s = {}
    else:
        u = update(s, u)
        v = update(s, v)

    if u == v:
        return s

    if isinstance(u, AnyJoyType) and isinstance(v, AnyJoyType):
        if u >= v:
            s[u] = v
            return s
        if v >= u:
            s[v] = u
            return s
        raise ValueError('Cannot unify %r and %r.' % (u, v))

    if isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != len(v) != 2:
            raise ValueError(repr((u, v)))
        for uu, vv in zip(u, v):
            s = unify(uu, vv, s)
            if s == False: # (instead of a substitution dict.)
                break
        return s
 
    if isinstance(v, tuple):
        if not stacky(u):
            raise ValueError('Cannot unify %r and %r.' % (u, v))
        s[u] = v
        return s

    if isinstance(u, tuple):
        if not stacky(v):
            raise ValueError('Cannot unify %r and %r.' % (v, u))
        s[v] = u
        return s

    return False


def stacky(thing):
    return thing.__class__ in {AnyJoyType, StackJoyType}
```

Rewrite the stack effect comments:


```python
def defs():

    roll_down = (A[1], A[2], A[3]), (A[2], A[3], A[1])

    roll_up = (A[1], A[2], A[3]), (A[3], A[1], A[2])

    pop = (A[1],), ()

    popop = (A[2], A[1],), ()

    popd = (A[2], A[1],), (A[1],)

    popdd = (A[3], A[2], A[1],), (A[2], A[1],)

    swap = (A[1], A[2]), (A[2], A[1])

    rest = ((A[1], S[1]),), (S[1],)

    rrest = C(rest, rest)

    cons = (A[1], S[1]), ((A[1], S[1]),)

    ccons = C(cons, cons)

    uncons = ((A[1], S[1]),), (A[1], S[1])

    swons = C(swap, cons)

    dup = (A[1],), (A[1], A[1])

    dupd = (A[2], A[1]), (A[2], A[2], A[1])

    mul = (N[1], N[2]), (N[3],)
    
    sqrt = C(dup, mul)

    first = ((A[1], S[1]),), (A[1],)

    second = C(rest, first)

    third = C(rest, second)

    tuck = (A[2], A[1]), (A[1], A[2], A[1])

    over = (A[2], A[1]), (A[2], A[1], A[2])
    
    succ = pred = (N[1],), (N[2],)
    
    divmod_ = pm = (N[2], N[1]), (N[4], N[3])

    return locals()
```


```python
DEFS = defs()
```


```python
for name, stack_effect_comment in sorted(DEFS.items()):
    print name, '=', doc_from_stack_effect(*stack_effect_comment)
```

    ccons = (a0 a1 [.0.] -- [a0 a1 .0.])
    cons = (a1 [.1.] -- [a1 .1.])
    divmod_ = (n2 n1 -- n4 n3)
    dup = (a1 -- a1 a1)
    dupd = (a2 a1 -- a2 a2 a1)
    first = ([a1 .1.] -- a1)
    mul = (n1 n2 -- n3)
    over = (a2 a1 -- a2 a1 a2)
    pm = (n2 n1 -- n4 n3)
    pop = (a1 --)
    popd = (a2 a1 -- a1)
    popdd = (a3 a2 a1 -- a2 a1)
    popop = (a2 a1 --)
    pred = (n1 -- n2)
    rest = ([a1 .1.] -- [.1.])
    roll_down = (a1 a2 a3 -- a2 a3 a1)
    roll_up = (a1 a2 a3 -- a3 a1 a2)
    rrest = ([a0 a1 .0.] -- [.0.])
    second = ([a0 a1 .0.] -- a1)
    sqrt = (n0 -- n1)
    succ = (n1 -- n2)
    swap = (a1 a2 -- a2 a1)
    swons = ([.0.] a0 -- [a0 .0.])
    third = ([a0 a1 a2 .0.] -- a2)
    tuck = (a2 a1 -- a1 a2 a1)
    uncons = ([a1 .1.] -- a1 [.1.])



```python
globals().update(DEFS)
```

#### Compose `dup` and `mul`


```python
C(dup, mul)
```




    ((n0,), (n1,))



Revisit the `F` function, works fine.


```python
F = reduce(C, (pop, swap, roll_down, rest, rest, cons, cons))
F
```




    (((a0, (a1, s0)), a2, a3, a4), ((a3, (a2, s0)),))




```python
print doc_from_stack_effect(*F)
```

    ([a0 a1 .0.] a2 a3 a4 -- [a3 a2 .0.])


Some otherwise inefficient functions are no longer to be feared.  We can also get the effect of combinators in some limited cases.


```python
def neato(*funcs):
    print doc_from_stack_effect(*reduce(C, funcs))
```


```python
# e.g. [swap] dip
neato(roll_up, swap, roll_down)
```

    (a0 a1 a2 -- a1 a0 a2)



```python
# e.g. [popop] dip
neato(popdd, roll_down, pop)
```

    (a0 a1 a2 a3 -- a2 a3)



```python
# Reverse the order of the top three items.
neato(roll_up, swap)
```

    (a0 a1 a2 -- a2 a1 a0)


#### `compile_()` version 2
Because the type labels represent themselves as valid Python identifiers the `compile_()` function doesn't need to generate them anymore:


```python
def compile_(name, f, doc=None):
    inputs, outputs = f
    if doc is None:
        doc = doc_from_stack_effect(inputs, outputs)
    i = o = Symbol('stack')
    for term in inputs:
        i = term, i
    for term in outputs:
        o = term, o
    return '''def %s(stack):
    """%s"""
    %s = stack
    return %s''' % (name, doc, i, o)
```


```python
print compile_('F', F)
```

    def F(stack):
        """([a0 a1 .0.] a2 a3 a4 -- [a3 a2 .0.])"""
        (a4, (a3, (a2, ((a0, (a1, s0)), stack)))) = stack
        return ((a3, (a2, s0)), stack)


But it cannot magically create new functions that involve e.g. math and such.  Note that this is *not* a `sqr` function implementation:


```python
print compile_('sqr', C(dup, mul))
```

    def sqr(stack):
        """(n0 -- n1)"""
        (n0, stack) = stack
        return (n1, stack)


#### `compilable()`
The functions that *can* be compiled are the ones that have only `AnyJoyType` and `StackJoyType` labels in their stack effect comments.  We can write a function to check that:


```python
from itertools import imap


def compilable(f):
    return isinstance(f, tuple) and all(imap(compilable, f)) or stacky(f)
```


```python
for name, stack_effect_comment in sorted(defs().items()):
    if compilable(stack_effect_comment):
        print name, '=', doc_from_stack_effect(*stack_effect_comment)
```

    ccons = (a0 a1 [.0.] -- [a0 a1 .0.])
    cons = (a1 [.1.] -- [a1 .1.])
    dup = (a1 -- a1 a1)
    dupd = (a2 a1 -- a2 a2 a1)
    first = ([a1 .1.] -- a1)
    over = (a2 a1 -- a2 a1 a2)
    pop = (a1 --)
    popd = (a2 a1 -- a1)
    popdd = (a3 a2 a1 -- a2 a1)
    popop = (a2 a1 --)
    rest = ([a1 .1.] -- [.1.])
    roll_down = (a1 a2 a3 -- a2 a3 a1)
    roll_up = (a1 a2 a3 -- a3 a1 a2)
    rrest = ([a0 a1 .0.] -- [.0.])
    second = ([a0 a1 .0.] -- a1)
    swap = (a1 a2 -- a2 a1)
    swons = ([.0.] a0 -- [a0 .0.])
    third = ([a0 a1 a2 .0.] -- a2)
    tuck = (a2 a1 -- a1 a2 a1)
    uncons = ([a1 .1.] -- a1 [.1.])


## Functions that use the Stack

Consider the `stack` function which grabs the whole stack, quotes it, and puts it on itself:

    stack (...     -- ... [...]        )
    stack (... a   -- ... a [a ...]    )
    stack (... b a -- ... b a [a b ...])

We would like to represent this in Python somehow. 
To do this we use a simple, elegant trick.

    stack         S   -- (         S,           S)
    stack     (a, S)  -- (     (a, S),      (a, S))
    stack (a, (b, S)) -- ( (a, (b, S)), (a, (b, S)))

Instead of representing the stack effect comments as a single tuple (with N items in it) we use the same cons-list structure to hold the sequence and `unify()` the whole comments.

### `stack∘uncons`
Let's try composing `stack` and `uncons`.  We want this result:

    stack∘uncons (... a -- ... a a [...])

The stack effects are:

    stack = S -- (S, S)

    uncons = ((a, Z), S) -- (Z, (a, S))

Unifying:

      S    -- (S, S) ∘ ((a, Z), S) -- (Z, (a,   S   ))
                                                        w/ { S: (a, Z) }
    (a, Z) --        ∘             -- (Z, (a, (a, Z)))

So:

    stack∘uncons == (a, Z) -- (Z, (a, (a, Z)))

It works.

### `stack∘uncons∘uncons`
Let's try `stack∘uncons∘uncons`:

    (a, S     ) -- (S,      (a, (a, S     ))) ∘ ((b, Z),  S`             ) -- (Z, (b,   S`   ))
    
                                                                                    w/ { S: (b, Z) }
                                                                                    
    (a, (b, Z)) -- ((b, Z), (a, (a, (b, Z)))) ∘ ((b, Z),  S`             ) -- (Z, (b,   S`   ))
    
                                                                                    w/ { S`: (a, (a, (b, Z))) }
                                                                                    
    (a, (b, Z)) -- ((b, Z), (a, (a, (b, Z)))) ∘ ((b, Z), (a, (a, (b, Z)))) -- (Z, (b, (a, (a, (b, Z)))))

    (a, (b, Z)) -- (Z, (b, (a, (a, (b, Z)))))

It works.

#### `compose()` version 2
This function has to be modified to use the new datastructures and it is no longer recursive, instead recursion happens as part of unification.


```python
def compose(f, g):

    (f_in, f_out), (g_in, g_out) = f, g

    if not g_in:
        fg_in, fg_out = f_in, stack_concat(g_out, f_out)

    elif not f_out:
        fg_in, fg_out = stack_concat(f_in, g_in), g_out

    else: # Unify and update.

        s = unify(g_in, f_out)

        if s == False:  # s can also be the empty dict, which is ok.
            raise TypeError('Cannot unify %r and %r.' % (fo, gi))

        fg_in, fg_out = update(s, (f_in, g_out))

    return fg_in, fg_out


stack_concat = lambda q, e: (q[0], stack_concat(q[1], e)) if q else e
```

I don't want to rewrite all the defs myself, so I'll write a little conversion function instead.  This is programmer's laziness.


```python
def sequence_to_stack(seq, stack=StackJoyType(23)):
    for item in seq: stack = item, stack
    return stack

NEW_DEFS = {
    name: (sequence_to_stack(i), sequence_to_stack(o))
    for name, (i, o) in DEFS.iteritems()
}

globals().update(NEW_DEFS)
```


```python
stack = S[0], (S[0], S[0])
```


```python
C(stack, uncons)
```




    ((a0, s0), (s0, (a0, (a0, s0))))




```python
C(C(stack, uncons), uncons)
```




    ((a0, (a1, s0)), (s0, (a1, (a0, (a0, (a1, s0))))))



The display function should be changed too.

### `doc_from_stack_effect()` version 2
Clunky junk, but it will suffice for now.


```python
def doc_from_stack_effect(inputs, outputs):
    switch = [False]  # Do we need to display the '...' for the rest of the main stack?
    i, o = _f(inputs, switch), _f(outputs, switch)
    if switch[0]:
        i.append('...')
        o.append('...')
    return '(%s--%s)' % (
        ' '.join(reversed([''] + i)),
        ' '.join(reversed(o + [''])),
    )


def _f(term, switch):
    a = []
    while term and isinstance(term, tuple):
        item, term = term
        a.append(item)
    assert isinstance(term, StackJoyType), repr(term)
    a = [_to_str(i, term, switch) for i in a]
    return a


def _to_str(term, stack, switch):
    if not isinstance(term, tuple):
        if term == stack:
            switch[0] = True
            return '[...]'
        return (
            '[.%i.]' % term.number
            if isinstance(term, StackJoyType)
            else str(term)
        )

    a = []
    while term and isinstance(term, tuple):
        item, term = term
        a.append(_to_str(item, stack, switch))
    assert isinstance(term, StackJoyType), repr(term)
    if term == stack:
        switch[0] = True
        end = '...'
    else:
        end = '.%i.' % term.number
    a.append(end)
    return '[%s]' % ' '.join(a)
```


```python
for name, stack_effect_comment in sorted(NEW_DEFS.items()):
    print name, '=', doc_from_stack_effect(*stack_effect_comment)
```

    ccons = (a0 a1 [.0.] -- [a0 a1 .0.])
    cons = (a1 [.1.] -- [a1 .1.])
    divmod_ = (n2 n1 -- n4 n3)
    dup = (a1 -- a1 a1)
    dupd = (a2 a1 -- a2 a2 a1)
    first = ([a1 .1.] -- a1)
    mul = (n1 n2 -- n3)
    over = (a2 a1 -- a2 a1 a2)
    pm = (n2 n1 -- n4 n3)
    pop = (a1 --)
    popd = (a2 a1 -- a1)
    popdd = (a3 a2 a1 -- a2 a1)
    popop = (a2 a1 --)
    pred = (n1 -- n2)
    rest = ([a1 .1.] -- [.1.])
    roll_down = (a1 a2 a3 -- a2 a3 a1)
    roll_up = (a1 a2 a3 -- a3 a1 a2)
    rrest = ([a0 a1 .0.] -- [.0.])
    second = ([a0 a1 .0.] -- a1)
    sqrt = (n0 -- n1)
    succ = (n1 -- n2)
    swap = (a1 a2 -- a2 a1)
    swons = ([.0.] a0 -- [a0 .0.])
    third = ([a0 a1 a2 .0.] -- a2)
    tuck = (a2 a1 -- a1 a2 a1)
    uncons = ([a1 .1.] -- a1 [.1.])



```python
print ; print doc_from_stack_effect(*stack)
print ; print doc_from_stack_effect(*C(stack, uncons))
print ; print doc_from_stack_effect(*C(C(stack, uncons), uncons))
print ; print doc_from_stack_effect(*C(C(stack, uncons), cons))
```

    
    (... -- ... [...])
    
    (... a0 -- ... a0 a0 [...])
    
    (... a1 a0 -- ... a1 a0 a0 a1 [...])
    
    (... a0 -- ... a0 [a0 ...])



```python
print doc_from_stack_effect(*C(ccons, stack))
```

    (... a1 a0 [.0.] -- ... [a1 a0 .0.] [[a1 a0 .0.] ...])



```python
Q = C(ccons, stack)

Q
```




    ((s0, (a0, (a1, s1))), (((a1, (a0, s0)), s1), ((a1, (a0, s0)), s1)))



#### `compile_()` version 3
This makes the `compile_()` function pretty simple as the stack effect comments are now already in the form needed for the Python code:


```python
def compile_(name, f, doc=None):
    i, o = f
    if doc is None:
        doc = doc_from_stack_effect(i, o)
    return '''def %s(stack):
    """%s"""
    %s = stack
    return %s''' % (name, doc, i, o)
```


```python
print compile_('Q', Q)
```

    def Q(stack):
        """(... a1 a0 [.0.] -- ... [a1 a0 .0.] [[a1 a0 .0.] ...])"""
        (s0, (a0, (a1, s1))) = stack
        return (((a1, (a0, s0)), s1), ((a1, (a0, s0)), s1))



```python
unstack = (S[1], S[0]), S[1]
enstacken = S[0], (S[0], S[1])
```


```python
print doc_from_stack_effect(*unstack)
```

    ([.1.] --)



```python
print doc_from_stack_effect(*enstacken)
```

    (-- [.0.])



```python
print doc_from_stack_effect(*C(cons, unstack))
```

    (a0 [.0.] -- a0)



```python
print doc_from_stack_effect(*C(cons, enstacken))
```

    (a0 [.0.] -- [[a0 .0.] .1.])



```python
C(cons, unstack)
```




    ((s0, (a0, s1)), (a0, s0))



## Sets of Stack Effects
...

## `concat`

How to deal with `concat`?

    concat ([.0.] [.1.] -- [.0. .1.])
    
We would like to represent this in Python somehow...


```python
concat = (S[0], S[1]), ((S[0], S[1]),)
```

But this is actually `cons` with the first argument restricted to be a stack:

    ([.0.] [.1.] -- [[.0.] .1.])

What we have implemented so far would actually only permit:

    ([.0.] [.1.] -- [.2.])


```python
concat = (S[0], S[1]), (S[2],)
```

    
Which works but can lose information.  Consider `cons concat`, this is how much information we *could* retain:

    (1 [.0.] [.1.] -- [1 .0. .1.])

As opposed to just:

    (1 [.0.] [.1.] -- [.2.])

    
Which works but can lose information.  Consider `cons concat`, this is how much information we *could* retain:

    (1 [.0.] [.1.] -- [1 .0. .1.]) uncons uncons

    (1 [.0.] [.1.] -- 1 [.0. .1.])        uncons
                                                    So far so good...
    (1 [2 .2.] [.1.] -- 1 2 [.2. .1.])




    (1 [.0.] [.1.] -- 1 [.0. .1.]) ([a1 .10.] -- a1 [.10.])
                                                             w/ { [a1 .10.] : [  .0.   .1.] }
                                                           -or-
                                                             w/ { [  .0.   .1.] : [a1 .10.    ] }







## Typing Combinators

TBD

This is an open subject.

The obvious thing is that you now need two pairs of tuples to describe the combinators' effects,  a stack effect comment and an expression effect comment:

    dip (a [F] --)--(-- F a)

One thing that might help is...

## Abstract Interpretation

## Something else...
    [4 5 ...] 2 1 0 pop∘swap∘roll<∘rest∘rest∘cons∘cons
    [4 5 ...] 2 1       swap∘roll<∘rest∘rest∘cons∘cons
    [4 5 ...] 1 2            roll<∘rest∘rest∘cons∘cons
    1 2 [4 5 ...]                  rest∘rest∘cons∘cons
    1 2   [5 ...]                       rest∘cons∘cons
    1 2     [...]                            cons∘cons
    1     [2 ...]                                 cons
        [1 2 ...]

Eh?
