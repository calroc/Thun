
# Type Inference

Cf. ["Type Inference in Stack-Based Programming Languages"](http://prl.ccs.neu.edu/blog/2017/03/10/type-inference-in-stack-based-programming-languages/) by Rob Kleffner, 2017-03-10.

## Part I: Pöial's Rules

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
The third rule is actually two rules.  These two rules deal with composing functions when the second one will consume one of items the first one produces.  The two types must be [*unified*](https://en.wikipedia.org/wiki/Robinson's_unification_algorithm) or a type conflict declared.

       (a -- b t[i])∘(c u[j] -- d)   t <= u (t is subtype of u)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == t[k] == u[j]
                                             ^

       (a -- b t[i])∘(c u[j] -- d)   u <= t (u is subtype of t)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == u[k] == u[j]

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

## Part II: Implementation

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



### Stack Functions
Here's that trick to represent functions like `rest` and `cons` that manipulate stacks.  We use a cons-list of tuples and give the tails their own numbers.  Then everything above already works. 


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
      3 4  5   1 2 0     2 1  5

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
    elif s:
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



## Part III: Compiling Stack Functions
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
    


## Part IV: Types and Subtypes of Arguments
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
    elif s:
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
        raise TypeError('Cannot unify %r and %r.' % (u, v))

    if isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != len(v) != 2:
            raise TypeError(repr((u, v)))
        for uu, vv in zip(u, v):
            s = unify(uu, vv, s)
            if s == False: # (instead of a substitution dict.)
                break
        return s
 
    if isinstance(v, tuple):
        if not stacky(u):
            raise TypeError('Cannot unify %r and %r.' % (u, v))
        s[u] = v
        return s

    if isinstance(u, tuple):
        if not stacky(v):
            raise TypeError('Cannot unify %r and %r.' % (v, u))
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


## Part V: Functions that use the Stack

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
This function has to be modified to use the new datastructures and it is no longer recursive, instead recursion happens as part of unification.  Further, the first and second of Pöial's rules are now handled automatically by the unification algorithm.


```python
def compose(f, g):
    (f_in, f_out), (g_in, g_out) = f, g
    s = unify(g_in, f_out)
    if s == False:  # s can also be the empty dict, which is ok.
        raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))
    return update(s, (f_in, g_out))
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



## Part VI: Multiple Stack Effects
...


```python
class IntJoyType(NumberJoyType): prefix = 'i'


F = map(FloatJoyType, _R)
I = map(IntJoyType, _R)
```


```python
muls = [
     ((I[2], (I[1], S[0])), (I[3], S[0])),
     ((F[2], (I[1], S[0])), (F[3], S[0])),
     ((I[2], (F[1], S[0])), (F[3], S[0])),
     ((F[2], (F[1], S[0])), (F[3], S[0])),
]
```


```python
for f in muls:
    print doc_from_stack_effect(*f)
```

    (i1 i2 -- i3)
    (i1 f2 -- f3)
    (f1 i2 -- f3)
    (f1 f2 -- f3)



```python
for f in muls:
    try:
        e = C(dup, f)
    except TypeError:
        continue
    print doc_from_stack_effect(*dup), doc_from_stack_effect(*f), doc_from_stack_effect(*e)
```

    (a1 -- a1 a1) (i1 i2 -- i3) (i0 -- i1)
    (a1 -- a1 a1) (f1 f2 -- f3) (f0 -- f1)



```python
from itertools import product


def meta_compose(F, G):
    for f, g in product(F, G):
        try:
            yield C(f, g)
        except TypeError:
            pass


def MC(F, G):
    return sorted(set(meta_compose(F, G)))
```


```python
for f in MC([dup], muls):
    print doc_from_stack_effect(*f)
```

    (f0 -- f1)
    (i0 -- i1)



```python
for f in MC([dup], [mul]):
    print doc_from_stack_effect(*f)
```

    (n0 -- n1)


### Representing an Unbounded Sequence of Types

We can borrow a trick from [Brzozowski's Derivatives of Regular Expressions](https://en.wikipedia.org/wiki/Brzozowski_derivative) to invent a new type of type variable, a "sequence type" (I think this is what they mean in the literature by that term...) or "[Kleene Star](https://en.wikipedia.org/wiki/Kleene_star)" type.  I'm going to represent it as a type letter and the asterix, so a sequence of zero or more `AnyJoyType` variables would be:

    A*

The `A*` works by splitting the universe into two alternate histories:

    A* -> 0 | A A*

The Kleene star variable disappears in one universe, and in the other it turns into an `AnyJoyType` variable followed by itself again.  We have to return all universes (represented by their substitution dicts, the "unifiers") that don't lead to type conflicts.

Consider unifying two stacks (the lowercase letters are any type variables of the kinds we have defined so far):

    [a A* b .0.] U [c d .1.]
                              w/ {c: a}
    [  A* b .0.] U [  d .1.]

Now we have to split universes to unify `A*`.  In the first universe it disappears:

    [b .0.] U [d .1.]
                       w/ {d: b, .1.: .0.} 
         [] U []

While in the second it spawns an `A`, which we will label `e`:

    [e A* b .0.] U [d .1.]
                            w/ {d: e}
    [  A* b .0.] U [  .1.]
                            w/ {.1.: A* b .0.}
    [  A* b .0.] U [  .1.]

Giving us two unifiers:

    {c: a,  d: b,  .1.:      .0.}
    {c: a,  d: e,  .1.: A* b .0.}


```python
class KleeneStar(object):

    kind = AnyJoyType

    def __init__(self, number):
        self.number = number
        self.count = 0
        self.prefix = repr(self)

    def __repr__(self):
        return '%s%i*' % (self.kind.prefix, self.number)

    def another(self):
        self.count += 1
        return self.kind(10000 * self.number + self.count)

    def __eq__(self, other):
        return (
            isinstance(other, self.__class__)
            and other.number == self.number
        )

    def __ge__(self, other):
        return self.kind >= other.kind

    def __add__(self, other):
        return self.__class__(self.number + other)
    __radd__ = __add__
    
    def __hash__(self):
        return hash(repr(self))

class AnyStarJoyType(KleeneStar): kind = AnyJoyType
class NumberStarJoyType(KleeneStar): kind = NumberJoyType
#class FloatStarJoyType(KleeneStar): kind = FloatJoyType
#class IntStarJoyType(KleeneStar): kind = IntJoyType
class StackStarJoyType(KleeneStar): kind = StackJoyType


As = map(AnyStarJoyType, _R)
Ns = map(NumberStarJoyType, _R)
Ss = map(StackStarJoyType, _R)
```

#### `unify()` version 4
Can now return multiple results...


```python
def unify(u, v, s=None):
    if s is None:
        s = {}
    elif s:
        u = update(s, u)
        v = update(s, v)

    if u == v:
        return s,

    if isinstance(u, AnyJoyType) and isinstance(v, AnyJoyType):
        if u >= v:
            s[u] = v
            return s,
        if v >= u:
            s[v] = u
            return s,
        raise TypeError('Cannot unify %r and %r.' % (u, v))

    if isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != len(v) != 2:
            raise TypeError(repr((u, v)))
            
        a, b = v
        if isinstance(a, KleeneStar):
            # Two universes, in one the Kleene star disappears and unification
            # continues without it...
            s0 = unify(u, b)
            
            # In the other it spawns a new variable.
            s1 = unify(u, (a.another(), v))
            
            t = s0 + s1
            for sn in t:
                sn.update(s)
            return t

        a, b = u
        if isinstance(a, KleeneStar):
            s0 = unify(v, b)
            s1 = unify(v, (a.another(), u))
            t = s0 + s1
            for sn in t:
                sn.update(s)
            return t

        ses = unify(u[0], v[0], s)
        results = ()
        for sn in ses:
            results += unify(u[1], v[1], sn)
        return results
 
    if isinstance(v, tuple):
        if not stacky(u):
            raise TypeError('Cannot unify %r and %r.' % (u, v))
        s[u] = v
        return s,

    if isinstance(u, tuple):
        if not stacky(v):
            raise TypeError('Cannot unify %r and %r.' % (v, u))
        s[v] = u
        return s,

    return ()


def stacky(thing):
    return thing.__class__ in {AnyJoyType, StackJoyType}
```


```python
a = (As[1], S[1])
a
```




    (a1*, s1)




```python
b = (A[1], S[2])
b
```




    (a1, s2)




```python
for result in unify(b, a):
    print result, '->', update(result, a), update(result, b)
```

    {s1: (a1, s2)} -> (a1*, (a1, s2)) (a1, s2)
    {a1: a10001, s2: (a1*, s1)} -> (a1*, s1) (a10001, (a1*, s1))



```python
for result in unify(a, b):
    print result, '->', update(result, a), update(result, b)
```

    {s1: (a1, s2)} -> (a1*, (a1, s2)) (a1, s2)
    {a1: a10002, s2: (a1*, s1)} -> (a1*, s1) (a10002, (a1*, s1))



    (a1*, s1)       [a1*]       (a1, s2)        [a1]

    (a1*, (a1, s2)) [a1* a1]    (a1, s2)        [a1]

    (a1*, s1)       [a1*]       (a2, (a1*, s1)) [a2 a1*]


```python
sum_ = ((Ns[1], S[1]), S[0]), (N[0], S[0])

print doc_from_stack_effect(*sum_)
```

    ([n1* .1.] -- n0)



```python
f = (N[1], (N[2], (N[3], S[1]))), S[0]

print doc_from_stack_effect(S[0], f)
```

    (-- [n1 n2 n3 .1.])



```python
for result in unify(sum_[0], f):
    print result, '->', update(result, sum_[1])
```

    {s1: (n1, (n2, (n3, s1)))} -> (n0, s0)
    {n1: n10001, s1: (n2, (n3, s1))} -> (n0, s0)
    {n1: n10001, s1: (n3, s1), n2: n10002} -> (n0, s0)
    {n1: n10001, s1: (n1*, s1), n3: n10003, n2: n10002} -> (n0, s0)


#### `compose()` version 3
This function has to be modified to yield multiple results.


```python
def compose(f, g):
    (f_in, f_out), (g_in, g_out) = f, g
    s = unify(g_in, f_out)
    if not s:
        raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))
    for result in s:
        yield update(result, (f_in, g_out))

```


```python
def meta_compose(F, G):
    for f, g in product(F, G):
        try:
            for result in C(f, g):
                yield result
        except TypeError:
            pass


def C(f, g):
    f, g = relabel(f, g)
    for fg in compose(f, g):
        yield delabel(fg)
```


```python
for f in MC([dup], muls):
    print doc_from_stack_effect(*f)
```

    (f0 -- f1)
    (i0 -- i1)



```python


for f in MC([dup], [sum_]):
    print doc_from_stack_effect(*f)
```

    ([n0* .0.] -- [n0* .0.] n0)



```python


for f in MC([cons], [sum_]):
    print doc_from_stack_effect(*f)
```

    (a0 [.0.] -- n0)
    (n0 [n0* .0.] -- n1)



```python
sum_ = (((N[1], (Ns[1], S[1])), S[0]), (N[0], S[0]))
print doc_from_stack_effect(*cons),
print doc_from_stack_effect(*sum_),

for f in MC([cons], [sum_]):
    print doc_from_stack_effect(*f)
```

    (a1 [.1.] -- [a1 .1.]) ([n1 n1* .1.] -- n0) (n0 [n0* .0.] -- n1)



```python
a = (A[4], (As[1], (A[3], S[1])))
a
```




    (a4, (a1*, (a3, s1)))




```python
b = (A[1], (A[2], S[2]))
b
```




    (a1, (a2, s2))




```python
for result in unify(b, a):
    print result
```

    {a1: a4, s2: s1, a2: a3}
    {a1: a4, s2: (a1*, (a3, s1)), a2: a10003}



```python
for result in unify(a, b):
    print result
```

    {s2: s1, a2: a3, a4: a1}
    {s2: (a1*, (a3, s1)), a2: a10004, a4: a1}


## Part VII: Typing Combinators

TBD

This is an open subject.

The obvious thing is that you now need two pairs of tuples to describe the combinators' effects,  a stack effect comment and an expression effect comment:

    dip (a [F] --)--(-- F a)

One thing that might help is...

Consider the type of:

    [cons] dip

Obviously it would be:

    (a1 [..1] a2 -- [a1 ..1] a2)

`dip` itself could have:

    (a1 [..1] -- ... then what?



```python
class SymbolJoyType(AnyJoyType): prefix = 'F'

W = map(SymbolJoyType, _R)

k = S[0], ((W[1], S[2]), S[0])
Symbol('cons')
print doc_from_stack_effect(*k)

```

    (-- [F1 .2.])



```python
dip_a = ((W[1], S[2]), (A[1], S[0]))
```


```python
d = relabel(S[0], dip_a)
print doc_from_stack_effect(*d)
```

    (-- a1001 [F1001 .1002.])



```python
s = list(unify(d[1], k[1]))[0]
s
```




    {s0: (a1001, s1000), s1002: s2, F1001: F1}




```python
j = update(s, k)
```


```python
print doc_from_stack_effect(*j)
```

    (a1001 -- a1001 [F1 .2.])



```python
j
```




    ((a1001, s1000), ((F1, s2), (a1001, s1000)))




```python
cons
```




    ((s1, (a1, s23)), ((a1, s1), s23))




```python
for f in MC([k], [dup]):
    print doc_from_stack_effect(*f)
```

    (-- [F0 .1.] [F0 .1.])



```python
l = S[0], ((cons, S[2]), (A[1], S[0]))
```


```python
print doc_from_stack_effect(*l)
```

    (-- a1 [[[[.1.] a1 .23.] [a1 .1.] .23.] .2.])



```python

def dip_t(F):
    (quote, (a1, sec)) = F[1]
    G = F[0], sec
    P = S[3], (a1, S[3])
    a = [P]
    while isinstance(quote, tuple):
        term, quote = quote
        a.append(term)
    a.append(G)
    return a[::-1]




```


```python
from joy.utils.stack import iter_stack
```


```python
a, b, c = dip_t(l)
```


```python
a
```




    (s0, s0)




```python
b
```




    ((s1, (a1, s23)), ((a1, s1), s23))




```python
c
```




    (s3, (a1, s3))




```python
MC([a], [b])
```




    [((s0, (a0, s1)), ((a0, s0), s1))]




```python
kjs = MC(MC([a], [b]), [c])
kjs
```




    [((s0, (a0, s1)), (a1, ((a0, s0), s1)))]




```python
print doc_from_stack_effect(*kjs[0])
```

    (a0 [.0.] -- [a0 .0.] a1)


    (a0 [.0.] -- [a0 .0.] a1)
    
       a0 [.0.] a1 [cons] dip
    ----------------------------
       [a0 .0.] a1


```python
stack_concat = lambda q, e: (q[0], stack_concat(q[1], e)) if q else e
```


```python
class SymbolJoyType(AnyJoyType):
    prefix = 'F'

    def __init__(self, name, sec, number):
        self.name = name
        self.stack_effects = sec
        self.number = number

class CombinatorJoyType(SymbolJoyType): prefix = 'C'

def dip_t(stack, expression):
    (quote, (a1, stack)) = stack
    expression = stack_concat(quote, (a1, expression))
    return stack, expression

CONS = SymbolJoyType('cons', [cons], 23)
DIP = CombinatorJoyType('dip', [dip_t], 44)


def kav(F, e):
    #i, stack = F
    if not e:
        return [(F, e)]
    n, e = e
    if isinstance(n, SymbolJoyType):
        Fs = []
        for sec in n.stack_effects:
            Fs.extend(MC([F], sec))
        return [kav(Fn, e) for Fn in Fs]
    if isinstance(n, CombinatorJoyType):
        res = []
        for f in n.stack_effects:
            s, e = f(F[1], e)
            new_F = F[0], s
            res.extend(kav(new_F, e))
        return res
    lit = S[0], (n, S[0])
    return [kav(Fn, e) for Fn in MC([F], [lit])]

```

compare, and be amazed:


```python
def dip_t(stack, expression):
    (quote, (a1, stack)) = stack
    expression = stack_concat(quote, (a1, expression))
    return stack, expression
```


```python
def dip(stack, expression, dictionary):
    (quote, (x, stack)) = stack
    expression = (x, expression)
    return stack, concat(quote, expression), dictionary
```

And that brings us to current Work-In-Progress.  I'm pretty hopeful that the mixed-mode inferencer/interpreter `kav()` function along with the ability to specify multiple implementations for the combinators will permit modelling of the stack effects of e.g. `ifte`.  If I can keep up the pace I should be able to verify that conjecture by the end of June.

The rest of this stuff is junk and/or unfinished material.

### `concat`

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

### represent `concat`

    ([.0.] [.1.] -- [A*(.0.) .1.])

Meaning that `A*` on the right-hand side should all the crap from `.0.`.

    ([      .0.] [.1.] -- [      A* .1.])
    ([a     .0.] [.1.] -- [a     A* .1.])
    ([a b   .0.] [.1.] -- [a b   A* .1.])
    ([a b c .0.] [.1.] -- [a b c A* .1.])

    

or...

    ([       .0.] [.1.] -- [       .1.])
    ([a      .0.] [.1.] -- [a      .1.])
    ([a b    .0.] [.1.] -- [a b    .1.])
    ([a b  c .0.] [.1.] -- [a b  c .1.])
    ([a A* c .0.] [.1.] -- [a A* c .1.])

    

    (a, (b, S0)) . S1 = (a, (b, (A*, S1)))


```python
class Astar(object):
    def __repr__(self):
        return 'A*'


def concat(s0, s1):
    a = []
    while isinstance(s0, tuple):
        term, s0 = s0
        a.append(term)
    assert isinstance(s0, StackJoyType), repr(s0)
    s1 = Astar(), s1
    for term in reversed(a):
        s1 = term, s1
    return s1
```


```python
a, b = (A[1], S[0]), (A[2], S[1])
```


```python
concat(a, b)
```




    (a1, (A*, (a2, s1)))



## Appendix: Joy in the Logical Paradigm
For this to work the type label classes have to be modified to let `T >= t` succeed, where e.g. `T` is `IntJoyType` and `t` is `int`


```python
F = reduce(C, (pop, swap, roll_down, rest, rest, cons, cons))

print doc_from_stack_effect(*F)
```


    ---------------------------------------------------------------------------

    ValueError                                Traceback (most recent call last)

    <ipython-input-137-4b4cb6ff86e5> in <module>()
          1 F = reduce(C, (pop, swap, roll_down, rest, rest, cons, cons))
          2 
    ----> 3 print doc_from_stack_effect(*F)
    

    <ipython-input-99-ddee30dbb1a6> in C(f, g)
         10 def C(f, g):
         11     f, g = relabel(f, g)
    ---> 12     for fg in compose(f, g):
         13         yield delabel(fg)


    <ipython-input-98-5eb7ac5ad2c2> in compose(f, g)
          1 def compose(f, g):
    ----> 2     (f_in, f_out), (g_in, g_out) = f, g
          3     s = unify(g_in, f_out)
          4     if not s:
          5         raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))


    <ipython-input-99-ddee30dbb1a6> in C(f, g)
         10 def C(f, g):
         11     f, g = relabel(f, g)
    ---> 12     for fg in compose(f, g):
         13         yield delabel(fg)


    <ipython-input-98-5eb7ac5ad2c2> in compose(f, g)
          1 def compose(f, g):
    ----> 2     (f_in, f_out), (g_in, g_out) = f, g
          3     s = unify(g_in, f_out)
          4     if not s:
          5         raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))


    <ipython-input-99-ddee30dbb1a6> in C(f, g)
         10 def C(f, g):
         11     f, g = relabel(f, g)
    ---> 12     for fg in compose(f, g):
         13         yield delabel(fg)


    <ipython-input-98-5eb7ac5ad2c2> in compose(f, g)
          1 def compose(f, g):
    ----> 2     (f_in, f_out), (g_in, g_out) = f, g
          3     s = unify(g_in, f_out)
          4     if not s:
          5         raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))


    <ipython-input-99-ddee30dbb1a6> in C(f, g)
         10 def C(f, g):
         11     f, g = relabel(f, g)
    ---> 12     for fg in compose(f, g):
         13         yield delabel(fg)


    <ipython-input-98-5eb7ac5ad2c2> in compose(f, g)
          1 def compose(f, g):
    ----> 2     (f_in, f_out), (g_in, g_out) = f, g
          3     s = unify(g_in, f_out)
          4     if not s:
          5         raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))


    <ipython-input-99-ddee30dbb1a6> in C(f, g)
         10 def C(f, g):
         11     f, g = relabel(f, g)
    ---> 12     for fg in compose(f, g):
         13         yield delabel(fg)


    <ipython-input-98-5eb7ac5ad2c2> in compose(f, g)
          1 def compose(f, g):
    ----> 2     (f_in, f_out), (g_in, g_out) = f, g
          3     s = unify(g_in, f_out)
          4     if not s:
          5         raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))


    ValueError: need more than 1 value to unpack



```python
from joy.parser import text_to_expression
```


```python
s = text_to_expression('[3 4 ...] 2 1')
s
```


```python
L = unify(F[1], s)
L
```


```python
F[1]
```


```python
F[1][0]
```


```python
s[0]
```

## [Abstract Interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation)
I *think* this might be sorta what I'm doing above with the `kav()` function...
  In any event "mixed-mode" interpreters that include values and type variables and can track constraints, etc. will be, uh, super-useful.  And Abstract Interpretation should be a rich source of ideas.

