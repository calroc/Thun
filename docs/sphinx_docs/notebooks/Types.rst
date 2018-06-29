
The Blissful Elegance of Typing Joy
===================================

This notebook presents a simple type inferencer for Joy code. It can
infer the stack effect of most Joy expressions. It's built largely by
means of existing ideas and research. (A great overview of the existing
knowledge is a talk `"Type Inference in Stack-Based Programming
Languages" <http://prl.ccs.neu.edu/blog/2017/03/10/type-inference-in-stack-based-programming-languages/>`__
given by Rob Kleffner on or about 2017-03-10 as part of a course on the
history of programming languages.)

The notebook starts with a simple inferencer based on the work of Jaanus
Pöial which we then progressively elaborate to cover more Joy semantics.
Along the way we write a simple "compiler" that emits Python code for
what I like to call Yin functions. (Yin functions are those that only
rearrange values in stacks, as opposed to Yang functions that actually
work on the values themselves.)

Part I: Pöial's Rules
---------------------

`"Typing Tools for Typeless Stack Languages" by Jaanus
Pöial <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.212.6026>`__

::

    @INPROCEEDINGS{Pöial06typingtools,
        author = {Jaanus Pöial},
        title = {Typing tools for typeless stack languages},
        booktitle = {In 23rd Euro-Forth Conference},
        year = {2006},
        pages = {40--46}
    }

First Rule
~~~~~~~~~~

This rule deals with functions (and literals) that put items on the
stack ``(-- d)``:

::

       (a -- b)∘(-- d)
    ---------------------
         (a -- b d)

Second Rule
~~~~~~~~~~~

This rule deals with functions that consume items from the stack
``(a --)``:

::

       (a --)∘(c -- d)
    ---------------------
         (c a -- d)

Third Rule
~~~~~~~~~~

The third rule is actually two rules. These two rules deal with
composing functions when the second one will consume one of items the
first one produces. The two types must be
`*unified* <https://en.wikipedia.org/wiki/Robinson's_unification_algorithm>`__
or a type conflict declared.

::

       (a -- b t[i])∘(c u[j] -- d)   t <= u (t is subtype of u)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == t[k] == u[j]
                                             ^

       (a -- b t[i])∘(c u[j] -- d)   u <= t (u is subtype of t)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == u[k] == u[j]

Let's work through some examples by hand to develop an intuition for the
algorithm.

There's a function in one of the other notebooks.

::

    F == pop swap roll< rest rest cons cons

It's all "stack chatter" and list manipulation so we should be able to
deduce its type.

Stack Effect Comments
~~~~~~~~~~~~~~~~~~~~~

Joy function types will be represented by Forth-style stack effect
comments. I'm going to use numbers instead of names to keep track of the
stack arguments. (A little bit like `De Bruijn
index <https://en.wikipedia.org/wiki/De_Bruijn_index>`__, at least it
reminds me of them):

::

    pop (1 --)

    swap (1 2 -- 2 1)

    roll< (1 2 3 -- 2 3 1)

These commands alter the stack but don't "look at" the values so these
numbers represent an "Any type".

``pop swap``
~~~~~~~~~~~~

::

    (1 --) (1 2 -- 2 1)

Here we encounter a complication. The argument numbers need to be made
unique among both sides. For this let's change ``pop`` to use 0:

::

    (0 --) (1 2 -- 2 1)

Following the second rule:

::

    (1 2 0 -- 2 1)

``pop∘swap roll<``
~~~~~~~~~~~~~~~~~~

::

    (1 2 0 -- 2 1) (1 2 3 -- 2 3 1)

Let's re-label them:

::

    (1a 2a 0a -- 2a 1a) (1b 2b 3b -- 2b 3b 1b)

Now we follow the rules.

We must unify ``1a`` and ``3b``, and ``2a`` and ``2b``, replacing the
terms in the forms:

::

    (1a 2a 0a -- 2a 1a) (1b 2b 3b -- 2b 3b 1b)
                                                w/  {1a: 3b}
    (3b 2a 0a -- 2a   ) (1b 2b    -- 2b 3b 1b)
                                                w/  {2a: 2b}
    (3b 2b 0a --      ) (1b       -- 2b 3b 1b)

Here we must apply the second rule:

::

       (3b 2b 0a --) (1b -- 2b 3b 1b)
    -----------------------------------
         (1b 3b 2b 0a -- 2b 3b 1b)

Now we de-label the type, uh, labels:

::

    (1b 3b 2b 0a -- 2b 3b 1b)

    w/ {
        1b: 1,
        3b: 2,
        2b: 3,
        0a: 0,
        }

    (1 2 3 0 -- 3 2 1)

And now we have the stack effect comment for ``pop∘swap∘roll<``.

Compiling ``pop∘swap∘roll<``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The simplest way to "compile" this function would be something like:

.. code:: ipython2

    def poswrd(s, e, d):
        return rolldown(*swap(*pop(s, e, d)))

However, internally this function would still be allocating tuples
(stack cells) and doing other unnecesssary work.

Looking ahead for a moment, from the stack effect comment:

::

    (1 2 3 0 -- 3 2 1)

We should be able to directly write out a Python function like:

.. code:: ipython2

    def poswrd(stack):
        (_, (a, (b, (c, stack)))) = stack
        return (c, (b, (a, stack)))

This eliminates the internal work of the first version. Because this
function only rearranges the stack and doesn't do any actual processing
on the stack items themselves all the information needed to implement it
is in the stack effect comment.

Functions on Stacks
~~~~~~~~~~~~~~~~~~~

These are slightly tricky.

::

    rest ( [1 ...] -- [...] )

    cons ( 1 [...] -- [1 ...] )

``pop∘swap∘roll< rest``
~~~~~~~~~~~~~~~~~~~~~~~

::

    (1 2 3 0 -- 3 2 1) ([1 ...] -- [...])

Re-label (instead of adding left and right tags I'm just taking the next
available index number for the right-side stack effect comment):

::

    (1 2 3 0 -- 3 2 1) ([4 ...] -- [...])

Unify and update:

::

    (1       2 3 0 -- 3 2 1) ([4 ...] -- [...])
                                                 w/ {1: [4 ...]}
    ([4 ...] 2 3 0 -- 3 2  ) (        -- [...])

Apply the first rule:

::

       ([4 ...] 2 3 0 -- 3 2) (-- [...])
    ---------------------------------------
         ([4 ...] 2 3 0 -- 3 2 [...])

And there we are.

``pop∘swap∘roll<∘rest rest``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's do it again.

::

    ([4 ...] 2 3 0 -- 3 2 [...]) ([1 ...] -- [...])

Re-label (the tails of the lists on each side each get their own label):

::

    ([4 .0.] 2 3 0 -- 3 2 [.0.]) ([5 .1.] -- [.1.])

Unify and update (note the opening square brackets have been omited in
the substitution dict, this is deliberate and I'll explain below):

::

    ([4 .0.]   2 3 0 -- 3 2 [.0.]  ) ([5 .1.] -- [.1.])
                                                        w/ { .0.] : 5 .1.] }
    ([4 5 .1.] 2 3 0 -- 3 2 [5 .1.]) ([5 .1.] -- [.1.])

How do we find ``.0.]`` in ``[4 .0.]`` and replace it with ``5 .1.]``
getting the result ``[4 5 .1.]``? This might seem hard, but because the
underlying structure of the Joy list is a cons-list in Python it's
actually pretty easy. I'll explain below.

Next we unify and find our two terms are the same already: ``[5 .1.]``:

::

    ([4 5 .1.] 2 3 0 -- 3 2 [5 .1.]) ([5 .1.] -- [.1.])

Giving us:

::

    ([4 5 .1.] 2 3 0 -- 3 2) (-- [.1.])

From here we apply the first rule and get:

::

    ([4 5 .1.] 2 3 0 -- 3 2 [.1.])

Cleaning up the labels:

::

    ([4 5 ...] 2 3 1 -- 3 2 [...])

This is the stack effect of ``pop∘swap∘roll<∘rest∘rest``.

``pop∘swap∘roll<∘rest∘rest cons``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    ([4 5 ...] 2 3 1 -- 3 2 [...]) (1 [...] -- [1 ...])

Re-label:

::

    ([4 5 .1.] 2 3 1 -- 3 2 [.1.]) (6 [.2.] -- [6 .2.])

Unify:

::

    ([4 5 .1.] 2 3 1 -- 3 2 [.1.]) (6 [.2.] -- [6 .2.])
                                                         w/ { .1.] : .2.] }
    ([4 5 .2.] 2 3 1 -- 3 2      ) (6       -- [6 .2.])
                                                         w/ {2: 6}
    ([4 5 .2.] 6 3 1 -- 3        ) (        -- [6 .2.])

First rule:

::

    ([4 5 .2.] 6 3 1 -- 3 [6 .2.])

Re-label:

::

    ([4 5 ...] 2 3 1 -- 3 [2 ...])

Done.

``pop∘swap∘roll<∘rest∘rest∘cons cons``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One more time.

::

    ([4 5 ...] 2 3 1 -- 3 [2 ...]) (1 [...] -- [1 ...])

Re-label:

::

    ([4 5 .1.] 2 3 1 -- 3 [2 .1.]) (6 [.2.] -- [6 .2.])

Unify:

::

    ([4 5 .1.] 2 3 1 -- 3 [2 .1.]) (6 [.2.] -- [6 .2.]  )
                                                           w/ { .2.] : 2 .1.] }
    ([4 5 .1.] 2 3 1 -- 3        ) (6       -- [6 2 .1.])
                                                           w/ {3: 6}
    ([4 5 .1.] 2 6 1 --          ) (        -- [6 2 .1.])

First or second rule:

::

    ([4 5 .1.] 2 6 1 -- [6 2 .1.])

Clean up the labels:

::

    ([4 5 ...] 2 3 1 -- [3 2 ...])

And there you have it, the stack effect for
``pop∘swap∘roll<∘rest∘rest∘cons∘cons``.

::

    ([4 5 ...] 2 3 1 -- [3 2 ...])

From this stack effect comment it should be possible to construct the
following Python code:

.. code:: ipython2

    def F(stack):
        (_, (d, (c, ((a, (b, S0)), stack)))) = stack
        return (d, (c, S0)), stack

Part II: Implementation
-----------------------

Representing Stack Effect Comments in Python
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I'm going to use pairs of tuples of type descriptors, which will be
integers or tuples of type descriptors:

.. code:: ipython2

    roll_dn = (1, 2, 3), (2, 3, 1)
    
    pop = (1,), ()
    
    swap = (1, 2), (2, 1)

``compose()``
~~~~~~~~~~~~~

.. code:: ipython2

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

``unify()``
~~~~~~~~~~~

.. code:: ipython2

    def unify(u, v, s=None):
        if s is None:
            s = {}
    
        if isinstance(u, int):
            s[u] = v
        elif isinstance(v, int):
            s[v] = u
        else:
            s = False
    
        return s

``update()``
~~~~~~~~~~~~

.. code:: ipython2

    def update(s, term):
        if not isinstance(term, tuple):
            return s.get(term, term)
        return tuple(update(s, inner) for inner in term)

``relabel()``
~~~~~~~~~~~~~

.. code:: ipython2

    def relabel(left, right):
        return left, _1000(right)
    
    def _1000(right):
        if not isinstance(right, tuple):
            return 1000 + right
        return tuple(_1000(n) for n in right)
    
    relabel(pop, swap)




.. parsed-literal::

    (((1,), ()), ((1001, 1002), (1002, 1001)))



``delabel()``
~~~~~~~~~~~~~

.. code:: ipython2

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




.. parsed-literal::

    (((0,), ()), ((1, 2), (2, 1)))



``C()``
~~~~~~~

At last we put it all together in a function ``C()`` that accepts two
stack effect comments and returns their composition (or raises and
exception if they can't be composed due to type conflicts.)

.. code:: ipython2

    def C(f, g):
        f, g = relabel(f, g)
        fg = compose(f, g)
        return delabel(fg)

Let's try it out.

.. code:: ipython2

    C(pop, swap)




.. parsed-literal::

    ((1, 2, 0), (2, 1))



.. code:: ipython2

    C(C(pop, swap), roll_dn)




.. parsed-literal::

    ((3, 1, 2, 0), (2, 1, 3))



.. code:: ipython2

    C(swap, roll_dn)




.. parsed-literal::

    ((2, 0, 1), (1, 0, 2))



.. code:: ipython2

    C(pop, C(swap, roll_dn))




.. parsed-literal::

    ((3, 1, 2, 0), (2, 1, 3))



.. code:: ipython2

    poswrd = reduce(C, (pop, swap, roll_dn))
    poswrd




.. parsed-literal::

    ((3, 1, 2, 0), (2, 1, 3))



Stack Functions
~~~~~~~~~~~~~~~

Here's that trick to represent functions like ``rest`` and ``cons`` that
manipulate stacks. We use a cons-list of tuples and give the tails their
own numbers. Then everything above already works.

.. code:: ipython2

    rest = ((1, 2),), (2,)
    
    cons = (1, 2), ((1, 2),)

.. code:: ipython2

    C(poswrd, rest)




.. parsed-literal::

    (((3, 4), 1, 2, 0), (2, 1, 4))



Compare this to the stack effect comment we wrote above:

::

    ((  (3, 4), 1, 2, 0 ), ( 2, 1,   4  ))
    (   [4 ...] 2  3  0  --  3  2  [...])

The translation table, if you will, would be:

::

    {
    3: 4,
    4: ...],
    1: 2,
    2: 3,
    0: 0,
    }

.. code:: ipython2

    F = reduce(C, (pop, swap, roll_dn, rest, rest, cons, cons))
    
    F




.. parsed-literal::

    (((3, (4, 5)), 1, 2, 0), ((2, (1, 5)),))



Compare with the stack effect comment and you can see it works fine:

::

    ([4 5 ...] 2 3 1 -- [3 2 ...])
      3 4  5   1 2 0     2 1  5

Dealing with ``cons`` and ``uncons``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

However, if we try to compose e.g. ``cons`` and ``uncons`` it won't
work:

.. code:: ipython2

    uncons = ((1, 2),), (1, 2)

.. code:: ipython2

    try:
        C(cons, uncons)
    except Exception, e:
        print e


.. parsed-literal::

    Cannot unify (1, 2) and (1001, 1002).


``unify()`` version 2
^^^^^^^^^^^^^^^^^^^^^

The problem is that the ``unify()`` function as written doesn't handle
the case when both terms are tuples. We just have to add a clause to
deal with this recursively:

.. code:: ipython2

    def unify(u, v, s=None):
        if s is None:
            s = {}
        elif s:
            u = update(s, u)
            v = update(s, v)
    
        if isinstance(u, int):
            s[u] = v
    
        elif isinstance(v, int):
            s[v] = u
    
        elif isinstance(u, tuple) and isinstance(v, tuple):
    
            if len(u) != 2 or len(v) != 2:
                # Not a type error, caller passed in a bad value.
                raise ValueError(repr((u, v)))  # FIXME this message sucks.
    
            (a, b), (c, d) = u, v
            s = unify(a, c, s)
            if s != False:
                s = unify(b, d, s)
        else:
            s = False
    
        return s

.. code:: ipython2

    C(cons, uncons)




.. parsed-literal::

    ((0, 1), (0, 1))



Part III: Compiling Yin Functions
---------------------------------

Now consider the Python function we would like to derive:

.. code:: ipython2

    def F_python(stack):
        (_, (d, (c, ((a, (b, S0)), stack)))) = stack
        return (d, (c, S0)), stack

And compare it to the input stack effect comment tuple we just computed:

.. code:: ipython2

    F[0]




.. parsed-literal::

    ((3, (4, 5)), 1, 2, 0)



The stack-de-structuring tuple has nearly the same form as our input
stack effect comment tuple, just in the reverse order:

::

    (_, (d, (c, ((a, (b, S0)), stack))))

Remove the punctuation:

::

     _   d   c   (a, (b, S0))

Reverse the order and compare:

::

     (a, (b, S0))   c   d   _
    ((3, (4, 5 )),  1,  2,  0)

Eh?

And the return tuple

.. code:: ipython2

    F[1]




.. parsed-literal::

    ((2, (1, 5)),)



is similar to the output stack effect comment tuple:

::

    ((d, (c, S0)), stack)
    ((2, (1, 5 )),      )

This should make it pretty easy to write a Python function that accepts
the stack effect comment tuples and returns a new Python function
(either as a string of code or a function object ready to use) that
performs the semantics of that Joy function (described by the stack
effect.)

Python Identifiers
~~~~~~~~~~~~~~~~~~

We want to substitute Python identifiers for the integers. I'm going to
repurpose ``joy.parser.Symbol`` class for this:

.. code:: ipython2

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

``doc_from_stack_effect()``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As a convenience I've implemented a function to convert the Python stack
effect comment tuples to reasonable text format. There are some details
in how this code works that related to stuff later in the notebook, so
you should skip it for now and read it later if you're interested.

.. code:: ipython2

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

``compile_()``
~~~~~~~~~~~~~~

Now we can write a compiler function to emit Python source code. (The
underscore suffix distiguishes it from the built-in ``compile()``
function.)

.. code:: ipython2

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

Here it is in action:

.. code:: ipython2

    source = compile_('F', F)
    
    print source


.. parsed-literal::

    def F(stack):
        """([3 4 .5.] 1 2 0 -- [2 1 .5.])"""
        (a5, (a4, (a3, ((a0, (a1, a2)), stack)))) = stack
        return ((a4, (a3, a2)), stack)


Compare:

.. code:: ipython2

    def F_python(stack):
        (_, (d, (c, ((a, (b, S0)), stack)))) = stack
        return ((d, (c, S0)), stack)

Next steps:

.. code:: ipython2

    L = {}
    
    eval(compile(source, '__main__', 'single'), {}, L)
    
    L['F']




.. parsed-literal::

    <function F>



Let's try it out:

.. code:: ipython2

    from notebook_preamble import D, J, V
    from joy.library import SimpleFunctionWrapper

.. code:: ipython2

    D['F'] = SimpleFunctionWrapper(L['F'])

.. code:: ipython2

    J('[4 5 ...] 2 3 1 F')


.. parsed-literal::

    [3 2 ...]


With this, we have a partial Joy compiler that works on the subset of
Joy functions that manipulate stacks (both what I call "stack chatter"
and the ones that manipulate stacks on the stack.)

I'm probably going to modify the definition wrapper code to detect
definitions that can be compiled by this partial compiler and do it
automatically. It might be a reasonable idea to detect sequences of
compilable functions in definitions that have uncompilable functions in
them and just compile those. However, if your library is well-factored
this might be less helpful.

Compiling Library Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use ``compile_()`` to generate many primitives in the library
from their stack effect comments:

.. code:: ipython2

    def defs():
    
        rolldown = (1, 2, 3), (2, 3, 1)
    
        rollup = (1, 2, 3), (3, 1, 2)
    
        pop = (1,), ()
    
        swap = (1, 2), (2, 1)
    
        rest = ((1, 2),), (2,)
        
        rrest = C(rest, rest)
    
        cons = (1, 2), ((1, 2),)
    
        uncons = ((1, 2),), (1, 2)
        
        swons = C(swap, cons)
    
        return locals()

.. code:: ipython2

    for name, stack_effect_comment in sorted(defs().items()):
        print
        print compile_(name, stack_effect_comment)
        print


.. parsed-literal::

    
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
    
    
    def rolldown(stack):
        """(1 2 3 -- 2 3 1)"""
        (a2, (a1, (a0, stack))) = stack
        return (a0, (a2, (a1, stack)))
    
    
    def rollup(stack):
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
    


Part IV: Types and Subtypes of Arguments
----------------------------------------

So far we have dealt with types of functions, those dealing with simple
stack manipulation. Let's extend our machinery to deal with types of
arguments.

"Number" Type
~~~~~~~~~~~~~

Consider the definition of ``sqr``:

::

    sqr == dup mul

The ``dup`` function accepts one *anything* and returns two of that:

::

    dup (1 -- 1 1)

And ``mul`` accepts two "numbers" (we're ignoring ints vs. floats vs.
complex, etc., for now) and returns just one:

::

    mul (n n -- n)

So we're composing:

::

    (1 -- 1 1)∘(n n -- n)

The rules say we unify 1 with ``n``:

::

       (1 -- 1 1)∘(n n -- n)
    ---------------------------  w/  {1: n}
       (1 -- 1  )∘(n   -- n)

This involves detecting that "Any type" arguments can accept "numbers".
If we were composing these functions the other way round this is still
the case:

::

       (n n -- n)∘(1 -- 1 1)
    ---------------------------  w/  {1: n}
       (n n --  )∘(  -- n n) 

The important thing here is that the mapping is going the same way in
both cases, from the "any" integer to the number

Distinguishing Numbers
~~~~~~~~~~~~~~~~~~~~~~

We should also mind that the number that ``mul`` produces is not
(necessarily) the same as either of its inputs, which are not
(necessarily) the same as each other:

::

    mul (n2 n1 -- n3)


       (1  -- 1  1)∘(n2 n1 -- n3)
    --------------------------------  w/  {1: n2}
       (n2 -- n2  )∘(n2    -- n3)


       (n2 n1 -- n3)∘(1 -- 1  1 )
    --------------------------------  w/  {1: n3}
       (n2 n1 --   )∘(  -- n3 n3) 

Distinguishing Types
~~~~~~~~~~~~~~~~~~~~

So we need separate domains of "any" numbers and "number" numbers, and
we need to be able to ask the order of these domains. Now the notes on
the right side of rule three make more sense, eh?

::

       (a -- b t[i])∘(c u[j] -- d)   t <= u (t is subtype of u)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == t[k] == u[j]
                                             ^

       (a -- b t[i])∘(c u[j] -- d)   u <= t (u is subtype of t)
    -------------------------------
       (a -- b     )∘(c      -- d)   t[i] == u[k] == u[j]

The indices ``i``, ``k``, and ``j`` are the number part of our labels
and ``t`` and ``u`` are the domains.

By creative use of Python's "double underscore" methods we can define a
Python class hierarchy of Joy types and use the ``issubclass()`` method
to establish domain ordering, as well as other handy behaviour that will
make it fairly easy to reuse most of the code above.

.. code:: ipython2

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

Mess with it a little:

.. code:: ipython2

    from itertools import permutations

"Any" types can be specialized to numbers and stacks, but not vice
versa:

.. code:: ipython2

    for a, b in permutations((A[0], N[0], S[0]), 2):
        print a, '>=', b, '->', a >= b


.. parsed-literal::

    a0 >= n0 -> True
    a0 >= s0 -> True
    n0 >= a0 -> False
    n0 >= s0 -> False
    s0 >= a0 -> False
    s0 >= n0 -> False


Our crude `Numerical
Tower <https://en.wikipedia.org/wiki/Numerical_tower>`__ of *numbers* >
*floats* > *integers* works as well (but we're not going to use it yet):

.. code:: ipython2

    for a, b in permutations((A[0], N[0], FloatJoyType(0), IntJoyType(0)), 2):
        print a, '>=', b, '->', a >= b


.. parsed-literal::

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


Typing ``sqr``
~~~~~~~~~~~~~~

.. code:: ipython2

    dup = (A[1],), (A[1], A[1])
    
    mul = (N[1], N[2]), (N[3],)

.. code:: ipython2

    dup




.. parsed-literal::

    ((a1,), (a1, a1))



.. code:: ipython2

    mul




.. parsed-literal::

    ((n1, n2), (n3,))



Modifying the Inferencer
~~~~~~~~~~~~~~~~~~~~~~~~

Re-labeling still works fine:

.. code:: ipython2

    foo = relabel(dup, mul)
    
    foo




.. parsed-literal::

    (((a1,), (a1, a1)), ((n1001, n1002), (n1003,)))



``delabel()`` version 2
^^^^^^^^^^^^^^^^^^^^^^^

The ``delabel()`` function needs an overhaul. It now has to keep track
of how many labels of each domain it has "seen".

.. code:: ipython2

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
            seen[f] = f.__class__(c[f.prefix] + 1)
            c[f.prefix] += 1
            return seen[f]
    
        return tuple(delabel(inner, seen, c) for inner in f)

.. code:: ipython2

    delabel(foo)




.. parsed-literal::

    (((a1,), (a1, a1)), ((n1, n2), (n3,)))



``unify()`` version 3
^^^^^^^^^^^^^^^^^^^^^

.. code:: ipython2

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

Rewrite the stack effect comments:

.. code:: ipython2

    def defs():
    
        rolldown = (A[1], A[2], A[3]), (A[2], A[3], A[1])
    
        rollup = (A[1], A[2], A[3]), (A[3], A[1], A[2])
    
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

.. code:: ipython2

    DEFS = defs()

.. code:: ipython2

    for name, stack_effect_comment in sorted(DEFS.items()):
        print name, '=', doc_from_stack_effect(*stack_effect_comment)


.. parsed-literal::

    ccons = (a1 a2 [.1.] -- [a1 a2 .1.])
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
    rolldown = (a1 a2 a3 -- a2 a3 a1)
    rollup = (a1 a2 a3 -- a3 a1 a2)
    rrest = ([a1 a2 .1.] -- [.1.])
    second = ([a1 a2 .1.] -- a2)
    sqrt = (n1 -- n2)
    succ = (n1 -- n2)
    swap = (a1 a2 -- a2 a1)
    swons = ([.1.] a1 -- [a1 .1.])
    third = ([a1 a2 a3 .1.] -- a3)
    tuck = (a2 a1 -- a1 a2 a1)
    uncons = ([a1 .1.] -- a1 [.1.])


.. code:: ipython2

    globals().update(DEFS)

Compose ``dup`` and ``mul``
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: ipython2

    C(dup, mul)




.. parsed-literal::

    ((n1,), (n2,))



Revisit the ``F`` function, works fine.

.. code:: ipython2

    F = reduce(C, (pop, swap, rolldown, rest, rest, cons, cons))
    F




.. parsed-literal::

    (((a1, (a2, s1)), a3, a4, a5), ((a4, (a3, s1)),))



.. code:: ipython2

    print doc_from_stack_effect(*F)


.. parsed-literal::

    ([a1 a2 .1.] a3 a4 a5 -- [a4 a3 .1.])


Some otherwise inefficient functions are no longer to be feared. We can
also get the effect of combinators in some limited cases.

.. code:: ipython2

    def neato(*funcs):
        print doc_from_stack_effect(*reduce(C, funcs))

.. code:: ipython2

    # e.g. [swap] dip
    neato(rollup, swap, rolldown)


.. parsed-literal::

    (a1 a2 a3 -- a2 a1 a3)


.. code:: ipython2

    # e.g. [popop] dipd
    neato(popdd, rolldown, pop)


.. parsed-literal::

    (a1 a2 a3 a4 -- a3 a4)


.. code:: ipython2

    # Reverse the order of the top three items.
    neato(rollup, swap)


.. parsed-literal::

    (a1 a2 a3 -- a3 a2 a1)


``compile_()`` version 2
^^^^^^^^^^^^^^^^^^^^^^^^

Because the type labels represent themselves as valid Python identifiers
the ``compile_()`` function doesn't need to generate them anymore:

.. code:: ipython2

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

.. code:: ipython2

    print compile_('F', F)


.. parsed-literal::

    def F(stack):
        """([a1 a2 .1.] a3 a4 a5 -- [a4 a3 .1.])"""
        (a5, (a4, (a3, ((a1, (a2, s1)), stack)))) = stack
        return ((a4, (a3, s1)), stack)


But it cannot magically create new functions that involve e.g. math and
such. Note that this is *not* a ``sqr`` function implementation:

.. code:: ipython2

    print compile_('sqr', C(dup, mul))


.. parsed-literal::

    def sqr(stack):
        """(n1 -- n2)"""
        (n1, stack) = stack
        return (n2, stack)


(Eventually I should come back around to this becuase it's not tooo
difficult to exend this code to be able to compile e.g.
``n2 = mul(n1, n1)`` for ``mul`` with the right variable names and
insert it in the right place. It requires a little more support from the
library functions, in that we need to know to call ``mul()`` the Python
function for ``mul`` the Joy function, but since *most* of the math
functions (at least) are already wrappers it should be straightforward.)

``compilable()``
^^^^^^^^^^^^^^^^

The functions that *can* be compiled are the ones that have only
``AnyJoyType`` and ``StackJoyType`` labels in their stack effect
comments. We can write a function to check that:

.. code:: ipython2

    from itertools import imap
    
    
    def compilable(f):
        return isinstance(f, tuple) and all(imap(compilable, f)) or stacky(f)

.. code:: ipython2

    for name, stack_effect_comment in sorted(defs().items()):
        if compilable(stack_effect_comment):
            print name, '=', doc_from_stack_effect(*stack_effect_comment)


.. parsed-literal::

    ccons = (a1 a2 [.1.] -- [a1 a2 .1.])
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
    rolldown = (a1 a2 a3 -- a2 a3 a1)
    rollup = (a1 a2 a3 -- a3 a1 a2)
    rrest = ([a1 a2 .1.] -- [.1.])
    second = ([a1 a2 .1.] -- a2)
    swap = (a1 a2 -- a2 a1)
    swons = ([.1.] a1 -- [a1 .1.])
    third = ([a1 a2 a3 .1.] -- a3)
    tuck = (a2 a1 -- a1 a2 a1)
    uncons = ([a1 .1.] -- a1 [.1.])


Part V: Functions that use the Stack
------------------------------------

Consider the ``stack`` function which grabs the whole stack, quotes it,
and puts it on itself:

::

    stack (...     -- ... [...]        )
    stack (... a   -- ... a [a ...]    )
    stack (... b a -- ... b a [a b ...])

We would like to represent this in Python somehow. To do this we use a
simple, elegant trick.

::

    stack         S   -- (         S,           S)
    stack     (a, S)  -- (     (a, S),      (a, S))
    stack (a, (b, S)) -- ( (a, (b, S)), (a, (b, S)))

Instead of representing the stack effect comments as a single tuple
(with N items in it) we use the same cons-list structure to hold the
sequence and ``unify()`` the whole comments.

``stack∘uncons``
~~~~~~~~~~~~~~~~

Let's try composing ``stack`` and ``uncons``. We want this result:

::

    stack∘uncons (... a -- ... a a [...])

The stack effects are:

::

    stack = S -- (S, S)

    uncons = ((a, Z), S) -- (Z, (a, S))

Unifying:

::

      S    -- (S, S) ∘ ((a, Z), S) -- (Z, (a,   S   ))
                                                        w/ { S: (a, Z) }
    (a, Z) --        ∘             -- (Z, (a, (a, Z)))

So:

::

    stack∘uncons == (a, Z) -- (Z, (a, (a, Z)))

It works.

``stack∘uncons∘uncons``
~~~~~~~~~~~~~~~~~~~~~~~

Let's try ``stack∘uncons∘uncons``:

::

    (a, S     ) -- (S,      (a, (a, S     ))) ∘ ((b, Z),  S`             ) -- (Z, (b,   S`   ))

                                                                                    w/ { S: (b, Z) }
                                                                                    
    (a, (b, Z)) -- ((b, Z), (a, (a, (b, Z)))) ∘ ((b, Z),  S`             ) -- (Z, (b,   S`   ))

                                                                                    w/ { S`: (a, (a, (b, Z))) }
                                                                                    
    (a, (b, Z)) -- ((b, Z), (a, (a, (b, Z)))) ∘ ((b, Z), (a, (a, (b, Z)))) -- (Z, (b, (a, (a, (b, Z)))))

    (a, (b, Z)) -- (Z, (b, (a, (a, (b, Z)))))

It works.

``compose()`` version 2
^^^^^^^^^^^^^^^^^^^^^^^

This function has to be modified to use the new datastructures and it is
no longer recursive, instead recursion happens as part of unification.
Further, the first and second of Pöial's rules are now handled
automatically by the unification algorithm. (One easy way to see this is
that now an empty stack effect comment is represented by a
``StackJoyType`` instance which is not "falsey" and so neither of the
first two rules' ``if`` clauses will ever be ``True``. Later on I change
the "truthiness" of ``StackJoyType`` to false to let e.g.
``joy.utils.stack.concat`` work with our stack effect comment cons-list
tuples.)

.. code:: ipython2

    def compose(f, g):
        (f_in, f_out), (g_in, g_out) = f, g
        s = unify(g_in, f_out)
        if s == False:  # s can also be the empty dict, which is ok.
            raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))
        return update(s, (f_in, g_out))

I don't want to rewrite all the defs myself, so I'll write a little
conversion function instead. This is programmer's laziness.

.. code:: ipython2

    def sequence_to_stack(seq, stack=StackJoyType(23)):
        for item in seq: stack = item, stack
        return stack
    
    NEW_DEFS = {
        name: (sequence_to_stack(i), sequence_to_stack(o))
        for name, (i, o) in DEFS.iteritems()
    }
    NEW_DEFS['stack'] = S[0], (S[0], S[0])
    NEW_DEFS['swaack'] = (S[1], S[0]), (S[0], S[1])
    globals().update(NEW_DEFS)

.. code:: ipython2

    C(stack, uncons)




.. parsed-literal::

    ((a1, s1), (s1, (a1, (a1, s1))))



.. code:: ipython2

    reduce(C, (stack, uncons, uncons))




.. parsed-literal::

    ((a1, (a2, s1)), (s1, (a2, (a1, (a1, (a2, s1))))))



The display function should be changed too.

``doc_from_stack_effect()`` version 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Clunky junk, but it will suffice for now.

.. code:: ipython2

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

.. code:: ipython2

    for name, stack_effect_comment in sorted(NEW_DEFS.items()):
        print name, '=', doc_from_stack_effect(*stack_effect_comment)


.. parsed-literal::

    ccons = (a1 a2 [.1.] -- [a1 a2 .1.])
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
    rolldown = (a1 a2 a3 -- a2 a3 a1)
    rollup = (a1 a2 a3 -- a3 a1 a2)
    rrest = ([a1 a2 .1.] -- [.1.])
    second = ([a1 a2 .1.] -- a2)
    sqrt = (n1 -- n2)
    stack = (... -- ... [...])
    succ = (n1 -- n2)
    swaack = ([.1.] -- [.0.])
    swap = (a1 a2 -- a2 a1)
    swons = ([.1.] a1 -- [a1 .1.])
    third = ([a1 a2 a3 .1.] -- a3)
    tuck = (a2 a1 -- a1 a2 a1)
    uncons = ([a1 .1.] -- a1 [.1.])


.. code:: ipython2

    print ; print doc_from_stack_effect(*stack)
    print ; print doc_from_stack_effect(*C(stack, uncons))
    print ; print doc_from_stack_effect(*reduce(C, (stack, uncons, uncons)))
    print ; print doc_from_stack_effect(*reduce(C, (stack, uncons, cons)))


.. parsed-literal::

    
    (... -- ... [...])
    
    (... a1 -- ... a1 a1 [...])
    
    (... a2 a1 -- ... a2 a1 a1 a2 [...])
    
    (... a1 -- ... a1 [a1 ...])


.. code:: ipython2

    print doc_from_stack_effect(*C(ccons, stack))


.. parsed-literal::

    (... a2 a1 [.1.] -- ... [a2 a1 .1.] [[a2 a1 .1.] ...])


.. code:: ipython2

    Q = C(ccons, stack)
    
    Q




.. parsed-literal::

    ((s1, (a1, (a2, s2))), (((a2, (a1, s1)), s2), ((a2, (a1, s1)), s2)))



``compile_()`` version 3
^^^^^^^^^^^^^^^^^^^^^^^^

This makes the ``compile_()`` function pretty simple as the stack effect
comments are now already in the form needed for the Python code:

.. code:: ipython2

    def compile_(name, f, doc=None):
        i, o = f
        if doc is None:
            doc = doc_from_stack_effect(i, o)
        return '''def %s(stack):
        """%s"""
        %s = stack
        return %s''' % (name, doc, i, o)

.. code:: ipython2

    print compile_('Q', Q)


.. parsed-literal::

    def Q(stack):
        """(... a2 a1 [.1.] -- ... [a2 a1 .1.] [[a2 a1 .1.] ...])"""
        (s1, (a1, (a2, s2))) = stack
        return (((a2, (a1, s1)), s2), ((a2, (a1, s1)), s2))


.. code:: ipython2

    unstack = (S[1], S[0]), S[1]
    enstacken = S[0], (S[0], S[1])

.. code:: ipython2

    print doc_from_stack_effect(*unstack)


.. parsed-literal::

    ([.1.] --)


.. code:: ipython2

    print doc_from_stack_effect(*enstacken)


.. parsed-literal::

    (-- [.0.])


.. code:: ipython2

    print doc_from_stack_effect(*C(cons, unstack))


.. parsed-literal::

    (a1 [.1.] -- a1)


.. code:: ipython2

    print doc_from_stack_effect(*C(cons, enstacken))


.. parsed-literal::

    (a1 [.1.] -- [[a1 .1.] .2.])


.. code:: ipython2

    C(cons, unstack)




.. parsed-literal::

    ((s1, (a1, s2)), (a1, s1))



Part VI: Multiple Stack Effects
-------------------------------

...

.. code:: ipython2

    class IntJoyType(NumberJoyType): prefix = 'i'
    
    
    F = map(FloatJoyType, _R)
    I = map(IntJoyType, _R)

.. code:: ipython2

    muls = [
         ((I[2], (I[1], S[0])), (I[3], S[0])),
         ((F[2], (I[1], S[0])), (F[3], S[0])),
         ((I[2], (F[1], S[0])), (F[3], S[0])),
         ((F[2], (F[1], S[0])), (F[3], S[0])),
    ]

.. code:: ipython2

    for f in muls:
        print doc_from_stack_effect(*f)


.. parsed-literal::

    (i1 i2 -- i3)
    (i1 f2 -- f3)
    (f1 i2 -- f3)
    (f1 f2 -- f3)


.. code:: ipython2

    for f in muls:
        try:
            e = C(dup, f)
        except TypeError:
            continue
        print doc_from_stack_effect(*dup), doc_from_stack_effect(*f), doc_from_stack_effect(*e)


.. parsed-literal::

    (a1 -- a1 a1) (i1 i2 -- i3) (i1 -- i2)
    (a1 -- a1 a1) (f1 f2 -- f3) (f1 -- f2)


.. code:: ipython2

    from itertools import product
    
    
    def meta_compose(F, G):
        for f, g in product(F, G):
            try:
                yield C(f, g)
            except TypeError:
                pass
    
    
    def MC(F, G):
        return sorted(set(meta_compose(F, G)))

.. code:: ipython2

    for f in MC([dup], [mul]):
        print doc_from_stack_effect(*f)


.. parsed-literal::

    (n1 -- n2)


.. code:: ipython2

    for f in MC([dup], muls):
        print doc_from_stack_effect(*f)


.. parsed-literal::

    (f1 -- f2)
    (i1 -- i2)


Representing an Unbounded Sequence of Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can borrow a trick from `Brzozowski's Derivatives of Regular
Expressions <https://en.wikipedia.org/wiki/Brzozowski_derivative>`__ to
invent a new type of type variable, a "sequence type" (I think this is
what they mean in the literature by that term...) or "`Kleene
Star <https://en.wikipedia.org/wiki/Kleene_star>`__" type. I'm going to
represent it as a type letter and the asterix, so a sequence of zero or
more ``AnyJoyType`` variables would be:

::

    A*

The ``A*`` works by splitting the universe into two alternate histories:

::

    A* -> 0 | A A*

The Kleene star variable disappears in one universe, and in the other it
turns into an ``AnyJoyType`` variable followed by itself again. We have
to return all universes (represented by their substitution dicts, the
"unifiers") that don't lead to type conflicts.

Consider unifying two stacks (the lowercase letters are any type
variables of the kinds we have defined so far):

::

    [a A* b .0.] U [c d .1.]
                              w/ {c: a}
    [  A* b .0.] U [  d .1.]

Now we have to split universes to unify ``A*``. In the first universe it
disappears:

::

    [b .0.] U [d .1.]
                       w/ {d: b, .1.: .0.} 
         [] U []

While in the second it spawns an ``A``, which we will label ``e``:

::

    [e A* b .0.] U [d .1.]
                            w/ {d: e}
    [  A* b .0.] U [  .1.]
                            w/ {.1.: A* b .0.}
    [  A* b .0.] U [  A* b .0.]

Giving us two unifiers:

::

    {c: a,  d: b,  .1.:      .0.}
    {c: a,  d: e,  .1.: A* b .0.}

.. code:: ipython2

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

``unify()`` version 4
^^^^^^^^^^^^^^^^^^^^^

Can now return multiple results...

.. code:: ipython2

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

.. code:: ipython2

    a = (As[1], S[1])
    a




.. parsed-literal::

    (a1*, s1)



.. code:: ipython2

    b = (A[1], S[2])
    b




.. parsed-literal::

    (a1, s2)



.. code:: ipython2

    for result in unify(b, a):
        print result, '->', update(result, a), update(result, b)


.. parsed-literal::

    {s1: (a1, s2)} -> (a1*, (a1, s2)) (a1, s2)
    {a1: a10001, s2: (a1*, s1)} -> (a1*, s1) (a10001, (a1*, s1))


.. code:: ipython2

    for result in unify(a, b):
        print result, '->', update(result, a), update(result, b)


.. parsed-literal::

    {s1: (a1, s2)} -> (a1*, (a1, s2)) (a1, s2)
    {a1: a10002, s2: (a1*, s1)} -> (a1*, s1) (a10002, (a1*, s1))


::

    (a1*, s1)       [a1*]       (a1, s2)        [a1]

    (a1*, (a1, s2)) [a1* a1]    (a1, s2)        [a1]

    (a1*, s1)       [a1*]       (a2, (a1*, s1)) [a2 a1*]

.. code:: ipython2

    sum_ = ((Ns[1], S[1]), S[0]), (N[0], S[0])
    
    print doc_from_stack_effect(*sum_)


.. parsed-literal::

    ([n1* .1.] -- n0)


.. code:: ipython2

    f = (N[1], (N[2], (N[3], S[1]))), S[0]
    
    print doc_from_stack_effect(S[0], f)


.. parsed-literal::

    (-- [n1 n2 n3 .1.])


.. code:: ipython2

    for result in unify(sum_[0], f):
        print result, '->', update(result, sum_[1])


.. parsed-literal::

    {s1: (n1, (n2, (n3, s1)))} -> (n0, s0)
    {n1: n10001, s1: (n2, (n3, s1))} -> (n0, s0)
    {n1: n10001, s1: (n3, s1), n2: n10002} -> (n0, s0)
    {n1: n10001, s1: (n1*, s1), n3: n10003, n2: n10002} -> (n0, s0)


``compose()`` version 3
^^^^^^^^^^^^^^^^^^^^^^^

This function has to be modified to yield multiple results.

.. code:: ipython2

    def compose(f, g):
        (f_in, f_out), (g_in, g_out) = f, g
        s = unify(g_in, f_out)
        if not s:
            raise TypeError('Cannot unify %r and %r.' % (f_out, g_in))
        for result in s:
            yield update(result, (f_in, g_out))


.. code:: ipython2

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

.. code:: ipython2

    for f in MC([dup], muls):
        print doc_from_stack_effect(*f)


.. parsed-literal::

    (f1 -- f2)
    (i1 -- i2)


.. code:: ipython2

    
    
    for f in MC([dup], [sum_]):
        print doc_from_stack_effect(*f)


.. parsed-literal::

    ([n1* .1.] -- [n1* .1.] n1)


.. code:: ipython2

    
    
    for f in MC([cons], [sum_]):
        print doc_from_stack_effect(*f)


.. parsed-literal::

    (a1 [.1.] -- n1)
    (n1 [n1* .1.] -- n2)


.. code:: ipython2

    sum_ = (((N[1], (Ns[1], S[1])), S[0]), (N[0], S[0]))
    print doc_from_stack_effect(*cons),
    print doc_from_stack_effect(*sum_),
    
    for f in MC([cons], [sum_]):
        print doc_from_stack_effect(*f)


.. parsed-literal::

    (a1 [.1.] -- [a1 .1.]) ([n1 n1* .1.] -- n0) (n1 [n1* .1.] -- n2)


.. code:: ipython2

    a = (A[4], (As[1], (A[3], S[1])))
    a




.. parsed-literal::

    (a4, (a1*, (a3, s1)))



.. code:: ipython2

    b = (A[1], (A[2], S[2]))
    b




.. parsed-literal::

    (a1, (a2, s2))



.. code:: ipython2

    for result in unify(b, a):
        print result


.. parsed-literal::

    {a1: a4, s2: s1, a2: a3}
    {a1: a4, s2: (a1*, (a3, s1)), a2: a10003}


.. code:: ipython2

    for result in unify(a, b):
        print result


.. parsed-literal::

    {s2: s1, a2: a3, a4: a1}
    {s2: (a1*, (a3, s1)), a2: a10004, a4: a1}


Part VII: Typing Combinators
----------------------------

In order to compute the stack effect of combinators you kinda have to
have the quoted programs they expect available. In the most general
case, the ``i`` combinator, you can't say anything about its stack
effect other than it expects one quote:

::

    i (... [.1.] -- ... .1.)

Or

::

    i (... [A* .1.] -- ... A*)

Consider the type of:

::

    [cons] dip

Obviously it would be:

::

    (a1 [..1] a2 -- [a1 ..1] a2)

``dip`` itself could have:

::

    (a1 [..1] -- ... then what?

Without any information about the contents of the quote we can't say
much about the result.

Hybrid Inferencer/Interpreter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I think there's a way forward. If we convert our list (of terms we are
composing) into a stack structure we can use it as a *Joy expression*,
then we can treat the *output half* of a function's stack effect comment
as a Joy interpreter stack, and just execute combinators directly. We
can hybridize the compostition function with an interpreter to evaluate
combinators, compose non-combinator functions, and put type variables on
the stack. For combinators like ``branch`` that can have more than one
stack effect we have to "split universes" again and return both.

Joy Types for Functions
^^^^^^^^^^^^^^^^^^^^^^^

We need a type variable for Joy functions that can go in our expressions
and be used by the hybrid inferencer/interpreter. They have to store a
name and a list of stack effects.

.. code:: ipython2

    class FunctionJoyType(AnyJoyType):
    
        def __init__(self, name, sec, number):
            self.name = name
            self.stack_effects = sec
            self.number = number
    
        def __add__(self, other):
            return self
        __radd__ = __add__
    
        def __repr__(self):
            return self.name

Specialized for Simple Functions and Combinators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For non-combinator functions the stack effects list contains stack
effect comments (represented by pairs of cons-lists as described above.)

.. code:: ipython2

    class SymbolJoyType(FunctionJoyType):
        prefix = 'F'

For combinators the list contains Python functions.

.. code:: ipython2

    class CombinatorJoyType(FunctionJoyType):
    
        prefix = 'C'
    
        def __init__(self, name, sec, number, expect=None):
            super(CombinatorJoyType, self).__init__(name, sec, number)
            self.expect = expect
    
        def enter_guard(self, f):
            if self.expect is None:
                return f
            g = self.expect, self.expect
            new_f = list(compose(f, g, ()))
            assert len(new_f) == 1, repr(new_f)
            return new_f[0][1]

For simple combinators that have only one effect (like ``dip``) you only
need one function and it can be the combinator itself.

.. code:: ipython2

    import joy.library
    
    dip = CombinatorJoyType('dip', [joy.library.dip], 23)

For combinators that can have more than one effect (like ``branch``) you
have to write functions that each implement the action of one of the
effects.

.. code:: ipython2

    def branch_true(stack, expression, dictionary):
        (then, (else_, (flag, stack))) = stack
        return stack, concat(then, expression), dictionary
    
    def branch_false(stack, expression, dictionary):
        (then, (else_, (flag, stack))) = stack
        return stack, concat(else_, expression), dictionary
    
    branch = CombinatorJoyType('branch', [branch_true, branch_false], 100)

You can also provide an optional stack effect, input-side only, that
will then be used as an identity function (that accepts and returns
stacks that match the "guard" stack effect) which will be used to guard
against type mismatches going into the evaluation of the combinator.

``infer()``
^^^^^^^^^^^

With those in place, we can define a function that accepts a sequence of
Joy type variables, including ones representing functions (not just
values), and attempts to grind out all the possible stack effects of
that expression.

One tricky thing is that type variables *in the expression* have to be
updated along with the stack effects after doing unification or we risk
losing useful information. This was a straightforward, if awkward,
modification to the call structure of ``meta_compose()`` et. al.

.. code:: ipython2

    ID = S[0], S[0]  # Identity function.
    
    
    def infer(*expression):
        return sorted(set(_infer(list_to_stack(expression))))
    
    
    def _infer(e, F=ID):
        _log_it(e, F)
        if not e:
            return [F]
    
        n, e = e
    
        if isinstance(n, SymbolJoyType):
            eFG = meta_compose([F], n.stack_effects, e)
            res = flatten(_infer(e, Fn) for e, Fn in eFG)
    
        elif isinstance(n, CombinatorJoyType):
            fi, fo = n.enter_guard(F)
            res = flatten(_interpret(f, fi, fo, e) for f in n.stack_effects)
    
        elif isinstance(n, Symbol):
            assert n not in FUNCTIONS, repr(n)
            func = joy.library._dictionary[n]
            res = _interpret(func, F[0], F[1], e)
    
        else:
            fi, fo = F
            res = _infer(e, (fi, (n, fo)))
    
        return res
    
    
    def _interpret(f, fi, fo, e):
        new_fo, ee, _ = f(fo, e, {})
        ee = update(FUNCTIONS, ee)  # Fix Symbols.
        new_F = fi, new_fo
        return _infer(ee, new_F)
    
    
    def _log_it(e, F):
        _log.info(
            u'%3i %s ∘ %s',
            len(inspect_stack()),
            doc_from_stack_effect(*F),
            expression_to_string(e),
            )

Work in Progress
^^^^^^^^^^^^^^^^

And that brings us to current Work-In-Progress. The mixed-mode
inferencer/interpreter ``infer()`` function seems to work well. There
are details I should document, and the rest of the code in the
"polytypes" module (FIXME link to its docs here!) should be explained...
There is cruft to convert the definitions in ``DEFS`` to the new
``SymbolJoyType`` objects, and some combinators. Here is an example of
output from the current code :

.. code:: ipython2

    1/0  # (Don't try to run this cell!  It's not going to work.  This is "read only" code heh..)
    
    logging.basicConfig(format='%(message)s', stream=sys.stdout, level=logging.INFO)
    
    globals().update(FUNCTIONS)
    
    h = infer((pred, s2), (mul, s3), (div, s4), (nullary, (bool, s5)), dipd, branch)
    
    print '-' * 40
    
    for fi, fo in h:
        print doc_from_stack_effect(fi, fo)

The numbers at the start of the lines are the current depth of the
Python call stack. They're followed by the current computed stack effect
(initialized to ``ID``) then the pending expression (the inference of
the stack effect of which is the whole object of the current example.)

In this example we are implementing (and inferring) ``ifte`` as
``[nullary bool] dipd branch`` which shows off a lot of the current
implementation in action.

::

      7 (--) ∘ [pred] [mul] [div] [nullary bool] dipd branch
      8 (-- [pred ...2]) ∘ [mul] [div] [nullary bool] dipd branch
      9 (-- [pred ...2] [mul ...3]) ∘ [div] [nullary bool] dipd branch
     10 (-- [pred ...2] [mul ...3] [div ...4]) ∘ [nullary bool] dipd branch
     11 (-- [pred ...2] [mul ...3] [div ...4] [nullary bool ...5]) ∘ dipd branch
     15 (-- [pred ...5]) ∘ nullary bool [mul] [div] branch
     19 (-- [pred ...2]) ∘ [stack] dinfrirst bool [mul] [div] branch
     20 (-- [pred ...2] [stack ]) ∘ dinfrirst bool [mul] [div] branch
     22 (-- [pred ...2] [stack ]) ∘ dip infra first bool [mul] [div] branch
     26 (--) ∘ stack [pred] infra first bool [mul] [div] branch
     29 (... -- ... [...]) ∘ [pred] infra first bool [mul] [div] branch
     30 (... -- ... [...] [pred ...1]) ∘ infra first bool [mul] [div] branch
     34 (--) ∘ pred s1 swaack first bool [mul] [div] branch
     37 (n1 -- n2) ∘ [n1] swaack first bool [mul] [div] branch
     38 (... n1 -- ... n2 [n1 ...]) ∘ swaack first bool [mul] [div] branch
     41 (... n1 -- ... n1 [n2 ...]) ∘ first bool [mul] [div] branch
     44 (n1 -- n1 n2) ∘ bool [mul] [div] branch
     47 (n1 -- n1 b1) ∘ [mul] [div] branch
     48 (n1 -- n1 b1 [mul ...1]) ∘ [div] branch
     49 (n1 -- n1 b1 [mul ...1] [div ...2]) ∘ branch
     53 (n1 -- n1) ∘ div
     56 (f2 f1 -- f3) ∘ 
     56 (i1 f1 -- f2) ∘ 
     56 (f1 i1 -- f2) ∘ 
     56 (i2 i1 -- f1) ∘ 
     53 (n1 -- n1) ∘ mul
     56 (f2 f1 -- f3) ∘ 
     56 (i1 f1 -- f2) ∘ 
     56 (f1 i1 -- f2) ∘ 
     56 (i2 i1 -- i3) ∘ 
    ----------------------------------------
    (f2 f1 -- f3)
    (i1 f1 -- f2)
    (f1 i1 -- f2)
    (i2 i1 -- f1)
    (i2 i1 -- i3)

Conclusion
----------

We built a simple type inferencer, and a kind of crude "compiler" for a
subset of Joy functions. Then we built a more powerful inferencer that
actually does some evaluation and explores branching code paths

Work remains to be done:

-  the rest of the library has to be covered
-  figure out how to deal with ``loop`` and ``genrec``, etc..
-  extend the types to check values (see the appendix)
-  other kinds of "higher order" type variables, OR, AND, etc..
-  maybe rewrite in Prolog for great good?
-  definitions
-  don't permit composition of functions that don't compose
-  auto-compile compilable functions
-  Compiling more than just the Yin functions.
-  getting better visibility (than Python debugger.)
-  DOOOOCS!!!! Lots of docs!
-  docstrings all around
-  improve this notebook (it kinda falls apart at the end narratively. I
   went off and just started writing code to see if it would work. It
   does, but now I have to come back and describe here what I did.

Appendix: Joy in the Logical Paradigm
-------------------------------------

For *type checking* to work the type label classes have to be modified
to let ``T >= t`` succeed, where e.g. ``T`` is ``IntJoyType`` and ``t``
is ``int``. If you do that you can take advantage of the *logical
relational* nature of the stack effect comments to "compute in reverse"
as it were. There's a working demo of this at the end of the
``polytypes`` module. But if you're interested in all that you should
just use Prolog!

Anyhow, type *checking* is a few easy steps away.

.. code:: ipython2

    def _ge(self, other):
        return (issubclass(other.__class__, self.__class__)
                or hasattr(self, 'accept')
                and isinstance(other, self.accept))
    
    AnyJoyType.__ge__ = _ge
    AnyJoyType.accept = tuple, int, float, long, str, unicode, bool, Symbol
    StackJoyType.accept = tuple
