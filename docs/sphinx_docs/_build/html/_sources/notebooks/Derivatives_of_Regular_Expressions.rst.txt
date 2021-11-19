∂RE
===

Brzozowski’s Derivatives of Regular Expressions
-----------------------------------------------

Legend:

::

   ∧ intersection
   ∨ union
   ∘ concatenation (see below)
   ¬ complement
   ϕ empty set (aka ∅)
   λ singleton set containing just the empty string
   I set of all letters in alphabet

Derivative of a set ``R`` of strings and a string ``a``:

::

   ∂a(R)

   ∂a(a) → λ
   ∂a(λ) → ϕ
   ∂a(ϕ) → ϕ
   ∂a(¬a) → ϕ
   ∂a(R*) → ∂a(R)∘R*
   ∂a(¬R) → ¬∂a(R)
   ∂a(R∘S) → ∂a(R)∘S ∨ δ(R)∘∂a(S)
   ∂a(R ∧ S) → ∂a(R) ∧ ∂a(S)
   ∂a(R ∨ S) → ∂a(R) ∨ ∂a(S)

   ∂ab(R) = ∂b(∂a(R))

Auxiliary predicate function ``δ`` (I call it ``nully``) returns either
``λ`` if ``λ ⊆ R`` or ``ϕ`` otherwise:

::

   δ(a) → ϕ
   δ(λ) → λ
   δ(ϕ) → ϕ
   δ(R*) → λ
   δ(¬R) δ(R)≟ϕ → λ
   δ(¬R) δ(R)≟λ → ϕ
   δ(R∘S) → δ(R) ∧ δ(S)
   δ(R ∧ S) → δ(R) ∧ δ(S)
   δ(R ∨ S) → δ(R) ∨ δ(S)

Some rules we will use later for “compaction”:

::

   R ∧ ϕ = ϕ ∧ R = ϕ

   R ∧ I = I ∧ R = R

   R ∨ ϕ = ϕ ∨ R = R

   R ∨ I = I ∨ R = I

   R∘ϕ = ϕ∘R = ϕ

   R∘λ = λ∘R = R

Concatination of sets: for two sets A and B the set A∘B is defined as:

{a∘b for a in A for b in B}

E.g.:

{‘a’, ‘b’}∘{‘c’, ‘d’} → {‘ac’, ‘ad’, ‘bc’, ‘bd’}

Implementation
--------------

.. code:: python

    from functools import partial as curry
    from itertools import product

``ϕ`` and ``λ``
~~~~~~~~~~~~~~~

The empty set and the set of just the empty string.

.. code:: ipython2

    phi = frozenset()   # ϕ
    y = frozenset({''}) # λ

Two-letter Alphabet
~~~~~~~~~~~~~~~~~~~

I’m only going to use two symbols (at first) becaase this is enough to
illustrate the algorithm and because you can represent any other
alphabet with two symbols (if you had to.)

I chose the names ``O`` and ``l`` (uppercase “o” and lowercase “L”) to
look like ``0`` and ``1`` (zero and one) respectively.

.. code:: ipython2

    syms = O, l = frozenset({'0'}), frozenset({'1'})

Representing Regular Expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To represent REs in Python I’m going to use tagged tuples. A *regular
expression* is one of:

::

   O
   l
   (KSTAR, R)
   (NOT, R)
   (AND, R, S)
   (CONS, R, S)
   (OR, R, S)

Where ``R`` and ``S`` stand for *regular expressions*.

.. code:: ipython2

    AND, CONS, KSTAR, NOT, OR = 'and cons * not or'.split()  # Tags are just strings.

Because they are formed of ``frozenset``, ``tuple`` and ``str`` objects
only, these datastructures are immutable.

String Representation of RE Datastructures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython2

    def stringy(re):
        '''
        Return a nice string repr for a regular expression datastructure.
        '''
        if re == I: return '.'
        if re in syms: return next(iter(re))
        if re == y: return '^'
        if re == phi: return 'X'
    
        assert isinstance(re, tuple), repr(re)
        tag = re[0]
    
        if tag == KSTAR:
            body = stringy(re[1])
            if not body: return body
            if len(body) > 1: return '(' + body + ")*"
            return body + '*'
    
        if tag == NOT:
            body = stringy(re[1])
            if not body: return body
            if len(body) > 1: return '(' + body + ")'"
            return body + "'"
    
        r, s = stringy(re[1]), stringy(re[2])
        if tag == CONS: return r + s
        if tag == OR:   return '%s | %s' % (r, s)
        if tag == AND:  return '(%s) & (%s)' % (r, s)
    
        raise ValueError

``I``
~~~~~

Match anything. Often spelled “.”

::

   I = (0|1)*

.. code:: ipython2

    I = (KSTAR, (OR, O, l))

.. code:: ipython2

    print stringy(I)


.. parsed-literal::

    .


``(.111.) & (.01 + 11*)'``
~~~~~~~~~~~~~~~~~~~~~~~~~~

The example expression from Brzozowski:

::

   (.111.) & (.01 + 11*)'
      a    &  (b  +  c)'

Note that it contains one of everything.

.. code:: ipython2

    a = (CONS, I, (CONS, l, (CONS, l, (CONS, l, I))))
    b = (CONS, I, (CONS, O, l))
    c = (CONS, l, (KSTAR, l))
    it = (AND, a, (NOT, (OR, b, c)))

.. code:: ipython2

    print stringy(it)


.. parsed-literal::

    (.111.) & ((.01 | 11*)')


``nully()``
~~~~~~~~~~~

Let’s get that auxiliary predicate function ``δ`` out of the way.

.. code:: ipython2

    def nully(R):
        '''
        δ - Return λ if λ ⊆ R otherwise ϕ.
        '''
    
        # δ(a) → ϕ
        # δ(ϕ) → ϕ
        if R in syms or R == phi:
            return phi
    
        # δ(λ) → λ
        if R == y:
            return y
    
        tag = R[0]
    
        # δ(R*) → λ
        if tag == KSTAR:
            return y
    
        # δ(¬R) δ(R)≟ϕ → λ
        # δ(¬R) δ(R)≟λ → ϕ
        if tag == NOT:
            return phi if nully(R[1]) else y
    
        # δ(R∘S) → δ(R) ∧ δ(S)
        # δ(R ∧ S) → δ(R) ∧ δ(S)
        # δ(R ∨ S) → δ(R) ∨ δ(S)
        r, s = nully(R[1]), nully(R[2])
        return r & s if tag in {AND, CONS} else r | s

No “Compaction”
~~~~~~~~~~~~~~~

This is the straightforward version with no “compaction”. It works fine,
but does waaaay too much work because the expressions grow each
derivation.

.. code:: ipython2

    def D(symbol):
    
        def derv(R):
    
            # ∂a(a) → λ
            if R == {symbol}:
                return y
    
            # ∂a(λ) → ϕ
            # ∂a(ϕ) → ϕ
            # ∂a(¬a) → ϕ
            if R == y or R == phi or R in syms:
                return phi
            
            tag = R[0]
    
            # ∂a(R*) → ∂a(R)∘R*
            if tag == KSTAR:
                return (CONS, derv(R[1]), R)
    
            # ∂a(¬R) → ¬∂a(R)
            if tag == NOT:
                return (NOT, derv(R[1]))
    
            r, s = R[1:]
    
            # ∂a(R∘S) → ∂a(R)∘S ∨ δ(R)∘∂a(S)
            if tag == CONS:
                A = (CONS, derv(r), s)  # A = ∂a(R)∘S
                # A ∨ δ(R) ∘ ∂a(S)
                # A ∨  λ   ∘ ∂a(S) → A ∨ ∂a(S)
                # A ∨  ϕ   ∘ ∂a(S) → A ∨ ϕ → A
                return (OR, A, derv(s)) if nully(r) else A
    
            # ∂a(R ∧ S) → ∂a(R) ∧ ∂a(S)
            # ∂a(R ∨ S) → ∂a(R) ∨ ∂a(S)
            return (tag, derv(r), derv(s))
    
        return derv

Compaction Rules
~~~~~~~~~~~~~~~~

.. code:: ipython2

    def _compaction_rule(relation, one, zero, a, b):
        return (
            b if a == one else  # R*1 = 1*R = R
            a if b == one else
            zero if a == zero or b == zero else  # R*0 = 0*R = 0
            (relation, a, b)
            )

An elegant symmetry.

.. code:: ipython2

    # R ∧ I = I ∧ R = R
    # R ∧ ϕ = ϕ ∧ R = ϕ
    _and = curry(_compaction_rule, AND, I, phi)
    
    # R ∨ ϕ = ϕ ∨ R = R
    # R ∨ I = I ∨ R = I
    _or = curry(_compaction_rule, OR, phi, I)
    
    # R∘λ = λ∘R = R
    # R∘ϕ = ϕ∘R = ϕ
    _cons = curry(_compaction_rule, CONS, y, phi)

Memoizing
~~~~~~~~~

We can save re-processing by remembering results we have already
computed. RE datastructures are immutable and the ``derv()`` functions
are *pure* so this is fine.

.. code:: ipython2

    class Memo(object):
    
        def __init__(self, f):
            self.f = f
            self.calls = self.hits = 0
            self.mem = {}
    
        def __call__(self, key):
            self.calls += 1
            try:
                result = self.mem[key]
                self.hits += 1
            except KeyError:
                result = self.mem[key] = self.f(key)
            return result

With “Compaction”
~~~~~~~~~~~~~~~~~

This version uses the rules above to perform compaction. It keeps the
expressions from growing too large.

.. code:: ipython2

    def D_compaction(symbol):
    
        @Memo
        def derv(R):
    
            # ∂a(a) → λ
            if R == {symbol}:
                return y
    
            # ∂a(λ) → ϕ
            # ∂a(ϕ) → ϕ
            # ∂a(¬a) → ϕ
            if R == y or R == phi or R in syms:
                return phi
    
            tag = R[0]
    
            # ∂a(R*) → ∂a(R)∘R*
            if tag == KSTAR:
                return _cons(derv(R[1]), R)
    
            # ∂a(¬R) → ¬∂a(R)
            if tag == NOT:
                return (NOT, derv(R[1]))
    
            r, s = R[1:]
    
            # ∂a(R∘S) → ∂a(R)∘S ∨ δ(R)∘∂a(S)
            if tag == CONS:
                A = _cons(derv(r), s)  # A = ∂a(r)∘s
                # A ∨ δ(R) ∘ ∂a(S)
                # A ∨  λ   ∘ ∂a(S) → A ∨ ∂a(S)
                # A ∨  ϕ   ∘ ∂a(S) → A ∨ ϕ → A
                return _or(A, derv(s)) if nully(r) else A
    
            # ∂a(R ∧ S) → ∂a(R) ∧ ∂a(S)
            # ∂a(R ∨ S) → ∂a(R) ∨ ∂a(S)
            dr, ds = derv(r), derv(s)
            return _and(dr, ds) if tag == AND else _or(dr, ds)
    
        return derv

Let’s try it out…
-----------------

(FIXME: redo.)

.. code:: ipython2

    o, z = D_compaction('0'), D_compaction('1')
    REs = set()
    N = 5
    names = list(product(*(N * [(0, 1)])))
    dervs = list(product(*(N * [(o, z)])))
    for name, ds in zip(names, dervs):
        R = it
        ds = list(ds)
        while ds:
            R = ds.pop()(R)
            if R == phi or R == I:
                break
            REs.add(R)
    
    print stringy(it) ; print
    print o.hits, '/', o.calls
    print z.hits, '/', z.calls
    print
    for s in sorted(map(stringy, REs), key=lambda n: (len(n), n)):
        print s


.. parsed-literal::

    (.111.) & ((.01 | 11*)')
    
    92 / 122
    92 / 122
    
    (.01)'
    (.01 | 1)'
    (.01 | ^)'
    (.01 | 1*)'
    (.111.) & ((.01 | 1)')
    (.111. | 11.) & ((.01 | ^)')
    (.111. | 11. | 1.) & ((.01)')
    (.111. | 11.) & ((.01 | 1*)')
    (.111. | 11. | 1.) & ((.01 | 1*)')


Should match:

::

   (.111.) & ((.01 | 11*)')

   92 / 122
   92 / 122

   (.01     )'
   (.01 | 1 )'
   (.01 | ^ )'
   (.01 | 1*)'
   (.111.)            & ((.01 | 1 )')
   (.111. | 11.)      & ((.01 | ^ )')
   (.111. | 11.)      & ((.01 | 1*)')
   (.111. | 11. | 1.) & ((.01     )')
   (.111. | 11. | 1.) & ((.01 | 1*)')

Larger Alphabets
----------------

We could parse larger alphabets by defining patterns for e.g. each byte
of the ASCII code. Or we can generalize this code. If you study the code
above you’ll see that we never use the “set-ness” of the symbols ``O``
and ``l``. The only time Python set operators (``&`` and ``|``) appear
is in the ``nully()`` function, and there they operate on (recursively
computed) outputs of that function, never ``O`` and ``l``.

What if we try:

::

   (OR, O, l)

   ∂1((OR, O, l))
                               ∂a(R ∨ S) → ∂a(R) ∨ ∂a(S)
   ∂1(O) ∨ ∂1(l)
                               ∂a(¬a) → ϕ
   ϕ ∨ ∂1(l)
                               ∂a(a) → λ
   ϕ ∨ λ
                               ϕ ∨ R = R
   λ

And compare it to:

::

   {'0', '1')

   ∂1({'0', '1'))
                               ∂a(R ∨ S) → ∂a(R) ∨ ∂a(S)
   ∂1({'0')) ∨ ∂1({'1'))
                               ∂a(¬a) → ϕ
   ϕ ∨ ∂1({'1'))
                               ∂a(a) → λ
   ϕ ∨ λ
                               ϕ ∨ R = R
   λ

This suggests that we should be able to alter the functions above to
detect sets and deal with them appropriately. Exercise for the Reader
for now.

State Machine
-------------

We can drive the regular expressions to flesh out the underlying state
machine transition table.

::

   .111. & (.01 + 11*)'

Says, “Three or more 1’s and not ending in 01 nor composed of all 1’s.”

.. figure:: omg.svg
   :alt: State Machine Graph

   State Machine Graph

Start at ``a`` and follow the transition arrows according to their
labels. Accepting states have a double outline. (Graphic generated with
`Dot from Graphviz <http://www.graphviz.org/>`__.) You’ll see that only
paths that lead to one of the accepting states will match the regular
expression. All other paths will terminate at one of the non-accepting
states.

There’s a happy path to ``g`` along 111:

::

   a→c→e→g

After you reach ``g`` you’re stuck there eating 1’s until you see a 0,
which takes you to the ``i→j→i|i→j→h→i`` “trap”. You can’t reach any
other states from those two loops.

If you see a 0 before you see 111 you will reach ``b``, which forms
another “trap” with ``d`` and ``f``. The only way out is another happy
path along 111 to ``h``:

::

   b→d→f→h

Once you have reached ``h`` you can see as many 1’s or as many 0’ in a
row and still be either still at ``h`` (for 1’s) or move to ``i`` (for
0’s). If you find yourself at ``i`` you can see as many 0’s, or
repetitions of 10, as there are, but if you see just a 1 you move to
``j``.

RE to FSM
~~~~~~~~~

So how do we get the state machine from the regular expression?

It turns out that each RE is effectively a state, and each arrow points
to the derivative RE in respect to the arrow’s symbol.

If we label the initial RE ``a``, we can say:

::

   a --0--> ∂0(a)
   a --1--> ∂1(a)

And so on, each new unique RE is a new state in the FSM table.

Here are the derived REs at each state:

::

   a = (.111.) & ((.01 | 11*)')
   b = (.111.) & ((.01 | 1)')
   c = (.111. | 11.) & ((.01 | 1*)')
   d = (.111. | 11.) & ((.01 | ^)')
   e = (.111. | 11. | 1.) & ((.01 | 1*)')
   f = (.111. | 11. | 1.) & ((.01)')
   g = (.01 | 1*)'
   h = (.01)'
   i = (.01 | 1)'
   j = (.01 | ^)'

You can see the one-way nature of the ``g`` state and the ``hij`` “trap”
in the way that the ``.111.`` on the left-hand side of the ``&``
disappears once it has been matched.

.. code:: ipython2

    from collections import defaultdict
    from pprint import pprint
    from string import ascii_lowercase

.. code:: ipython2

    d0, d1 = D_compaction('0'), D_compaction('1')

``explore()``
~~~~~~~~~~~~~

.. code:: ipython2

    def explore(re):
    
        # Don't have more than 26 states...
        names = defaultdict(iter(ascii_lowercase).next)
    
        table, accepting = dict(), set()
    
        to_check = {re}
        while to_check:
    
            re = to_check.pop()
            state_name = names[re]
    
            if (state_name, 0) in table:
                continue
    
            if nully(re):
                accepting.add(state_name)
    
            o, i = d0(re), d1(re)
            table[state_name, 0] = names[o] ; to_check.add(o)
            table[state_name, 1] = names[i] ; to_check.add(i)
    
        return table, accepting

.. code:: ipython2

    table, accepting = explore(it)
    table




.. parsed-literal::

    {('a', 0): 'b',
     ('a', 1): 'c',
     ('b', 0): 'b',
     ('b', 1): 'd',
     ('c', 0): 'b',
     ('c', 1): 'e',
     ('d', 0): 'b',
     ('d', 1): 'f',
     ('e', 0): 'b',
     ('e', 1): 'g',
     ('f', 0): 'b',
     ('f', 1): 'h',
     ('g', 0): 'i',
     ('g', 1): 'g',
     ('h', 0): 'i',
     ('h', 1): 'h',
     ('i', 0): 'i',
     ('i', 1): 'j',
     ('j', 0): 'i',
     ('j', 1): 'h'}



.. code:: ipython2

    accepting




.. parsed-literal::

    {'h', 'i'}



Generate Diagram
~~~~~~~~~~~~~~~~

Once we have the FSM table and the set of accepting states we can
generate the diagram above.

.. code:: ipython2

    _template = '''\
    digraph finite_state_machine {
      rankdir=LR;
      size="8,5"
      node [shape = doublecircle]; %s;
      node [shape = circle];
    %s
    }
    '''
    
    def link(fr, nm, label):
        return '  %s -> %s [ label = "%s" ];' % (fr, nm, label)
    
    
    def make_graph(table, accepting):
        return _template % (
            ' '.join(accepting),
            '\n'.join(
              link(from_, to, char)
              for (from_, char), (to) in sorted(table.iteritems())
              )
            )

.. code:: ipython2

    print make_graph(table, accepting)


.. parsed-literal::

    digraph finite_state_machine {
      rankdir=LR;
      size="8,5"
      node [shape = doublecircle]; i h;
      node [shape = circle];
      a -> b [ label = "0" ];
      a -> c [ label = "1" ];
      b -> b [ label = "0" ];
      b -> d [ label = "1" ];
      c -> b [ label = "0" ];
      c -> e [ label = "1" ];
      d -> b [ label = "0" ];
      d -> f [ label = "1" ];
      e -> b [ label = "0" ];
      e -> g [ label = "1" ];
      f -> b [ label = "0" ];
      f -> h [ label = "1" ];
      g -> i [ label = "0" ];
      g -> g [ label = "1" ];
      h -> i [ label = "0" ];
      h -> h [ label = "1" ];
      i -> i [ label = "0" ];
      i -> j [ label = "1" ];
      j -> i [ label = "0" ];
      j -> h [ label = "1" ];
    }
    


Drive a FSM
~~~~~~~~~~~

There are *lots* of FSM libraries already. Once you have the state
transition table they should all be straightforward to use. State
Machine code is very simple. Just for fun, here is an implementation in
Python that imitates what “compiled” FSM code might look like in an
“unrolled” form. Most FSM code uses a little driver loop and a table
datastructure, the code below instead acts like JMP instructions
(“jump”, or GOTO in higher-level-but-still-low-level languages) to
hard-code the information in the table into a little patch of branches.

Trampoline Function
^^^^^^^^^^^^^^^^^^^

Python has no GOTO statement but we can fake it with a “trampoline”
function.

.. code:: ipython2

    def trampoline(input_, jump_from, accepting):
        I = iter(input_)
        while True:
            try:
                bounce_to = jump_from(I)
            except StopIteration:
                return jump_from in accepting
            jump_from = bounce_to

Stream Functions
^^^^^^^^^^^^^^^^

Little helpers to process the iterator of our data (a “stream” of “1”
and “0” characters, not bits.)

.. code:: ipython2

    getch = lambda I: int(next(I))
    
    
    def _1(I):
        '''Loop on ones.'''
        while getch(I): pass
    
    
    def _0(I):
        '''Loop on zeros.'''
        while not getch(I): pass

A Finite State Machine
^^^^^^^^^^^^^^^^^^^^^^

With those preliminaries out of the way, from the state table of
``.111. & (.01 + 11*)'`` we can immediately write down state machine
code. (You have to imagine that these are GOTO statements in C or
branches in assembly and that the state names are branch destination
labels.)

.. code:: ipython2

    a = lambda I: c if getch(I) else b
    b = lambda I: _0(I) or d
    c = lambda I: e if getch(I) else b
    d = lambda I: f if getch(I) else b
    e = lambda I: g if getch(I) else b
    f = lambda I: h if getch(I) else b
    g = lambda I: _1(I) or i
    h = lambda I: _1(I) or i
    i = lambda I: _0(I) or j
    j = lambda I: h if getch(I) else i

Note that the implementations of ``h`` and ``g`` are identical ergo
``h = g`` and we could eliminate one in the code but ``h`` is an
accepting state and ``g`` isn’t.

.. code:: ipython2

    def acceptable(input_):
        return trampoline(input_, a, {h, i})

.. code:: ipython2

    for n in range(2**5):
        s = bin(n)[2:]
        print '%05s' % s, acceptable(s)


.. parsed-literal::

        0 False
        1 False
       10 False
       11 False
      100 False
      101 False
      110 False
      111 False
     1000 False
     1001 False
     1010 False
     1011 False
     1100 False
     1101 False
     1110 True
     1111 False
    10000 False
    10001 False
    10010 False
    10011 False
    10100 False
    10101 False
    10110 False
    10111 True
    11000 False
    11001 False
    11010 False
    11011 False
    11100 True
    11101 False
    11110 True
    11111 False


Reversing the Derivatives to Generate Matching Strings
------------------------------------------------------

(UNFINISHED) Brzozowski also shewed how to go from the state machine to
strings and expressions…

Each of these states is just a name for a Brzozowskian RE, and so, other
than the initial state ``a``, they can can be described in terms of the
derivative-with-respect-to-N of some other state/RE:

::

   c = d1(a)
   b = d0(a)
   b = d0(c)
   ...
   i = d0(j)
   j = d1(i)

Consider:

::

   c = d1(a)
   b = d0(c)

Substituting:

::

   b = d0(d1(a))

Unwrapping:

::

   b = d10(a)

’’’

::

   j = d1(d0(j))

Unwrapping:

::

   j = d1(d0(j)) = d01(j)

We have a loop or “fixed point”.

::

   j = d01(j) = d0101(j) = d010101(j) = ...

hmm…

::

   j = (01)*


