[STRIKEOUT:Cerrect]
===================

[STRIKEOUT:Corroct]
===================

Correct Programming
===================

Symbolic Logic in the Laws of Form ○ Python Expressions to Represent
Forms ○ Reify Forms in an Environment ○ Building Circuits ○ Simplifying
Expressions ○ SAT Solver ○ A Model of Computation

Introduction
============

In 1969 George Spencer-Brown (GSB) published `"Laws of
Form" <https://en.wikipedia.org/wiki/Laws_of_Form>`__ which presented a
logical system based on a single action, a distinction, that is both an
operation and a value. This notebook describes a Python implementation
that mimics the Laws of Form notation and uses it to develop a model of
computer circuits.

The Laws of Form
----------------

See `The Markable Mark <http://www.markability.net/>`__.

Arithmetic
^^^^^^^^^^

::

    (()) =
    ()() = ()

Calculus
^^^^^^^^

::

    A((B)) = AB
    A() = ()
    A(AB) = A(B)

I call these three laws the **Bricken Basis** after `William
Bricken <http://wbricken.com/>`__ who figured out that the third law is
complete with the other two. GSB had the first two laws and "Each Way"
as the basis. (TODO: Find and include the references for all this.)

(If anything here is unclear read `The Markable
Mark <http://www.markability.net/>`__. George Burnett-Stuart has done a
fantastic job there explaining the *Laws of Form*.)

Python Sets and Strings as Laws of Form Calculus Expressions
------------------------------------------------------------

We can use data structures made solely out of Python ``frozenset`` and
string objects to represent the forms of the Laws of Form notation. I'm
going to use the terms "expression" and "form" interchangably in this
document.

.. code:: ipython2

    class Form(frozenset):
    
        def __str__(self):
            # Because frozenset is immutable, and the contents are all string or frozenset,
            # we can cache the string repr of a form.
            try:
                return self._str
            except AttributeError:
                self._str = '(%s)' % ' '.join(sorted(map(str, self)))
            return self._str
    
        __repr__ = __str__
        
    
    def F(*terms):
        '''Create a Form from terms.'''
        return Form([
            term if isinstance(term, (basestring, Form)) else F(*term)
            for term in terms
        ])

Define a few variable names.

.. code:: ipython2

    a, b, c = 'abc'

Some examples of forms.

.. code:: ipython2

    A = F(a, b, c)
    A




.. parsed-literal::

    (a b c)



.. code:: ipython2

    B = F(a, (b, (c,)))
    B




.. parsed-literal::

    (((c) b) a)



Forms like ``a b c`` must be enclosed in a pair of nested containers
like so ``(( a b c ))``, this lets us treat them as a single (Python)
object without inverting the logical value of the form.

.. code:: ipython2

    C = F((a, b, c))
    C




.. parsed-literal::

    ((a b c))



Duplicate terms in a form are automatically removed by ``frozenset``.

.. code:: ipython2

    F(a, (b,), a, (b,))




.. parsed-literal::

    ((b) a)



Order is irrelevant, again due to ``frozenset``.

.. code:: ipython2

    F(b, a, c) == F(a, b, c)




.. parsed-literal::

    True



It's prefectly okay to create forms out of other forms (not just
strings.)

.. code:: ipython2

    F(A, (B, (C,)), a)




.. parsed-literal::

    (((((a b c))) (((c) b) a)) (a b c) a)



Mark and Void.
--------------

.. code:: ipython2

    Mark = F()
    Mark




.. parsed-literal::

    ()



There is no way to represent Void directly in a programming language so
we have to use the simplest Void-valued form instead.

.. code:: ipython2

    Void = F(Mark)
    Void




.. parsed-literal::

    (())



Environments
------------

We can use a Python ``dict`` as a context or environment that supplies
values (Mark or Void) for the names in a form.

.. code:: ipython2

    env = dict(a=Mark, b=Mark, c=Mark)

The ``reify(form, environment)`` Function
-----------------------------------------

Given forms with string variable names in them we want to be able to
substitute values from an environment. If these values are Mark or Void
the result will be a pure arithmentic form.

.. code:: ipython2

    def reify(form, environment):
        if isinstance(form, basestring):
            return environment.get(form, form)
        return Form(reify(inner, environment) for inner in form)

.. code:: ipython2

    for form in (A, B, C):
        print form, u'⟶', reify(form, env)


.. parsed-literal::

    (a b c) ⟶ (())
    (((c) b) a) ⟶ (((()) ()) ())
    ((a b c)) ⟶ ((()))


The ``void(form)`` Function
---------------------------

Once the forms have been rendered to pure arithmetic we can use the
``void()`` function to find the value of each expression.

.. code:: ipython2

    def void(form):
        return any(not void(i) for i in form)

The ``void()`` function returns a Boolean value (Python ``True`` or
``False``), for convenience let's write a function that returns the Mark
or Void value of a form.

.. code:: ipython2

    def value_of(form, m=Mark, v=Void):
        return (m, v)[void(form)]

Now we can use the ``void()`` function (by way of ``value_of()``) to
calculate the base value of each expression structure.

.. code:: ipython2

    for form in (A, B, C):
        arith = reify(form, env)
        print form, u'⟶', arith, u'⟶', value_of(arith)


.. parsed-literal::

    (a b c) ⟶ (()) ⟶ (())
    (((c) b) a) ⟶ (((()) ()) ()) ⟶ (())
    ((a b c)) ⟶ ((())) ⟶ ()


All Possible Environments
-------------------------

For :math:`n` variables there are :math:`2^n` possible assignments of
the two values of Mark and Void. If we generate environments that each
contain one of the possible assignments of names to the base value we
can evaluate an expression containing those names and compute its value.

.. code:: ipython2

    from itertools import product, izip
    
    
    BASE = Void, Mark
    
    
    def environments_of_variables(*variables):
        universe = [BASE] * len(variables)
        for values in product(*universe):
            yield dict(izip(variables, values))
    
    
    envs = list(environments_of_variables(*'abc'))
    
    
    envs




.. parsed-literal::

    [{'a': (()), 'b': (()), 'c': (())},
     {'a': (()), 'b': (()), 'c': ()},
     {'a': (()), 'b': (), 'c': (())},
     {'a': (()), 'b': (), 'c': ()},
     {'a': (), 'b': (()), 'c': (())},
     {'a': (), 'b': (()), 'c': ()},
     {'a': (), 'b': (), 'c': (())},
     {'a': (), 'b': (), 'c': ()}]



This is a bit hard to read, so let's define a helper function to convert
an environment to a string format.

.. code:: ipython2

    def format_env(env, m='()', v='  '):
        return ' '.join((v, m)[not env[k]] for k in sorted(env))
    
    # Note that Mark is an empty frozenset so in a Boolean context in Python it is False,
    # likewise Void is a set with one member, so Python considers it True in a Boolean context.
    # The `not` in the expression is just to force such a Boolean context, and we compensate
    # by putting `v` in the zero-is-False position in the indexed tuple.

Now we can print out the environments in a table. Notice that it looks
just like a list of the eight three-bit binary numbers.

.. code:: ipython2

    print 'i  a  b  c  i in Binary'
    for i, env in enumerate(envs):
        print i, format_env(env, v='--'), '%3s' % (bin(i)[2:],)


.. parsed-literal::

    i  a  b  c  i in Binary
    0 -- -- --   0
    1 -- -- ()   1
    2 -- () --  10
    3 -- () ()  11
    4 () -- -- 100
    5 () -- () 101
    6 () () -- 110
    7 () () () 111


Reify the Forms with Each Meaning
---------------------------------

Let's pick one of the expressions and iterate through the environments
showing the result of reifying that expression in that environment.

.. code:: ipython2

    print B
    print '-----------'
    for i, env in enumerate(envs):
        e = reify(B, env)
        print i, format_env(env, v='--'), u'⟶', e, u'⟶', value_of(e, m='()', v='')


.. parsed-literal::

    (((c) b) a)
    -----------
    0 -- -- -- ⟶ ((((())) (())) (())) ⟶ ()
    1 -- -- () ⟶ (((())) (())) ⟶ 
    2 -- () -- ⟶ ((((())) ()) (())) ⟶ ()
    3 -- () () ⟶ (((()) ()) (())) ⟶ ()
    4 () -- -- ⟶ ((((())) (())) ()) ⟶ 
    5 () -- () ⟶ (((())) ()) ⟶ 
    6 () () -- ⟶ ((((())) ()) ()) ⟶ 
    7 () () () ⟶ (((()) ()) ()) ⟶ 


Truth Table
-----------

Let's render the above as a `Truth
Table <https://en.wikipedia.org/wiki/Truth_table>`__.

.. code:: ipython2

    def truth_table_3(expression):
        print expression
        print ' a  b  c | Value'
        print '---------+------'
        for E in envs:
            e = reify(expression, E)
            print format_env(E), '|', value_of(e, m='()', v='')

.. code:: ipython2

    truth_table_3(B)


.. parsed-literal::

    (((c) b) a)
     a  b  c | Value
    ---------+------
             | ()
          () | 
       ()    | ()
       () () | ()
    ()       | 
    ()    () | 
    () ()    | 
    () () () | 


This makes it clear that *each expression in Laws of Form calculus is
describing a digital Boolean circuit*. The names are its inputs and its
Void/Mark value is its output. Each boundary is a `multi-input **NOR**
gate <https://en.wikipedia.org/wiki/Logical_NOR>`__, known as the Peirce
arrow or Quine dagger (See `Sheffer
stroke <https://en.wikipedia.org/wiki/Sheffer_stroke>`__ and `NOR
gate <https://en.wikipedia.org/wiki/NOR_gate>`__.) Instead of two
Boolean values there is only one value and non-existance.

Let's build Circuits
====================

In order to work with expressions as digital circuits, let's define some
helper functions that will create logic circuits out of simpler forms.
The names of the functions below reflect the choice of Mark as Boolean
``True`` but this is `just a convention <#Appendix:-Duals>`__.

.. code:: ipython2

    nor = lambda *bits: F(*bits)
    or_ = lambda *bits: F(bits)
    and_ = lambda *bits: Form(F(bit) for bit in bits)
    nand = lambda *bits: nor(and_(*bits))
    nxor = eqiv = lambda a, b: F((a, (b,)), ((a,), b))
    xor = lambda a, b: F(nxor(a, b))
    
    # To build logical expressions with Void as Boolean True use these functions.
    anti_nor = nand
    anti_or = and_
    anti_and = or_
    anti_nand = nor
    anti_eqiv = xor
    anti_xor = eqiv

Some examples:

.. code:: ipython2

    a, b, c = 'abc'
    
    
    some_expressions = (
        nor(a, b, c),
        or_(a, b, c),
        and_(a, b, c),
        nand(a, b, c),
        xor(a, b),
        eqiv(a, b),
        xor(a, xor(b, c)),
    )
    
    
    for expression in some_expressions:
        print expression


.. parsed-literal::

    (a b c)
    ((a b c))
    ((a) (b) (c))
    (((a) (b) (c)))
    ((((a) b) ((b) a)))
    (((a) b) ((b) a))
    ((((((((b) c) ((c) b)))) a) (((((b) c) ((c) b))) (a))))


And let's rewrite the ``truth_table_3()`` function to make it work for
any number of variables.

.. code:: ipython2

    def yield_variables_of(expression):
        '''Yield all string members of an expression.'''
        if isinstance(expression, basestring):
            yield expression
        else:
            for inner in expression:
                for leaf in yield_variables_of(inner):
                    yield leaf
    
    
    def collect_names(expression):
        '''Return a set of the variables mentioned in an expression.'''
        return set(yield_variables_of(expression))
    
    
    def truth_table(expression):
        '''Print a truth table for an expression.'''
        names = sorted(collect_names(expression))
        header = ' ' + '  '.join(names)
        n = 1 + len(header)
        header += ' | Value'
        print expression
        print header
        print '-' * n + '+------'
        for env in environments_of_variables(*names):
            e = reify(expression, env)
            print format_env(env), '|', ['()', ''][void(e)]

We can use this ``truth_table()`` function to examine the expressions we
created above.

.. code:: ipython2

    truth_table(nor(a, b, c))


.. parsed-literal::

    (a b c)
     a  b  c | Value
    ---------+------
             | ()
          () | 
       ()    | 
       () () | 
    ()       | 
    ()    () | 
    () ()    | 
    () () () | 


.. code:: ipython2

    truth_table(or_(a, b, c))


.. parsed-literal::

    ((a b c))
     a  b  c | Value
    ---------+------
             | 
          () | ()
       ()    | ()
       () () | ()
    ()       | ()
    ()    () | ()
    () ()    | ()
    () () () | ()


.. code:: ipython2

    truth_table(and_(a, b, c))


.. parsed-literal::

    ((a) (b) (c))
     a  b  c | Value
    ---------+------
             | 
          () | 
       ()    | 
       () () | 
    ()       | 
    ()    () | 
    () ()    | 
    () () () | ()


.. code:: ipython2

    truth_table(xor(a, b))


.. parsed-literal::

    ((((a) b) ((b) a)))
     a  b | Value
    ------+------
          | 
       () | ()
    ()    | ()
    () () | 


.. code:: ipython2

    truth_table(eqiv(a, b))


.. parsed-literal::

    (((a) b) ((b) a))
     a  b | Value
    ------+------
          | ()
       () | 
    ()    | 
    () () | ()


.. code:: ipython2

    truth_table(xor(a, xor(b, c)))


.. parsed-literal::

    ((((((((b) c) ((c) b)))) a) (((((b) c) ((c) b))) (a))))
     a  b  c | Value
    ---------+------
             | 
          () | ()
       ()    | ()
       () () | 
    ()       | ()
    ()    () | 
    () ()    | 
    () () () | ()


.. code:: ipython2

    E1 = and_(
        or_(and_(a, b), and_(b, c), and_(c, a)),  # Any two variables...
        nand(a, b, c)  # ...but not all three.
    )
    truth_table(E1)


.. parsed-literal::

    ((((((a) (b)) ((a) (c)) ((b) (c))))) ((((a) (b) (c)))))
     a  b  c | Value
    ---------+------
             | 
          () | 
       ()    | 
       () () | ()
    ()       | 
    ()    () | ()
    () ()    | ()
    () () () | 


This is a
`brute-force <https://en.wikipedia.org/wiki/Brute-force_search>`__
`SAT <https://en.wikipedia.org/wiki/Boolean_satisfiability_problem>`__
`solver <https://en.wikipedia.org/wiki/Boolean_satisfiability_problem#Algorithms_for_solving_SAT>`__
that doesn't even bother to stop once it's found a solution.

Expressions from Truth Tables
-----------------------------

Sometimes we will have a function for which we know the behavior (truth
table) but not an expression and we want the expression. For example,
imagine that we didn't just create the expression for this table:

::

     a  b  c | Value
    ---------+------
             | 
          () | 
       ()    | 
       () () | ()
    ()       | 
    ()    () | ()
    () ()    | ()
    () () () | 

Each Row can be Represented as an Expression
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To write an expression for this table, first we should understand that
each row can be represented as an expression.

::

             ⟶ ( a   b   c )
          () ⟶ ( a   b  (c))
       ()    ⟶ ( a  (b)  c )
       () () ⟶ ( a  (b) (c))
    ()       ⟶ ((a)  b   c )
    ()    () ⟶ ((a)  b  (c))
    () ()    ⟶ ((a) (b)  c )
    () () () ⟶ ((a) (b) (c))

Each of the above expressions will be true (Mark-valued) for only one
possible combination of the three input variables. For example, let's
look at the sixth expression above:

.. code:: ipython2

    e6 = F((a,), b, (c,))
    truth_table(e6)


.. parsed-literal::

    ((a) (c) b)
     a  b  c | Value
    ---------+------
             | 
          () | 
       ()    | 
       () () | 
    ()       | 
    ()    () | ()
    () ()    | 
    () () () | 


To make an expression that is Mark-valued for just certain rows of the
table, pick those rows' expressions,

::

       () () | ( a  (b) (c))
    ()    () | ((a)  b  (c))
    () ()    | ((a) (b)  c )

And write them down as terms in an **OR** expression:

::

    E = (a(b)(c)) ((a)b(c)) ((a)(b)c)

In conventional notation this is called `Disjunctive normal
form <https://en.wikipedia.org/wiki/Disjunctive_normal_form>`__:

::

    E = (¬a ∧ b ∧ c) ∨ (a ∧ ¬b ∧ c) ∨ (a ∧ b ∧ ¬c)

Here it is in action:

.. code:: ipython2

    e4 = ( a,   (b,),  (c,))
    e6 = ((a,),  b,    (c,))
    e7 = ((a,), (b,),   c  )
    
    E2 = or_(e4, e6, e7)
    
    truth_table(E2)


.. parsed-literal::

    ((((a) (b) c) ((a) (c) b) ((b) (c) a)))
     a  b  c | Value
    ---------+------
             | 
          () | 
       ()    | 
       () () | ()
    ()       | 
    ()    () | ()
    () ()    | ()
    () () () | 


Equivalence
~~~~~~~~~~~

Note that the expression E2 above is equivalent to the ealier expression
E1 that has the same truth table, in other words:

::

    ((((((a) (b)) ((b) (c)) ((c) (a))))) ((((a) (b) (c)))))

equals

::

    (((a (b) (c)) ((a) b (c)) ((a) (b) c)))

We can demonstrate this equivalence by evaluating the expression formed
by ``eqiv()`` from these two.

For every environment (from the set of possible values for the
variables) if both expressions have the same value when evaluated then
the ``eqiv()`` of those expressions will be Mark-valued (true in our
chosen context.)

.. code:: ipython2

    truth_table(eqiv(E1, E2))


.. parsed-literal::

    (((((((((a) (b)) ((a) (c)) ((b) (c))))) ((((a) (b) (c)))))) ((((a) (b) c) ((a) (c) b) ((b) (c) a)))) (((((((a) (b)) ((a) (c)) ((b) (c))))) ((((a) (b) (c))))) (((((a) (b) c) ((a) (c) b) ((b) (c) a))))))
     a  b  c | Value
    ---------+------
             | ()
          () | ()
       ()    | ()
       () () | ()
    ()       | ()
    ()    () | ()
    () ()    | ()
    () () () | ()


The truth table above shows that the equivalence expression is true
(Mark-valued by our current convention) for all possible assignments of
Mark/Void to the three variables ``a``, ``b``, and ``c``. This indicates
that the expression is a **tautology**.

`Half-Bit Adder <https://en.wikipedia.org/wiki/Adder_%28electronics%29#Half_adder>`__
-------------------------------------------------------------------------------------

If you have two binary digits ("bits") and you are interested in the
(binary) sum of these digits you will need two circuits, one for the
"ones place" and one for the "twos place" or "carry bit".

Consider:

::

    a b | c s
    ----+----
    0 0 | 0 0
    0 1 | 0 1
    1 0 | 0 1
    1 1 | 1 0

Treating each output column ('c' for carry, 's' for sum) as a single
expression, it's easy to see that the carry bit is just **AND** and the
sum bit is just **XOR** of the two input bits.

.. code:: ipython2

    a, b = 'ab'
    
    
    half_bit_adder = {
        'Sum': xor(a, b),
        'Carry': and_(a, b),
    }
    
    
    for name, expr in half_bit_adder.items():
        print name
        truth_table(expr)
        print


.. parsed-literal::

    Carry
    ((a) (b))
     a  b | Value
    ------+------
          | 
       () | 
    ()    | 
    () () | ()
    
    Sum
    ((((a) b) ((b) a)))
     a  b | Value
    ------+------
          | 
       () | ()
    ()    | ()
    () () | 
    


`Full-bit Adder <https://en.wikipedia.org/wiki/Adder_%28electronics%29#Full_adder>`__
-------------------------------------------------------------------------------------

In order to add two multi-bit binary numbers we need adder circuits that
are designed to work with *three* input bits: the two bits to add
together and a carry bit from the previous addition:

::

     a  b Cin   Sum Cout
     0  0  0  |  0  0
     0  0  1  |  1  0
     0  1  0  |  1  0
     0  1  1  |  0  1
     1  0  0  |  1  0
     1  0  1  |  0  1
     1  1  0  |  0  1
     1  1  1  |  1  1

Looking back at our table of three-variable expressions:

::

             ⟶ ( a   b   c )
          () ⟶ ( a   b  (c))
       ()    ⟶ ( a  (b)  c )
       () () ⟶ ( a  (b) (c))
    ()       ⟶ ((a)  b   c )
    ()    () ⟶ ((a)  b  (c))
    () ()    ⟶ ((a) (b)  c )
    () () () ⟶ ((a) (b) (c))

We can easily determine expressions for sum and carry:

::

    Sum = (a b (c)) (a (b) c) ((a) b c) ((a) (b) (c))

    Cout = (a (b) (c)) ((a) b (c)) ((a) (b) c) ((a) (b) (c))

.. code:: ipython2

    Sum = F(( (a, b, (c,)), (a, (b,), c), ((a,), b, c), ((a,), (b,), (c,)) ),)
    Carry = F(( (a, (b,), (c,)), ((a,), b, (c,)), ((a,), (b,),  c), ((a,), (b,), (c,)) ),)

.. code:: ipython2

    print 'Sum'
    truth_table(Sum)
    print
    print 'Carry'
    truth_table(Carry)


.. parsed-literal::

    Sum
    ((((a) (b) (c)) ((a) b c) ((b) a c) ((c) a b)))
     a  b  c | Value
    ---------+------
             | 
          () | ()
       ()    | ()
       () () | 
    ()       | ()
    ()    () | 
    () ()    | 
    () () () | ()
    
    Carry
    ((((a) (b) (c)) ((a) (b) c) ((a) (c) b) ((b) (c) a)))
     a  b  c | Value
    ---------+------
             | 
          () | 
       ()    | 
       () () | ()
    ()       | 
    ()    () | ()
    () ()    | ()
    () () () | ()


Let's make a ``full_bit_adder()`` function that can define new
expressions in terms of variables (or expressions) passed into it.

.. code:: ipython2

    def full_bit_adder(a, b, c):
        return (
            F(( (a, b, (c,)), (a, (b,), c), ((a,), b, c), ((a,), (b,), (c,)) ),),
            F(( (a, (b,), (c,)), ((a,), b, (c,)), ((a,), (b,),  c), ((a,), (b,), (c,)) ),),
        )

Now we can chain it to make a set of circuits that define together an
eight-bit adder circuit with carry.

.. code:: ipython2

    sum0, cout = full_bit_adder('a0', 'b0', 'Cin')
    sum1, cout = full_bit_adder('a1', 'b1', cout)
    sum2, cout = full_bit_adder('a2', 'b2', cout)
    sum3, cout = full_bit_adder('a3', 'b3', cout)
    sum4, cout = full_bit_adder('a4', 'b4', cout)
    sum5, cout = full_bit_adder('a5', 'b5', cout)
    sum6, cout = full_bit_adder('a6', 'b6', cout)
    sum7, cout = full_bit_adder('a7', 'b7', cout)

Unfortunately, the sizes of the resulting expression explode:

.. code:: ipython2

    map(len, map(str, (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7, cout)))




.. parsed-literal::

    [63, 327, 1383, 5607, 22503, 90087, 360423, 1441767, 1441773]



Using the definitions for Sum and Carry
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could also use the definitions from the `Wikipedia
article <https://en.wikipedia.org/wiki/Adder_%28electronics%29#Full_adder>`__:

::

    S = A ⊕ B ⊕ C
    Cout = (A ⋅ B) + (Cin ⋅ (A ⊕ B))

.. code:: ipython2

    def full_bit_adder(a, b, c):
        return (
            xor(xor(a, b), c),
            or_(and_(a, b), and_(c, xor(a, b))),
        )

.. code:: ipython2

    sum0, cout = full_bit_adder('a0', 'b0', 'Cin')

.. code:: ipython2

    print 'Sum'
    truth_table(sum0)
    print
    print 'Carry'
    truth_table(cout) 


.. parsed-literal::

    Sum
    ((((((((a0) b0) ((b0) a0)))) Cin) (((((a0) b0) ((b0) a0))) (Cin))))
     Cin  a0  b0 | Value
    -------------+------
             | 
          () | ()
       ()    | ()
       () () | 
    ()       | ()
    ()    () | 
    () ()    | 
    () () () | ()
    
    Carry
    ((((((((a0) b0) ((b0) a0)))) (Cin)) ((a0) (b0))))
     Cin  a0  b0 | Value
    -------------+------
             | 
          () | 
       ()    | 
       () () | ()
    ()       | 
    ()    () | ()
    () ()    | ()
    () () () | ()


.. code:: ipython2

    sum1, cout = full_bit_adder('a1', 'b1', cout)
    sum2, cout = full_bit_adder('a2', 'b2', cout)
    sum3, cout = full_bit_adder('a3', 'b3', cout)
    sum4, cout = full_bit_adder('a4', 'b4', cout)
    sum5, cout = full_bit_adder('a5', 'b5', cout)
    sum6, cout = full_bit_adder('a6', 'b6', cout)
    sum7, cout = full_bit_adder('a7', 'b7', cout)

The sizes of these expression are much more tractable:

.. code:: ipython2

    map(len, map(str, (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7, cout)))




.. parsed-literal::

    [67, 159, 251, 343, 435, 527, 619, 711, 371]



Simplifying Expressions
=======================

The ``Form`` Python datastructure is based on ``frozenset`` so duplicate
terms are automatically removed and order of terms is irrelevant just as
we would prefer. But we want to be able to automatically simplify forms
beyond just that. Ideally, we would like a function that applies the
rules of the calculus automatically:

::

    A((B)) = AB
    A() = ()
    A(AB) = A(B)

I'm going to specify the behaviour of the desired function in a
unittest.

.. code:: ipython2

    import unittest

Three Easy Cases
~~~~~~~~~~~~~~~~

Let's deal with three easy cases first: string, the Mark, and the Void.
The ``simplify()`` function should just return them unchanged.

.. code:: ipython2

    class UnwrapTest0(unittest.TestCase):
    
        def testMark(self):
            self.assertEqual(Mark, simplify(Mark))
      
        def testVoid(self):
            self.assertEqual(Void, simplify(Void))
    
        def testLeaf(self):
            self.assertEqual('a', simplify('a'))
    
    
    def simplify(form):
        # Three easy cases, for strings, Mark, or Void, just return it.
        if isinstance(form, basestring) or form in BASE:
            return form
    
    
    if __name__ == '__main__':
        unittest.main(argv=['ignored', 'UnwrapTest0'], exit=False)


.. parsed-literal::

    ...
    ----------------------------------------------------------------------
    Ran 3 tests in 0.004s
    
    OK


``(a)``
~~~~~~~

A single string in a form ``(a)`` should also be returned unchanged:

.. code:: ipython2

    class UnwrapTest1(unittest.TestCase):
    
        def testNegatedLeaf(self):
            a = nor('a')
            self.assertEqual(a, simplify(a))
    
    
    def simplify(form):
    
        # Three easy cases, for strings, Mark, or Void, just return it.
        if isinstance(form, basestring) or form in BASE:
            return form
        
        # We know it's a Form and it's not empty (else it would be the Mark and
        # returned above.)
        
        # Let's just recurse.
        return Form(simplify(inner) for inner in form)
    
    
    if __name__ == '__main__':
        unittest.main(argv=['ignored', 'UnwrapTest1'], exit=False)


.. parsed-literal::

    .
    ----------------------------------------------------------------------
    Ran 1 test in 0.001s
    
    OK


Doubly-Wrapped Forms
~~~~~~~~~~~~~~~~~~~~

So far, so good. But what about ``((a))``? This should be returned as
just ``a``. And ``((a b))`` should remain ``((a b))`` because we can't
represent just ``a b`` as a single Python object, so we have to retain
the outer pair of containers to hold them without inverting the
Mark/Void value (if we just used one container.)

.. code:: ipython2

    class UnwrapTest2(unittest.TestCase):
    
        def testUnwrapLeaf(self):
            '''((a)) = a'''
            a = or_('a')
            self.assertEqual('a', simplify(a))
    
        def testDoNotUnwrapTwoLeaves(self):
            '''((a b)) = ((a b))'''
            a = or_('a', 'b')
            self.assertEqual(a, simplify(a))
    
    
    def simplify(form):
    
        # Three easy cases, for strings, Mark, or Void, just return it.
        if isinstance(form, basestring) or form in BASE:
            return form
        
        # We know it's a Form and it's not empty (else it would be the Mark and
        # returned above.)
        
        # Let's just recurse.
        result = Form(simplify(inner) for inner in form)
        
        # Check for ((a)) and return just a.
        # If there is more than one item in the inner container ((a b..))
        # then we must keep the outer containers.
        if len(result) == 1:
            inner, = result  # inner = (a)
            if isinstance(inner, Form) and len(inner) == 1:
                a, = inner
                return a
    
        return result
    
    
    if __name__ == '__main__':
        unittest.main(argv=['ignored', 'UnwrapTest2'], exit=False)


.. parsed-literal::

    ..
    ----------------------------------------------------------------------
    Ran 2 tests in 0.002s
    
    OK


Does it work for ``(((a))) = (a)`` and ``((((a)))) = a`` and so on?

.. code:: ipython2

    class UnwrapTest3(unittest.TestCase):
    
        def testMultiUnwrapLeaf(self):
            A = 'a'
            B = nor(A)
            a = nor(B)
            self.assertEqual(A, simplify(a))
            a = nor(a)
            self.assertEqual(B, simplify(a))
            a = nor(a)
            self.assertEqual(A, simplify(a))
            a = nor(a)
            self.assertEqual(B, simplify(a))
            a = nor(a)
            self.assertEqual(A, simplify(a))
            a = nor(a)
            self.assertEqual(B, simplify(a))
    
        def testMultiDoNotUnwrapTwoLeaves(self):
            e = F('a', 'b')
            f = a = nor(e)
            self.assertEqual(f, simplify(a))
            a = nor(a)
            self.assertEqual(e, simplify(a))
            a = nor(a)
            self.assertEqual(f, simplify(a))
            a = nor(a)
            self.assertEqual(e, simplify(a))
            a = nor(a)
            self.assertEqual(f, simplify(a))
            a = nor(a)
            self.assertEqual(e, simplify(a))
    
    # Technically, several of the tests above are redundant,
    # I'm not willing to figure out the right point ot stop
    # right now, so I just do extra tests.
    
    if __name__ == '__main__':
        unittest.main(argv=['ignored', 'UnwrapTest3'], exit=False)


.. parsed-literal::

    ..
    ----------------------------------------------------------------------
    Ran 2 tests in 0.003s
    
    OK


Unwrapping Inner Forms
~~~~~~~~~~~~~~~~~~~~~~

But now let's trick our function, it can't handle
``(a ((b c))) = (a b c)`` yet. This is going to require an auxiliary
helper function that is similar to ``simplify()`` but that yields terms
into an outer context.

.. code:: ipython2

    class UnwrapTest4(unittest.TestCase):
    
        def testMultiUnwrapLeaf(self):
            a, b, c = 'abc'
            f = F(a,((b, c),))
            e = F(a, b, c)
            self.assertEqual(e, simplify(f))
    
        def testMulti_blah_Leaf(self):
            a, b, c = 'abc'
            f = F(a,(((b, c),),),)
            e = F(a, (b, c))
            self.assertEqual(e, simplify(f))
    
        def testMulti_blah_blah_Leaf(self):
            a, b, c, d = 'abcd'
            f = F(a,((((b, c),), d),))
            e = F(a, b, c, d)
            self.assertEqual(e, simplify(f))
    
    
    def simplify(form):
    
        # Three easy cases, for strings, Mark, or Void, just return it.
        if isinstance(form, basestring) or form in BASE:
            return form
    
        # We know it's a Form and it's not empty (else it would be the Mark and
        # returned above.)
        
        result = []
        for inner in simplify_gen(form):  # Use the generator instead of recursing into simplify().
            result.append(inner)
        result = Form(result)
       
        # Check for ((a)) and return just a.
        # If there is more than one item in the inner container ((a b..))
        # then we must keep the outer containers.
        if len(result) == 1:
            inner, = result  # inner = (a)
            if isinstance(inner, Form):
                if len(inner) == 1:
                    a, = inner
                    return a
                else:
                    # len(inner) cannot be 0, because that means form is Void
                    # and would already have been returned.
                    assert len(inner) > 1, repr(inner)
                    
                    # What to do here?
                    # We cannot yield the items in inner into the containing context
                    # because we don't have it (or even know if it exists.)
                    # Therefore we need a different simplify() generator function that yields
                    # the simplified contents of a form, and we have to call that instead
                    # of recurring on simplify() above.
                    pass
                    
    
        return result
    
    
    def simplify_gen(form):
        
        for inner in form:
            
            inner = simplify(inner)
            # Now inner is simplified, except for ((a b...)) which simplify() can't handle.
    
            # Three easy cases, strings, Mark, or Void.
            if isinstance(inner, basestring):
                yield inner
                continue
    
            if inner == Mark:
                yield inner
                assert False  # The simplify() function will not keep iterating after this.
                return  # Partial implementation of ()A = ().
            
            if inner == Void:
                continue  # Omit Void.  Implementation of (()) = .
        
            # We know it's a Form and it's not empty (else it would be the Mark and
            # yielded above.)
        
            # Check for ((...)) and return just ... .
            if len(inner) > 1:  # (foo bar)
                yield inner
                continue
    
            assert len(inner) == 1, repr(inner)  # Just in case...
    
            inner_inner, = inner
            if isinstance(inner_inner, Form):  # inner_inner = (...)
                for inner_inner_inner in inner_inner:
                    yield inner_inner_inner
                continue
    
            #else:  # inner_inner = foo ; inner = (foo)
            
            yield inner
    
            
    if __name__ == '__main__':
        unittest.main(argv=['ignored', 'UnwrapTest4'], exit=False)


.. parsed-literal::

    ...
    ----------------------------------------------------------------------
    Ran 3 tests in 0.005s
    
    OK


Marks
~~~~~

If the Mark occurs in a sub-form it should *Occlude* all sibling
sub-forms, rendering its container form Void.

.. code:: ipython2

    class MarkTest0(unittest.TestCase):
    
        def testMarkOccludes0(self):
            a, b, c = 'abc'
            f = F(a, (), b, c)
            self.assertEqual(Void, simplify(f))
    
        def testMarkOccludes1(self):
            a, b, c = 'abc'
            f = F(a, (b, c, ()))
            e = F(a)
            self.assertEqual(e, simplify(f))
    
        def testMarkOccludes2(self):
            a, b, c = 'abc'
            f = F(a, (b, ((), c)))
            e = F(a, (b,))
            self.assertEqual(e, simplify(f))
    
    
    def simplify(form):
    
        # Three easy cases, for strings, Mark, or Void, just return it.
        if isinstance(form, basestring) or form in BASE:
            return form
    
        # We know it's a Form and it's not empty (else it would be the Mark and
        # returned above.)
    
        result = []
        for inner in simplify_gen(form):
            if inner == Mark:
                return Void  # Discard any other inner forms, form is Void.
            result.append(inner)
        result = Form(result)
    
        # Check for ((a)) and return just a.
        # If there is more than one item in the inner container ((a b..))
        # then we must keep the outer containers.
        if len(result) == 1:
            inner, = result  # inner = (a)
            if isinstance(inner, Form):
                if len(inner) == 1:
                    a, = inner
                    return a                
    
        return result
    
    
    if __name__ == '__main__':
        unittest.main(argv=['ignored', 'MarkTest0'], exit=False)


.. parsed-literal::

    ...
    ----------------------------------------------------------------------
    Ran 3 tests in 0.004s
    
    OK


Pervade
~~~~~~~

So we have ``(()) = --`` and ``()A = ()`` what about ``A(AB) = A(B)``?

.. code:: ipython2

    class PervadeTest0(unittest.TestCase):
    
        def testPervade0(self):
            a = 'a'
            f = F(a, (a,))
            self.assertEqual(Void, simplify(f))
    
        def testPervade1(self):
            a = 'a'
            f = F(((a,),), (a,))
            self.assertEqual(Void, simplify(f))
            
        def testPervade2(self):
            a = 'a'
            b = 'b'
            f = F(a, (b, (a,)))
            e = F(a)
            self.assertEqual(e, simplify(f))
    
    
    def simplify(form, exclude=None):
    
        # Three easy cases, for strings, Mark, or Void, just return it.
        if isinstance(form, basestring) or form in BASE:
            return form
    
        # We know it's a Form and it's not empty (else it would be the Mark and
        # returned above.)
    
        if exclude is None:
            exclude = set()
    
        new_stuff = form - exclude 
        exclude = exclude | new_stuff
    
        result = []
        for inner in simplify_gen(new_stuff, exclude):
            if inner == Mark:
                return Void  # Discard any other inner forms, form is Void.
            result.append(inner)
        result = Form(result)
    
        # Check for ((a)) and return just a.
        # If there is more than one item in the inner container ((a b..))
        # then we must keep the outer containers.
        if len(result) == 1:
            inner, = result  # inner = (a)
            if isinstance(inner, Form):
                if len(inner) == 1:
                    a, = inner
                    return a                
    
        return result
    
    
    def simplify_gen(form, exclude):
        
        for inner in form:
            
            inner = simplify(inner, exclude)
            # Now inner is simplified, except for ((a b...)) which simplify() can't handle.
    
            # Three easy cases, strings, Mark, or Void.
            if isinstance(inner, basestring):
                yield inner
                continue
    
            if inner == Mark:
                yield inner
                assert False  # The simplify() function will not keep iterating after this.
                return  # Partial implementation of ()A = ().
            
            if inner == Void:
                continue  # Omit Void.  Implementation of (()) = .
        
            # We know it's a Form and it's not empty (else it would be the Mark and
            # yielded above.)
        
            # Check for ((...)) and return just ... .
            if len(inner) > 1:  # (foo bar)
                yield inner
                continue
    
            assert len(inner) == 1, repr(inner)  # Just in case...
    
            inner_inner, = inner
            if isinstance(inner_inner, Form):  # inner_inner = (...)
                for inner_inner_inner in inner_inner:
                    yield inner_inner_inner
                continue
    
            #else:  # inner_inner = foo ; inner = (foo)
            
            yield inner
    
    
    if __name__ == '__main__':
        unittest.main(argv=['ignored', 'PervadeTest0'], exit=False)


.. parsed-literal::

    ...
    ----------------------------------------------------------------------
    Ran 3 tests in 0.004s
    
    OK


TODO set up `Hypothesis <http://hypothesis.works/>`__ to generate test
cases...

.. code:: ipython2

    # Run ALL the tests!
    
    if __name__ == '__main__':
        unittest.main(argv=['ignored'], exit=False)


.. parsed-literal::

    .................
    ----------------------------------------------------------------------
    Ran 17 tests in 0.022s
    
    OK


`Using "Each-Way" to Simplify Forms <http://www.markability.net/case_analysis.htm>`__
-------------------------------------------------------------------------------------

GSB called this "Each-Way":

::

    a = ((a b) (a (b)))

.. code:: ipython2

    truth_table(F((a, b), (a, (b,))))


.. parsed-literal::

    (((b) a) (a b))
     a  b | Value
    ------+------
          | 
       () | 
    ()    | ()
    () () | ()


The form says, "if b then a else a". I'll come back to the
interpretation of "Each-Way" as an ``if-then-else`` statement later.

The thing to note here is that the value for ``a`` can be a whole
expression which appears twice in the new form: once next to ``b`` and
once next to ``(b)``.

In the first case we can remove any occurances of ``b`` from the ``a``
next to it

::

    b (...(b c (d ...)))
    b (...(  c (d ...)))

and in the second case we can change any occurances of ``b`` to the
Mark.

::

    (b)(...(b     c (d ...)))
    (b)((b)(b     c (d ...)))
    (b)(...(b (b) c (d ...)))
    (b)(...(b ( ) c (d ...)))
    (b)(...(  ( )          ))
    (b)(...                 )

We can send ``(b)`` into the form until it reaches and ``b``, at which
point ``b(b)`` becomes ``()`` and sweeps out any siblings rendering its
containing form Void.

For the first case we can use ``simplify()`` and pass in ``b`` as a
member of the ``exclude`` set.

.. code:: ipython2

    A = F(a, (b, (c,)))
    A




.. parsed-literal::

    (((c) b) a)



.. code:: ipython2

    simplify(A, {b})




.. parsed-literal::

    (a c)



``with_mark``
~~~~~~~~~~~~~

In the second case ``(b)...b... = (b)...()...`` we can modify the
``simplify()`` function to accept a name that it should treat as
Mark-valued.

.. code:: ipython2

    class MarkitTest0(unittest.TestCase):
    
        def testMarkit0(self):
            a, b, c = 'abc'
            f = F(a, (b, (c,)))
            e = F(a)
            self.assertEqual(e, simplify(f, with_mark=b))
    
    
    def simplify(form, exclude=None, with_mark=None):
    
        # Three easy cases, for strings, Mark, or Void, just return it.
        if isinstance(form, basestring) or form in BASE:
            return form
    
        # We know it's a Form and it's not empty (else it would be the Mark and
        # returned above.)
    
        if exclude is None:
            exclude = set()
    
        new_stuff = form - exclude 
        exclude = exclude | new_stuff
    
        result = []
        for inner in simplify_gen(new_stuff, exclude, with_mark):
            if inner == Mark or inner == with_mark:
                return Void  # Discard any other inner forms, form is Void.
            result.append(inner)
        result = Form(result)
    
        # Check for ((a)) and return just a.
        # If there is more than one item in the inner container ((a b..))
        # then we must keep the outer containers.
        if len(result) == 1:
            inner, = result  # inner = (a)
            if isinstance(inner, Form):
                if len(inner) == 1:
                    a, = inner
                    return a                
    
        return result
    
    
    def simplify_gen(form, exclude, with_mark):
        
        for inner in form:
            
            inner = simplify(inner, exclude, with_mark)
            # Now inner is simplified, except for ((a b...)) which simplify() can't handle.
    
            # Three easy cases, strings, Mark, or Void.
            if isinstance(inner, basestring):
                yield inner
                continue
    
            if inner == Mark or inner == with_mark:
                yield Mark
                assert False  # The simplify() function will not keep iterating after this.
                return  # Partial implementation of ()A = ().
            
            if inner == Void:
                continue  # Omit Void.  Implementation of (()) = .
        
            # We know it's a Form and it's not empty (else it would be the Mark and
            # yielded above.)
        
            # Check for ((...)) and return just ... .
            if len(inner) > 1:  # (foo bar)
                yield inner
                continue
    
            assert len(inner) == 1, repr(inner)  # Just in case...
    
            inner_inner, = inner
            if isinstance(inner_inner, Form):  # inner_inner = (...)
                for inner_inner_inner in inner_inner:
                    if inner_inner_inner == with_mark:
                        yield Mark
                        assert False  # The simplify() function will not keep iterating after this.
                        return  # Never reached, could delete this line.
                    yield inner_inner_inner
                continue
    
            #else:  # inner_inner = foo ; inner = (foo)
            
            yield inner
    
    
    if __name__ == '__main__':
        unittest.main(argv=['ignored', 'MarkitTest0'], exit=False)


.. parsed-literal::

    .
    ----------------------------------------------------------------------
    Ran 1 test in 0.001s
    
    OK


.. code:: ipython2

    simplify(A, with_mark=b)




.. parsed-literal::

    (a)



Now we can create a new form that is equivalent to ``A``.

.. code:: ipython2

    def each_way(form, name):
        return simplify(F(
            ( name  , form),
            ((name,), simplify(form, with_mark=name)),
        ))
    
    Ab = each_way(A, b)
    
    print A, '=', Ab


.. parsed-literal::

    (((c) b) a) = (((a c) b) ((a) (b)))


In this particular case the original form ``A`` was so simple that the
new version ``Ab`` is actually a bit larger. With a large expression to
start with the form after simplification would (usually) be smaller.

Simplifying the Full-Bit Adder
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Recall our original expressions for the sum and carry bits of a full-bit
adder circuit, derived from the truth tables:

.. code:: ipython2

    Sum = F(( (a, b, (c,)), (a, (b,), c), ((a,), b, c), ((a,), (b,), (c,)) ),)
    Carry = F(( (a, (b,), (c,)), ((a,), b, (c,)), ((a,), (b,),  c), ((a,), (b,), (c,)) ),)

.. code:: ipython2

    Sum




.. parsed-literal::

    ((((a) (b) (c)) ((a) b c) ((b) a c) ((c) a b)))



.. code:: ipython2

    Carry




.. parsed-literal::

    ((((a) (b) (c)) ((a) (b) c) ((a) (c) b) ((b) (c) a)))



And the expressions derived from the definitions on Wikipedia:

.. code:: ipython2

    Sum, Carry = full_bit_adder(a, b, c)

.. code:: ipython2

    Sum




.. parsed-literal::

    ((((((((a) b) ((b) a)))) c) (((((a) b) ((b) a))) (c))))



.. code:: ipython2

    Carry




.. parsed-literal::

    ((((((((a) b) ((b) a)))) (c)) ((a) (b))))



We can use the ``each_way()`` function to look for simpler equivalent
forms by, for example, iterating though the names and trying it with
each. Try the following cells with both versions of the ``Sum`` and
``Carry`` above.

.. code:: ipython2

    print Sum
    for name in (a, b, c, a, b, c):
        Sum = each_way(Sum, name)
        print Sum


.. parsed-literal::

    ((((((((a) b) ((b) a)))) c) (((((a) b) ((b) a))) (c))))
    ((((b) (c)) (a) (b c)) (((b) c) ((c) b) a))
    (((((a) (c)) (a c)) b) ((((a) c) ((c) a)) (b)))
    (((((a) (b)) (a b)) c) ((((a) b) ((b) a)) (c)))
    (((((b) (c)) (b c)) a) ((((b) c) ((c) b)) (a)))
    (((((a) (c)) (a c)) b) ((((a) c) ((c) a)) (b)))
    (((((a) (b)) (a b)) c) ((((a) b) ((b) a)) (c)))


.. code:: ipython2

    print Carry
    for name in (a, b, c, a, b, c):
        Carry = each_way(Carry, name)
        print Carry


.. parsed-literal::

    ((((((((a) b) ((b) a)))) (c)) ((a) (b))))
    ((((b) (c)) a) ((a) b c))
    (((((a) c) (a)) b) ((b) a c))
    (((((b) a) (b)) c) ((c) a b))
    (((((c) b) (c)) a) ((a) b c))
    (((((a) c) (a)) b) ((b) a c))
    (((((b) a) (b)) c) ((c) a b))


Let's redefine the ``full_bit_adder()`` function with the smallest
version of each above.

.. code:: ipython2

    def full_bit_adder(a, b, c):
        '''From the truth table.'''
        return (
            F(( (a, b, (c,)), (a, (b,), c), ((a,), b, c), ((a,), (b,), (c,)) ),),
            F(( (a, (b,), (c,)), ((a,), b, (c,)), ((a,), (b,),  c), ((a,), (b,), (c,)) ),),
        )
    # Sizes: [63, 327, 1383, 5607, 22503, 90087, 360423, 1441767, 1441773]

.. code:: ipython2

    def full_bit_adder(a, b, c):
        '''Simplest forms from above.'''
        return (
            F( (((b,), (c,)), (a,), (b, c)), (((b,), c), ((c,), b), a) ),
            F( (((a,), (c,)), b), ((b,), a, c) ),
        )
    # Sizes: [57, 177, 417, 897, 1857, 3777, 7617, 15297, 7653]

.. code:: ipython2

    def full_bit_adder(a, b, c):
        '''Based on the definitions from Wikipedia.'''
        return (
            xor(xor(a, b), c),  # ((((((((a) b) ((b) a)))) c) (((((a) b) ((b) a))) (c))))
            or_(and_(a, b), and_(c, xor(a, b))),  # ((((((((a) b) ((b) a)))) (c)) ((a) (b))))
        )
    # Sizes: [67, 159, 251, 343, 435, 527, 619, 711, 371]

.. code:: ipython2

    def full_bit_adder(a, b, c):
        '''Based on the definitions from Wikipedia.'''
        return (
            simplify(xor(xor(a, b), c)),
            simplify(or_(and_(a, b), and_(c, xor(a, b)))),
        )
    # Sizes: [59, 135, 211, 287, 363, 439, 515, 591, 311]

In this case, the version from the definitions does much better than the
other two.

.. code:: ipython2

    Sum, Carry = full_bit_adder(a, b, c)
    
    truth_table(Sum)
    print
    truth_table(Carry)
    print
    
    sum0, cout = full_bit_adder('a0', 'b0', 'Cin')
    sum1, cout = full_bit_adder('a1', 'b1', cout)
    sum2, cout = full_bit_adder('a2', 'b2', cout)
    sum3, cout = full_bit_adder('a3', 'b3', cout)
    sum4, cout = full_bit_adder('a4', 'b4', cout)
    sum5, cout = full_bit_adder('a5', 'b5', cout)
    sum6, cout = full_bit_adder('a6', 'b6', cout)
    sum7, cout = full_bit_adder('a7', 'b7', cout)
    
    print map(len, map(str, (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7, cout)))


.. parsed-literal::

    ((((((a) b) ((b) a)) c) (((a) b) ((b) a) (c))))
     a  b  c | Value
    ---------+------
             | 
          () | ()
       ()    | ()
       () () | 
    ()       | ()
    ()    () | 
    () ()    | 
    () () () | ()
    
    ((((((a) b) ((b) a)) (c)) ((a) (b))))
     a  b  c | Value
    ---------+------
             | 
          () | 
       ()    | 
       () () | ()
    ()       | 
    ()    () | ()
    () ()    | ()
    () () () | ()
    
    [59, 135, 211, 287, 363, 439, 515, 591, 311]


`Davis–Putnam–Logemann–Loveland (DPLL) algorithm <https://en.wikipedia.org/wiki/Davis%E2%80%93Putnam%E2%80%93Logemann%E2%80%93Loveland_algorithm>`__ SAT Solver
===============================================================================================================================================================

This is something of an Interlude, we aren't going to use it below, but
it's too cool to omit mention.

We can use the ``simplify()`` function to create a more efficient SAT
solver along the lines of the DPLL algorithm.

It works by selecting a name from the form, and simplifying the form
with that name first as ``Void`` then as ``Mark``, then recursing with
the new form and the next name. If the resulting simplified form becomes
the ``Mark`` then our choices (of assigning ``Void`` or ``Mark`` to the
names selected so far) constitute a "solution" to the original form.
That is, if we ``reify()`` the form with the *environment* returned by
the ``dpll()`` function the result will be Mark-valued.

.. code:: ipython2

    def dpll(E, partial=None, unit=True):
        if partial is None:
            partial = {}
        else:
            partial = partial.copy() # so we can backtrack later..
    
        if isinstance(E, basestring):
            partial[E] = Mark
            return partial
    
        if unit:
            E = assign_unit_clauses(E, partial)
    
        if not E:
            return partial
    
        if Mark in E:
            return
    
        v = next_symbol_of(E)
    
        partial[v] = Void
    
        res = dpll(simplify(E, {v}), partial, unit)
        if res is not None: 
            return res
    
        partial[v] = Mark
    
        return dpll(simplify(E, with_mark=v), partial, unit)
    
    
    def assign_unit_clauses(E, partial):
        '''
        Find and assign values to an "unit clauses" in the form, simplifying as you go.
        A unit clause is a bare name or a negated name: a or (a), for these clauses we
        can set them to Void or Mark, respectively, to contibute to making their containing
        Form the Mark.
        '''
        on, off, E = find_units(E)
        while on or off:
            while on:
                if on & off: return Void
                term = first_of(on)
                partial[term] = Mark
                ON, OFF, E = find_units(simplify(E, with_mark=term))
                on |= ON
                on.remove(term)
                off |= OFF
            while off:
                if on & off: return Void
                term = first_of(off)
                partial[term] = Void
                ON, OFF, E = find_units(simplify(E, {term}))
                off |= OFF
                off.remove(term)
                on |= ON
        return E
    
    
    def next_symbol_of(E):
        if isinstance(E, basestring):
            return E
        for it in E:
            return next_symbol_of(it)
        raise Exception("no more symbols")
    
    
    def find_units(E):
        '''
        Return two sets and a possibly-reduced E.  The literals in the first
        set must be Void and those in the second must be set Mark to have the
        entire expression become Void.
        '''
        on, off, poly = set(), set(), set()
        for clause in E:
            if isinstance(clause, basestring):
                off.add(clause)
                continue
            if len(clause) != 1:
                poly.add(clause)
                continue
            (n,) = clause # Unwrap one layer of containment.
            if isinstance(n, basestring):
                on.add(n)
            else:
                poly.add(clause)
        return on, off, Form(poly)
    
    
    # Return any item from a form.
    first_of = lambda form: next(iter(form))

.. code:: ipython2

    A = F(a, (b, (c,)))
    truth_table(A)
    solution = dpll(A)
    arith = reify(A, solution)
    print
    print 'A solution:', solution
    print
    print 'Reifies to', arith, u'⟶', value_of(arith)


.. parsed-literal::

    (((c) b) a)
     a  b  c | Value
    ---------+------
             | ()
          () | 
       ()    | ()
       () () | ()
    ()       | 
    ()    () | 
    () ()    | 
    () () () | 
    
    A solution: {'a': (()), 'c': (()), 'b': (())}
    
    Reifies to ((((())) (())) (())) ⟶ ()


``dpll_iter()``
~~~~~~~~~~~~~~~

We can write a generator version of the function that keeps looking for
solutions after the first.

.. code:: ipython2

    def dpll_iter(E, partial=None, unit=True):
        if partial is None:
            partial = {}
        else:
            partial = partial.copy() # so we can backtrack later..
    
        if isinstance(E, basestring):
            partial[E] = Mark
            yield partial
            return
    
        if unit:
            E = assign_unit_clauses(E, partial)
    
        if not E:
            yield partial
            return
    
        if Mark in E:
            return
    
        v = next_symbol_of(E)
    
        partial[v] = Void
    
        for res in dpll_iter(simplify(E, {v}), partial, unit):
            yield res
    
        partial[v] = Mark
    
        for res in dpll_iter(simplify(E, with_mark=v), partial, unit):
            yield res

.. code:: ipython2

    for solution in dpll_iter(A):
        print (solution)


.. parsed-literal::

    {'a': (()), 'c': (()), 'b': (())}
    {'a': (()), 'b': ()}


.. code:: ipython2

    Sum, Carry = full_bit_adder(a, b, c)

.. code:: ipython2

    for solution in dpll_iter(Sum, unit=False):
        r = reify(Sum, solution)
        print (solution), r, '=', simplify(r)


.. parsed-literal::

    {'a': (()), 'c': (), 'b': (())} (((((((())) (()))) ()) ((((())) (())) (())))) = ()
    {'a': (()), 'c': (()), 'b': ()} (((((((())) ()) ((()))) (())) ((((())) ()) ((()))))) = ()
    {'a': (), 'c': (()), 'b': (())} (((((((())) ()) ((()))) (())) ((((())) ()) ((()))))) = ()
    {'a': (), 'c': (), 'b': ()} ((((((()) ())) ()) (((()) ()) (())))) = ()


.. code:: ipython2

    for solution in dpll_iter(Carry, unit=False):
        r = reify(Carry, solution)
        print (solution), r, '=', simplify(r)


.. parsed-literal::

    {'a': (()), 'c': (), 'b': ()} (((((((())) ()) ((()))) (())) (((())) (())))) = ()
    {'a': (), 'c': (), 'b': (())} (((((((())) ()) ((()))) (())) (((())) (())))) = ()
    {'a': (), 'b': ()} ((((((()) ())) (c)) ((())))) = ()


Notice that the reified form still has ``c`` in it but that doesn't
prevent the ``simplify()`` function from reducing the form to the Mark.
This should be the case for all solutions generated by the
``dpll_iter()`` function.

.. code:: ipython2

    for solution in dpll_iter(cout):
        print simplify(reify(cout, solution)),


.. parsed-literal::

    () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () (((a5) a5)) (((a5) a5)) () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () (((a5) a5)) (((a5) a5)) () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () (((a5) a5)) (((a5) a5)) () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () () (((a5) a5)) (((a5) a5)) () ()


Interesting! Some solutions do not ``simplify()`` completely in one go.
The form ``(((a5) a5))`` is Mark-valued:

::

    (((a5) a5))
    (((  ) a5))
    (((  )   ))
    (         )
    ()

.. code:: ipython2

    E = F(((a,), a))
    print E
    print simplify(E)


.. parsed-literal::

    (((a) a))
    ()


Something to keep in mind.

Now back to Circuits
====================

Using the Adder Circuits to Add
-------------------------------

In order to keep things tractable I'm going to use just four bits rather
than eight.

.. code:: ipython2

    sum0, cout = full_bit_adder('a0', 'b0', 'Cin')
    sum1, cout = full_bit_adder('a1', 'b1', cout)
    sum2, cout = full_bit_adder('a2', 'b2', cout)
    sum3, cout = full_bit_adder('a3', 'b3', cout)

Put the circuit expressions into a handy dictionary, and we are ready to
add some numbers.

.. code:: ipython2

    CIRCUITS = {
        'sum0': sum0,
        'sum1': sum1,
        'sum2': sum2,
        'sum3': sum3,
        'cout': cout,
    }

A bunch of crufty junk to print out a nice truth table with the columns
arranged to make it (relatively) easy to see the addition.

.. code:: ipython2

    def stringy_env(env):
        return {
            k: value_of(e, v='--', m='()')
            for k, e in env.items()
        }
    
    INPUTs = ('Cin', 'a3', 'a2', 'a1', 'a0', 'b3', 'b2', 'b1', 'b0')
    OUTPUTs = ('cout', 'sum3', 'sum2', 'sum1', 'sum0')
    
    format_string = '%(' + ')s %('.join(INPUTs) + ')s | %(' + ')s %('.join(OUTPUTs) + ')s'
    
    
    print 'Ci|a3 a2 a1 a0|b3 b2 b1 b0 | Co s3 s2 s1 s0'
    results = {}
    for env in environments_of_variables(*INPUTs):
    
        for name, expression in CIRCUITS.items():
            results[name] = value_of(reify(expression, env))
    
        env.update(results)
        print format_string % stringy_env(env)


.. parsed-literal::

    Ci|a3 a2 a1 a0|b3 b2 b1 b0 | Co s3 s2 s1 s0
    -- -- -- -- -- -- -- -- -- | -- -- -- -- --
    -- -- -- -- -- -- -- -- () | -- -- -- -- ()
    -- -- -- -- -- -- -- () -- | -- -- -- () --
    -- -- -- -- -- -- -- () () | -- -- -- () ()
    -- -- -- -- -- -- () -- -- | -- -- () -- --
    -- -- -- -- -- -- () -- () | -- -- () -- ()
    -- -- -- -- -- -- () () -- | -- -- () () --
    -- -- -- -- -- -- () () () | -- -- () () ()
    -- -- -- -- -- () -- -- -- | -- () -- -- --
    -- -- -- -- -- () -- -- () | -- () -- -- ()
    -- -- -- -- -- () -- () -- | -- () -- () --
    -- -- -- -- -- () -- () () | -- () -- () ()
    -- -- -- -- -- () () -- -- | -- () () -- --
    -- -- -- -- -- () () -- () | -- () () -- ()
    -- -- -- -- -- () () () -- | -- () () () --
    -- -- -- -- -- () () () () | -- () () () ()
    -- -- -- -- () -- -- -- -- | -- -- -- -- ()
    -- -- -- -- () -- -- -- () | -- -- -- () --
    -- -- -- -- () -- -- () -- | -- -- -- () ()
    -- -- -- -- () -- -- () () | -- -- () -- --
    -- -- -- -- () -- () -- -- | -- -- () -- ()
    -- -- -- -- () -- () -- () | -- -- () () --
    -- -- -- -- () -- () () -- | -- -- () () ()
    -- -- -- -- () -- () () () | -- () -- -- --
    -- -- -- -- () () -- -- -- | -- () -- -- ()
    -- -- -- -- () () -- -- () | -- () -- () --
    -- -- -- -- () () -- () -- | -- () -- () ()
    -- -- -- -- () () -- () () | -- () () -- --
    -- -- -- -- () () () -- -- | -- () () -- ()
    -- -- -- -- () () () -- () | -- () () () --
    -- -- -- -- () () () () -- | -- () () () ()
    -- -- -- -- () () () () () | () -- -- -- --
    -- -- -- () -- -- -- -- -- | -- -- -- () --
    -- -- -- () -- -- -- -- () | -- -- -- () ()
    -- -- -- () -- -- -- () -- | -- -- () -- --
    -- -- -- () -- -- -- () () | -- -- () -- ()
    -- -- -- () -- -- () -- -- | -- -- () () --
    -- -- -- () -- -- () -- () | -- -- () () ()
    -- -- -- () -- -- () () -- | -- () -- -- --
    -- -- -- () -- -- () () () | -- () -- -- ()
    -- -- -- () -- () -- -- -- | -- () -- () --
    -- -- -- () -- () -- -- () | -- () -- () ()
    -- -- -- () -- () -- () -- | -- () () -- --
    -- -- -- () -- () -- () () | -- () () -- ()
    -- -- -- () -- () () -- -- | -- () () () --
    -- -- -- () -- () () -- () | -- () () () ()
    -- -- -- () -- () () () -- | () -- -- -- --
    -- -- -- () -- () () () () | () -- -- -- ()
    -- -- -- () () -- -- -- -- | -- -- -- () ()
    -- -- -- () () -- -- -- () | -- -- () -- --
    -- -- -- () () -- -- () -- | -- -- () -- ()
    -- -- -- () () -- -- () () | -- -- () () --
    -- -- -- () () -- () -- -- | -- -- () () ()
    -- -- -- () () -- () -- () | -- () -- -- --
    -- -- -- () () -- () () -- | -- () -- -- ()
    -- -- -- () () -- () () () | -- () -- () --
    -- -- -- () () () -- -- -- | -- () -- () ()
    -- -- -- () () () -- -- () | -- () () -- --
    -- -- -- () () () -- () -- | -- () () -- ()
    -- -- -- () () () -- () () | -- () () () --
    -- -- -- () () () () -- -- | -- () () () ()
    -- -- -- () () () () -- () | () -- -- -- --
    -- -- -- () () () () () -- | () -- -- -- ()
    -- -- -- () () () () () () | () -- -- () --
    -- -- () -- -- -- -- -- -- | -- -- () -- --
    -- -- () -- -- -- -- -- () | -- -- () -- ()
    -- -- () -- -- -- -- () -- | -- -- () () --
    -- -- () -- -- -- -- () () | -- -- () () ()
    -- -- () -- -- -- () -- -- | -- () -- -- --
    -- -- () -- -- -- () -- () | -- () -- -- ()
    -- -- () -- -- -- () () -- | -- () -- () --
    -- -- () -- -- -- () () () | -- () -- () ()
    -- -- () -- -- () -- -- -- | -- () () -- --
    -- -- () -- -- () -- -- () | -- () () -- ()
    -- -- () -- -- () -- () -- | -- () () () --
    -- -- () -- -- () -- () () | -- () () () ()
    -- -- () -- -- () () -- -- | () -- -- -- --
    -- -- () -- -- () () -- () | () -- -- -- ()
    -- -- () -- -- () () () -- | () -- -- () --
    -- -- () -- -- () () () () | () -- -- () ()
    -- -- () -- () -- -- -- -- | -- -- () -- ()
    -- -- () -- () -- -- -- () | -- -- () () --
    -- -- () -- () -- -- () -- | -- -- () () ()
    -- -- () -- () -- -- () () | -- () -- -- --
    -- -- () -- () -- () -- -- | -- () -- -- ()
    -- -- () -- () -- () -- () | -- () -- () --
    -- -- () -- () -- () () -- | -- () -- () ()
    -- -- () -- () -- () () () | -- () () -- --
    -- -- () -- () () -- -- -- | -- () () -- ()
    -- -- () -- () () -- -- () | -- () () () --
    -- -- () -- () () -- () -- | -- () () () ()
    -- -- () -- () () -- () () | () -- -- -- --
    -- -- () -- () () () -- -- | () -- -- -- ()
    -- -- () -- () () () -- () | () -- -- () --
    -- -- () -- () () () () -- | () -- -- () ()
    -- -- () -- () () () () () | () -- () -- --
    -- -- () () -- -- -- -- -- | -- -- () () --
    -- -- () () -- -- -- -- () | -- -- () () ()
    -- -- () () -- -- -- () -- | -- () -- -- --
    -- -- () () -- -- -- () () | -- () -- -- ()
    -- -- () () -- -- () -- -- | -- () -- () --
    -- -- () () -- -- () -- () | -- () -- () ()
    -- -- () () -- -- () () -- | -- () () -- --
    -- -- () () -- -- () () () | -- () () -- ()
    -- -- () () -- () -- -- -- | -- () () () --
    -- -- () () -- () -- -- () | -- () () () ()
    -- -- () () -- () -- () -- | () -- -- -- --
    -- -- () () -- () -- () () | () -- -- -- ()
    -- -- () () -- () () -- -- | () -- -- () --
    -- -- () () -- () () -- () | () -- -- () ()
    -- -- () () -- () () () -- | () -- () -- --
    -- -- () () -- () () () () | () -- () -- ()
    -- -- () () () -- -- -- -- | -- -- () () ()
    -- -- () () () -- -- -- () | -- () -- -- --
    -- -- () () () -- -- () -- | -- () -- -- ()
    -- -- () () () -- -- () () | -- () -- () --
    -- -- () () () -- () -- -- | -- () -- () ()
    -- -- () () () -- () -- () | -- () () -- --
    -- -- () () () -- () () -- | -- () () -- ()
    -- -- () () () -- () () () | -- () () () --
    -- -- () () () () -- -- -- | -- () () () ()
    -- -- () () () () -- -- () | () -- -- -- --
    -- -- () () () () -- () -- | () -- -- -- ()
    -- -- () () () () -- () () | () -- -- () --
    -- -- () () () () () -- -- | () -- -- () ()
    -- -- () () () () () -- () | () -- () -- --
    -- -- () () () () () () -- | () -- () -- ()
    -- -- () () () () () () () | () -- () () --
    -- () -- -- -- -- -- -- -- | -- () -- -- --
    -- () -- -- -- -- -- -- () | -- () -- -- ()
    -- () -- -- -- -- -- () -- | -- () -- () --
    -- () -- -- -- -- -- () () | -- () -- () ()
    -- () -- -- -- -- () -- -- | -- () () -- --
    -- () -- -- -- -- () -- () | -- () () -- ()
    -- () -- -- -- -- () () -- | -- () () () --
    -- () -- -- -- -- () () () | -- () () () ()
    -- () -- -- -- () -- -- -- | () -- -- -- --
    -- () -- -- -- () -- -- () | () -- -- -- ()
    -- () -- -- -- () -- () -- | () -- -- () --
    -- () -- -- -- () -- () () | () -- -- () ()
    -- () -- -- -- () () -- -- | () -- () -- --
    -- () -- -- -- () () -- () | () -- () -- ()
    -- () -- -- -- () () () -- | () -- () () --
    -- () -- -- -- () () () () | () -- () () ()
    -- () -- -- () -- -- -- -- | -- () -- -- ()
    -- () -- -- () -- -- -- () | -- () -- () --
    -- () -- -- () -- -- () -- | -- () -- () ()
    -- () -- -- () -- -- () () | -- () () -- --
    -- () -- -- () -- () -- -- | -- () () -- ()
    -- () -- -- () -- () -- () | -- () () () --
    -- () -- -- () -- () () -- | -- () () () ()
    -- () -- -- () -- () () () | () -- -- -- --
    -- () -- -- () () -- -- -- | () -- -- -- ()
    -- () -- -- () () -- -- () | () -- -- () --
    -- () -- -- () () -- () -- | () -- -- () ()
    -- () -- -- () () -- () () | () -- () -- --
    -- () -- -- () () () -- -- | () -- () -- ()
    -- () -- -- () () () -- () | () -- () () --
    -- () -- -- () () () () -- | () -- () () ()
    -- () -- -- () () () () () | () () -- -- --
    -- () -- () -- -- -- -- -- | -- () -- () --
    -- () -- () -- -- -- -- () | -- () -- () ()
    -- () -- () -- -- -- () -- | -- () () -- --
    -- () -- () -- -- -- () () | -- () () -- ()
    -- () -- () -- -- () -- -- | -- () () () --
    -- () -- () -- -- () -- () | -- () () () ()
    -- () -- () -- -- () () -- | () -- -- -- --
    -- () -- () -- -- () () () | () -- -- -- ()
    -- () -- () -- () -- -- -- | () -- -- () --
    -- () -- () -- () -- -- () | () -- -- () ()
    -- () -- () -- () -- () -- | () -- () -- --
    -- () -- () -- () -- () () | () -- () -- ()
    -- () -- () -- () () -- -- | () -- () () --
    -- () -- () -- () () -- () | () -- () () ()
    -- () -- () -- () () () -- | () () -- -- --
    -- () -- () -- () () () () | () () -- -- ()
    -- () -- () () -- -- -- -- | -- () -- () ()
    -- () -- () () -- -- -- () | -- () () -- --
    -- () -- () () -- -- () -- | -- () () -- ()
    -- () -- () () -- -- () () | -- () () () --
    -- () -- () () -- () -- -- | -- () () () ()
    -- () -- () () -- () -- () | () -- -- -- --
    -- () -- () () -- () () -- | () -- -- -- ()
    -- () -- () () -- () () () | () -- -- () --
    -- () -- () () () -- -- -- | () -- -- () ()
    -- () -- () () () -- -- () | () -- () -- --
    -- () -- () () () -- () -- | () -- () -- ()
    -- () -- () () () -- () () | () -- () () --
    -- () -- () () () () -- -- | () -- () () ()
    -- () -- () () () () -- () | () () -- -- --
    -- () -- () () () () () -- | () () -- -- ()
    -- () -- () () () () () () | () () -- () --
    -- () () -- -- -- -- -- -- | -- () () -- --
    -- () () -- -- -- -- -- () | -- () () -- ()
    -- () () -- -- -- -- () -- | -- () () () --
    -- () () -- -- -- -- () () | -- () () () ()
    -- () () -- -- -- () -- -- | () -- -- -- --
    -- () () -- -- -- () -- () | () -- -- -- ()
    -- () () -- -- -- () () -- | () -- -- () --
    -- () () -- -- -- () () () | () -- -- () ()
    -- () () -- -- () -- -- -- | () -- () -- --
    -- () () -- -- () -- -- () | () -- () -- ()
    -- () () -- -- () -- () -- | () -- () () --
    -- () () -- -- () -- () () | () -- () () ()
    -- () () -- -- () () -- -- | () () -- -- --
    -- () () -- -- () () -- () | () () -- -- ()
    -- () () -- -- () () () -- | () () -- () --
    -- () () -- -- () () () () | () () -- () ()
    -- () () -- () -- -- -- -- | -- () () -- ()
    -- () () -- () -- -- -- () | -- () () () --
    -- () () -- () -- -- () -- | -- () () () ()
    -- () () -- () -- -- () () | () -- -- -- --
    -- () () -- () -- () -- -- | () -- -- -- ()
    -- () () -- () -- () -- () | () -- -- () --
    -- () () -- () -- () () -- | () -- -- () ()
    -- () () -- () -- () () () | () -- () -- --
    -- () () -- () () -- -- -- | () -- () -- ()
    -- () () -- () () -- -- () | () -- () () --
    -- () () -- () () -- () -- | () -- () () ()
    -- () () -- () () -- () () | () () -- -- --
    -- () () -- () () () -- -- | () () -- -- ()
    -- () () -- () () () -- () | () () -- () --
    -- () () -- () () () () -- | () () -- () ()
    -- () () -- () () () () () | () () () -- --
    -- () () () -- -- -- -- -- | -- () () () --
    -- () () () -- -- -- -- () | -- () () () ()
    -- () () () -- -- -- () -- | () -- -- -- --
    -- () () () -- -- -- () () | () -- -- -- ()
    -- () () () -- -- () -- -- | () -- -- () --
    -- () () () -- -- () -- () | () -- -- () ()
    -- () () () -- -- () () -- | () -- () -- --
    -- () () () -- -- () () () | () -- () -- ()
    -- () () () -- () -- -- -- | () -- () () --
    -- () () () -- () -- -- () | () -- () () ()
    -- () () () -- () -- () -- | () () -- -- --
    -- () () () -- () -- () () | () () -- -- ()
    -- () () () -- () () -- -- | () () -- () --
    -- () () () -- () () -- () | () () -- () ()
    -- () () () -- () () () -- | () () () -- --
    -- () () () -- () () () () | () () () -- ()
    -- () () () () -- -- -- -- | -- () () () ()
    -- () () () () -- -- -- () | () -- -- -- --
    -- () () () () -- -- () -- | () -- -- -- ()
    -- () () () () -- -- () () | () -- -- () --
    -- () () () () -- () -- -- | () -- -- () ()
    -- () () () () -- () -- () | () -- () -- --
    -- () () () () -- () () -- | () -- () -- ()
    -- () () () () -- () () () | () -- () () --
    -- () () () () () -- -- -- | () -- () () ()
    -- () () () () () -- -- () | () () -- -- --
    -- () () () () () -- () -- | () () -- -- ()
    -- () () () () () -- () () | () () -- () --
    -- () () () () () () -- -- | () () -- () ()
    -- () () () () () () -- () | () () () -- --
    -- () () () () () () () -- | () () () -- ()
    -- () () () () () () () () | () () () () --
    () -- -- -- -- -- -- -- -- | -- -- -- -- ()
    () -- -- -- -- -- -- -- () | -- -- -- () --
    () -- -- -- -- -- -- () -- | -- -- -- () ()
    () -- -- -- -- -- -- () () | -- -- () -- --
    () -- -- -- -- -- () -- -- | -- -- () -- ()
    () -- -- -- -- -- () -- () | -- -- () () --
    () -- -- -- -- -- () () -- | -- -- () () ()
    () -- -- -- -- -- () () () | -- () -- -- --
    () -- -- -- -- () -- -- -- | -- () -- -- ()
    () -- -- -- -- () -- -- () | -- () -- () --
    () -- -- -- -- () -- () -- | -- () -- () ()
    () -- -- -- -- () -- () () | -- () () -- --
    () -- -- -- -- () () -- -- | -- () () -- ()
    () -- -- -- -- () () -- () | -- () () () --
    () -- -- -- -- () () () -- | -- () () () ()
    () -- -- -- -- () () () () | () -- -- -- --
    () -- -- -- () -- -- -- -- | -- -- -- () --
    () -- -- -- () -- -- -- () | -- -- -- () ()
    () -- -- -- () -- -- () -- | -- -- () -- --
    () -- -- -- () -- -- () () | -- -- () -- ()
    () -- -- -- () -- () -- -- | -- -- () () --
    () -- -- -- () -- () -- () | -- -- () () ()
    () -- -- -- () -- () () -- | -- () -- -- --
    () -- -- -- () -- () () () | -- () -- -- ()
    () -- -- -- () () -- -- -- | -- () -- () --
    () -- -- -- () () -- -- () | -- () -- () ()
    () -- -- -- () () -- () -- | -- () () -- --
    () -- -- -- () () -- () () | -- () () -- ()
    () -- -- -- () () () -- -- | -- () () () --
    () -- -- -- () () () -- () | -- () () () ()
    () -- -- -- () () () () -- | () -- -- -- --
    () -- -- -- () () () () () | () -- -- -- ()
    () -- -- () -- -- -- -- -- | -- -- -- () ()
    () -- -- () -- -- -- -- () | -- -- () -- --
    () -- -- () -- -- -- () -- | -- -- () -- ()
    () -- -- () -- -- -- () () | -- -- () () --
    () -- -- () -- -- () -- -- | -- -- () () ()
    () -- -- () -- -- () -- () | -- () -- -- --
    () -- -- () -- -- () () -- | -- () -- -- ()
    () -- -- () -- -- () () () | -- () -- () --
    () -- -- () -- () -- -- -- | -- () -- () ()
    () -- -- () -- () -- -- () | -- () () -- --
    () -- -- () -- () -- () -- | -- () () -- ()
    () -- -- () -- () -- () () | -- () () () --
    () -- -- () -- () () -- -- | -- () () () ()
    () -- -- () -- () () -- () | () -- -- -- --
    () -- -- () -- () () () -- | () -- -- -- ()
    () -- -- () -- () () () () | () -- -- () --
    () -- -- () () -- -- -- -- | -- -- () -- --
    () -- -- () () -- -- -- () | -- -- () -- ()
    () -- -- () () -- -- () -- | -- -- () () --
    () -- -- () () -- -- () () | -- -- () () ()
    () -- -- () () -- () -- -- | -- () -- -- --
    () -- -- () () -- () -- () | -- () -- -- ()
    () -- -- () () -- () () -- | -- () -- () --
    () -- -- () () -- () () () | -- () -- () ()
    () -- -- () () () -- -- -- | -- () () -- --
    () -- -- () () () -- -- () | -- () () -- ()
    () -- -- () () () -- () -- | -- () () () --
    () -- -- () () () -- () () | -- () () () ()
    () -- -- () () () () -- -- | () -- -- -- --
    () -- -- () () () () -- () | () -- -- -- ()
    () -- -- () () () () () -- | () -- -- () --
    () -- -- () () () () () () | () -- -- () ()
    () -- () -- -- -- -- -- -- | -- -- () -- ()
    () -- () -- -- -- -- -- () | -- -- () () --
    () -- () -- -- -- -- () -- | -- -- () () ()
    () -- () -- -- -- -- () () | -- () -- -- --
    () -- () -- -- -- () -- -- | -- () -- -- ()
    () -- () -- -- -- () -- () | -- () -- () --
    () -- () -- -- -- () () -- | -- () -- () ()
    () -- () -- -- -- () () () | -- () () -- --
    () -- () -- -- () -- -- -- | -- () () -- ()
    () -- () -- -- () -- -- () | -- () () () --
    () -- () -- -- () -- () -- | -- () () () ()
    () -- () -- -- () -- () () | () -- -- -- --
    () -- () -- -- () () -- -- | () -- -- -- ()
    () -- () -- -- () () -- () | () -- -- () --
    () -- () -- -- () () () -- | () -- -- () ()
    () -- () -- -- () () () () | () -- () -- --
    () -- () -- () -- -- -- -- | -- -- () () --
    () -- () -- () -- -- -- () | -- -- () () ()
    () -- () -- () -- -- () -- | -- () -- -- --
    () -- () -- () -- -- () () | -- () -- -- ()
    () -- () -- () -- () -- -- | -- () -- () --
    () -- () -- () -- () -- () | -- () -- () ()
    () -- () -- () -- () () -- | -- () () -- --
    () -- () -- () -- () () () | -- () () -- ()
    () -- () -- () () -- -- -- | -- () () () --
    () -- () -- () () -- -- () | -- () () () ()
    () -- () -- () () -- () -- | () -- -- -- --
    () -- () -- () () -- () () | () -- -- -- ()
    () -- () -- () () () -- -- | () -- -- () --
    () -- () -- () () () -- () | () -- -- () ()
    () -- () -- () () () () -- | () -- () -- --
    () -- () -- () () () () () | () -- () -- ()
    () -- () () -- -- -- -- -- | -- -- () () ()
    () -- () () -- -- -- -- () | -- () -- -- --
    () -- () () -- -- -- () -- | -- () -- -- ()
    () -- () () -- -- -- () () | -- () -- () --
    () -- () () -- -- () -- -- | -- () -- () ()
    () -- () () -- -- () -- () | -- () () -- --
    () -- () () -- -- () () -- | -- () () -- ()
    () -- () () -- -- () () () | -- () () () --
    () -- () () -- () -- -- -- | -- () () () ()
    () -- () () -- () -- -- () | () -- -- -- --
    () -- () () -- () -- () -- | () -- -- -- ()
    () -- () () -- () -- () () | () -- -- () --
    () -- () () -- () () -- -- | () -- -- () ()
    () -- () () -- () () -- () | () -- () -- --
    () -- () () -- () () () -- | () -- () -- ()
    () -- () () -- () () () () | () -- () () --
    () -- () () () -- -- -- -- | -- () -- -- --
    () -- () () () -- -- -- () | -- () -- -- ()
    () -- () () () -- -- () -- | -- () -- () --
    () -- () () () -- -- () () | -- () -- () ()
    () -- () () () -- () -- -- | -- () () -- --
    () -- () () () -- () -- () | -- () () -- ()
    () -- () () () -- () () -- | -- () () () --
    () -- () () () -- () () () | -- () () () ()
    () -- () () () () -- -- -- | () -- -- -- --
    () -- () () () () -- -- () | () -- -- -- ()
    () -- () () () () -- () -- | () -- -- () --
    () -- () () () () -- () () | () -- -- () ()
    () -- () () () () () -- -- | () -- () -- --
    () -- () () () () () -- () | () -- () -- ()
    () -- () () () () () () -- | () -- () () --
    () -- () () () () () () () | () -- () () ()
    () () -- -- -- -- -- -- -- | -- () -- -- ()
    () () -- -- -- -- -- -- () | -- () -- () --
    () () -- -- -- -- -- () -- | -- () -- () ()
    () () -- -- -- -- -- () () | -- () () -- --
    () () -- -- -- -- () -- -- | -- () () -- ()
    () () -- -- -- -- () -- () | -- () () () --
    () () -- -- -- -- () () -- | -- () () () ()
    () () -- -- -- -- () () () | () -- -- -- --
    () () -- -- -- () -- -- -- | () -- -- -- ()
    () () -- -- -- () -- -- () | () -- -- () --
    () () -- -- -- () -- () -- | () -- -- () ()
    () () -- -- -- () -- () () | () -- () -- --
    () () -- -- -- () () -- -- | () -- () -- ()
    () () -- -- -- () () -- () | () -- () () --
    () () -- -- -- () () () -- | () -- () () ()
    () () -- -- -- () () () () | () () -- -- --
    () () -- -- () -- -- -- -- | -- () -- () --
    () () -- -- () -- -- -- () | -- () -- () ()
    () () -- -- () -- -- () -- | -- () () -- --
    () () -- -- () -- -- () () | -- () () -- ()
    () () -- -- () -- () -- -- | -- () () () --
    () () -- -- () -- () -- () | -- () () () ()
    () () -- -- () -- () () -- | () -- -- -- --
    () () -- -- () -- () () () | () -- -- -- ()
    () () -- -- () () -- -- -- | () -- -- () --
    () () -- -- () () -- -- () | () -- -- () ()
    () () -- -- () () -- () -- | () -- () -- --
    () () -- -- () () -- () () | () -- () -- ()
    () () -- -- () () () -- -- | () -- () () --
    () () -- -- () () () -- () | () -- () () ()
    () () -- -- () () () () -- | () () -- -- --
    () () -- -- () () () () () | () () -- -- ()
    () () -- () -- -- -- -- -- | -- () -- () ()
    () () -- () -- -- -- -- () | -- () () -- --
    () () -- () -- -- -- () -- | -- () () -- ()
    () () -- () -- -- -- () () | -- () () () --
    () () -- () -- -- () -- -- | -- () () () ()
    () () -- () -- -- () -- () | () -- -- -- --
    () () -- () -- -- () () -- | () -- -- -- ()
    () () -- () -- -- () () () | () -- -- () --
    () () -- () -- () -- -- -- | () -- -- () ()
    () () -- () -- () -- -- () | () -- () -- --
    () () -- () -- () -- () -- | () -- () -- ()
    () () -- () -- () -- () () | () -- () () --
    () () -- () -- () () -- -- | () -- () () ()
    () () -- () -- () () -- () | () () -- -- --
    () () -- () -- () () () -- | () () -- -- ()
    () () -- () -- () () () () | () () -- () --
    () () -- () () -- -- -- -- | -- () () -- --
    () () -- () () -- -- -- () | -- () () -- ()
    () () -- () () -- -- () -- | -- () () () --
    () () -- () () -- -- () () | -- () () () ()
    () () -- () () -- () -- -- | () -- -- -- --
    () () -- () () -- () -- () | () -- -- -- ()
    () () -- () () -- () () -- | () -- -- () --
    () () -- () () -- () () () | () -- -- () ()
    () () -- () () () -- -- -- | () -- () -- --
    () () -- () () () -- -- () | () -- () -- ()
    () () -- () () () -- () -- | () -- () () --
    () () -- () () () -- () () | () -- () () ()
    () () -- () () () () -- -- | () () -- -- --
    () () -- () () () () -- () | () () -- -- ()
    () () -- () () () () () -- | () () -- () --
    () () -- () () () () () () | () () -- () ()
    () () () -- -- -- -- -- -- | -- () () -- ()
    () () () -- -- -- -- -- () | -- () () () --
    () () () -- -- -- -- () -- | -- () () () ()
    () () () -- -- -- -- () () | () -- -- -- --
    () () () -- -- -- () -- -- | () -- -- -- ()
    () () () -- -- -- () -- () | () -- -- () --
    () () () -- -- -- () () -- | () -- -- () ()
    () () () -- -- -- () () () | () -- () -- --
    () () () -- -- () -- -- -- | () -- () -- ()
    () () () -- -- () -- -- () | () -- () () --
    () () () -- -- () -- () -- | () -- () () ()
    () () () -- -- () -- () () | () () -- -- --
    () () () -- -- () () -- -- | () () -- -- ()
    () () () -- -- () () -- () | () () -- () --
    () () () -- -- () () () -- | () () -- () ()
    () () () -- -- () () () () | () () () -- --
    () () () -- () -- -- -- -- | -- () () () --
    () () () -- () -- -- -- () | -- () () () ()
    () () () -- () -- -- () -- | () -- -- -- --
    () () () -- () -- -- () () | () -- -- -- ()
    () () () -- () -- () -- -- | () -- -- () --
    () () () -- () -- () -- () | () -- -- () ()
    () () () -- () -- () () -- | () -- () -- --
    () () () -- () -- () () () | () -- () -- ()
    () () () -- () () -- -- -- | () -- () () --
    () () () -- () () -- -- () | () -- () () ()
    () () () -- () () -- () -- | () () -- -- --
    () () () -- () () -- () () | () () -- -- ()
    () () () -- () () () -- -- | () () -- () --
    () () () -- () () () -- () | () () -- () ()
    () () () -- () () () () -- | () () () -- --
    () () () -- () () () () () | () () () -- ()
    () () () () -- -- -- -- -- | -- () () () ()
    () () () () -- -- -- -- () | () -- -- -- --
    () () () () -- -- -- () -- | () -- -- -- ()
    () () () () -- -- -- () () | () -- -- () --
    () () () () -- -- () -- -- | () -- -- () ()
    () () () () -- -- () -- () | () -- () -- --
    () () () () -- -- () () -- | () -- () -- ()
    () () () () -- -- () () () | () -- () () --
    () () () () -- () -- -- -- | () -- () () ()
    () () () () -- () -- -- () | () () -- -- --
    () () () () -- () -- () -- | () () -- -- ()
    () () () () -- () -- () () | () () -- () --
    () () () () -- () () -- -- | () () -- () ()
    () () () () -- () () -- () | () () () -- --
    () () () () -- () () () -- | () () () -- ()
    () () () () -- () () () () | () () () () --
    () () () () () -- -- -- -- | () -- -- -- --
    () () () () () -- -- -- () | () -- -- -- ()
    () () () () () -- -- () -- | () -- -- () --
    () () () () () -- -- () () | () -- -- () ()
    () () () () () -- () -- -- | () -- () -- --
    () () () () () -- () -- () | () -- () -- ()
    () () () () () -- () () -- | () -- () () --
    () () () () () -- () () () | () -- () () ()
    () () () () () () -- -- -- | () () -- -- --
    () () () () () () -- -- () | () () -- -- ()
    () () () () () () -- () -- | () () -- () --
    () () () () () () -- () () | () () -- () ()
    () () () () () () () -- -- | () () () -- --
    () () () () () () () -- () | () () () -- ()
    () () () () () () () () -- | () () () () --
    () () () () () () () () () | () () () () ()


A Model of Computation.
=======================

That was a bit steep, let's formalize it and make it a little easier to
work with.

First let's have a *register* of named values:

.. code:: ipython2

    R = {name: Void for name in 'Cin a3 a2 a1 a0 b3 b2 b1 b0 Cout'.split()}

Let's have a *program* of named expressions that give new values when
evaluated in terms of the current values in **R** (this is just the same
``CIRCUITS``, but feeding back the results into the "b" bits):

.. code:: ipython2

    P = {
        'b0': sum0,
        'b1': sum1,
        'b2': sum2,
        'b3': sum3,
        'Cout': cout,
    }

One *cycle* of the machine means to evaluate each named expression in
the program with the current values in the register.

.. code:: ipython2

    make_reify_reducer = lambda env: lambda form: value_of(reify(form, env))
    
    
    def cycle(program, register):
        rr = make_reify_reducer(register)
        return {bit: rr(expression) for bit, expression in program.iteritems()}

With all the register values at "zero" (Void) nothing happens.

.. code:: ipython2

    R.update(cycle(P, R))
    R




.. parsed-literal::

    {'Cin': (()),
     'Cout': (()),
     'a0': (()),
     'a1': (()),
     'a2': (()),
     'a3': (()),
     'b0': (()),
     'b1': (()),
     'b2': (()),
     'b3': (())}



Let's make a nice display function to inspect our little adder computer.

.. code:: ipython2

    def show_as_int(*names):
        '''
        Return a function that converts a sequence of
        named bits (as Void/Mark values) into a integer.
        '''
        def inner(register):
            i, n = 0, 1
            for name in names:
                if not register[name]:
                    i += n
                n <<= 1
            return i
        return inner

.. code:: ipython2

    a_register = show_as_int('a0', 'a1', 'a2', 'a3')
    b_register = show_as_int('b0', 'b1', 'b2', 'b3')
    
    
    def show_computer_state(R):
        print 'a: %-3i b: %-3i Cin: %-3i Cout: %-3i' % (
            a_register(R),
            b_register(R),
            int(not R['Cin']),
            int(not R['Cout']),
        )

.. code:: ipython2

    show_computer_state(R)


.. parsed-literal::

    a: 0   b: 0   Cin: 0   Cout: 0  


Let's set one bit to true (Mark-valued in the chosen convention. We
could have Void be true but we would have to form the circuit
expressions differently.)

.. code:: ipython2

    R['a0'] = Mark

Now let's count to twenty.

.. code:: ipython2

    for _ in range(20):
        show_computer_state(R)
        R.update(cycle(P, R))



.. parsed-literal::

    a: 1   b: 0   Cin: 0   Cout: 0  
    a: 1   b: 1   Cin: 0   Cout: 0  
    a: 1   b: 2   Cin: 0   Cout: 0  
    a: 1   b: 3   Cin: 0   Cout: 0  
    a: 1   b: 4   Cin: 0   Cout: 0  
    a: 1   b: 5   Cin: 0   Cout: 0  
    a: 1   b: 6   Cin: 0   Cout: 0  
    a: 1   b: 7   Cin: 0   Cout: 0  
    a: 1   b: 8   Cin: 0   Cout: 0  
    a: 1   b: 9   Cin: 0   Cout: 0  
    a: 1   b: 10  Cin: 0   Cout: 0  
    a: 1   b: 11  Cin: 0   Cout: 0  
    a: 1   b: 12  Cin: 0   Cout: 0  
    a: 1   b: 13  Cin: 0   Cout: 0  
    a: 1   b: 14  Cin: 0   Cout: 0  
    a: 1   b: 15  Cin: 0   Cout: 0  
    a: 1   b: 0   Cin: 0   Cout: 1  
    a: 1   b: 1   Cin: 0   Cout: 0  
    a: 1   b: 2   Cin: 0   Cout: 0  
    a: 1   b: 3   Cin: 0   Cout: 0  


You can see that at the sixteenth step the "Cout" carry bit is true and
the count cycles back to zero.

.. code:: ipython2

    # Reset the register.
    R = {name: Void for name in 'Cin a3 a2 a1 a0 b3 b2 b1 b0 Cout'.split()}
    
    # Count by three.
    R['a0'] = R['a1'] = Mark
    
    # Print out twenty cycles.
    for _ in range(20):
        show_computer_state(R)
        R.update(cycle(P, R))


.. parsed-literal::

    a: 3   b: 0   Cin: 0   Cout: 0  
    a: 3   b: 3   Cin: 0   Cout: 0  
    a: 3   b: 6   Cin: 0   Cout: 0  
    a: 3   b: 9   Cin: 0   Cout: 0  
    a: 3   b: 12  Cin: 0   Cout: 0  
    a: 3   b: 15  Cin: 0   Cout: 0  
    a: 3   b: 2   Cin: 0   Cout: 1  
    a: 3   b: 5   Cin: 0   Cout: 0  
    a: 3   b: 8   Cin: 0   Cout: 0  
    a: 3   b: 11  Cin: 0   Cout: 0  
    a: 3   b: 14  Cin: 0   Cout: 0  
    a: 3   b: 1   Cin: 0   Cout: 1  
    a: 3   b: 4   Cin: 0   Cout: 0  
    a: 3   b: 7   Cin: 0   Cout: 0  
    a: 3   b: 10  Cin: 0   Cout: 0  
    a: 3   b: 13  Cin: 0   Cout: 0  
    a: 3   b: 0   Cin: 0   Cout: 1  
    a: 3   b: 3   Cin: 0   Cout: 0  
    a: 3   b: 6   Cin: 0   Cout: 0  
    a: 3   b: 9   Cin: 0   Cout: 0  


You can see that the "b" bits are indeed counting by threes: 0, 3, 6, 9,
12, 15 & carry, 2, 5, 8, 11, 14 & carry, 1, 4, 7, 10, 13 & carry, 0, 3,
6, 9, ...

This is my basic model for computation: A register, a program, and a
cycle function. Note that reducing the form on each cycle isn't
necessary, we can run the cycles and just ``reify()`` without reducing
and we get new circuits that define bits in terms of the register values
N cycles in the past.

Simple One-Dimensional Cellular Automaton
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython2

    # Universe
    U = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    
    
    # Register.
    R = {name: Void for name in U}
    
    
    # A program to XOR each bit with its neighbor, wrapping at the edge.
    P = {
        name: xor(name, U[(U.index(name) + 1) % len(U)])
        for name in U
    }
    
    
    def show(reg):
        '''Simple visualization of the register.'''
        print ''.join('.0'[not reg[name]] for name in U)
    
    
    # Set one "bit" in the register.
    R[U[-1]] = Mark
    
    
    # Run through some cycles.
    for _ in range(100):
        show(R)
        R.update(cycle(P, R))


.. parsed-literal::

    ...................................................0
    ..................................................00
    .................................................0.0
    ................................................0000
    ...............................................0...0
    ..............................................00..00
    .............................................0.0.0.0
    ............................................00000000
    ...........................................0.......0
    ..........................................00......00
    .........................................0.0.....0.0
    ........................................0000....0000
    .......................................0...0...0...0
    ......................................00..00..00..00
    .....................................0.0.0.0.0.0.0.0
    ....................................0000000000000000
    ...................................0...............0
    ..................................00..............00
    .................................0.0.............0.0
    ................................0000............0000
    ...............................0...0...........0...0
    ..............................00..00..........00..00
    .............................0.0.0.0.........0.0.0.0
    ............................00000000........00000000
    ...........................0.......0.......0.......0
    ..........................00......00......00......00
    .........................0.0.....0.0.....0.0.....0.0
    ........................0000....0000....0000....0000
    .......................0...0...0...0...0...0...0...0
    ......................00..00..00..00..00..00..00..00
    .....................0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0
    ....................00000000000000000000000000000000
    ...................0...............................0
    ..................00..............................00
    .................0.0.............................0.0
    ................0000............................0000
    ...............0...0...........................0...0
    ..............00..00..........................00..00
    .............0.0.0.0.........................0.0.0.0
    ............00000000........................00000000
    ...........0.......0.......................0.......0
    ..........00......00......................00......00
    .........0.0.....0.0.....................0.0.....0.0
    ........0000....0000....................0000....0000
    .......0...0...0...0...................0...0...0...0
    ......00..00..00..00..................00..00..00..00
    .....0.0.0.0.0.0.0.0.................0.0.0.0.0.0.0.0
    ....0000000000000000................0000000000000000
    ...0...............0...............0...............0
    ..00..............00..............00..............00
    .0.0.............0.0.............0.0.............0.0
    0000............0000............0000............0000
    ...0...........0...0...........0...0...........0....
    ..00..........00..00..........00..00..........00....
    .0.0.........0.0.0.0.........0.0.0.0.........0.0....
    0000........00000000........00000000........0000....
    ...0.......0.......0.......0.......0.......0...0...0
    ..00......00......00......00......00......00..00..00
    .0.0.....0.0.....0.0.....0.0.....0.0.....0.0.0.0.0.0
    0000....0000....0000....0000....0000....000000000000
    ...0...0...0...0...0...0...0...0...0...0............
    ..00..00..00..00..00..00..00..00..00..00............
    .0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0............
    0000000000000000000000000000000000000000............
    .......................................0...........0
    ......................................00..........00
    .....................................0.0.........0.0
    ....................................0000........0000
    ...................................0...0.......0...0
    ..................................00..00......00..00
    .................................0.0.0.0.....0.0.0.0
    ................................00000000....00000000
    ...............................0.......0...0.......0
    ..............................00......00..00......00
    .............................0.0.....0.0.0.0.....0.0
    ............................0000....00000000....0000
    ...........................0...0...0.......0...0...0
    ..........................00..00..00......00..00..00
    .........................0.0.0.0.0.0.....0.0.0.0.0.0
    ........................000000000000....000000000000
    .......................0...........0...0...........0
    ......................00..........00..00..........00
    .....................0.0.........0.0.0.0.........0.0
    ....................0000........00000000........0000
    ...................0...0.......0.......0.......0...0
    ..................00..00......00......00......00..00
    .................0.0.0.0.....0.0.....0.0.....0.0.0.0
    ................00000000....0000....0000....00000000
    ...............0.......0...0...0...0...0...0.......0
    ..............00......00..00..00..00..00..00......00
    .............0.0.....0.0.0.0.0.0.0.0.0.0.0.0.....0.0
    ............0000....000000000000000000000000....0000
    ...........0...0...0.......................0...0...0
    ..........00..00..00......................00..00..00
    .........0.0.0.0.0.0.....................0.0.0.0.0.0
    ........000000000000....................000000000000
    .......0...........0...................0...........0
    ......00..........00..................00..........00
    .....0.0.........0.0.................0.0.........0.0
    ....0000........0000................0000........0000


A More Efficient Implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before building larger "computers" I want to switch to a more efficient
implementation based on a register as a ``set`` of names that are
currently Mark-valued, and a ``set_solve()`` function that evaluates a
form in terms of such a ``set``, and assuming all other names are
Void-valued.

.. code:: ipython2

    def set_solve(form, marks):
        '''
        Given a form and a set of names that are Marks assume all other names
        in the form are Void and reduce to basic value (Mark or Void.)
        '''
        return (
            (Void, Mark)[form in marks]
            if isinstance(form, basestring)
            else _set_solve(form, marks)
        )
    
    
    def _set_solve(form, marks):
        for inner in form:
            if isinstance(inner, basestring):
                if inner in marks: 
                    return Void
                continue
            if not _set_solve(inner, marks): # Mark
                return Void
        return Mark

.. code:: ipython2

    A = F(a, (b, (c,)))
    print A
    print set_solve(A, {a})
    print set_solve(A, {b})
    print set_solve(A, {c})


.. parsed-literal::

    (((c) b) a)
    (())
    ()
    (())


To calculate the new R first collect all the names in R that are not
mentioned in P (and so cannot be set to Void by it) then add the names
evaluated by solving P's expressions with the marks in R.

.. code:: ipython2

    def set_cycle(R, P):
        return R.difference(P).union(
            signal
            for signal, expression in P.iteritems()
            if not set_solve(expression, R)
            )

.. code:: ipython2

    # Universe
    U = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    
    
    # Register.
    R = {U[-1]}
    
    
    # A program to XOR each bit with its neighbor, wrapping at the edge.
    P = {
        name: xor(name, U[(U.index(name) + 1) % len(U)])
        for name in U
    }
    
    
    def show(reg):
        '''Simple visualization of the register.'''
        print ''.join('.0'[name in reg] for name in U)
    
    
    # Run through some cycles.
    for _ in range(100):
        show(R)
        R = set_cycle(R, P)


.. parsed-literal::

    ...................................................0
    ..................................................00
    .................................................0.0
    ................................................0000
    ...............................................0...0
    ..............................................00..00
    .............................................0.0.0.0
    ............................................00000000
    ...........................................0.......0
    ..........................................00......00
    .........................................0.0.....0.0
    ........................................0000....0000
    .......................................0...0...0...0
    ......................................00..00..00..00
    .....................................0.0.0.0.0.0.0.0
    ....................................0000000000000000
    ...................................0...............0
    ..................................00..............00
    .................................0.0.............0.0
    ................................0000............0000
    ...............................0...0...........0...0
    ..............................00..00..........00..00
    .............................0.0.0.0.........0.0.0.0
    ............................00000000........00000000
    ...........................0.......0.......0.......0
    ..........................00......00......00......00
    .........................0.0.....0.0.....0.0.....0.0
    ........................0000....0000....0000....0000
    .......................0...0...0...0...0...0...0...0
    ......................00..00..00..00..00..00..00..00
    .....................0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0
    ....................00000000000000000000000000000000
    ...................0...............................0
    ..................00..............................00
    .................0.0.............................0.0
    ................0000............................0000
    ...............0...0...........................0...0
    ..............00..00..........................00..00
    .............0.0.0.0.........................0.0.0.0
    ............00000000........................00000000
    ...........0.......0.......................0.......0
    ..........00......00......................00......00
    .........0.0.....0.0.....................0.0.....0.0
    ........0000....0000....................0000....0000
    .......0...0...0...0...................0...0...0...0
    ......00..00..00..00..................00..00..00..00
    .....0.0.0.0.0.0.0.0.................0.0.0.0.0.0.0.0
    ....0000000000000000................0000000000000000
    ...0...............0...............0...............0
    ..00..............00..............00..............00
    .0.0.............0.0.............0.0.............0.0
    0000............0000............0000............0000
    ...0...........0...0...........0...0...........0....
    ..00..........00..00..........00..00..........00....
    .0.0.........0.0.0.0.........0.0.0.0.........0.0....
    0000........00000000........00000000........0000....
    ...0.......0.......0.......0.......0.......0...0...0
    ..00......00......00......00......00......00..00..00
    .0.0.....0.0.....0.0.....0.0.....0.0.....0.0.0.0.0.0
    0000....0000....0000....0000....0000....000000000000
    ...0...0...0...0...0...0...0...0...0...0............
    ..00..00..00..00..00..00..00..00..00..00............
    .0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0............
    0000000000000000000000000000000000000000............
    .......................................0...........0
    ......................................00..........00
    .....................................0.0.........0.0
    ....................................0000........0000
    ...................................0...0.......0...0
    ..................................00..00......00..00
    .................................0.0.0.0.....0.0.0.0
    ................................00000000....00000000
    ...............................0.......0...0.......0
    ..............................00......00..00......00
    .............................0.0.....0.0.0.0.....0.0
    ............................0000....00000000....0000
    ...........................0...0...0.......0...0...0
    ..........................00..00..00......00..00..00
    .........................0.0.0.0.0.0.....0.0.0.0.0.0
    ........................000000000000....000000000000
    .......................0...........0...0...........0
    ......................00..........00..00..........00
    .....................0.0.........0.0.0.0.........0.0
    ....................0000........00000000........0000
    ...................0...0.......0.......0.......0...0
    ..................00..00......00......00......00..00
    .................0.0.0.0.....0.0.....0.0.....0.0.0.0
    ................00000000....0000....0000....00000000
    ...............0.......0...0...0...0...0...0.......0
    ..............00......00..00..00..00..00..00......00
    .............0.0.....0.0.0.0.0.0.0.0.0.0.0.0.....0.0
    ............0000....000000000000000000000000....0000
    ...........0...0...0.......................0...0...0
    ..........00..00..00......................00..00..00
    .........0.0.0.0.0.0.....................0.0.0.0.0.0
    ........000000000000....................000000000000
    .......0...........0...................0...........0
    ......00..........00..................00..........00
    .....0.0.........0.0.................0.0.........0.0
    ....0000........0000................0000........0000


.. code:: ipython2

    def set_show_as_int(*names):
        def inner(register):
            i, n = 0, 1
            for name in names:
                if name in register:
                    i += n
                n <<= 1
            return i
        return inner

Each-Way as If... Then...
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython2

    def ifte(predicate, true, false):
        return F(
            ((predicate,), true),
            ( predicate  , false),
        )
    
    
    E = ifte(a, b, c)
    
    
    truth_table(E)


.. parsed-literal::

    (((a) b) (a c))
     a  b  c | Value
    ---------+------
             | 
          () | ()
       ()    | 
       () () | ()
    ()       | 
    ()    () | 
    () ()    | ()
    () () () | ()


If ``a`` is Mark-valued the value of the whole form is that of ``b``,
but if ``a`` is Void-valued the value of the whole form is that of
``c``.

::

    w/ a = ()

    ((( a) b) ( a c))
    (((()) b) (() c))
    ((     b) (()  ))
    ((     b)       )
           b

    w/ a =

    (((a) b) (a c))
    ((( ) b) (  c))
    ((( )  ) (  c))
    (        (  c))
                c

Flip-Flops for Memory
---------------------

.. code:: ipython2

    def flip_flop(q, reset, set_):
        return F(reset, (q, set_))
    
    q, r, s = 'qrs'
    E = flip_flop(q, r, s)
    truth_table(E)


.. parsed-literal::

    ((q s) r)
     q  r  s | Value
    ---------+------
             | 
          () | ()
       ()    | 
       () () | 
    ()       | ()
    ()    () | ()
    () ()    | 
    () () () | 


This is a form that can be used in a circuit to "remember" a value.

::

    w/ r = ()

    ((q s)  r)
    ((q s) ())
    (      ())

    w/ s = (), r = ___

    ((q  s) r)
    ((q ())  )
    ((  ())  )
    (        )

    w/ s = ___, r = ___

    ((q s) r)
    ((q  )  )
      q

If both are Void then the form is just ``q``, if ``r`` is Mark then the
form is Void, otherwise if ``s`` is Mark the form becomes Mark. This is
called a "flip-flop" circuit, and it comprises a simple machine to
remember one bit.

Consider a simple computer:

.. code:: ipython2

    U = q, r, s = 'qrs'
    
    P = {
        q: flip_flop(q, r, s),
        r: Void,
        s: Void,
    }
    
    R = set()  # Initially all signals are off, Void-valued.

.. code:: ipython2

    bools_of = lambda universe: lambda register: (name in register for name in universe)
    
    
    def simple_show(universe):
        B = bools_of(universe)
        def _shower(register):
            print ''.join('.0'[b] for b in B(register))
        return _shower

.. code:: ipython2

    show = simple_show(U)
    show(R)


.. parsed-literal::

    ...


.. code:: ipython2

    def SET(R):
        R |= {s}
        show(R)
        return set_cycle(R, P)
    
    
    def RESET(R):
        R |= {r}
        show(R)
        return set_cycle(R, P)

.. code:: ipython2

    print U
    show(R) ; R = set_cycle(R, P)
    show(R) ; R = set_cycle(R, P)
    show(R) ; R = set_cycle(R, P)
    
    R = SET(R)
    
    show(R) ; R = set_cycle(R, P)
    show(R) ; R = set_cycle(R, P)
    
    R = SET(R)
    
    show(R) ; R = set_cycle(R, P)
    show(R) ; R = set_cycle(R, P)
    
    R = RESET(R)
    
    show(R) ; R = set_cycle(R, P)
    show(R) ; R = set_cycle(R, P)
    
    R = RESET(R)
    
    show(R) ; R = set_cycle(R, P)
    show(R) ; R = set_cycle(R, P)



.. parsed-literal::

    qrs
    ...
    ...
    ...
    ..0
    0..
    0..
    0.0
    0..
    0..
    00.
    ...
    ...
    .0.
    ...
    ...


You can see that ``q`` is stable unless ``s`` or ``r`` set or reset it.

Using Flip-Flops and If...Then...Else... to make RAM
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use the system we have developed so far to build addressable RAM.

.. code:: ipython2

    # Width in bits of the DATA registers.
    WIDTH = 2
    
    # Width in bits of the ADDR registers.
    LENGTH = 3  # Actual length 2**LENGTH

.. code:: ipython2

    P = {}

We'll assume a single ``WRITE`` bit that sets a RAM location determined
by the ``ADDR`` sub-register to the contents of the ``DATA``
sub-register.

.. code:: ipython2

    WRITE = 'WRITE'

.. code:: ipython2

    # Address register.
    ADDR = ['addr_%i' % i for i in range(LENGTH)]
    ADDR




.. parsed-literal::

    ['addr_0', 'addr_1', 'addr_2']



.. code:: ipython2

    # Data register.
    DATA = ['a%i' % i for i in range(WIDTH)]
    DATA




.. parsed-literal::

    ['a0', 'a1']



We can recognize which ``RAM`` location we want to address by using the
`expressions for each row from
above <#Each-Row-can-be-Represented-as-an-Expression>`__ to create a
predicate for each location that depends on the ``ADDR`` bits.

.. code:: ipython2

    def make_ram(program, addrs, data, width, write):
        RAM = []
        for addr, env in enumerate(environments_of_variables(*addrs)):
    
            addr_predicate = F((name,) if env[name] else name for name in env)
    
            predicate = and_(write, addr_predicate)
    
            for bit in range(width):
                ram = 'RAM_%i_%i' % (addr, bit)
                RAM.append(ram)
                program[ram] = simplify(ifte(predicate, data[bit], ram))
    
        return RAM

.. code:: ipython2

    RAM = make_ram(P, ADDR, DATA, WIDTH, WRITE)
    RAM




.. parsed-literal::

    ['RAM_0_0',
     'RAM_0_1',
     'RAM_1_0',
     'RAM_1_1',
     'RAM_2_0',
     'RAM_2_1',
     'RAM_3_0',
     'RAM_3_1',
     'RAM_4_0',
     'RAM_4_1',
     'RAM_5_0',
     'RAM_5_1',
     'RAM_6_0',
     'RAM_6_1',
     'RAM_7_0',
     'RAM_7_1']



.. code:: ipython2

    P




.. parsed-literal::

    {'RAM_0_0': (((((addr_0) (addr_1) (addr_2)) (WRITE)) RAM_0_0) (((addr_0) (addr_1) (addr_2)) (WRITE) a0)),
     'RAM_0_1': (((((addr_0) (addr_1) (addr_2)) (WRITE)) RAM_0_1) (((addr_0) (addr_1) (addr_2)) (WRITE) a1)),
     'RAM_1_0': (((((addr_0) (addr_1) addr_2) (WRITE)) RAM_1_0) (((addr_0) (addr_1) addr_2) (WRITE) a0)),
     'RAM_1_1': (((((addr_0) (addr_1) addr_2) (WRITE)) RAM_1_1) (((addr_0) (addr_1) addr_2) (WRITE) a1)),
     'RAM_2_0': (((((addr_0) (addr_2) addr_1) (WRITE)) RAM_2_0) (((addr_0) (addr_2) addr_1) (WRITE) a0)),
     'RAM_2_1': (((((addr_0) (addr_2) addr_1) (WRITE)) RAM_2_1) (((addr_0) (addr_2) addr_1) (WRITE) a1)),
     'RAM_3_0': (((((addr_0) addr_1 addr_2) (WRITE)) RAM_3_0) (((addr_0) addr_1 addr_2) (WRITE) a0)),
     'RAM_3_1': (((((addr_0) addr_1 addr_2) (WRITE)) RAM_3_1) (((addr_0) addr_1 addr_2) (WRITE) a1)),
     'RAM_4_0': (((((addr_1) (addr_2) addr_0) (WRITE)) RAM_4_0) (((addr_1) (addr_2) addr_0) (WRITE) a0)),
     'RAM_4_1': (((((addr_1) (addr_2) addr_0) (WRITE)) RAM_4_1) (((addr_1) (addr_2) addr_0) (WRITE) a1)),
     'RAM_5_0': (((((addr_1) addr_0 addr_2) (WRITE)) RAM_5_0) (((addr_1) addr_0 addr_2) (WRITE) a0)),
     'RAM_5_1': (((((addr_1) addr_0 addr_2) (WRITE)) RAM_5_1) (((addr_1) addr_0 addr_2) (WRITE) a1)),
     'RAM_6_0': (((((addr_2) addr_0 addr_1) (WRITE)) RAM_6_0) (((addr_2) addr_0 addr_1) (WRITE) a0)),
     'RAM_6_1': (((((addr_2) addr_0 addr_1) (WRITE)) RAM_6_1) (((addr_2) addr_0 addr_1) (WRITE) a1)),
     'RAM_7_0': ((((WRITE) (addr_0 addr_1 addr_2)) RAM_7_0) ((WRITE) (addr_0 addr_1 addr_2) a0)),
     'RAM_7_1': ((((WRITE) (addr_0 addr_1 addr_2)) RAM_7_1) ((WRITE) (addr_0 addr_1 addr_2) a1))}



.. code:: ipython2

    E = P['RAM_0_0']
    E




.. parsed-literal::

    (((((addr_0) (addr_1) (addr_2)) (WRITE)) RAM_0_0) (((addr_0) (addr_1) (addr_2)) (WRITE) a0))



Note that if the ``WRITE`` bit is unset then each ``RAM`` bit is just
set to itself.

.. code:: ipython2

    simplify(E, exclude={'WRITE'})




.. parsed-literal::

    'RAM_0_0'



But if the ``WRITE`` bit is set then each ``RAM`` location still depends
on the -per-location-predicate.

.. code:: ipython2

    simplify(E, with_mark='WRITE')




.. parsed-literal::

    ((((addr_0) (addr_1) (addr_2)) a0) ((addr_0) (addr_1) (addr_2) RAM_0_0))



.. code:: ipython2

    def make_accumulator(program, addrs, data, width, read, alts):
    
        addr_predicates = [
            F((name,) if env[name] else name for name in env)
            for env in environments_of_variables(*addrs)
        ]
    
        for bit in range(width):
            a = data[bit]
            alt = alts[bit]
            foo = Void
            for addr, predicate in enumerate(addr_predicates):
                ram = 'RAM_%i_%i' % (addr, bit)
                foo = (ifte(predicate, ram, foo))
            program[a] = ifte(read, foo, alt)

.. code:: ipython2

    p = {}
    make_accumulator(p, ADDR, DATA, WIDTH, 'READ', DATA)
    p




.. parsed-literal::

    {'a0': ((((((((((((((((((((((addr_0) (addr_1) (addr_2)))) RAM_0_0) ((((addr_0) (addr_1) (addr_2))) (()))) (((addr_0) (addr_1) addr_2))) (((((addr_0) (addr_1) addr_2))) RAM_1_0)) (((addr_0) (addr_2) addr_1))) (((((addr_0) (addr_2) addr_1))) RAM_2_0)) (((addr_0) addr_1 addr_2))) (((((addr_0) addr_1 addr_2))) RAM_3_0)) (((addr_1) (addr_2) addr_0))) (((((addr_1) (addr_2) addr_0))) RAM_4_0)) (((addr_1) addr_0 addr_2))) (((((addr_1) addr_0 addr_2))) RAM_5_0)) (((addr_2) addr_0 addr_1))) (((((addr_2) addr_0 addr_1))) RAM_6_0)) ((addr_0 addr_1 addr_2))) ((((addr_0 addr_1 addr_2))) RAM_7_0)) (READ)) (READ a0)),
     'a1': ((((((((((((((((((((((addr_0) (addr_1) (addr_2)))) RAM_0_1) ((((addr_0) (addr_1) (addr_2))) (()))) (((addr_0) (addr_1) addr_2))) (((((addr_0) (addr_1) addr_2))) RAM_1_1)) (((addr_0) (addr_2) addr_1))) (((((addr_0) (addr_2) addr_1))) RAM_2_1)) (((addr_0) addr_1 addr_2))) (((((addr_0) addr_1 addr_2))) RAM_3_1)) (((addr_1) (addr_2) addr_0))) (((((addr_1) (addr_2) addr_0))) RAM_4_1)) (((addr_1) addr_0 addr_2))) (((((addr_1) addr_0 addr_2))) RAM_5_1)) (((addr_2) addr_0 addr_1))) (((((addr_2) addr_0 addr_1))) RAM_6_1)) ((addr_0 addr_1 addr_2))) ((((addr_0 addr_1 addr_2))) RAM_7_1)) (READ)) (READ a1))}



.. code:: ipython2

    E = p[DATA[0]]
    E




.. parsed-literal::

    ((((((((((((((((((((((addr_0) (addr_1) (addr_2)))) RAM_0_0) ((((addr_0) (addr_1) (addr_2))) (()))) (((addr_0) (addr_1) addr_2))) (((((addr_0) (addr_1) addr_2))) RAM_1_0)) (((addr_0) (addr_2) addr_1))) (((((addr_0) (addr_2) addr_1))) RAM_2_0)) (((addr_0) addr_1 addr_2))) (((((addr_0) addr_1 addr_2))) RAM_3_0)) (((addr_1) (addr_2) addr_0))) (((((addr_1) (addr_2) addr_0))) RAM_4_0)) (((addr_1) addr_0 addr_2))) (((((addr_1) addr_0 addr_2))) RAM_5_0)) (((addr_2) addr_0 addr_1))) (((((addr_2) addr_0 addr_1))) RAM_6_0)) ((addr_0 addr_1 addr_2))) ((((addr_0 addr_1 addr_2))) RAM_7_0)) (READ)) (READ a0))



.. code:: ipython2

    simplify(E, with_mark='READ')




.. parsed-literal::

    ((((((((((((((((((addr_0) (addr_1) (addr_2)) RAM_0_0) ((addr_0) (addr_1) (addr_2))) (addr_0) (addr_1) addr_2) (((addr_0) (addr_1) addr_2) RAM_1_0)) (addr_0) (addr_2) addr_1) (((addr_0) (addr_2) addr_1) RAM_2_0)) (addr_0) addr_1 addr_2) (((addr_0) addr_1 addr_2) RAM_3_0)) (addr_1) (addr_2) addr_0) (((addr_1) (addr_2) addr_0) RAM_4_0)) (addr_1) addr_0 addr_2) (((addr_1) addr_0 addr_2) RAM_5_0)) (addr_2) addr_0 addr_1) (((addr_2) addr_0 addr_1) RAM_6_0)) addr_0 addr_1 addr_2) ((addr_0 addr_1 addr_2) RAM_7_0))



.. code:: ipython2

    simplify(E, {'READ'})




.. parsed-literal::

    'a0'



.. code:: ipython2

    each_way(E, 'a0')




.. parsed-literal::

    ((((((((((((((((((((((addr_0) (addr_1) (addr_2)) RAM_0_0) ((addr_0) (addr_1) (addr_2))) (addr_0) (addr_1) addr_2) (((addr_0) (addr_1) addr_2) RAM_1_0)) (addr_0) (addr_2) addr_1) (((addr_0) (addr_2) addr_1) RAM_2_0)) (addr_0) addr_1 addr_2) (((addr_0) addr_1 addr_2) RAM_3_0)) (addr_1) (addr_2) addr_0) (((addr_1) (addr_2) addr_0) RAM_4_0)) (addr_1) addr_0 addr_2) (((addr_1) addr_0 addr_2) RAM_5_0)) (addr_2) addr_0 addr_1) (((addr_2) addr_0 addr_1) RAM_6_0)) addr_0 addr_1 addr_2) ((addr_0 addr_1 addr_2) RAM_7_0)) (READ)) (READ)) a0) ((((addr_0 addr_1 addr_2) RAM_7_0) (RAM_6_0 addr_0 addr_1 addr_2)) (READ) (a0)))






.. code:: ipython2

    each_way(E, 'WRITE')




.. parsed-literal::

    ((((((((((((((((((((((addr_0) (addr_1) (addr_2)) RAM_0_0) ((addr_0) (addr_1) (addr_2))) (addr_0) (addr_1) addr_2) (((addr_0) (addr_1) addr_2) RAM_1_0)) (addr_0) (addr_2) addr_1) (((addr_0) (addr_2) addr_1) RAM_2_0)) (addr_0) addr_1 addr_2) (((addr_0) addr_1 addr_2) RAM_3_0)) (addr_1) (addr_2) addr_0) (((addr_1) (addr_2) addr_0) RAM_4_0)) (addr_1) addr_0 addr_2) (((addr_1) addr_0 addr_2) RAM_5_0)) (addr_2) addr_0 addr_1) (((addr_2) addr_0 addr_1) RAM_6_0)) addr_0 addr_1 addr_2) ((addr_0 addr_1 addr_2) RAM_7_0)) (READ)) (READ a0)) WRITE) ((((((addr_0 addr_1 addr_2) RAM_7_0) (RAM_6_0 addr_0 addr_1 addr_2)) (READ)) (READ a0)) (WRITE)))



.. code:: ipython2

    each_way(E, 'addr_0')




.. parsed-literal::

    ((((((((((((((addr_1) (addr_2)) RAM_4_0) ((addr_1) (addr_2) RAM_3_0)) (addr_1) addr_2) (((addr_1) addr_2) RAM_5_0)) (addr_2) addr_1) (((addr_2) addr_1) RAM_6_0)) addr_1 addr_2) ((addr_1 addr_2) RAM_7_0)) (READ)) (READ a0)) addr_0) ((((READ) RAM_7_0) (READ a0)) (addr_0)))



.. code:: ipython2

    each_way(E, 'a0')




.. parsed-literal::

    ((((((((((((((((((((((addr_0) (addr_1) (addr_2)) RAM_0_0) ((addr_0) (addr_1) (addr_2))) (addr_0) (addr_1) addr_2) (((addr_0) (addr_1) addr_2) RAM_1_0)) (addr_0) (addr_2) addr_1) (((addr_0) (addr_2) addr_1) RAM_2_0)) (addr_0) addr_1 addr_2) (((addr_0) addr_1 addr_2) RAM_3_0)) (addr_1) (addr_2) addr_0) (((addr_1) (addr_2) addr_0) RAM_4_0)) (addr_1) addr_0 addr_2) (((addr_1) addr_0 addr_2) RAM_5_0)) (addr_2) addr_0 addr_1) (((addr_2) addr_0 addr_1) RAM_6_0)) addr_0 addr_1 addr_2) ((addr_0 addr_1 addr_2) RAM_7_0)) (READ)) (READ)) a0) ((((addr_0 addr_1 addr_2) RAM_7_0) (RAM_6_0 addr_0 addr_1 addr_2)) (READ) (a0)))



.. code:: ipython2

    simplify(each_way(E, 'WRITE'), with_mark='WRITE')




.. parsed-literal::

    (((((addr_0 addr_1 addr_2) RAM_7_0) (RAM_6_0 addr_0 addr_1 addr_2)) (READ)) (READ a0))




, Sorting Networks for routing, more basic functions.

Eventually: Orchestration with Joy.

FIN for now.
============

Appendix: Demonstration of A(AB) = A(B)
---------------------------------------

The rule ``A(AB) = A(B)`` is the powerhouse of LoF.

w/ A = ()

::

      A(AB) = A(B)
    ()(()B) = ()(B)
         () = ()

w/ A =

::

      A(AB) = A(B)
        (B) = (B)

Be aware of the recursive nature of this rule:

::

    A(...(...(A B)))
    A(.A.(...(A B)))
    A(.A.(.A.(A B)))
    A(.A.(.A.(  B)))
    A(.A.(...(  B)))
    A(...(...(  B)))

There is this too:

::

    (A)(...(...(... A B)))
    (A)((A)(...(... A B)))
    (A)((A)((A)(... A B)))
    (A)((A)((A)((A) A B)))
    (A)((A)((A)(( ) A B)))
    (A)((A)(...(( )    )))
    (A)(...(...         ))

Summarized:

::

    (A)(...(...(...  A )))
    (A)(...(...(... () )))
    (A)(...(...         ))

Appendix: Reduce String Expressions by Substitution
---------------------------------------------------

Given a string form of an arithmetic expression (in other words a string
that consists of only balanced pairs of parentheses) we can reduce it to
its Mark/Void value by substitution to a
`fixed-point <https://en.wikipedia.org/wiki/Fixed_point_%28mathematics%29>`__.

.. code:: ipython2

    # Translate the LoF Arithmetic rules to string substitution rules.
    reduce_string = lambda s: (
        s
        .replace('()()', '()')
        .replace('(())', '')
    )

.. code:: ipython2

    def to_fixed_point(initial_value, F, limit=10000):
        '''Do value = F(value) until value == F(value).'''
        
        next_value = F(initial_value)
        
        while next_value != initial_value:
            
            if not limit:  # A safety mechanism.  Bail out after N iterations.
                raise RuntimeError('Reached limit of allowed iterations without finding fixed point.')
            limit -= 1
            
            initial_value = next_value
            next_value = F(initial_value)
        
        return initial_value

.. code:: ipython2

    from operator import add
    
    
    def dyck_language(left, right=0, t='', A='(', B=')', op=add):
        '''Yield balanced pairs of A and B.'''
        if not (left or right):
            yield t
            return
    
        if left > 0:
            for i in dyck_language(left - 1, right + 1, op(t, A), A, B, op):
                yield i
    
        if right > 0:
            for i in dyck_language(left, right - 1, op(t, B), A, B, op):
                yield i

.. code:: ipython2

    for s in dyck_language(5):
        print s, u'⟶', to_fixed_point(s, reduce_string)


.. parsed-literal::

    ((((())))) ⟶ ()
    (((()()))) ⟶ 
    (((())())) ⟶ ()
    (((()))()) ⟶ 
    (((())))() ⟶ ()
    ((()(()))) ⟶ ()
    ((()()())) ⟶ ()
    ((()())()) ⟶ 
    ((()()))() ⟶ ()
    ((())(())) ⟶ ()
    ((())()()) ⟶ 
    ((())())() ⟶ ()
    ((()))(()) ⟶ ()
    ((()))()() ⟶ ()
    (()((()))) ⟶ 
    (()(()())) ⟶ 
    (()(())()) ⟶ 
    (()(()))() ⟶ ()
    (()()(())) ⟶ 
    (()()()()) ⟶ 
    (()()())() ⟶ ()
    (()())(()) ⟶ 
    (()())()() ⟶ ()
    (())((())) ⟶ ()
    (())(()()) ⟶ 
    (())(())() ⟶ ()
    (())()(()) ⟶ ()
    (())()()() ⟶ ()
    ()(((()))) ⟶ ()
    ()((()())) ⟶ ()
    ()((())()) ⟶ ()
    ()((()))() ⟶ ()
    ()(()(())) ⟶ ()
    ()(()()()) ⟶ ()
    ()(()())() ⟶ ()
    ()(())(()) ⟶ ()
    ()(())()() ⟶ ()
    ()()((())) ⟶ ()
    ()()(()()) ⟶ ()
    ()()(())() ⟶ ()
    ()()()(()) ⟶ ()
    ()()()()() ⟶ ()


Appendix: ``void()`` and ``mark()``
-----------------------------------

The ``void()`` and ``mark()`` functions can be defined recursively in
terms of each other. Note that ``void()`` uses ``any()`` while
``mark()`` uses ``all()``. These functions implement a depth-first
search. If we used versions of ``any()`` and ``all()`` that evaluated
their arguments in parallel ``void()`` could return after the ``True``
result while ``mark()`` depends on all terms's results so its runtime
will be bound by term with the greatest runtime.

.. code:: ipython2

    def void(form):
        return any(mark(i) for i in form)
    
    def mark(form):
        return all(void(i) for i in form)
    
    for form in BASE:
        for func in (void, mark):
            print form, 'is', func.__name__, '?', func(form)


.. parsed-literal::

    (()) is void ? True
    (()) is mark ? False
    () is void ? False
    () is mark ? True


Appendix: Duals
---------------

The Void and the Mark are not Boolean true and false. Rather they
correspond to non-existance and existance. When translating a
traditional logical statement into Laws of Form the first thing we must
do is choose which mapping we would like to use: true = Mark or true =
Void. The notation works the same way once the translation is made, so
the only real criteria for choosing is which gives the smaller form.

If you examine the truth tables for the basic forms above in light of
this, you can see that each defines two logical functions depending on
whether you treat Void as true and Mark as false, or Void as false and
Mark as true.

For example, the juxtaposition of two terms next to each other ``a b``
corresponds to **OR** if Mark is true, and to **AND** if Void is true.
Likewise, the form ``((a)(b))`` is **AND** if Mark is true and **OR** if
Void is true.

Consider:

::

    (A ∧ ¬B) ∨ (C ∧ D)

(This reads "(A and not B) or (C and D)" in case you have a hard time
remembering what the symbols mean like I do.)

If we choose Mark to be true then the form is:

::

    ((A) B) ((C)(D))

If we choose Void to be true then the form is:

::

    ((A (B)) (C D))

As I said above, the notation works the same way either way, so once the
translation is made you can forget about the Boolean true/false and just
work with the Laws of Form rules. Logic is containers.

De Morgan Duals
~~~~~~~~~~~~~~~

Consider also the `De Morgan
dual <https://en.wikipedia.org/wiki/De_Morgan%27s_laws>`__ of the
original statement:

::

    ¬((¬A ∨ B) ∧ (¬C ∨ ¬D))

If we choose Mark to be true then the form is:

::

    (( ((A) B) ((C)(D)) ))

The outer pair of containers can be deleted leaving the same form as
above:

::

    ((A) B) ((C)(D))

Likewise, if we choose Void to be true then the form is:

::

    ((((A)) (B)) (((C)) ((D))))

Again, A((B)) => AB reduces this form to the same one above:

::

    ((A (B)) (C D))

In the Laws of Form there are no De Morgan Dual statements. If you
translate a logic statement and its dual into Laws of Form notation they
both reduce to the same form.

To me this is a clear indication that the Laws of Form are superior to
the traditional notation involving ``¬ ∨ ∧``, etc. LoF replaces all the
symbols with just names and boundaries. The Laws of Form are not
dualistic, they work directly in terms of existance and non-existance.
Duality only comes in when you interpret the forms as Boolean statements
and have to pick a mapping.

Misc. Junk
==========

.. code:: ipython2

    from collections import Counter
    
    histo = Counter(yield_variables_of(cout))
    histo




.. parsed-literal::

    Counter({'Cin': 1,
             'a0': 3,
             'a1': 3,
             'a2': 3,
             'a3': 3,
             'b0': 3,
             'b1': 3,
             'b2': 3,
             'b3': 3})



.. code:: ipython2

    #import pprint as pp
    
    
    #E = cout
    #for var, count in histo.most_common():
    #    print 'stan', var, count
    #    E = to_fixed_point(simplify(standardize(E, var)), simplify)
    #    print len(str(E))
    #    pp.pprint(dict(Counter(yield_variables_of(E))))
    #    print '------'

Rather than manually calling ``standard_form()`` let's define a function
that reduces a form to a (hopefully) smaller equivalent form by going
through all the variables in the form and using ``standard_form()`` with
each. Along with clean and unwrap we can drive an expression to a fixed
point.

.. code:: ipython2

    def STAN(form):
        for var, _ in Counter(yield_variables_of(form)).most_common():
            form = to_fixed_point(simplify(standardize(form, var)), simplify)
        return form

.. code:: ipython2

    sum0, cout = full_bit_adder('a0', 'b0', 'Cin')
    sum1, cout = full_bit_adder('a1', 'b1', cout)
    sum2, cout = full_bit_adder('a2', 'b2', cout)
    sum3, cout = full_bit_adder('a3', 'b3', cout)
    
    map(len, map(str, (sum0, sum1, sum2, sum3, cout)))




.. parsed-literal::

    [59, 135, 211, 287, 159]



.. code:: ipython2

    sum0, cout = full_bit_adder('a0', 'b0', 'Cin')
    sum0 = to_fixed_point(sum0, STAN)
    cout = to_fixed_point(cout, STAN)
    
    sum1, cout = full_bit_adder('a1', 'b1', cout)
    sum1 = to_fixed_point(sum1, STAN)
    cout = to_fixed_point(cout, STAN)
    
    sum2, cout = full_bit_adder('a2', 'b2', cout)
    sum2 = to_fixed_point(sum2, STAN)
    cout = to_fixed_point(cout, STAN)
    
    sum3, cout = full_bit_adder('a3', 'b3', cout)
    sum3 = to_fixed_point(sum3, STAN)
    cout = to_fixed_point(cout, STAN)
    
    
    map(len, map(str, (sum0, sum1, sum2, sum3, cout)))


::


    ---------------------------------------------------------------------------

    NameError                                 Traceback (most recent call last)

    <ipython-input-140-34665b9e1d61> in <module>()
          1 sum0, cout = full_bit_adder('a0', 'b0', 'Cin')
    ----> 2 sum0 = to_fixed_point(sum0, STAN)
          3 cout = to_fixed_point(cout, STAN)
          4 
          5 sum1, cout = full_bit_adder('a1', 'b1', cout)


    <ipython-input-132-bf263ba512a2> in to_fixed_point(initial_value, F, limit)
          2     '''Do value = F(value) until value == F(value).'''
          3 
    ----> 4     next_value = F(initial_value)
          5 
          6     while next_value != initial_value:


    <ipython-input-138-99179e550c4a> in STAN(form)
          1 def STAN(form):
          2     for var, _ in Counter(yield_variables_of(form)).most_common():
    ----> 3         form = to_fixed_point(simplify(standardize(form, var)), simplify)
          4     return form


    NameError: global name 'standardize' is not defined


It would be useful and fun to write a simple search algorithm that tried
different ways to reduce a form to see if it could find particulaly
compact versions.

Let's generate the expressions for the next two output bits, and the
carry bit.

The ``sum3`` bit expression is pretty big.

.. code:: ipython2

    sum3

But it's only about 1/9th of size of the previous version (which was
9261.)

.. code:: ipython2

    len(str(sum3))







Let's simplify the first one manually just for fun:

::

    (((((())) (())) ((()))))
      ((    )     ) (    )
                    (    )

Sure enough, it reduces to Mark after just a few applications of the
rule ``(()) = __`` (the underscores indicates the absence of any value,
aka Void.) We could also just delete variables that are Void in the
original expression:

::

    ((((a)b)(c)))
      (( ) )( )
            ( )


.. code:: ipython2

    C = F((a, b))
    for form in (A, B, C):
        arth = reify(form, env)
        print form, u'⟶', arth, u'⟶', value_of(arth)




.. code:: ipython2

    print A
    Aa = simplify(A, {a})
    print a, Aa
    Aab = simplify(Aa, {b})
    print a, b, Aab
    Aabc = simplify(Aab, {c})
    print a, b, c, Aabc

.. code:: ipython2

    print a, Aa
    Aab = simplify(Aa, with_mark=b)
    print a, F(b), Aab

.. code:: ipython2

    print a, Aa
    Aac = simplify(Aa, with_mark=c)
    print a, F(c), Aac

.. code:: ipython2

    print a, Aa
    Aac = simplify(Aa, {c})
    print a, c, Aac





.. code:: ipython2

    from collections import Counter
    
    histo = Counter(yield_variables_of(sum7))
    histo

.. code:: ipython2

    len(str(sum7))

Immediately we can just call ``simplify()`` until it stops shinking the
expression.

.. code:: ipython2

    s7 = simplify(sum7)
    len(str(s7))

.. code:: ipython2

    s7 = simplify(s7)
    len(str(s7))

Once was enough (we should consider adding a call to ``simplify()`` in
the ``full_bit_adder()`` function.)

Let's try using ``each_way()`` with the most common names in the form.

.. code:: ipython2

    s7 = simplify(each_way(s7, 'a0'))
    len(str(s7))

.. code:: ipython2

    s7 = simplify(s7)
    len(str(s7))

.. code:: ipython2

    Counter(yield_variables_of(s7))

.. code:: ipython2

    s7 = sum7
    
    #for name, count in histo.most_common():
    #    s7 = simplify(each_way(s7, name))
    #    print len(str(s7))



.. code:: ipython2

    def super_simple(form):
        return to_fixed_point(form, simplify)

.. code:: ipython2

    len(str(sum7))
    s7 = super_simple(sum7)
    len(str(s7))

.. code:: ipython2

    s7 = sum7
    
    #for name, count in histo.most_common():
    #    s7 = super_simple(each_way(s7, name))
    #    print len(str(s7))





.. code:: ipython2

    print ' '.join(name[:2] for name in sorted(R))
    for _ in range(20):
        print format_env(R), '=', b_register(R)
        R.update(cycle(P, R))




