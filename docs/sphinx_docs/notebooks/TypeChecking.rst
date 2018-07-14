
Type Checking
=============

.. code:: ipython2

    import logging, sys
    
    logging.basicConfig(
      format='%(message)s',
      stream=sys.stdout,
      level=logging.INFO,
      )

.. code:: ipython2

    from joy.utils.polytypes import (
        doc_from_stack_effect, 
        infer,
        reify,
        unify,
        FUNCTIONS,
        JoyTypeError,
    )

.. code:: ipython2

    D = FUNCTIONS.copy()
    del D['product']
    globals().update(D)

An Example
----------

.. code:: ipython2

    fi, fo = infer(pop, swap, rolldown, rrest, ccons)[0]


.. parsed-literal::

     25 (--) ∘ pop swap rolldown rrest ccons
     28 (a1 --) ∘ swap rolldown rrest ccons
     31 (a3 a2 a1 -- a2 a3) ∘ rolldown rrest ccons
     34 (a4 a3 a2 a1 -- a2 a3 a4) ∘ rrest ccons
     37 ([a4 a5 ...1] a3 a2 a1 -- a2 a3 [...1]) ∘ ccons
     40 ([a4 a5 ...1] a3 a2 a1 -- [a2 a3 ...1]) ∘ 


.. code:: ipython2

    print doc_from_stack_effect(fi, fo)


.. parsed-literal::

    ([a4 a5 ...1] a3 a2 a1 -- [a2 a3 ...1])


.. code:: ipython2

    from joy.parser import text_to_expression
    from joy.utils.stack import stack_to_string


.. code:: ipython2

    e = text_to_expression('0 1 2 [3 4]')  # reverse order
    print stack_to_string(e)


.. parsed-literal::

    [3 4] 2 1 0


.. code:: ipython2

    u = unify(e, fi)[0]
    u




.. parsed-literal::

    {a1: 0, a2: 1, a3: 2, a4: 3, a5: 4, s2: (), s1: ()}



.. code:: ipython2

    g = reify(u, (fi, fo))
    print doc_from_stack_effect(*g)


.. parsed-literal::

    (... [3 4 ] 2 1 0 -- ... [1 2 ])


Unification Works "in Reverse"
------------------------------

.. code:: ipython2

    e = text_to_expression('[2 3]')

.. code:: ipython2

    u = unify(e, fo)[0]  # output side, not input side
    u




.. parsed-literal::

    {a2: 2, a3: 3, s2: (), s1: ()}



.. code:: ipython2

    g = reify(u, (fi, fo))
    print doc_from_stack_effect(*g)


.. parsed-literal::

    (... [a4 a5 ] 3 2 a1 -- ... [2 3 ])


Failing a Check
---------------

.. code:: ipython2

    fi, fo = infer(dup, mul)[0]


.. parsed-literal::

     25 (--) ∘ dup mul
     28 (a1 -- a1 a1) ∘ mul
     31 (f1 -- f2) ∘ 
     31 (i1 -- i2) ∘ 


.. code:: ipython2

    e = text_to_expression('"two"')
    print stack_to_string(e)


.. parsed-literal::

    'two'


.. code:: ipython2

    try:
        unify(e, fi)
    except JoyTypeError, err:
        print err


.. parsed-literal::

    Cannot unify 'two' and f1.

