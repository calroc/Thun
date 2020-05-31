Advent of Code 2017
===================

December 6th
------------

::

    [0 2 7 0] dup max

.. code:: ipython2

    from notebook_preamble import D, J, V, define

.. code:: ipython2

    J('[0 2 7 0] dup max')


.. parsed-literal::

    [0 2 7 0] 7


.. code:: ipython2

    from joy.library import SimpleFunctionWrapper
    from joy.utils.stack import list_to_stack
    
    
    @SimpleFunctionWrapper
    def index_of(stack):
        '''Given a sequence and a item, return the index of the item, or -1 if not found.
    
        E.g.:
    
           [a b c] a index_of
        ------------------------
                   0
    
           [a b c] d index_of
        ------------------------
                -1
    
        '''
        item, (sequence, stack) = stack
        i = 0
        while sequence:
            term, sequence = sequence
            if term == item:
                break
            i += 1
        else:
            i = -1
        return i, stack
    
    
    D['index_of'] = index_of

.. code:: ipython2

    J('[0 2 7 0] 7 index_of')


.. parsed-literal::

    2


.. code:: ipython2

    J('[0 2 7 0] 23 index_of')


.. parsed-literal::

    -1


Starting at ``index`` distribute ``count`` "blocks" to the "banks" in
the sequence.

::

    [...] count index distribute
    ----------------------------
               [...]

This seems like it would be a PITA to implement in Joypy...

.. code:: ipython2

    from joy.utils.stack import iter_stack, list_to_stack
    
    
    @SimpleFunctionWrapper
    def distribute(stack):
        '''Starting at index+1 distribute count "blocks" to the "banks" in the sequence.
    
        [...] count index distribute
        ----------------------------
                   [...]
    
        '''
        index, (count, (sequence, stack)) = stack
        assert count >= 0
        cheat = list(iter_stack(sequence))
        n = len(cheat)
        assert index < n
        cheat[index] = 0
        while count:
            index += 1
            index %= n
            cheat[index] += 1
            count -= 1
        return list_to_stack(cheat), stack
    
    
    D['distribute'] = distribute

.. code:: ipython2

    J('[0 2 7 0] dup max [index_of] nullary distribute')


.. parsed-literal::

    [2 4 1 2]


.. code:: ipython2

    J('[2 4 1 2] dup max [index_of] nullary distribute')


.. parsed-literal::

    [3 1 2 3]


.. code:: ipython2

    J('[3 1 2 3] dup max [index_of] nullary distribute')


.. parsed-literal::

    [0 2 3 4]


.. code:: ipython2

    J('[0 2 3 4] dup max [index_of] nullary distribute')


.. parsed-literal::

    [1 3 4 1]


.. code:: ipython2

    J('[1 3 4 1] dup max [index_of] nullary distribute')


.. parsed-literal::

    [2 4 1 2]


Recalling "Generator Programs"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    [a F] x
    [a F] a F 

    [a F] a swap [C] dip rest cons
    a   [a F]    [C] dip rest cons
    a C [a F]            rest cons
    a C   [F]                 cons

    w/ C == dup G

    a dup G [F] cons
    a a   G [F] cons

    w/ G == dup max [index_of] nullary distribute

.. code:: ipython2

    define('direco == dip rest cons')

.. code:: ipython2

    define('G == [direco] cons [swap] swoncat cons')

.. code:: ipython2

    define('make_distributor == [dup dup max [index_of] nullary distribute] G')

.. code:: ipython2

    J('[0 2 7 0] make_distributor 6 [x] times pop')


.. parsed-literal::

    [0 2 7 0] [2 4 1 2] [3 1 2 3] [0 2 3 4] [1 3 4 1] [2 4 1 2]


A function to drive a generator and count how many states before a repeat.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First draft:

::

    [] [GEN] x [pop index_of 0 >=] [pop size --] [[swons] dip x] primrec

(?)

::

    []       [GEN] x [pop index_of 0 >=] [pop size --] [[swons] dip x] primrec
    [] [...] [GEN]   [pop index_of 0 >=] [pop size --] [[swons] dip x] primrec
    [] [...] [GEN]    pop index_of 0 >=
    [] [...]              index_of 0 >=
                                -1 0 >=
                                 False

Base case

::

    [] [...] [GEN] [pop index_of 0 >=] [pop size --] [[swons] dip x] primrec
    [] [...] [GEN]                      pop size --
    [] [...]                                size --
    [] [...]                                size --

A mistake, ``popop`` and no need for ``--``

::

    [] [...] [GEN] popop size
    []                   size
    n

Recursive case

::

    [] [...] [GEN] [pop index_of 0 >=] [popop size] [[swons] dip x] primrec
    [] [...] [GEN]                                   [swons] dip x  F
    [] [...] swons [GEN]                                         x  F
    [[...]]        [GEN]                                         x  F
    [[...]] [...]  [GEN]                                            F

    [[...]] [...] [GEN] F

What have we learned?

::

    F == [pop index_of 0 >=] [popop size] [[swons] dip x] primrec

.. code:: ipython2

    define('count_states == [] swap x [pop index_of 0 >=] [popop size] [[swons] dip x] primrec')

.. code:: ipython2

    define('AoC2017.6 == make_distributor count_states')

.. code:: ipython2

    J('[0 2 7 0] AoC2017.6')


.. parsed-literal::

    5


.. code:: ipython2

    J('[1 1 1] AoC2017.6')


.. parsed-literal::

    4


.. code:: ipython2

    J('[8 0 0 0 0 0] AoC2017.6')


.. parsed-literal::

    15

