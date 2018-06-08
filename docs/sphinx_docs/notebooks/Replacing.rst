
Replacing Functions in the Dictionary
=====================================

For now, there is no way to define new functions from within the Joy
language. All functions (and the interpreter) all accept and return a
dictionary parameter (in addition to the stack and expression) so that
we can implement e.g. a function that adds new functions to the
dictionary. However, there's no function that does that. Adding a new
function to the dictionary is a meta-interpreter action, you have to do
it in Python, not Joy.

.. code:: ipython2

    from notebook_preamble import D, J, V

A long trace
------------

.. code:: ipython2

    V('[23 18] average')


.. parsed-literal::

                                      . [23 18] average
                              [23 18] . average
                              [23 18] . [sum 1.0 *] [size] cleave /
                  [23 18] [sum 1.0 *] . [size] cleave /
           [23 18] [sum 1.0 *] [size] . cleave /
           [23 18] [sum 1.0 *] [size] . [i] app2 [popd] dip /
       [23 18] [sum 1.0 *] [size] [i] . app2 [popd] dip /
    [23 18] [[sum 1.0 *] [23 18]] [i] . infra first [[size] [23 18]] [i] infra first [popd] dip /
                  [23 18] [sum 1.0 *] . i [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                              [23 18] . sum 1.0 * [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                                   41 . 1.0 * [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                               41 1.0 . * [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                                 41.0 . [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                       41.0 [[23 18]] . swaack first [[size] [23 18]] [i] infra first [popd] dip /
                       [23 18] [41.0] . first [[size] [23 18]] [i] infra first [popd] dip /
                         [23 18] 41.0 . [[size] [23 18]] [i] infra first [popd] dip /
        [23 18] 41.0 [[size] [23 18]] . [i] infra first [popd] dip /
    [23 18] 41.0 [[size] [23 18]] [i] . infra first [popd] dip /
                       [23 18] [size] . i [41.0 [23 18]] swaack first [popd] dip /
                              [23 18] . size [41.0 [23 18]] swaack first [popd] dip /
                              [23 18] . 0 swap [pop ++] step [41.0 [23 18]] swaack first [popd] dip /
                            [23 18] 0 . swap [pop ++] step [41.0 [23 18]] swaack first [popd] dip /
                            0 [23 18] . [pop ++] step [41.0 [23 18]] swaack first [popd] dip /
                   0 [23 18] [pop ++] . step [41.0 [23 18]] swaack first [popd] dip /
                        0 23 [pop ++] . i [18] [pop ++] step [41.0 [23 18]] swaack first [popd] dip /
                                 0 23 . pop ++ [18] [pop ++] step [41.0 [23 18]] swaack first [popd] dip /
                                    0 . ++ [18] [pop ++] step [41.0 [23 18]] swaack first [popd] dip /
                                    1 . [18] [pop ++] step [41.0 [23 18]] swaack first [popd] dip /
                               1 [18] . [pop ++] step [41.0 [23 18]] swaack first [popd] dip /
                      1 [18] [pop ++] . step [41.0 [23 18]] swaack first [popd] dip /
                        1 18 [pop ++] . i [41.0 [23 18]] swaack first [popd] dip /
                                 1 18 . pop ++ [41.0 [23 18]] swaack first [popd] dip /
                                    1 . ++ [41.0 [23 18]] swaack first [popd] dip /
                                    2 . [41.0 [23 18]] swaack first [popd] dip /
                     2 [41.0 [23 18]] . swaack first [popd] dip /
                     [23 18] 41.0 [2] . first [popd] dip /
                       [23 18] 41.0 2 . [popd] dip /
                [23 18] 41.0 2 [popd] . dip /
                         [23 18] 41.0 . popd 2 /
                                 41.0 . 2 /
                               41.0 2 . /
                                 20.5 . 


Replacing ``size`` with a Python version
----------------------------------------

Both ``sum`` and ``size`` each convert a sequence to a single value.

::

     sum == 0 swap [+] step
    size == 0 swap [pop ++] step

An efficient ``sum`` function is already in the library. But for
``size`` we can use a “compiled” version hand-written in Python to speed
up evaluation and make the trace more readable.

.. code:: ipython2

    from joy.library import SimpleFunctionWrapper
    from joy.utils.stack import iter_stack
    
    
    @SimpleFunctionWrapper
    def size(stack):
        '''Return the size of the sequence on the stack.'''
        sequence, stack = stack
        n = 0
        for _ in iter_stack(sequence):
            n += 1
        return n, stack

Now we replace the old version in the dictionary with the new version,
and re-evaluate the expression.

.. code:: ipython2

    D['size'] = size

A shorter trace
---------------

You can see that ``size`` now executes in a single step.

.. code:: ipython2

    V('[23 18] average')


.. parsed-literal::

                                      . [23 18] average
                              [23 18] . average
                              [23 18] . [sum 1.0 *] [size] cleave /
                  [23 18] [sum 1.0 *] . [size] cleave /
           [23 18] [sum 1.0 *] [size] . cleave /
           [23 18] [sum 1.0 *] [size] . [i] app2 [popd] dip /
       [23 18] [sum 1.0 *] [size] [i] . app2 [popd] dip /
    [23 18] [[sum 1.0 *] [23 18]] [i] . infra first [[size] [23 18]] [i] infra first [popd] dip /
                  [23 18] [sum 1.0 *] . i [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                              [23 18] . sum 1.0 * [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                                   41 . 1.0 * [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                               41 1.0 . * [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                                 41.0 . [[23 18]] swaack first [[size] [23 18]] [i] infra first [popd] dip /
                       41.0 [[23 18]] . swaack first [[size] [23 18]] [i] infra first [popd] dip /
                       [23 18] [41.0] . first [[size] [23 18]] [i] infra first [popd] dip /
                         [23 18] 41.0 . [[size] [23 18]] [i] infra first [popd] dip /
        [23 18] 41.0 [[size] [23 18]] . [i] infra first [popd] dip /
    [23 18] 41.0 [[size] [23 18]] [i] . infra first [popd] dip /
                       [23 18] [size] . i [41.0 [23 18]] swaack first [popd] dip /
                              [23 18] . size [41.0 [23 18]] swaack first [popd] dip /
                                    2 . [41.0 [23 18]] swaack first [popd] dip /
                     2 [41.0 [23 18]] . swaack first [popd] dip /
                     [23 18] 41.0 [2] . first [popd] dip /
                       [23 18] 41.0 2 . [popd] dip /
                [23 18] 41.0 2 [popd] . dip /
                         [23 18] 41.0 . popd 2 /
                                 41.0 . 2 /
                               41.0 2 . /
                                 20.5 . 

