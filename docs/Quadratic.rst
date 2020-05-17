.. code:: ipython2

    from notebook_preamble import J, V, define

`Quadratic formula <https://en.wikipedia.org/wiki/Quadratic_formula>`__
=======================================================================

Cf.
`jp-quadratic.html <http://www.kevinalbrecht.com/code/joy-mirror/jp-quadratic.html>`__

::

      -b ± sqrt(b^2 - 4 * a * c)
   --------------------------------
               2 * a

:math:`\frac{-b \pm \sqrt{b^2 - 4ac}}{2a}`

Write a straightforward program with variable names.
----------------------------------------------------

This math translates to Joy code in a straightforward manner. We are
going to use named variables to keep track of the arguments, then write
a definition without them.

``-b``
~~~~~~

::

   b neg

``sqrt(b^2 - 4 * a * c)``
~~~~~~~~~~~~~~~~~~~~~~~~~

::

   b sqr 4 a c * * - sqrt

``/2a``
~~~~~~~

::

   a 2 * /

``±``
~~~~~

There is a function ``pm`` that accepts two values on the stack and
replaces them with their sum and difference.

::

   pm == [+] [-] cleave popdd

Putting Them Together
~~~~~~~~~~~~~~~~~~~~~

::

   b neg b sqr 4 a c * * - sqrt pm a 2 * [/] cons app2

We use ``app2`` to compute both roots by using a quoted program
``[2a /]`` built with ``cons``.

Derive a definition.
--------------------

Working backwards we use ``dip`` and ``dipd`` to extract the code from
the variables:

::

   b             neg  b      sqr 4 a c   * * - sqrt pm a    2 * [/] cons app2
   b            [neg] dupdip sqr 4 a c   * * - sqrt pm a    2 * [/] cons app2
   b a c       [[neg] dupdip sqr 4] dipd * * - sqrt pm a    2 * [/] cons app2
   b a c a    [[[neg] dupdip sqr 4] dipd * * - sqrt pm] dip 2 * [/] cons app2
   b a c over [[[neg] dupdip sqr 4] dipd * * - sqrt pm] dip 2 * [/] cons app2

The three arguments are to the left, so we can “chop off” everything to
the right and say it’s the definition of the ``quadratic`` function:

.. code:: ipython2

    define('quadratic == over [[[neg] dupdip sqr 4] dipd * * - sqrt pm] dip 2 * [/] cons app2')

Let’s try it out:

.. code:: ipython2

    J('3 1 1 quadratic')


.. parsed-literal::

    -0.3819660112501051 -2.618033988749895


If you look at the Joy evaluation trace you can see that the first few
lines are the ``dip`` and ``dipd`` combinators building the main program
by incorporating the values on the stack. Then that program runs and you
get the results. This is pretty typical of Joy code.

.. code:: ipython2

    V('-5 1 4 quadratic')


.. parsed-literal::

                                                       . -5 1 4 quadratic
                                                    -5 . 1 4 quadratic
                                                  -5 1 . 4 quadratic
                                                -5 1 4 . quadratic
                                                -5 1 4 . over [[[neg] dupdip sqr 4] dipd * * - sqrt pm] dip 2 * [/] cons app2
                                              -5 1 4 1 . [[[neg] dupdip sqr 4] dipd * * - sqrt pm] dip 2 * [/] cons app2
    -5 1 4 1 [[[neg] dupdip sqr 4] dipd * * - sqrt pm] . dip 2 * [/] cons app2
                                                -5 1 4 . [[neg] dupdip sqr 4] dipd * * - sqrt pm 1 2 * [/] cons app2
                           -5 1 4 [[neg] dupdip sqr 4] . dipd * * - sqrt pm 1 2 * [/] cons app2
                                                    -5 . [neg] dupdip sqr 4 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                              -5 [neg] . dupdip sqr 4 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                                    -5 . neg -5 sqr 4 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                                     5 . -5 sqr 4 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                                  5 -5 . sqr 4 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                                  5 -5 . dup mul 4 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                               5 -5 -5 . mul 4 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                                  5 25 . 4 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                                5 25 4 . 1 4 * * - sqrt pm 1 2 * [/] cons app2
                                              5 25 4 1 . 4 * * - sqrt pm 1 2 * [/] cons app2
                                            5 25 4 1 4 . * * - sqrt pm 1 2 * [/] cons app2
                                              5 25 4 4 . * - sqrt pm 1 2 * [/] cons app2
                                               5 25 16 . - sqrt pm 1 2 * [/] cons app2
                                                   5 9 . sqrt pm 1 2 * [/] cons app2
                                                 5 3.0 . pm 1 2 * [/] cons app2
                                               8.0 2.0 . 1 2 * [/] cons app2
                                             8.0 2.0 1 . 2 * [/] cons app2
                                           8.0 2.0 1 2 . * [/] cons app2
                                             8.0 2.0 2 . [/] cons app2
                                         8.0 2.0 2 [/] . cons app2
                                         8.0 2.0 [2 /] . app2
                                           [8.0] [2 /] . infra first [2.0] [2 /] infra first
                                                   8.0 . 2 / [] swaack first [2.0] [2 /] infra first
                                                 8.0 2 . / [] swaack first [2.0] [2 /] infra first
                                                   4.0 . [] swaack first [2.0] [2 /] infra first
                                                4.0 [] . swaack first [2.0] [2 /] infra first
                                                 [4.0] . first [2.0] [2 /] infra first
                                                   4.0 . [2.0] [2 /] infra first
                                             4.0 [2.0] . [2 /] infra first
                                       4.0 [2.0] [2 /] . infra first
                                                   2.0 . 2 / [4.0] swaack first
                                                 2.0 2 . / [4.0] swaack first
                                                   1.0 . [4.0] swaack first
                                             1.0 [4.0] . swaack first
                                             4.0 [1.0] . first
                                               4.0 1.0 . 


