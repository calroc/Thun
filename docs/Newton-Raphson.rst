
`Newton's method <https://en.wikipedia.org/wiki/Newton%27s_method>`__
=====================================================================

.. code:: ipython2

    from notebook_preamble import J, V, define

Cf. `"Why Functional Programming Matters" by John
Hughes <https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf>`__

:math:`a_{i+1} = \frac{(a_i+\frac{n}{a_i})}{2}`

Let's define a function that computes the above equation:

::

         n a Q
    ---------------
       (a+n/a)/2

    n a tuck / + 2 /
    a n a    / + 2 /
    a n/a      + 2 /
    a+n/a        2 /
    (a+n/a)/2

We want it to leave n but replace a, so we execute it with ``unary``:

::

    Q == [tuck / + 2 /] unary

.. code:: ipython2

    define('Q == [tuck / + 2 /] unary')

And a function to compute the error:

::

    n a sqr - abs
    |n-a**2|

This should be ``nullary`` so as to leave both n and a on the stack
below the error.

::

    err == [sqr - abs] nullary

.. code:: ipython2

    define('err == [sqr - abs] nullary')

Now we can define a recursive program that expects a number ``n``, an
initial estimate ``a``, and an epsilon value ``ε``, and that leaves on
the stack the square root of ``n`` to within the precision of the
epsilon value. (Later on we'll refine it to generate the initial
estimate and hard-code an epsilon value.)

::

    n a ε square-root
    -----------------
          √n

If we apply the two functions ``Q`` and ``err`` defined above we get the
next approximation and the error on the stack below the epsilon.

::

    n a ε [Q err] dip
    n a Q err ε 
    n a'  err ε 
    n a' e    ε

Let's define the recursive function from here. Start with ``ifte``; the
predicate and the base case behavior are obvious:

::

    n a' e ε [<] [popop popd] [J] ifte

Base-case

::

    n a' e ε popop popd
    n a'           popd
      a'

The recursive branch is pretty easy. Discard the error and recur.

::

    w/ K == [<] [popop popd] [J] ifte

    n a' e ε J
    n a' e ε popd [Q err] dip [K] i
    n a'   ε      [Q err] dip [K] i
    n a' Q err ε              [K] i
    n a''  e   ε               K

This fragment alone is pretty useful.

.. code:: ipython2

    define('K == [<] [popop popd] [popd [Q err] dip] primrec')

.. code:: ipython2

    J('25 10 0.001 dup K')


.. parsed-literal::

    5.000000232305737


.. code:: ipython2

    J('25 10 0.000001 dup K')


.. parsed-literal::

    5.000000000000005


So now all we need is a way to generate an initial approximation and an
epsilon value:

::

    square-root == dup 3 / 0.000001 dup K

.. code:: ipython2

    define('square-root == dup 3 / 0.000001 dup K')

.. code:: ipython2

    J('36 square-root')


.. parsed-literal::

    6.000000000000007


.. code:: ipython2

    J('4895048365636 square-root')


.. parsed-literal::

    2212475.6192184356


.. code:: ipython2

    2212475.6192184356 * 2212475.6192184356




.. parsed-literal::

    4895048365636.0


