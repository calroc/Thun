Joy Example Code
================

A brief example of Joy programming from the `Advent of Code 2017,
December 1st puzzle <https://adventofcode.com/2017/day/1>`__.

    Given a sequence of digits (your puzzle input) and find the sum of
    all digits that match the next digit in the list. The list is
    circular, so the digit after the last digit is the first digit in
    the list.

For example:

-  1122 produces a sum of 3 (1 + 2) because the first digit (1) matches
   the second digit and the third digit (2) matches the fourth digit.
-  1111 produces 4 because each digit (all 1) matches the next.
-  1234 produces 0 because no digit matches the next.
-  91212129 produces 9 because the only digit that matches the next one
   is the last digit, 9.

.. code:: ipython3

    from notebook_preamble import J, V, define

I'll assume the input is a Joy sequence of integers (as opposed to a
string or something else.)

We might proceed by creating a word that makes a copy of the sequence
with the first item moved to the last, and zips it with the original to
make a list of pairs, and a another word that adds (one of) each pair to
a total if the pair matches.

::

    AoC2017.1 == pair_up total_matches

Let's derive ``pair_up``:

::

         [a b c] pair_up
    -------------------------
       [[a b] [b c] [c a]]

Straightforward (although the order of each pair is reversed, due to the
way ``zip`` works, but it doesn't matter for this program):

::

    [a b c] dup
    [a b c] [a b c] uncons swap
    [a b c] [b c] a unit concat
    [a b c] [b c a] zip
    [[b a] [c b] [a c]]

.. code:: ipython3

    define('pair_up dup uncons swap unit concat zip')

.. code:: ipython3

    J('[1 2 3] pair_up')


.. parsed-literal::

    [[2 1] [3 2] [1 3]]


.. code:: ipython3

    J('[1 2 2 3] pair_up')


.. parsed-literal::

    [[2 1] [2 2] [3 2] [1 3]]


Now we need to derive ``total_matches``. It will be a ``step`` function:

::

    total_matches == 0 swap [F] step

Where ``F`` will have the pair to work with, and it will basically be a
``branch`` or ``ifte``.

::

    total [n m] F

It will probably be easier to write if we dequote the pair:

::

       total [n m] i F′
    ----------------------
         total n m F′

Now ``F′`` becomes just:

::

    total n m [=] [pop +] [popop] ifte

So:

::

    F == i [=] [pop +] [popop] ifte

And thus:

::

    total_matches == 0 swap [i [=] [pop +] [popop] ifte] step

.. code:: ipython3

    define('total_matches 0 swap [i [=] [pop +] [popop] ifte] step')

.. code:: ipython3

    J('[1 2 3] pair_up total_matches')


.. parsed-literal::

    0


.. code:: ipython3

    J('[1 2 2 3] pair_up total_matches')


.. parsed-literal::

    2


Now we can define our main program and evaluate it on the examples.

.. code:: ipython3

    define('AoC2017.1 pair_up total_matches')

.. code:: ipython3

    J('[1 1 2 2] AoC2017.1')


.. parsed-literal::

    3


.. code:: ipython3

    J('[1 1 1 1] AoC2017.1')


.. parsed-literal::

    4


.. code:: ipython3

    J('[1 2 3 4] AoC2017.1')


.. parsed-literal::

    0


.. code:: ipython3

    J('[9 1 2 1 2 1 2 9] AoC2017.1')


.. parsed-literal::

    9


::

          pair_up == dup uncons swap unit concat zip
    total_matches == 0 swap [i [=] [pop +] [popop] ifte] step

        AoC2017.1 == pair_up total_matches

Next Part of the Challenge
--------------------------

Now the paired digit is "halfway" round.

::

    [a b c d] dup size 2 / [drop] [take reverse] cleave concat zip

.. code:: ipython3

    J('[1 2 3 4] dup size 2 / [drop] [take reverse] cleave concat zip')


.. parsed-literal::

    [[3 1] [4 2] [1 3] [2 4]]


I realized that each pair is repeated...

.. code:: ipython3

    J('[1 2 3 4] dup size 2 / [drop] [take reverse] cleave  zip')


.. parsed-literal::

    [1 2 3 4] [[1 3] [2 4]]


.. code:: ipython3

    define('AoC2017.1.extra dup size 2 / [drop] [take reverse] cleave  zip swap pop total_matches 2 *')

.. code:: ipython3

    J('[1 2 1 2] AoC2017.1.extra')


.. parsed-literal::

    6


.. code:: ipython3

    J('[1 2 2 1] AoC2017.1.extra')


.. parsed-literal::

    0


.. code:: ipython3

    J('[1 2 3 4 2 5] AoC2017.1.extra')


.. parsed-literal::

    4


Refactor FTW
============

With Joy a great deal of the heuristics from Forth programming carry
over nicely. For example, refactoring into small, well-scoped commands
with mnemonic names...

::

             rotate_seq == uncons swap unit concat
                pair_up == dup rotate_seq zip
           add_if_match == [=] [pop +] [popop] ifte
          total_matches == [i add_if_match] step_zero

              AoC2017.1 == pair_up total_matches

           half_of_size == dup size 2 /
               split_at == [drop] [take reverse] cleave
          pair_up.extra == half_of_size split_at zip swap pop

        AoC2017.1.extra == pair_up.extra total_matches 2 *
