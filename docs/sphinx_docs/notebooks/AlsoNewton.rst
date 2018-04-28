***************
``within``
***************

    The remainder of a square root finder is a function *within*, which takes a tolerance and a list of approximations and looks down the list for two successive approximations that differ by no more than the given tolerance.

From `"Why Functional Programming Matters" by John
Hughes <https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf>`__

(And note that by "list" he means a lazily-evaluated list.)

Using a :doc:`generator <Generator Programs>` driven by ``x`` and assuming such for square root approximations (or whatever) ``G``, and further assuming that the first term ``a`` has been generated already and epsilon ``ε`` is handy on the stack...

::

       a [b G] ε within
    -------------------- a b - abs ε <=
          b

       a [b G] ε within
    -------------------- a b - abs ε >
           .
       [b G] x ε ...
       b [c G] ε ...
           .
    --------------------
       b [c G] ε within



Predicate
^^^^^^^^^^^^^

::

    a [b G]             ε [first - abs] dip <=
    a [b G] first - abs ε                   <=
    a b           - abs ε                   <=
    a-b             abs ε                   <=
    abs(a-b)            ε                   <=
    (abs(a-b)<=ε)


::

    P == [first - abs] dip <=


Base-Case
^^^^^^^^^^^^^

::

    a [b G] ε roll< popop first
      [b G] ε a     popop first
      [b G]               first
       b

::

   B == roll< popop first

Recur
^^^^^^^^^^^^^

::

    a [b G] ε R0 [within] R1


1. Discard ``a``.
2. Use ``x`` combinator to generate next term from G.
3. Run ``within`` with ``i`` (it is a ``primrec`` function.)

::

    a [b G]        ε R0           [within] R1
    a [b G]        ε [popd x] dip [within] i
    a [b G] popd x ε              [within] i
      [b G]      x ε              [within] i
    b [c G]        ε              [within] i
    b [c G]        ε               within

    b [c G] ε within

::

    R0 == [popd x] dip

Setting up
^^^^^^^^^^

::

    [a G] x ε
    a [b G] ε

::

    within == x ε [[first - abs] dip <=] [roll< popop first] [[popd x] dip] primrec
























