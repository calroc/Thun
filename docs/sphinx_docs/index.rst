.. Thun documentation master file, created by
   sphinx-quickstart on Sun Apr 22 15:19:55 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Thun |release| Documentation
============================

Thun is dialect of Joy written in Python.

`Joy`_ is a programming language created by Manfred von Thun that is easy to
use and understand and has many other nice properties.  This Python
package implements an interpreter for a dialect of Joy that attempts to
stay very close to the spirit of Joy but does not precisely match the
behaviour of the original version(s) written in C.  The main difference
between Thun and the originals, other than being written in Python, is
that it works by the "Continuation-Passing Style".

Joy is:

* `Purely Functional <https://en.wikipedia.org/wiki/Purely_functional_programming>`__
* `Stack-based <https://en.wikipedia.org/wiki/Stack-oriented_programming_language>`__
* `Concatinative`_ ( See also `concatenative.org <http://www.concatenative.org/wiki/view/Concatenative%20language>`__)
* :doc:`Categorical <notebooks/Categorical>`

I hope that this package is useful in the sense that it provides an
additional joy interpreter (the binary in the archive from La Trobe seems
to run just fine on my modern Linux machine!)  But I also hope that you
can read and understand the Python code and play with the implementation
itself.

.. _Joy: https://en.wikipedia.org/wiki/Joy_(programming_language)

.. _Concatinative: https://en.wikipedia.org/wiki/Concatenative_programming_language



Example Code
--------------------------------------------------

Here is an example of Joy code::

    [[[abs]ii <=][[<>][pop !-]||]&&][[!-][[++]][[--]]ifte dip][[pop !-][--][++]ifte]ifte

It might seem unreadable but with a little familiarity it becomes just as
legible as any other notation.  Some layout helps::

    [   [[abs] ii <=]
        [
            [<>] [pop !-] ||
        ] &&
    ]
    [[    !-] [[++]] [[--]] ifte dip]
    [[pop !-]  [--]   [++]  ifte    ]
    ifte

This function accepts two integers on the stack and increments or
decrements one of them such that the new pair of numbers is the next
coordinate pair in a square spiral (like the kind used to construct an
Ulam Spiral).  For more information see :doc:`notebooks/Square_Spiral` 


Quick Start
--------------------------------------------------

Install from `PyPI`_ in the usual way::

    $ pip install Thun

To start the REPL::

    $ python -m joy

Continue with :doc:`the introduction <notebooks/Intro>`.

.. _PyPI: https://pypi.org/project/Thun/


Project Hosted on `OSDN`_
-------------------------

* `Source Repository`_ (Mercurial)
* `Bug tracker`_
* `Forums`_
* `Mailing list`_

.. _Bug tracker: https://osdn.net/projects/joypy/ticket/
.. _Forums: https://osdn.net/projects/joypy/forums/
.. _Mailing list: https://osdn.net/projects/joypy/lists/
.. _OSDN: https://osdn.net/projects/joypy/
.. _Source Repository: https://osdn.net/projects/joypy/scm/hg/Joypy/tree/tip/


Information on the Joy language
-------------------------------

See `the Wikipedia article`_ and `Kevin Albrecht's mirror of Manfred von Thun's original website for the Joy Programming Language`_ for more information on the Joy language.

The best source (no pun intended) for learning about Joy is the
information made available at the website of La Trobe University (see the
mirror link above) which contains source code for the
original C interpreter, Joy language source code for various functions,
and a great deal of fascinating material mostly written by Von Thun on
Joy and its deeper facets as well as how to program in it and several
interesting aspects.  It's quite a treasure trove.

.. _the Wikipedia article: https://en.wikipedia.org/wiki/Joy_(programming_language)

.. _Kevin Albrecht's mirror of Manfred von Thun's original website for the Joy Programming Language: http://www.kevinalbrecht.com/code/joy-mirror/index.html


Documentation on Thun Dialect
-----------------------------

The following is specific information for this dialect of Joy.

.. toctree::
   :maxdepth: 2

   notebooks/Intro
   joy
   stack
   parser
   pretty
   library
   lib
   types
   notebooks/index



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
