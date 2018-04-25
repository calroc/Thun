.. Thun documentation master file, created by
   sphinx-quickstart on Sun Apr 22 15:19:55 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Thun |release| Documentation
============================

Thun is dialect of Joy written in Python.

Joy is a programming language created by Manfred von Thun that is easy to
use and understand and has many other nice properties.  This Python
package implements an interpreter for a dialect of Joy that attempts to
stay very close to the spirit of Joy but does not precisely match the
behaviour of the original version(s) written in C.  The main difference
between Thun and the originals, other than being written in Python, is
that it works by the "Continuation-Passing Style".


Quick Start
--------------------------------------------------

Install from PyPI in the usual way::

    $ pip install Thun

To start the REPL::

    $ python -m joy



.. toctree::
   :maxdepth: 2
   :caption: Contents:

   joy
   stack
   parser
   pretty
   library
   lib



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
