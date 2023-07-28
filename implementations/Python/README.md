This is out of date.  The Python implementation was the original one.

﻿Thun

A Dialect of Joy.

v0.4.2

§.1 Introduction

Joy is a programming language created by Manfred von Thun that is easy to
use and understand and has many other nice properties.  This project
implements interpreters for a dialect that attempts to stay very close to
the spirit of Joy but does not precisely match the behaviour of the
original version written in C.

The best source (no pun intended) for learning about Joy is the
information made available at the website of La Trobe University (see the
references section below for the URL) which contains source code for the
original C interpreter, Joy language source code for various functions,
and a great deal of fascinating material mostly written by Von Thun on
Joy and its deeper facets as well as how to program in it and several
interesting aspects.  It's quite a treasure trove.


§.2 Installation

From PyPI in the usual way, e.g.:

    pip install Thun

Or if you have downloaded the source, from the top directory:

    python ./setup.py install

Or you can run the package directly from the top directory.

To start a crude REPL:

    python -m joy

There is a "quiet" mode for e.g. using joy from a shell script:

    python -m joy -q

This supresses the initial banner output and the prompt text.


§.3 Documentation

§.3.1 Jupyter Notebooks

The docs/ folder contains Jupyter notebooks, ... TODO

§.3.2 Sphinx Docs

Some of the documentation is in the form of ReST files

§.3.3 Building the Docs

Building the documentation is a little tricky at the moment.  It involves
a makefile that uses nbconvert to generate ReST files from some of the
notebooks, copies those to the sphinx source dir, then builds the HTML
output using sphinx.

Get the dependencies for (re)building the docs:

    pip install Thun[build-docs]
    make docs


§.4 Basics of Joy

Joy is stack-based.  There is a main stack that holds data items:
integers, floats, strings, functions, and sequences or quotes which hold
data items themselves.

    23 1.8 'a string' "another" dup [21 18 /] [1 [2 [3]]]

A Joy expression is just a sequence (a.k.a. "list") of items.  Sequences
intended as programs are called "quoted programs".  Evaluation proceeds
by iterating through the terms in the expression, putting all literals
onto the main stack and executing functions as they are encountered.
Functions receive the current stack and return the next stack.

§.4.1 Python Semantics

In general, where otherwise unspecified, the semantics of Thun are that
of the underlying Python. That means, for example, that integers are
unbounded (whatever your machine can handle), strings cannot be added to
integers but can be multiplied, Boolean True and False are effectively
identical to ints 1 and 0, empty sequences are considered False, etc.

Nothing is done about Python exceptions currently, although it would be
possible to capture the stack and expression just before the exception
and build a robust and flexible error handler.  Because they are both
just datastructures, you could immediately retry them under a debugger,
or edit either or both of the stack and expression.  All state is in one
or the other.

§.4.2 Literals and Simple Functions

    joy? 1 2 3
          . 1 2 3
        1 . 2 3
      1 2 . 3
    1 2 3 . 

    1 2 3 <-top

    joy? + +
    1 2 3 . + +
      1 5 . +
        6 . 

    6 <-top

    joy? 7 *
      6 . 7 *
    6 7 . *
     42 . 

    42 <-top

    joy? 


§.4.3 Combinators

The main loop is very simple as most of the action happens through what
are called "combinators": functions which accept quoted programs on the
stack and run them in various ways.  These combinators factor specific
patterns that provide the effect of control-flow in other languages (such
as ifte which is like if..then..else..)  Combinators receive the current
expession in addition to the stack and return the next expression.  They
work by changing the pending expression the interpreter is about to
execute.  The combinators could work by making recursive calls to the
interpreter and all intermediate state would be held in the call stack of
the implementation language, in this joy implementation they work instead
by changing the pending expression and intermediate state is put there.

    joy? 23 [0 >] [dup --] while

    ...

    -> 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23


TODO:

§.4.4 Definitions and More Elaborate Functions

§.4.5 Programming and Metaprogramming

§.4.6 Refactoring


§.5 This Implementation

Run with:

    python -m joy

Thun
 |-- COPYING - license
 |-- README - this file
 |
 |-- archive - info on Joy
 |   |-- Joy-Programming.zip
 |   `-- README
 |
 |-- docs - Various Examples and Demos
 |   |-- * - Jupyter Notebooks on Thun and supporting modules
 |   `-- README - Table of Contents
 |
 |-- joy
 |   |-- joy.py - main loop, REPL
 |   |-- library.py - Functions, Combinators, Definitions
 |   |-- parser.py - convert text to Joy datastructures
 |   |
 |   `-- utils
 |       |-- pretty_print.py - convert Joy datastructures to text
 |       `-- stack.py - work with stacks
 |
 |-- thun - Experimental Prolog Code
 |   |-- compiler.pl - A start on a compiler for Prof. Wirth's RISC CPU
 |   `-- thun.pl - An interpreter in the Logical Paradigm, compiler.
 |
 `-- setup.py



§.6 References & Further Reading


Wikipedia entry for Joy:
https://en.wikipedia.org/wiki/Joy_%28programming_language%29


Homepage at La Trobe University:
http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language



--------------------------------------------------

Misc...

Stack based - literals (as functions) - functions - combinators -
Refactoring and making new definitions - traces and comparing
performance - metaprogramming as programming, even the lowly integer
range function can be expressed in two phases: building a specialized
program and then executing it with a combinator - ?Partial evaluation?
- ?memoized dynamic dependency graphs? - algebra

--------------------------------------------------

Copyright © 2014-2022 Simon Forman

This file is part of Thun

Thun is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

Thun is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along
with Thun.  If not see <http://www.gnu.org/licenses/>.

