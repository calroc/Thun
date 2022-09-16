# Thun

A Dialect of Joy.

version 0.5.0

> Simple pleasures are the best.

Joy is a programming language created by Manfred von Thun that is easy to
use and understand and has many other nice properties.  This project
implements interpreters for a dialect that attempts to stay very close to
the spirit of Joy but does not precisely match the behaviour of the
original version written in C.

Joy is:

* [Purely Functional](https://en.wikipedia.org/wiki/Purely_functional_programming)
* [Stack-based](https://en.wikipedia.org/wiki/Stack-oriented_programming_language)
* [Concatinative](https://en.wikipedia.org/wiki/Concatenative_programming_language) (See also [concatenative.org](http://www.concatenative.org/wiki/view/Concatenative%20language))
* [Categorical](https://joypy.osdn.io/notebooks/Categorical.html)

The best source (no pun intended) for learning about Joy is the
information made available at the
[website of La Trobe University](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
which contains source code for the original C interpreter, Joy language source code for various functions,
and a great deal of fascinating material mostly written by Von Thun on
Joy and its deeper facets as well as how to program in it and several
interesting aspects.  It's quite a treasure trove.


## Example Code

Here is an example of Joy code:

    square_spiral ≡ [_p] [_then] [_else] ifte

    _p  ≡ [_p0] [_p1] &&
    _p0 ≡ [abs] ii <=
    _p1 ≡ [<>] [pop !-] ||

    _then ≡ [    !-] [[++]] [[--]] ifte dip
    _else ≡ [pop !-]  [--]   [++]  ifte

It might seem unreadable but with a little familiarity it becomes just as legible as any other notation.

This function accepts two integers on the stack and increments or
decrements one of them such that the new pair of numbers is the next
coordinate pair in a square spiral (like the kind used to construct an
[Ulam Spiral](https://en.wikipedia.org/wiki/Ulam_spiral)
).  For more information see [Square Spiral Example Joy Code](/notebooks/Square_Spiral.html)


## Project Hosted on [OSDN](https://osdn.net/projects/joypy/)

* [Source Repository](https://osdn.net/projects/joypy/scm/git/Thun/) ([mirror](https://github.com/calroc/Thun))
* [Bug tracker](https://todo.sr.ht/~sforman/thun-der) ([old tracker](https://osdn.net/projects/joypy/ticket/))
* [Forums](https://osdn.net/projects/joypy/forums/)
* [Mailing list](https://osdn.net/projects/joypy/lists/)


## Directory structure

    Thun
     |
     |-- LICENSE - GPLv3
     |-- README.md - this file
     |
     |-- archive
     |   |-- Joy-Programming.zip
     |   `-- README
     |
     |-- docs
     |   |-- Makefile - Generate https://joypy.osdn.io/ site.
     |   |-- notebooks - Jupyter Notebooks and supporting modules
     |   |-- reference - Docs for each function.
     |   |-- dep-graphs - Generated dependency graphs.
     |   `-- README - Table of Contents
     |
     `-- implementations
         |
         |-- Nim - interpreter
         |
         |-- Prolog - interpreter
         |            type inference
         |            work-in-progress compiler
         |
         |-- Python - interpreter
         |
         `-- defs.txt - common Joy definitions for all interpreters


## Documentation

### Jupyter Notebooks

[Notebooks](/notebooks/index.html)

The docs/notebooks dir contains Jupyter notebooks, ... TODO

### Function Reference

[Function Reference](/FuncRef.html)

### Building the Docs

Run `make` in the `docs` directory.


## Basics of Joy

Joy is stack-based.  There is a main stack that holds data items:
integers, bools, symbols, and sequences or quotes which hold
data items themselves.

    23 dup [21 18 /] [1 [2 [3]]]

A Joy expression is just a sequence (a.k.a. "list") of items.  Sequences
intended as programs are called "quoted programs".  Evaluation proceeds
by iterating through the terms in the expression, putting all literals
onto the main stack and executing functions as they are encountered.
Functions receive the current stack and return the next stack.


### Literals and Simple Functions

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


### Combinators

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


## TODO:

§.4.4 Definitions and More Elaborate Functions

§.4.5 Programming and Metaprogramming

§.4.6 Refactoring


§.6 References & Further Reading


[Wikipedia entry for Joy](https://en.wikipedia.org/wiki/Joy_%28programming_language%29)

[Homepage at La Trobe University](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)



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

