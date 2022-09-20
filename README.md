# Thun

A Dialect of Joy.

Version 0.5.0

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

[Wikipedia entry for Joy](https://en.wikipedia.org/wiki/Joy_%28programming_language%29)

[Homepage at La Trobe University](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)

[The original Thun/Joypy site](https://web.archive.org/web/20220411010035/https://joypy.osdn.io/)


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

### [Jupyter Notebooks](/notebooks/index.html)

### [Function Reference](/FuncRef.html)

### Building the Docs

Run `make` in the `docs` directory.  (This is a lie, it's more complex than
that.  Really you need to run (GNU) make in the `docs/notebooks` and
`docs/reference` dirs first, _then_ run `make` in the `docs` directory.)


## Installation

Clone the repo and follow the instructions in the individual `implementations` directories.


## Basics of Joy

Joy is built around three things: a __stack__ of data items, an __expression__
representing a program to evaluate, and a __dictionary__ of named functions.

Joy is [stack-based](https://en.wikipedia.org/wiki/Stack-oriented_programming_language).
There is a single main __stack__ that holds data items, which can be integers, bools,
symbols (names), or sequences of data items enclosed in square brackets (`[` or `]`).

    23 dup [21 18 add] true false [1 [2 [3]]] cons

A Joy __expression__ is just a sequence or list of items.  Sequences
intended as programs are called "quoted programs".  Evaluation proceeds
by iterating through the terms in an expression putting all literals (integers, bools, or lists)
onto the main stack and executing functions named by symbols as they are encountered.
Functions receive the current stack, expression, and dictionary and return the next stack.

The __dictionary__ associates symbols (strings) with Joy expressions that define the
available functions of the Joy system.  Together the stack, expression, and dictionary
are the entire state of the Joy interpreter.

### Interpreter

![joy_interpreter_flowchart.svg](/joy_interpreter_flowchart.svg)


### Stack / Quote / List / Sequence

When talking about Joy we use the terms "stack", "quote", "sequence",
"list", and others to mean the same thing: a simple linear datatype that
permits certain operations such as iterating and pushing and popping
values from (at least) one end.

> In describing Joy I have used the term quotation to describe all of the
> above, because I needed a word to describe the arguments to combinators
> which fulfill the same role in Joy as lambda abstractions (with
> variables) fulfill in the more familiar functional languages. I use the
> term list for those quotations whose members are what I call literals:
> numbers, characters, truth values, sets, strings and other quotations.
> All these I call literals because their occurrence in code results in
> them being pushed onto the stack. But I also call [London Paris] a list.
> So, [dup *] is a quotation but not a list.

From ["A Conversation with Manfred von Thun" w/ Stevan Apter](http://archive.vector.org.uk/art10000350)




### Literals and Simple Functions

TODO


### Combinators

The main loop is very simple as most of the action happens through what
are called __combinators__. These are functions which accept quoted programs on the
stack and run them in various ways.  These combinators reify specific
control-flow patterns (such as `ifte` which is like `if.. then.. else..` in other
languages.)  Combinators receive the current
expession in addition to the stack and return the next expression.  They
work by changing the pending expression the interpreter is about to
execute.  (The combinators could work by making recursive calls to the
interpreter and all intermediate state would be held in the call stack of
the implementation language, in this joy implementation they work instead
by changing the pending expression and intermediate state is put there.)

    joy? 23 [0 >] [dup --] while
    23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0



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

