# Thun

A Dialect of Joy.

Version 0.5.0

> Simple pleasures are the best.

[Joy](https://en.wikipedia.org/wiki/Joy_%28programming_language%29) is a
programming language created by Manfred von Thun that is easy to use and
understand and has many other nice properties.  **Thun** is a dialect of
Joy that attempts to stay very close to the spirit of Joy but does not
precisely match the behaviour of the original version written in C.  It
started as a Python project called "Joypy", but after someone claimed that
name on PyPI before me I renamed it to Thun in honor of Manfred Von Thun.
Now there are interpreters implemented in several additional languages
(C, Elm, Nim, OCaml, Prolog, Rust).

Joy is:

* [Purely Functional](https://en.wikipedia.org/wiki/Purely_functional_programming)
* [Stack-based](https://en.wikipedia.org/wiki/Stack-oriented_programming_language)
* [Concatinative](https://en.wikipedia.org/wiki/Concatenative_programming_language)
  (See also [concatenative.org](http://www.concatenative.org/wiki/view/Concatenative%20language))
* [Categorical](https://joypy.osdn.io/notebooks/Categorical.html)

The best source (no pun intended) for learning about Joy is the
information made available at the
[website of La Trobe University](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
| [(mirror)](https://www.kevinalbrecht.com/code/joy-mirror/)
which contains source code for the original C interpreter, Joy language source code for various functions,
and a great deal of fascinating material mostly written by Von Thun on
Joy and its deeper facets as well as how to program in it and several
interesting aspects.  It's quite a treasure trove.

* [Wikipedia entry for Joy](https://en.wikipedia.org/wiki/Joy_%28programming_language%29)
* [Homepage at La Trobe University](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
  [(Kevin Albrecht's mirror)](https://www.kevinalbrecht.com/code/joy-mirror/)
* [The original Thun/Joypy site](https://web.archive.org/web/20220411010035/https://joypy.osdn.io/)


## Example Code

Here is an example of Joy code.  This function `square_spiral` accepts
two integers and increments or decrements one of them such that the new
pair of numbers is the next coordinate pair in a square spiral (like the
kind used to construct an [Ulam Spiral](https://en.wikipedia.org/wiki/Ulam_spiral)).
For more information see [Square Spiral Example Joy Code](https://joypy.osdn.io/notebooks/Square_Spiral.html).

    square_spiral [_p] [_then] [_else] ifte

    _p  [_p0] [_p1] and
    _p0 [abs] ii <=
    _p1 [<>] [pop !-] or

    _then [    !-] [[++]] [[--]] ifte dip
    _else [pop !-]  [--]   [++]  ifte

It might seem unreadable but with familiarity it becomes as legible as any other notation.


## Project Hosted on [SourceHut](https://git.sr.ht/~sforman/Thun)

* [Source Repository](https://git.sr.ht/~sforman/Thun)
  ([mirror](https://github.com/calroc/Thun))
* [Bug tracker](https://todo.sr.ht/~sforman/thun-der)
  ([old tracker](https://osdn.net/projects/joypy/ticket/))
* [Forums](https://osdn.net/projects/joypy/forums/)
* [Mailing list](https://osdn.net/projects/joypy/lists/)


## Documentation

This document describes Joy in a general way below, however most of the
documentation is in the form of [Jupyter Notebooks](https://joypy.osdn.io/notebooks/index.html)
that go into more detail.

**[Jupyter Notebooks](https://joypy.osdn.io/notebooks/index.html)**

There's also a [Function Reference](https://git.sr.ht/~sforman/Thun/tree/trunk/item/docs/reference) that lists each
function and combinator by name and gives a brief description.  (It's
usually out of date, I'm working on it.)

**[Function Reference](https://git.sr.ht/~sforman/Thun/tree/trunk/item/docs/reference)**



### Building the Docs

Run `make` in the `docs` directory.  (This is a lie, it's more complex than
that.  Really you need to run (GNU) make in the `docs/notebooks` and
`docs/reference` dirs first, _then_ run `make` in the `docs` directory.)


## Directory structure

    Thun
    |-- LICENSE - GPLv3
    |-- README.md - this file
    |
    |-- archive
    |   |-- Joy-Programming.zip
    |   `-- README
    |
    |-- docs
    |   |-- dep-graphs - Generated dependency graphs.
    |   |-- html - Generated HTML docs.
    |   |-- notebooks - Jupyter Notebooks and supporting modules
    |   `-- reference - Docs for each function.
    |
    |-- implementations
    |   |-- defs.txt - common Joy definitions for all interpreters
    |   |-- C - interpreter
    |   |-- GNUProlog - interpreter
    |   |               type inference
    |   |               work-in-progress compiler
    |   |
    |   |-- Nim - interpreter
    |   |-- Ocaml - work-in-progress interpreter
    |   `-- Python - interpreter
    |
    `-- joy_code - Source code written in Joy.
        `-- bigints
            `-- bigints.joy


## Installation

Clone the repo:

    git clone https://git.sr.ht/~sforman/Thun

Then follow the instructions in the individual `implementations` directories.

(There isn't really any installation as such.
You can put the binaries in your ``PATH``.)


## Basics of Joy

The original Joy has several datatypes (such as strings and sets)
but the Thun dialect currently only uses four:

* Integers, signed and unbounded by machine word length (they are
  [bignums](https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic).)
* Boolean values ``true`` and ``false``.
* Lists quoted in `[` and `]` brackets.
* Symbols (names).

Joy is built around three things: a __stack__ of data items, an __expression__
representing a program to evaluate, and a __dictionary__ of named functions.

### Stack

Joy is [stack-based](https://en.wikipedia.org/wiki/Stack-oriented_programming_language).
There is a single main __stack__ that holds data items, which can be integers, bools,
symbols (names), or sequences of data items enclosed in square brackets (`[` or `]`).

We use the terms "stack", "quote", "sequence",
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

### Expression

A Joy __expression__ is just a sequence or list of items.  Sequences
intended as programs are called "quoted programs".  Evaluation proceeds
by iterating through the terms in an expression putting all literals
(integers, bools, or lists) onto the main stack and executing functions
named by symbols as they are encountered.  Functions receive the current
stack, expression, and dictionary and return the next stack, expression,
and dictionary.

### Dictionary

The __dictionary__ associates symbols (names) with Joy expressions that
define the available functions of the Joy system.  Together the stack,
expression, and dictionary are the entire state of the Joy interpreter.

### Interpreter

The Joy interpreter is extrememly simple. It accepts a stack, an
expression, and a dictionary, and it iterates through the expression
putting values onto the stack and delegating execution to functions which
it looks up in the dictionary.

![Joy Interpreter Flowchart](https://git.sr.ht/~sforman/Thun/blob/trunk/joy_interpreter_flowchart.svg)

All control flow works by
[Continuation Passing Style](https://en.wikipedia.org/wiki/Continuation-passing_style).
__Combinators__ (see below) alter control flow by prepending quoted programs to the pending
expression (aka "continuation".)

-------------------------------

From here it kinda falls apart...

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


### Core Words

This is the *basis* set of functions, the rest of functions in the Thun 
dialect of Joy are defined in terms of these:

    branch
    dip
    i
    loop

    clear
    concat
    cons
    dup
    first
    pop
    rest
    stack
    swaack
    swap
    truthy
    inscribe

    + - * / %

    < > >= <= != <> = 

    not

They could be grouped:

- Combinators (`branch` `dip` `i` `loop`)
- Stack Chatter (`clear` `dup` `pop` `stack` `swaack` `swap`)
- List Manipulation (`concat` `cons` `first` `rest`)
- Math (`+` `-` `*` `/` `%`)
- Comparison (`<` `>` `>=` `<=` `!=` `<>` `=`)
- Logic (`truthy` `not`)
- Programming (`inscribe`)

Some of these could be definitions, but we don't want to be completely
minimal at the cost of efficiency, eh?

    rest == [pop] infra

Also, custom error messages are nice?  (E.g. `rest` has a distinct error
from `pop`, at least in the current design.)


### AND, OR, XOR, NOT

There are three families (categories?) of these operations:

1. Logical ops that take and return Boolean values.
2. Bitwise ops that treat integers as bit-strings.
3. Short-Circuiting Combinators that accept two quoted programs
   and run top quote *iff* the second doesn't suffice to resolve the clause.
   (in other words `[A] [B] and` runs `B` only if `A` evaluates to `true`,
   and similarly for `or` but only if `A` evaluates to `false`.)

(So far, only the Elm interpreter implements the bitwise ops.  The others
two kinds of ops are defined in the `defs.txt` file, but you could implement
them in host language for greater efficiency if you like.)

| op  | Logical (Boolean) | Bitwise (Ints) | Short-Circuiting Combinators |
|-----|-------------------|----------------|------------------------------|
| AND |       `/\`        |     `&&`       |          `and `              |
|  OR |       `\/`        |    `\|\|`      |          `or`                |
| XOR |      `_\/_`       |    `xor`       |                              |
| NOT |      `not`        |                |                              |


--------------------------------------------------

Copyright © 2014 - 2023 Simon Forman

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

