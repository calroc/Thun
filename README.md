# Thun

A Dialect of Joy.

Version 0.5.0

> Simple pleasures are the best.

[Joy](https://en.wikipedia.org/wiki/Joy_%28programming_language%29) is a
programming language created by Manfred von Thun that is easy to use and
understand and has many other nice properties.  **Thun** is a dialect of
Joy that attempts to stay very close to the spirit of Joy but does not
precisely match the behaviour of the original version written in C.  It
started as a Python project called "Joypy", but after someone claimed
that name on PyPI before me I renamed it to Thun in honor of Manfred Von
Thun. Now there are interpreters implemented in several additional
languages (C, Elm, Nim, OCaml, Prolog, and Scheme).

Joy is:

* [Purely Functional](https://en.wikipedia.org/wiki/Purely_functional_programming)
* [Stack-based](https://en.wikipedia.org/wiki/Stack-oriented_programming_language)
* [Concatinative](https://en.wikipedia.org/wiki/Concatenative_programming_language)
  (See also [concatenative.org](http://www.concatenative.org/wiki/view/Concatenative%20language))
* [Categorical](https://ariadne.systems/pub/~sforman/Thun/notebooks/Categorical.html)

The best source for learning about Joy is the information made available
at the [website of La Trobe University](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
| [(mirror)](https://www.kevinalbrecht.com/code/joy-mirror/) which
contains source code for the original C interpreter, Joy language source
code for various functions, and a great deal of fascinating material
mostly written by Von Thun on Joy and its deeper facets as well as how to
program in it and several interesting aspects.  It's quite a treasure
trove.

* [Wikipedia entry for Joy](https://en.wikipedia.org/wiki/Joy_%28programming_language%29)
* [Homepage at La Trobe University](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
  [(Kevin Albrecht's mirror)](https://www.kevinalbrecht.com/code/joy-mirror/)
* [The original Thun/Joypy site](https://web.archive.org/web/20220411010035/https://joypy.osdn.io/)


## Example Code

Here is an example of Joy code.  This function `square_spiral` accepts
two integers and increments or decrements one of them such that the new
pair of numbers is the next coordinate pair in a square spiral (like the
kind used to construct an [Ulam Spiral](https://en.wikipedia.org/wiki/Ulam_spiral)).
For more information see [Square Spiral Example Joy Code](https://ariadne.systems/pub/~sforman/Thun/notebooks/Square_Spiral.html).

    square_spiral [p] [then] [else] ifte

    p  [p0] [p1] and
    p0 [abs] ii <=
    p1 [<>] [pop !-] or

    then [    !-] [[++]] [[--]] ifte dip
    else [pop !-]  [--]   [++]  ifte

It might seem unreadable but with familiarity it becomes as legible as
any other notation.


## Project Hosted on [Ariadne Systems](https://ariadne.systems/gogs/sforman/Thun)

* [Source Repository](https://ariadne.systems/gogs/sforman/Thun)
  ([mirror](https://github.com/calroc/Thun))
* [Bug tracker](https://ariadne.systems/gogs/sforman/Thun/issues)
  ([old tracker](https://osdn.net/projects/joypy/ticket/))
* [Forums](https://osdn.net/projects/joypy/forums/)
* [Mailing list](https://osdn.net/projects/joypy/lists/)


## Documentation

The `Thun.md` document describes the Thun dialect. Most of the rest of
documentation is in the form of
[Jupyter Notebooks](https://ariadne.systems/pub/~sforman/Thun/notebooks/index.html)
that go into more detail.

**[Jupyter Notebooks](https://ariadne.systems/pub/~sforman/Thun/notebooks/index.html)**

I had a Joy kernel for the Jupyter Notebook system, but I can no longer
figure out how to use it, so I'm rewriting the notebooks by hand.

There's also a [Function Reference](https://ariadne.systems/gogs/sforman/Thun/src/trunk/docs/reference/Function-Reference.md) that lists each
function and combinator by name and gives a brief description.  (It's
usually out of date, I'm working on it.)

**[Function Reference](https://ariadne.systems/gogs/sforman/Thun/src/trunk/docs/reference/Function-Reference.md)**

There is more in the `docs` directory but it's kind of a mess right now
(Aug 2023).


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
    |   |-- GNU Prolog - type inference
    |   |-- Elm - interpreter
    |   |-- Nim - interpreter
    |   |-- Ocaml - work-in-progress interpreter
    |   |-- Python - interpreter
    |   |-- Scheme - interpreter
    |   `-- SWI Prolog - interpreter
    |                    type inference
    |                    work-in-progress compiler
    |
    `-- joy_code - Source code written in Joy.
        `-- bigints
            `-- bigints.joy


## Installation

Clone the repo:

    git clone https://ariadne.systems/gogs/sforman/Thun.git

Then follow the instructions in the individual `implementations`
directories.  In most cases you can just run `make` and that will build a
binary called `joy` (in Python it's a script.)

There isn't really any installation as such. You can put the binaries in
your ``PATH``.







--------------------------------------------------

Copyright © 2014 - 2023 Simon Forman

This file is part of Thun

