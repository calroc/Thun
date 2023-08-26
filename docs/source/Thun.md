# Thun Specification

Version 0.5.0

## Grammar

The grammar of Thun is very simple.  A Thun expression is zero or more
Thun terms separated by blanks. Terms can be integers in decimal
notation, Booleans `true` and `false`, lists enclosed by square brackets
`[` and `]`, or symbols (names of functions.)

    joy ::= term*
    
    term ::= integer | bool | '[' joy ']' | symbol
    
    integer ::= [ '-' ] ('0'...'9')+
    
    bool ::= 'true' | 'false'
    
    symbol ::= char+
    
    char ::= <Any non-space other than '[' and ']'.>

Symbols can be composed of any characters except blanks and square
brackets.  Integers can be prefixed with a minus sign to denote negative
numbers.  The symbols `true` and `false` are reserved to denote their
respective Boolean values.

That's it.  That's the whole of the grammar.


## Types

The original Joy has several datatypes (such as strings and sets)
but the Thun dialect currently only uses four:

* Integers, signed and unbounded by machine word length (they are
  [bignums](https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic).)
* Boolean values ``true`` and ``false``.
* Lists quoted in `[` and `]` brackets.
* Symbols (names).


## Stack, Expression, Dictionary

Thun is built around three things: a __stack__ of data items, an
__expression__ representing a program to evaluate, and a __dictionary__
of named functions.

### Stack

Thun is
[stack-based](https://en.wikipedia.org/wiki/Stack-oriented_programming_language).
There is a single main __stack__ that holds data items, which can be
integers, bools, symbols (names), or sequences of data items enclosed in
square brackets (`[` or `]`).

We use the terms "stack", "quote", "sequence", "list", and others to mean
the same thing: a simple linear datatype that permits certain operations
such as iterating and pushing and popping values from (at least) one end.

> In describing Joy I have used the term quotation to describe all of the
> above, because I needed a word to describe the arguments to combinators
> which fulfill the same role in Joy as lambda abstractions (with
> variables) fulfill in the more familiar functional languages. I use the
> term list for those quotations whose members are what I call literals:
> numbers, characters, truth values, sets, strings and other quotations.
> All these I call literals because their occurrence in code results in
> them being pushed onto the stack. But I also call [London Paris] a
> list. So, [dup \*] is a quotation but not a list.

From ["A Conversation with Manfred von Thun" w/ Stevan Apter](http://archive.vector.org.uk/art10000350)

### Expression

A Thun __expression__ is just a sequence or list of items.  Sequences
intended as programs are called "quoted programs".  Evaluation proceeds
by iterating through the terms in an expression putting all literals
(integers, bools, or lists) onto the main stack and executing functions
named by symbols as they are encountered.  Functions receive the current
stack, expression, and dictionary and return the next stack, expression,
and dictionary.

### Dictionary

The __dictionary__ associates symbols (names) with Thun expressions that
define the available functions of the Thun system.  Together the stack,
expression, and dictionary are the entire state of the Thun interpreter.


## Interpreter

The Thun interpreter is extremely simple. It accepts a stack, an
expression, and a dictionary, and it iterates through the expression
putting values onto the stack and delegating execution to functions which
it looks up in the dictionary.

![Joy Interpreter Flowchart](https://git.sr.ht/~sforman/Thun/blob/trunk/joy_interpreter_flowchart.svg)

All control flow works by
[Continuation Passing Style](https://en.wikipedia.org/wiki/Continuation-passing_style).
__Combinators__ (see below) alter control flow by prepending quoted programs to the pending
expression (aka "continuation".)


## Literals, Functions, Combinators

Terms in Thun can be categorized into **literals**, simple **functions**
that operate on the stack only, and **combinators** that can prepend
quoted programs onto the pending expression ("continuation").

### Literals

Literal values (integers, Booleans, lists) are put onto the stack.
Literals can be thought of as functions that put accept a stack and
return it with the value they denote on top, if you like.

### Functions

Functions take values from the stack and push results onto it. There are
a few kinds of functions: math, comparison, list and stack manipulation.

### Combinators

__Combinators__ are functions which accept quoted programs on the stack
and run them in various ways by prepending them (or not) to the pending
expression.  These combinators reify specific control-flow patterns (such
as `ifte` which is like `if.. then.. else..` in other languages.)
Combinators receive the current expession in addition to the stack and
return the next expression.  They work by changing the pending expression
the interpreter is about to execute.

### Basis Functions

Thun has a set of *basis* functions which are implemented in the host
language.  The rest of functions in the Thun dialect are defined in terms
of these:

- Combinators: `branch` `dip` `i` `loop`
- Stack Chatter: `clear` `dup` `pop` `stack` `swaack` `swap`
- List Manipulation: `concat` `cons` `first` `rest`
- Math: `+` `-` `*` `/` `%`
- Comparison: `<` `>` `>=` `<=` `!=` `<>` `=`
- Logic: `truthy` `not`
- Programming: `inscribe`

### Definitions

Thun can be extended by adding new definitions to the
[defs.txt](https://git.sr.ht/~sforman/Thun/tree/trunk/item/implementations/defs.txt)
file and rebuilding the binaries.  Each line in the file is a definition
consisting of the new symbol name followed by an expression for the body
of the function.

The `defs.txt` file is just joy expressions, one per line, that have a
symbol followed by the definition for that symbol, e.g.:

    sqr dup mul

The definitions form a DAG (Directed Acyclic Graph) (there is actually a
cycle in the definition of `genrec` but that's the point, it is a cycle
to itself that captures the cyclical nature of recursive definitions.)

I don't imagine that people will read `defs.txt` to understand Thun code.
Instead people should read the notebooks that derive the functions to
understand them.  The reference docs should help, and to that end I'd
like to cross-link them with the notebooks.  The idea is that the docs
are the code and the code is just a way to make precise the ideas in the
docs.

### Adding Functions to the Dictionary with `inscribe`

You can use the `inscribe` command to put new definitions into the
dictionary at runtime, but they will not persist after the program ends.
The `inscribe` function is the only function that changes the dictionary.
It's meant for prototyping.  (You could abuse it to make variables by
storing "functions" in the dictionary that just contain literal values as
their bodies.)

    [foo bar baz] inscribe

This will put a definition for `foo` into the dictionary as `bar baz`.


## Problems

### Symbols as Data

Nothing prevents you from using symbols as data:

    joy? [cats]
    [cats]

But there's a potential pitfall: you might accidentally get a "bare"
unquoted symbol on the stack:

    joy? [cats]
    [cats]
    joy? first
    cats

That by itself won't break anything (the stack is just a list.)
But if you were to use, say, `dip`, in such a way as to put the symbol
back onto the expression, then when the interpreter encounters it, it
will attempt to evaluate it, which is almost certainly not what you want.

    cats
    joy? [23] dip
    Unknown: cats
    cats

At the very least you get an "Unknown" error, but if the symbol names a
function then the interpreter will attempt to evaluate it, probably
leading to an error.

I don't see an easy way around this.  Be careful?  It's kind of against
the spirit of the thing to just leave a footgun like that laying around,
but perhaps in practice it won't come up.  (Because writing Thun code by
derivation seems to lead to bug-free code, which is the kinda the point.)


### Variations between Interpreters

There are several small choices to be made when implementing a Thun
interpreter (TODO: make a comprehensive list), for example, the Python
interpreter keeps all of its functions in one dictionary but most of the
other interpreters have a `case` or `switch` statement for the built-in
functions and a separate hash table for definitions.  Additionally, of
the interpreters that have hash tables most of them check the hash table
after the `case` statement.  This means that one cannot "shadow" built-in
functions is some interpreters.  You can `inscribe` them, but the
interpreter will not look for them.

I haven't yet formally made a decision for how Thun *shall* work.
Letting built-ins be shadowed is fun and useful for exploration, and
letting them be inviolate is useful for unsurprising behaviour.

Another choice is how to handle duplicate definitions in general. Should
you be able to reuse a name?  Or should `inscribe` throw some sort of
error if you try?



--------------------------------------------------

Copyright © 2014 - 2023 Simon Forman

This file is part of Thun

