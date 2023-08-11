# Thun

A Dialect of Joy.

Version 0.5.0

> Simple pleasures are the best.

[Joy](https://en.wikipedia.org/wiki/Joy_%28programming_language%29) is a
programming language created by Manfred von Thun that is easy to use and
understand and has many other nice properties.  **Thun** is a dialect of
Joy that attempts to stay very close to the spirit of Joy but does not
precisely match the behaviour of the original version written in C.


## Grammar

The grammar of Joy is very simple.  A Joy expression is zero or more Joy
terms separated by blanks. Terms can be integers in decimal notation,
Booleans `true` and `false`, lists enclosed by square brackets `[` and `]`,
or symbols (names of functions.)

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

Joy is built around three things: a __stack__ of data items, an
__expression__ representing a program to evaluate, and a __dictionary__
of named functions.

### Stack

Joy is
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


## Interpreter

The Joy interpreter is extrememly simple. It accepts a stack, an
expression, and a dictionary, and it iterates through the expression
putting values onto the stack and delegating execution to functions which
it looks up in the dictionary.

![Joy Interpreter Flowchart](https://git.sr.ht/~sforman/Thun/blob/trunk/joy_interpreter_flowchart.svg)

All control flow works by
[Continuation Passing Style](https://en.wikipedia.org/wiki/Continuation-passing_style).
__Combinators__ (see below) alter control flow by prepending quoted programs to the pending
expression (aka "continuation".)


## Literals, Functions, Combinators


### Literals

Literal values (integers, Booleans, lists) are put onto the stack.

### Functions

Functions take values from the stack and push results onto it.

### Combinators

__Combinators__ are functions which accept quoted programs on the stack
and run them in various ways.  These combinators reify specific
control-flow patterns (such as `ifte` which is like `if.. then.. else..`
in other languages.)  Combinators receive the current expession in
addition to the stack and return the next expression.  They work by
changing the pending expression the interpreter is about to execute.  

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

Thun can be extended by adding new definitions to the `defs.txt` file and
rebuilding the binaries.  Each line in the file is a definition
consisting of the new symbol name followed by an expression for the body
of the function.

You can use the `inscribe` command to put new definitions into the
dictionary at runtime, but they will not persist after the program ends.
The `inscribe` function is the only function that changes the dictionary.
It's meant for prototyping.  (You could abuse it to make variables by
storing "functions" in the dictionary that just contain literal values as
their bodies.)

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
but perhaps in practice it won't come up.  (Because writing Joy code by
derivation seems to lead to bug-free code, which is the kinda the point.)


--------------------------------------------------

Copyright © 2014 - 2023 Simon Forman

This file is part of Thun
