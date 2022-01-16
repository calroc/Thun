# User Guide

There is no use interface as such.  At the moment you just load the
`thun.pl` file into SWI Prolog and use some of these "top-level"
predicates to interact with it.

------------------

## `joy/3`

    joy(InputString, StackIn, StackOut)

### Evaluation

Accepts a joy expression as a list of codes (in SWI Prolog you can use
backticks to quote a string literal and get codes):

    ?- joy(`+ *`, [int(2), int(3), int(10)], StackOut).
    StackOut = [int(50)] ;
    false.

    ?- joy(`2 3 + 10 *`, StackIn, StackOut).
    StackOut = [int(50)|StackIn] ;
    false.

### Type Checking

This predicate can also perform type checking:

    ?- joy(`2 [] +`, StackIn, StackOut).
    false.

### Type Inference

And type inference with CLP(FD) constraints on integer operations and
comparisons:

    ?- joy(`+ *`, StackIn, StackOut).
    StackIn = [int(_37782), int(_37792), int(_37802)|_37798],
    StackOut = [int(_37824)|_37798],
    _37782+_37792#=_37842,
    _37842*_37802#=_37824 ;
    false.

------------------

## `joy_parse//1`

If you just want to parse a string into a Joy expression use joy_parse//1
DCG, like so:

    phrase(joy_parse(Expression), InputString)

Or directly:

    joy_parse(Expression, InputString, [])


Example:

    ?- phrase(joy_parse(Expression), `1 [i]`).
    Expression = [int(1), list([symbol(i)])] ;
    false.

------------------

## `thun/3`

Once you have a (type-tagged) Joy exression as Prolog data-structure you
can use the `thun/3` predicate to evaluate it:

    thun(Expression, InputStack, OutputStack)


------------------

## `sjc/2`

    sjc(Name, InputString)

Helper function to see to what Prolog code a given Joy expression would
compile.  Give it a name (Prolog atom) and a list of code.

    ?- sjc(third, `third`).
    func(third, [list([_, _, A|_])|B], [A|B]).
    true ;
    false.

    ?- sjc(ccons, `ccons`).
    func(ccons, [list(C), B, A|D], [list([A, B|C])|D]).
    true ;
    false.

Compilation captures CLP(FD) constraints:

    ?- sjc(+*, `+ *`).
    func(+*, [int(D), int(E), int(B)|A], [int(C)|A]) :-
        maplist(call,
                [ clpfd:(F*B#=C),
                clpfd:(D+E#=F)
                ]).
    true ;
    false.

------------------

## `show_joy_compile/2`

    show_joy_compile(Name, Expression)

Same as `sjc/2` but you give it an already-parsed expression.

------------------

## `joy_compile/2`

    joy_compile(Name, Expression)

This actually asserts the new function definition into the Prolog rule
database. Use with care.

At some point I'll probably add a build phase that tries to pre-compile
all definitions it can into Prolog rules but for now this is just
experimental.

------------------

## `compiler/4`

    compiler(InputString, MachineCode, StackIn, StackOut)

Experimental *and* unfinished, this predicate attempts to build a list of
terms representing machine code for the RISC CPU that Prof. Wirth has
specified for his Project Oberon.

------------------

## `grow//0` & `shrink//0`

These DCGs recursively unfold or fold definitions in a Joy expression:

    ?- phrase(grow, [symbol(third)], Out).
    Out = [symbol(rest), symbol(rest), symbol(first)] ;
    Out = [symbol(rest), symbol(rest), symbol(first)] ;
    Out = [symbol(rest), symbol(second)] ;
    Out = [symbol(third)].

    ?- phrase(shrink, [symbol(rest), symbol(rest), symbol(first)], Out).
    Out = [symbol(rrest), symbol(first)] ;
    Out = [symbol(third)] ;
    Out = [symbol(rest), symbol(second)] ;
    Out = [symbol(rest), symbol(rest), symbol(first)].

They are more a proof-of-concept than useful at the moment.  I imagine
that it might be possible to set up some kind of automated search through
all the variations to see if a more efficient form (after compiling with
whatever optimizations) can be found.

------------------

## `joy_terms_to_string/2`

    joy_terms_to_string(Expr, String)

Converts a Joy expression into a string (an actual string, not a list of
codes. See the [SWI Prolog manual for more information on strings](https://www.swi-prolog.org/pldoc/man?section=strings).)

