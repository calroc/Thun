/*

████████╗██╗  ██╗██╗   ██╗███╗   ██╗
╚══██╔══╝██║  ██║██║   ██║████╗  ██║
   ██║   ███████║██║   ██║██╔██╗ ██║
   ██║   ██╔══██║██║   ██║██║╚██╗██║
   ██║   ██║  ██║╚██████╔╝██║ ╚████║
   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

A dialect of Joy.  Version -10.0.0.

    Copyright © 2018, 2019, 2020 Simon Forman

    This file is part of Thun

    Thun is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Thun is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Thun.  If not see <http://www.gnu.org/licenses/>.

(Big fonts are from Figlet "ANSI Shadow" http://www.patorjk.com/software/taag/#p=display&f=ANSI%20Shadow&t=formatter and "Small".)

Thun is an implementation of a dialect of the Joy executable notation.

Table of Contents
    Parser & Grammar
    Semantics
        Functions
        Combinators
        Definitions
    Compiler
        to Prolog
        to Machine Code
    Meta-Programming
        Expand/Contract Definitions
        Formatter
        Partial Reducer

 */

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(gensym)).
:- dynamic func/3.
:- dynamic def/2.


/*
An entry point.
*/

joy(InputString, StackIn, StackOut) :-
    text_to_expression(InputString, Expression),
    !,
    thun(Expression, StackIn, StackOut).

/*

██████╗  █████╗ ██████╗ ███████╗███████╗██████╗        ██╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗       ██║
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝    ████████╗
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗    ██╔═██╔═╝
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║    ██████║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝    ╚═════╝

 ██████╗ ██████╗  █████╗ ███╗   ███╗███╗   ███╗ █████╗ ██████╗
██╔════╝ ██╔══██╗██╔══██╗████╗ ████║████╗ ████║██╔══██╗██╔══██╗
██║  ███╗██████╔╝███████║██╔████╔██║██╔████╔██║███████║██████╔╝
██║   ██║██╔══██╗██╔══██║██║╚██╔╝██║██║╚██╔╝██║██╔══██║██╔══██╗
╚██████╔╝██║  ██║██║  ██║██║ ╚═╝ ██║██║ ╚═╝ ██║██║  ██║██║  ██║
 ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝

The grammar of Joy is very simple.  A Joy expression is zero or more Joy
terms (separated by blanks, see below) and terms can be
integers, Booleans, quoted Joy expressions, or symbols (names of
functions.)

    joy ::= term*

    term ::= integer | bool | '[' joy ']' | symbol

    integer ::= [ '-' | '+' ] ('0'...'9')+
    bool ::= 'true' | 'false'
    symbol ::= char+

    char ::= <Any non-space other than '[' and ']'.>

There are a few wrinkles in the handling of blank space between terms
because we want to be able to omit it around brackets:

Valid expressions:

    1 2 3
    1[2]3
    1 [ 2 ] 3
    true
    truedat  (a symbol prefixed with the name of a boolean)

Invalid:

    12three  (symbols can't start with numbers, and this shouldn't parse
              as [12 three].)

Symbols can be made of any non-blank characters except '['and ']' which
are fully reserved for list literals (aka "quotes"). 'true' and 'false'
would be valid symbols but they are reserved for Boolean literals.

Integers are converted to Prolog integers, symbols and bools to Prolog
atoms, and list literals to Prolog lists.

For now strings are neglected in favor of lists of numbers.  (But there's
no support for parsing string notation and converting to lists of ints.)

First lex the stream of codes into tokens separated by square brackets
or whitespace.  We keep the brackets and throw away the blanks.
*/

joy_lex([tok(Token)|Ls]) --> chars(Token), !, joy_lex(Ls).
joy_lex([  lbracket|Ls]) --> "[",          !, joy_lex(Ls).
joy_lex([  rbracket|Ls]) --> "]",          !, joy_lex(Ls).

joy_lex(Ls) --> [Space], {code_type(Space, space)}, !, joy_lex(Ls).

joy_lex([]) --> [].

% Then parse the tokens converting them to Prolog values and building up
% the list structures (if any.)

joy_parse([J|Js]) --> joy_term(J), !, joy_parse(Js).
joy_parse([]) --> [].

joy_term(list(J)) --> [lbracket], !, joy_parse(J), [rbracket].
joy_term(Atomic) --> [tok(Codes)], {joy_token(Atomic, Codes)}.

joy_token(int(I), Codes) :- number(I, Codes, []), !.  % See dcg/basics.
joy_token(bool(true), `true`) :- !.
joy_token(bool(false), `false`) :- !.
joy_token(symbol(S), Codes) :- atom_codes(S, Codes).


text_to_expression(Text, Expression) :-
    phrase(joy_lex(Tokens), Text), !,
    phrase(joy_parse(Expression), Tokens).

% Apologies for all the (green, I hope) cuts.  The strength of the Joy
% syntax is that it's uninteresting.

chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> [Ch], {Ch \== 0'[, Ch \== 0'], code_type(Ch, graph)}.


/* Here is an example of Joy code:

    [   [[abs] ii <=]
        [
            [<>] [pop !-] ||
        ] &&
    ]
    [[    !-] [[++]] [[--]] ifte dip]
    [[pop !-]  [--]   [++]  ifte    ]
    ifte

It probably seems unreadable but with a little familiarity it becomes
just as legible as any other notation.  This function accepts two
integers on the stack and increments or decrements one of them such that
the new pair of numbers is the next coordinate pair in a square spiral
(like that used to construct an Ulam Spiral).  It is adapted from the
code in the answer here:

https://stackoverflow.com/questions/398299/looping-in-a-spiral/31864777#31864777

It can be used with the x combinator to make a kind of generator for
spiral square coordinates.



███████╗███████╗███╗   ███╗ █████╗ ███╗   ██╗████████╗██╗ ██████╗███████╗
██╔════╝██╔════╝████╗ ████║██╔══██╗████╗  ██║╚══██╔══╝██║██╔════╝██╔════╝
███████╗█████╗  ██╔████╔██║███████║██╔██╗ ██║   ██║   ██║██║     ███████╗
╚════██║██╔══╝  ██║╚██╔╝██║██╔══██║██║╚██╗██║   ██║   ██║██║     ╚════██║
███████║███████╗██║ ╚═╝ ██║██║  ██║██║ ╚████║   ██║   ██║╚██████╗███████║
╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝ ╚═════╝╚══════╝

The fundamental Joy relation involves an expression and two stacks.  One
stack serves as input and the other as output.

    thun(Expression, InputStack, OutputStack)

The null expression (denoted by an empty Prolog list) is effectively an
identity function and serves as the end-of-processing marker.  As a
matter of efficiency (of Prolog) the thun/3 predicate picks off the first
term of the expression (if any) and passes it to thun/4 which can then
take advantage of Prolog indexing on the first term of a predicate. */

thun([], S, S).
thun([Term|E], Si, So) :- thun(Term, E, Si, So).

/* The thun/4 predicate was originally written in terms of the thun/3
predicate, which was very elegant, but prevented (I assume but have not
checked) tail-call recursion.  In order to alleviate this, partial
reduction is used to generate the actual thun/4 rules, see below.

Original thun/4 code:

thun(int(I),        E, Si, So) :- thun(E, [ int(I)|Si], So).
thun(bool(B),       E, Si, So) :- thun(E, [bool(B)|Si], So).
thun(list(L),       E, Si, So) :- thun(E, [list(L)|Si], So).
thun(symbol(Def),   E, Si, So) :- def(Def, Body), append(Body, E, Eo), thun(Eo, Si, So).
thun(symbol(Func),  E, Si, So) :- func(Func, Si, S),                   thun(E,  S,  So).
thun(symbol(Combo), E, Si, So) :- combo(Combo, Si, S, E, Eo),          thun(Eo, S,  So).

Integers, Boolean values, and lists are put onto the stack, symbols are
dispatched to one of three kinds of processing: functions, combinators
and definitions (see "defs.txt".) */

thun(A,    [], S, [A|S]) :- var(A), !.
thun(A, [T|E], S,   So)  :- var(A), !, thun(T, E, [A|S], So).

% Literals turn out okay.

thun(int(A),    [], B, [int(A)|B]).
thun(int(C), [A|B], D, E) :- thun(A, B, [int(C)|D], E).

thun(bool(A),    [], B, [bool(A)|B]).
thun(bool(C), [A|B], D, E) :- thun(A, B, [bool(C)|D], E).

thun(list(A),    [], B, [list(A)|B]).
thun(list(C), [A|B], D, E) :- thun(A, B, [list(C)|D], E).

% Partial reduction works for func/3 cases.

thun(symbol(A),    [], B, C) :- func(A, B, C).
thun(symbol(A), [C|D], B, F) :- func(A, B, E), thun(C, D, E, F).

% Combinators look ok too.

% thun(symbol(A), D, B, C) :- combo(A, B, C, D, []).
% thun(symbol(A), C, B, G) :- combo(A, B, F, C, [D|E]), thun(D, E, F, G).

% However, in this case, I think the original version will be more
% efficient.

thun(symbol(Combo), E, Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So).

% In the reduced rules Prolog will redo all the work of the combo/5
% predicate on backtracking through the second rule.  It will try
% combo/5, which usually won't end in Eo=[] so the first rule fails, then
% it will try combo/5 again in the second rule.  In the original form
% after combo/5 has completed Prolog has computed Eo and can index on it
% for thun/3.
%
% Neither functions nor definitions can affect the expression so this
% consideration doesn't apply to those rules.  The unification of the
% head clauses will distinguish the cases for them.

% Definitions don't work though (See "Partial Reducer" section below.)
% I hand-wrote the def/3 cases here.

thun(symbol(D),     [], Si, So) :- def(D, [DH| E]), thun(DH, E, Si, So).
thun(symbol(D), [H|E0], Si, So) :- def(D, [DH|DE]),
     append(DE, [H|E0], E), /* ................. */ thun(DH, E, Si, So).

% Partial reduction has been the subject of a great deal of research and
% I'm sure there's a way to make definitions work, but it's beyond the
% scope of the project at the moment.  It works well enough as-is that I'm
% happy to manually write out two rules by hand.

% Some error handling.

thun(symbol(Unknown), _, _, _) :-
    \+ def(Unknown, _),
    \+ func(Unknown, _, _),
    \+ combo(Unknown, _, _, _, _),
    write("Unknown: "),
    writeln(Unknown),
    fail.

/*

███████╗██╗   ██╗███╗   ██╗ ██████╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
██╔════╝██║   ██║████╗  ██║██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
█████╗  ██║   ██║██╔██╗ ██║██║        ██║   ██║██║   ██║██╔██╗ ██║███████╗
██╔══╝  ██║   ██║██║╚██╗██║██║        ██║   ██║██║   ██║██║╚██╗██║╚════██║
██║     ╚██████╔╝██║ ╚████║╚██████╗   ██║   ██║╚██████╔╝██║ ╚████║███████║
╚═╝      ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝

*/

func(words, S, [Words|S]) :- words(Words).

func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).

func(cons,   [list(A),      B |S], [list([B|A])|S]).
func(concat, [list(A), list(B)|S],     [list(C)|S]) :- append(B, A, C).
func(flatten,   [list(A)|S],   [list(B)|S]) :- flatten(A, B).
func(swaack,    [list(R)|S],   [list(S)|R]).
func(stack,              S ,   [list(S)|S]).
func(clear,              _ ,            []).
func(first, [list([X|_])|S],   [     X |S]).
func(rest,  [list([_|X])|S],   [list(X)|S]).
func(unit, [X|S], [list([X])|S]).

func(rolldown, [A, B, C|S], [B, C, A|S]).
func(dupd,        [A, B|S], [A, B, B|S]).
func(over,        [A, B|S], [B, A, B|S]).
func(tuck,        [A, B|S], [A, B, A|S]).
func(dupdd, [A, B, C|D], [A, B, C, C|D]).

% func(stackd, [A|B], [A, list(B)|B]).  % Doesn't compile.

func(shift, [list([B|A]), list(C)|D], [list(A), list([B|C])|D]).

func(rollup, Si, So) :- func(rolldown, So, Si).
func(uncons, Si, So) :- func(cons, So, Si).

func(bool, [     int(0)|S], [bool(false)|S]).
func(bool, [   list([])|S], [bool(false)|S]).
func(bool, [bool(false)|S], [bool(false)|S]).

func(bool, [     int(N)|S], [bool(true)|S]) :- N #\= 0.
func(bool, [list([_|_])|S], [bool(true)|S]).
func(bool, [ bool(true)|S], [bool(true)|S]).
% func(bool, [A|S], [bool(true)|S]) :- \+ func(bool, [A], [bool(false)]).

func('empty?', [    list([])|S], [ bool(true)|S]).
func('empty?', [ list([_|_])|S], [bool(false)|S]).

func('list?', [  list(_)|S], [ bool(true)|S]).
func('list?', [  bool(_)|S], [bool(false)|S]).
func('list?', [   int(_)|S], [bool(false)|S]).
func('list?', [symbol(_)|S], [bool(false)|S]).

func('one-or-more?', [list([_|_])|S], [ bool(true)|S]).
func('one-or-more?', [   list([])|S], [bool(false)|S]).

func(and, [bool(true),   bool(true)|S], [ bool(true)|S]).
func(and, [bool(true),  bool(false)|S], [bool(false)|S]).
func(and, [bool(false),  bool(true)|S], [bool(false)|S]).
func(and, [bool(false), bool(false)|S], [bool(false)|S]).

func(or,  [bool(true),   bool(true)|S], [ bool(true)|S]).
func(or,  [bool(true),  bool(false)|S], [ bool(true)|S]).
func(or,  [bool(false),  bool(true)|S], [ bool(true)|S]).
func(or,  [bool(false), bool(false)|S], [bool(false)|S]).

func( + ,  [int(A), int(B)|S], [int(A + B)|S]).
func( - ,  [int(A), int(B)|S], [int(B - A)|S]).
func( * ,  [int(A), int(B)|S], [int(A * B)|S]).
func( / ,  [int(A), int(B)|S], [int(B div A)|S]).
func('%',  [int(A), int(B)|S], [int(B mod A)|S]).
% func( + ,  [int(A), int(B)|S], [int(C)|S]) :- C #= A + B.
% func( - ,  [int(A), int(B)|S], [int(C)|S]) :- C #= B - A.
% func( * ,  [int(A), int(B)|S], [int(C)|S]) :- C #= A * B.
% func( / ,  [int(A), int(B)|S], [int(C)|S]) :- C #= B div A.
% func('%',  [int(A), int(B)|S], [int(C)|S]) :- C #= B mod A.

func('/%', [int(A), int(B)|S], [int(B div A), int(B mod A)|S]).
func( pm , [int(A), int(B)|S], [int(A + B), int(B - A)|S]).
% func('/%', [int(A), int(B)|S], [int(C), int(D)|S]) :- C #= B div A, D #= B mod A.
% func( pm , [int(A), int(B)|S], [int(C), int(D)|S]) :- C #= A + B,   D #= B - A.

func(>,  [int(A), int(B)|S], [    bool(B > A)|S]).
func(<,  [int(A), int(B)|S], [    bool(B < A)|S]).
func(=,  [int(A), int(B)|S], [ bool(eq(B, A))|S]).
func(>=, [int(A), int(B)|S], [   bool(B >= A)|S]).
func(<=, [int(A), int(B)|S], [   bool(B =< A)|S]).
func(<>, [int(A), int(B)|S], [bool(neq(B, A))|S]).
% func(>,  [int(A), int(B)|S], [T|S]) :- B #> A #<==> R, r_truth(R, T).
% func(<,  [int(A), int(B)|S], [T|S]) :- B #< A #<==> R, r_truth(R, T).
% func(=,  [int(A), int(B)|S], [T|S]) :- B #= A #<==> R, r_truth(R, T).
% func(>=, [int(A), int(B)|S], [T|S]) :- B #>= A #<==> R, r_truth(R, T).
% func(<=, [int(A), int(B)|S], [T|S]) :- B #=< A #<==> R, r_truth(R, T).
% func(<>, [int(A), int(B)|S], [T|S]) :- B #\= A #<==> R, r_truth(R, T).

func(sqr) --> func(dup), func(mul).  % Pretty neat.

r_truth(0, bool(false)).
r_truth(1, bool(true)).


/*

 ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗███╗   ██╗ █████╗ ████████╗ ██████╗ ██████╗ ███████╗
██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║████╗  ██║██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝
██║     ██║   ██║██╔████╔██║██████╔╝██║██╔██╗ ██║███████║   ██║   ██║   ██║██████╔╝███████╗
██║     ██║   ██║██║╚██╔╝██║██╔══██╗██║██║╚██╗██║██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║
╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝██║██║ ╚████║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║
 ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝

*/

combo(i,          [list(P)|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [list(P), X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [list(P), X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).

combo(dupdip, [list(P), X|S], [X|S], Ei, Eo) :- append(P, [X|Ei], Eo).

combo(branch, [list(T), list(_),  bool(true)|S], S, Ei, Eo) :- append(T, Ei, Eo).
combo(branch, [list(_), list(F), bool(false)|S], S, Ei, Eo) :- append(F, Ei, Eo).

combo(loop, [list(_), bool(false)|S], S, E,  E ).
combo(loop, [list(B),  bool(true)|S], S, Ei, Eo) :- append(B, [list(B), symbol(loop)|Ei], Eo).

combo(step, [list(_),    list([])|S],    S,  E,  E ).
combo(step, [list(P), list([X|Z])|S], [X|S], Ei, Eo) :- append(P, [list(Z), list(P), symbol(step)|Ei], Eo).

combo(times, [list(_), int(0)|S], S, E,  E ).
combo(times, [list(P), int(1)|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(times, [list(P), int(N)|S], S, Ei, Eo) :- N #>= 2, M #= N - 1, append(P, [int(M), list(P), symbol(times)|Ei], Eo).
combo(times, [list(_), int(N)|S], S, _,  _ ) :- N #< 0, fail.

combo(genrec, [R1, R0, Then, If|S],
              [  Else, Then, If|S], E, [symbol(ifte)|E]) :-
    append(R0, [list([If, Then, R0, R1, symbol(genrec)])|R1], Else).

/*
This is a crude but servicable implementation of the map combinator.

Obviously it would be nice to take advantage of the implied parallelism.
Instead the quoted program, stack, and terms in the input list are
transformed to simple Joy expressions that run the quoted program on
prepared copies of the stack that each have one of the input terms on
top.  These expressions are collected in a list and the whole thing is
evaluated (with infra) on an empty list, which becomes the output list.

The chief advantage of doing it this way (as opposed to using Prolog's
map) is that the whole state remains in the pending expression, so
there's nothing stashed in Prolog's call stack.  This preserves the nice
property that you can interrupt the Joy evaluation and save or transmit
the stack+expression knowing that you have all the state.
*/

combo(map, [list(_),   list([])|S],               [list([])|S], E,                E ) :- !.
combo(map, [list(P), list(List)|S], [list(Mapped), list([])|S], E, [symbol(infra)|E]) :-
    prepare_mapping(list(P), S, List, Mapped).

% Set up a program for each term in ListIn
%
%     [term S] [P] infrst
%
% prepare_mapping(P, S, ListIn, ListOut).

prepare_mapping(Pl, S, In, Out) :- prepare_mapping(Pl, S, In, [], Out).

prepare_mapping(    _,  _,     [],                                  Out,  Out) :- !.
prepare_mapping(    Pl, S, [T|In],                                  Acc,  Out) :-
    prepare_mapping(Pl, S,    In,  [list([T|S]), Pl, symbol(infrst)|Acc], Out).


/*

██████╗ ███████╗███████╗██╗███╗   ██╗██╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
██╔══██╗██╔════╝██╔════╝██║████╗  ██║██║╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
██║  ██║█████╗  █████╗  ██║██╔██╗ ██║██║   ██║   ██║██║   ██║██╔██╗ ██║███████╗
██║  ██║██╔══╝  ██╔══╝  ██║██║╚██╗██║██║   ██║   ██║██║   ██║██║╚██╗██║╚════██║
██████╔╝███████╗██║     ██║██║ ╚████║██║   ██║   ██║╚██████╔╝██║ ╚████║███████║
╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝

*/

joy_def(Codes) :-
    text_to_expression(Codes, [symbol(Name)|Body]),
    % writeln(Name),
    assert_def(Name, Body).

assert_defs(DefsFile) :-
    read_file_to_codes(DefsFile, Codes, []),
    lines(Codes, Lines),
    maplist(joy_def, Lines).

assert_def(Symbol, Body) :-
    (  % Don't let this "shadow" functions or combinators.
        \+ func(Symbol, _, _),
        \+ combo(Symbol, _, _, _, _)
    ) -> (  % Replace any existing defs of this name.
        retractall(def(Symbol, _)),
        assertz(def(Symbol, Body))
    ) ; true.

% Split on newline chars a list of codes into a list of lists of codes
% one per line.  Helper function.
lines([], []) :- !.
lines(Codes, [Line|Lines]) :- append(Line, [0'\n|Rest], Codes), !, lines(Rest, Lines).
lines(Codes, [Codes]).

:- assert_defs("defs.txt").


% A meta function that finds the names of all available functions.

words(Words) :-
    findall(Name, clause(func(Name, _, _), _), Funcs),
    findall(Name, clause(combo(Name, _, _, _, _), _), Combos, Funcs),
    findall(Name, clause(def(Name, _), _), Words0, Combos),
    list_to_set(Words0, Words1),
    sort(Words1, Words).


/*

 ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗██╗     ███████╗██████╗
██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║██║     ██╔════╝██╔══██╗
██║     ██║   ██║██╔████╔██║██████╔╝██║██║     █████╗  ██████╔╝
██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║██║     ██╔══╝  ██╔══██╗
╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║███████╗███████╗██║  ██║
 ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
  _         ___      _   _
 | |_ ___  | _ \_  _| |_| |_  ___ _ _
 |  _/ _ \ |  _/ || |  _| ' \/ _ \ ' \
  \__\___/ |_|  \_, |\__|_||_\___/_||_|
                |__/


We have a tabulator predicate.

*/

tabs(N) --> { N #> 0, M #= N - 1 },
    tab, tabs(M).

tabs(0) --> [].

nl --> "\n".

tab --> "    ".


/*

Convert Prolog terms to Python source.

 */

% stack_to_python(F) --> { writeln(F), fail }.

stack_to_python(S) --> {atom(S), !, atom_codes(S, C)}, C.
stack_to_python([]) --> "stack", !.
stack_to_python([Term|Tail]) -->
    "(", term_to_python(Term), ", ", stack_to_python(Tail), ")".


% Unify unbound terms with fresh Python identifiers.
pyvar(Prefix, Term, Codes) :-
    ( var(Term) -> gensym(Prefix, Term) ; atom(Term) ),
    atom_codes(Term, Codes).

term_to_python(Term) -->
    { pyvar(v, Term, Var) }, !, Var.

term_to_python(bool(Term)) --> term_to_python(Term).

term_to_python(int(Term)) -->
    { ( integer(Term) ->
        number_codes(Term, Int)
      ;
        pyvar(i, Term, Int)
      )
    },
    Int.

term_to_python(list(Term)) --> list_to_python(Term).

term_to_python(Term) --> Term.


list_to_python(Term) -->
    { pyvar(s, Term, Var) }, !, Var.

list_to_python([]) --> "()", !.

list_to_python([Term|Tail]) -->
    "(", term_to_python(Term), ", ", list_to_python(Tail), ")".



/*

Generate Python code.

 */


code_gen([Head|Tail]) --> Head, code_gen(Tail).
code_gen([]) --> [].

cg, Term --> [Term], cg.
cg --> [].

compile_fn(Name) --> gronk_fn(Name), cg, !.




/*


 ██████╗ ██████╗  ██████╗ ███╗   ██╗██╗  ██╗
██╔════╝ ██╔══██╗██╔═══██╗████╗  ██║██║ ██╔╝
██║  ███╗██████╔╝██║   ██║██╔██╗ ██║█████╔╝
██║   ██║██╔══██╗██║   ██║██║╚██╗██║██╔═██╗
╚██████╔╝██║  ██║╚██████╔╝██║ ╚████║██║  ██╗
 ╚═════╝ ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═╝

(GRONK stands for "I am bad at naming things.")

With gronk we're juggling four things:

    The incoming joy expression
    The outgoing code tokens (for the code gen)
    The incoming stack representation
    and outgoing stack representation

The basic formula is like so (the indent level is an implementation
detail):

gronk_fn_body(
    [joy expression]
    StackIn,
    StackOut,
    [code gen tokens]
    ).

(Let's leave out DCGs for now, eh?  Since I don't actually know how they
work really yet, do I?  ;P )

*/

gronk_fn(Name, Expr, CodeGens)
    :-
    CodeGens = ["def ", Name,"(stack, expression, dictionary):", nl,
                    tab, stack_to_python(StackIn), " = stack", nl|Cs],
    CGTail = [tab, "return ", stack_to_python(StackOut), ", expression, dictionary", nl],
    reset_gensym(s), reset_gensym(v), reset_gensym(i),
    gronk_fn_list(Expr, StackIn, StackOut, CGTail, Cs, 1).


gronk_fn_list(
    [list(BodyFalse), list(BodyTrue), symbol(branch)|Js],
    [bool(B)|StackIn],
    StackOut,
    CGTail,
    CodeGens,
    IndentLevel)
    :-
    !,
    J #= IndentLevel + 1,
    CodeGens = [
        tabs(IndentLevel), "if ", term_to_python(B), ":", nl|Cs0],
    True =  [tabs(J), stack_to_python(Stack), " = ", stack_to_python(StackT), nl,
        tabs(IndentLevel), "else:", nl|Cs1],
    False = [tabs(J), stack_to_python(Stack), " = ", stack_to_python(StackF), nl|Ck],
    gronk_fn_list(BodyTrue, StackIn, StackT, True, Cs0, J),
    gronk_fn_list(BodyFalse, StackIn, StackF, False, Cs1, J),
    gronk_fn_list(Js, Stack, StackOut, CGTail, Ck, IndentLevel).

gronk_fn_list(
    [list(Body), symbol(loop)|Js],
    [bool(B)|StackIn],
    StackOut,
    CGTail,
    CodeGens,
    IndentLevel)
    :-
    !,
    J #= IndentLevel + 1,
    CodeGens = [
        tabs(IndentLevel), term_to_python(Tos), " = ", term_to_python(B), nl,
        tabs(IndentLevel), "while ", term_to_python(Tos), ":", nl|Cs
        ],
    gronk_fn_list(Body, StackIn, [bool(Tos)|Stack], [tabs(J), stack_to_python(StackIn), " = ", stack_to_python(Stack), nl|Ck], Cs, J),
    gronk_fn_list(Js, StackIn, StackOut, CGTail, Ck, IndentLevel).
                    % ^^^^^^^  wha!? not Stack!?

gronk_fn_list(
    [list(Body), symbol(dip)|Js],
    [Term|StackIn],
    StackOut,
    CGTail,
    Cs,
    IndentLevel)
    :-
    !,
    gronk_fn_list(Body,      StackIn,    Stack,     Ck, Cs, IndentLevel),
    gronk_fn_list(Js,   [Term|Stack], StackOut, CGTail, Ck, IndentLevel).

gronk_fn_list(
    [symbol(step)|Js],
    [list(Body), list(B)|Stack0],
    Stack,
    CGTail,
    CodeGens,
    IndentLevel)
    :-
    !,
    J #= IndentLevel + 1,
    CodeGens = [
        tabs(IndentLevel), stack_to_python(Stack1), " = ", stack_to_python(Stack0), nl,
        tabs(IndentLevel), "while ", term_to_python(B), ":", nl,
            tabs(J), "(", term_to_python(T), ", ", term_to_python(B), ") = ", term_to_python(B), nl|CG2
        ],
    CG1 = [tabs(J), stack_to_python(Stack1), " = ", stack_to_python(Stack2), nl|CG0],
    gronk_fn_list(Body, [T|Stack1], Stack2, CG1, CG2, J),
    gronk_fn_list(Js, Stack1, Stack, CGTail, CG0, IndentLevel).

gronk_fn_list(
    [symbol(abs)|Js],
    [In|StackIn],
    StackOut,
    CGTail,
    [tabs(IndentLevel), term_to_python(Out), " = abs(", term_to_python(In), ")", nl|Cs],
    IndentLevel)
    :-
    !,  % green cut
    gronk_fn_list(Js, [Out|StackIn], StackOut, CGTail, Cs, IndentLevel).

gronk_fn_list(
    [symbol(bool)|Js],
    [In|StackIn],
    StackOut,
    CGTail,
    [tabs(IndentLevel), term_to_python(Out), " = bool(", term_to_python(In), ")", nl|Cs],
    IndentLevel)
    :-
    !,  % green cut
    gronk_fn_list(Js, [bool(Out)|StackIn], StackOut, CGTail, Cs, IndentLevel).

gronk_fn_list(
    [symbol(stack)|Js],
    StackIn,
    StackOut,
    CGTail,
    [tabs(IndentLevel), stack_to_python(Stack), " = (", stack_to_python(StackIn), ", ", stack_to_python(StackIn), ")", nl|Cs],
    IndentLevel)
    :-
    !,  % green cut
    gronk_fn_list(Js, Stack, StackOut, CGTail, Cs, IndentLevel).

gronk_fn_list(
    [symbol(swaack)|Js],
    [list(S)|StackIn],
    StackOut,
    CGTail,
    % [tabs(IndentLevel), "pass", nl|Cs],
    [tabs(IndentLevel), stack_to_python(Stack), " = (", stack_to_python(StackIn), ", ", stack_to_python(S), ")", nl|Cs],
    IndentLevel)
    :-
    !,  % green cut
    gronk_fn_list(Js, Stack, StackOut, CGTail, Cs, IndentLevel).

gronk_fn_list(
    [symbol(Sym)|Js],
    [int(B), int(A)|StackIn],
    StackOut,
    CGTail,
    [tabs(IndentLevel), term_to_python(int(C)), " = ", term_to_python(int(A)), Op, term_to_python(int(B)), nl|Cs],
    IndentLevel)
    :-
    bin_math_op(Sym, Op), !,  % green cut
    gronk_fn_list(Js, [int(C)|StackIn], StackOut, CGTail, Cs, IndentLevel).

gronk_fn_list(
    [symbol(Sym)|Js],
    [int(B), int(A)|StackIn],
    StackOut,
    CGTail,
    [tabs(IndentLevel), term_to_python(bool(C)), " = ", term_to_python(int(A)), Op, term_to_python(int(B)), nl|Cs],
    IndentLevel)
    :-
    bin_bool_op(Sym, Op), !,  % green cut
    gronk_fn_list(Js, [bool(C)|StackIn], StackOut, CGTail, Cs, IndentLevel).

gronk_fn_list([symbol(Sym)|Js], S0, S, C0, C, IndentLevel) :-
    yin(Sym),
    func(Sym, S0, S1), !,  % green cut
    gronk_fn_list(Js, S1, S, C0, C, IndentLevel).

gronk_fn_list([symbol(Sym)|Js], S0, S, C0, C, IndentLevel) :-
    yin(Sym),
    def(Sym, Body), !,  % green cut
    append(Body, Js, Expr),
    gronk_fn_list(Expr, S0, S, C0, C, IndentLevel).

gronk_fn_list([bool(true)|Js], S0, S, C0, C, IndentLevel) :- !,  % green cut
    gronk_fn_list(Js, [bool("True")|S0], S, C0, C, IndentLevel).

gronk_fn_list([bool(false)|Js], S0, S, C0, C, IndentLevel) :- !,  % green cut
    gronk_fn_list(Js, [bool("False")|S0], S, C0, C, IndentLevel).

gronk_fn_list([int(I)|Js], S0, S, C0, C, IndentLevel) :- !,  % green cut
    gronk_fn_list(Js, [int(I)|S0], S, C0, C, IndentLevel).

gronk_fn_list([list(L)|Js], S0, S, C0, C, IndentLevel) :- !,  % green cut
    gronk_fn_list(Js, [list(L)|S0], S, C0, C, IndentLevel).

gronk_fn_list([], Stack, Stack, Cs, Cs, _).


bin_math_op(+, " + ").
bin_math_op(-, " - ").
bin_math_op(*, " * ").
bin_math_op(div, " // ").
bin_math_op( / , " // ").
bin_math_op(mod, " % ").
bin_math_op('%', " % ").

bin_bool_op(>, " > ").
bin_bool_op(<, " < ").
bin_bool_op(=, " == ").
bin_bool_op(>=, " >= ").
bin_bool_op(<=, " <= ").
bin_bool_op(<>, " != ").

yin(bool).
yin(cons).
yin(dip).
yin(dup).
yin(dupd).
yin(dupdd).
yin(first).
yin(gcd).
yin(over).
yin(pop).
yin(product).
yin(rest).
yin(rolldown).
yin(rollup).
yin(shift).
yin(step).
yin(stackd).
yin(sum).
yin(swap).
yin(tuck).
yin(uncons).
yin(unit).
yin(Sym) :- def(Sym, Body), maplist(yins, Body).

yins(int(_)).
yins(bool(_)).
yins(list(_)).

yins(symbol(Sym)) :- yin(Sym).
yins(symbol(Sym)) :- bin_math_op(Sym, _).
yins(symbol(Sym)) :- bin_bool_op(Sym, _).


/*
concat
flatten
swaack
clear
bool+

list ops (empty? list? ...)
logic ops (and or ...)

COMBINATORS

 */


gronk(Name, BodyText) :-
    text_to_expression(BodyText, Expr),
    gronk_fn(Name, Expr, Out),
    code_gen(Out, A, []), !,
    string_codes(S, A),
    writeln(""),
    writeln(S).






do :-
    gronk("abs", `abs`),
    gronk("ccons", `ccons`),
    gronk("cons", `cons`),
    gronk("decr", `--`),
    gronk("dup", `dup`),
    gronk("dupd", `dupd`),
    gronk("dupdd", `dupdd`),
    gronk("first", `first`),
    gronk("fourth", `fourth`),
    gronk("incr", `++`),
    gronk("non_negative", `!-`),
    gronk("pop", `pop`),
    gronk("popd", `popd`),
    gronk("popop", `popop`),
    gronk("popopd", `popopd`),
    gronk("quoted", `quoted`),
    gronk("reco", `reco`),
    gronk("rest", `rest`),
    gronk("rrest", `rrest`),
    gronk("second", `second`),
    gronk("shift", `shift`),
    gronk("sqr", `sqr`),
    gronk("stackd", `stackd`),  % Compiling func(stackd, ...) doesn't work.
    gronk("swons", `swons`),
    gronk("third", `third`),
    gronk("truthy", `?`),
    gronk("tuckl", `<{}`),
    gronk("tuckld", `<<{}`),
    gronk("uncons", `uncons`),
    gronk("unit", `unit`),
    gronk("unswons", `unswons`),
    gronk("gcd", `gcd`),
    gronk("sum", `sum`),
    gronk("product", `product`),
    writeln("").
