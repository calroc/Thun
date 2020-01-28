﻿/* 

████████╗██╗  ██╗██╗   ██╗███╗   ██╗
╚══██╔══╝██║  ██║██║   ██║████╗  ██║
   ██║   ███████║██║   ██║██╔██╗ ██║
   ██║   ██╔══██║██║   ██║██║╚██╗██║
   ██║   ██║  ██║╚██████╔╝██║ ╚████║
   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

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


(Big fonts are from Figlet ANSI Shadow http://www.patorjk.com/software/taag/#p=display&f=ANSI%20Shadow&t=formatter )

 */

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- dynamic func/3.
:- dynamic def/2.


/*
An entry point.
*/

joy(InputString, StackIn, StackOut) :-
    phrase(joy_parse(Expression), InputString), !,
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
terms separated by blanks, and terms can be either integers, Booleans,
quoted Joy expressions, or symbols (names of functions.)

    joy ::= ( blanks term blanks )*

    term ::= integer | bool | '[' joy ']' | symbol

    integer ::= [ '-' | '+' ] ('0'...'9')+
    bool ::= 'true' | 'false' 
    symbol ::= char+

    char ::= <Any non-space other than '[' and ']'.>
    blanks ::= <Zero or more whitespace characters.>

For integer//1 and blanks//0 I delegate to SWI's dcg/basics library.  The
blank//0 matches and discards space and newline characters and integer//1
"processes an optional sign followed by a non-empty sequence of digits
into an integer." (https://www.swi-prolog.org/pldoc/man?section=basics)

Symbols can be made of any non-blank characters except '['and ']' which
are fully reserved for list literals (aka "quotes"). 'true' and 'false'
would be valid symbols but they are reserved for Boolean literals.

For now strings are neglected in favor of lists of numbers.  (But there's
no support for parsing string notation and converting to lists of ints.)

One wrinkle of the grammar is that numbers do not need to be followed by
blanks before the next match, which is nice when the next match is a
square bracket but a little weird when it's a symbol term.  E.g. "2[3]"
parses as [2, [3]] but "23x" parses as [23, x].  It's a minor thing not
worth disfiguring the grammar to change IMO.

Integers are converted to Prolog integers, symbols and bools to Prolog
atoms, and list literals to Prolog lists.

*/

joy_parse([J|Js]) --> blanks, joy_term(J), blanks, joy_parse(Js).
joy_parse([]) --> [].

joy_term(int(I)) --> integer(I), !.
joy_term(list(J)) --> "[", !, joy_parse(J), "]".
joy_term(bool(true)) --> "true", !.
joy_term(bool(false)) --> "false", !.
joy_term(symbol(S)) --> symbol(S).

symbol(C) --> chars(Chars), !, {atom_string(C, Chars)}.

chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> [Ch], {Ch \== 91, Ch \== 93, code_type(Ch, graph)}.


/*

███████╗███████╗███╗   ███╗ █████╗ ███╗   ██╗████████╗██╗ ██████╗███████╗
██╔════╝██╔════╝████╗ ████║██╔══██╗████╗  ██║╚══██╔══╝██║██╔════╝██╔════╝
███████╗█████╗  ██╔████╔██║███████║██╔██╗ ██║   ██║   ██║██║     ███████╗
╚════██║██╔══╝  ██║╚██╔╝██║██╔══██║██║╚██╗██║   ██║   ██║██║     ╚════██║
███████║███████╗██║ ╚═╝ ██║██║  ██║██║ ╚████║   ██║   ██║╚██████╗███████║
╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝ ╚═════╝╚══════╝


thun(Expression, InputStack, OutputStack)

*/

thun([], S, S).
thun([Term|E], Si, So) :- thun(Term, E, Si, So).

/*  Original code.

    thun( int(I), E, Si, So) :- thun(E, [ int(I)|Si], So).
    thun(bool(B), E, Si, So) :- thun(E, [bool(B)|Si], So).
    thun(list(L), E, Si, So) :- thun(E, [list(L)|Si], So).
    thun(symbol(Def),   E, Si, So) :- def(Def, Body), append(Body, E, Eo), thun(Eo, Si, So).
    thun(symbol(Func),  E, Si, So) :- func(Func, Si, S), thun(E, S, So).
    thun(symbol(Combo), E, Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So).

Partial reduction is used to generate the actual thun/4 rules, see below. */

% Literals turn out okay.

thun(int(A),    [], B, [int(A)|B]).
thun(int(C), [A|B], D, E) :- thun(A, B, [int(C)|D], E).

thun(bool(A),    [], B, [bool(A)|B]).
thun(bool(C), [A|B], D, E) :- thun(A, B, [bool(C)|D], E).

thun(list(A),    [], B, [list(A)|B]).
thun(list(C), [A|B], D, E) :- thun(A, B, [list(C)|D], E).

% I hand-wrote def/3 cases here.
thun(symbol(B),     [],  A,  D) :- def(B, [DH|DE]), thun(DH, DE, A, D).
thun(symbol(A), [H|E0], Si, So) :- def(A, [DH|DE]),
     append(DE, [H|E0], E), /* ................. */ thun(DH, E, Si, So).

% Partial reduction for func/3 cases works too,
thun(symbol(A),    [], B, C) :- func(A, B, C).
thun(symbol(A), [C|D], B, F) :- func(A, B, E), thun(C, D, E, F).

% ...but Combo gets all messed up.
% thun(symbol(A), D, B, C) :- combo(A, B, C, D, []).
% thun(symbol(A), C, B, G) :- combo(A, B, F, C, [D|E]), thun(D, E, F, G).

thun(symbol(Combo), Ei, Si, So) :- combo(Combo, Si, S, Ei, Eo), thun(Eo, S, So).

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

func( + ,  [int(A), int(B)|S], [int(C)|S]) :- C #= A + B.
func( - ,  [int(A), int(B)|S], [int(C)|S]) :- C #= B - A.
func( * ,  [int(A), int(B)|S], [int(C)|S]) :- C #= A * B.
func( / ,  [int(A), int(B)|S], [int(C)|S]) :- C #= B div A.
func('%',  [int(A), int(B)|S], [int(C)|S]) :- C #= B mod A.

func('/%', [int(A), int(B)|S], [int(C), int(D)|S]) :- C #= A div B, D #= A mod B.
func( pm , [int(A), int(B)|S], [int(C), int(D)|S]) :- C #= A + B,   D #= B - A.

func(>,  [int(A), int(B)|S], [T|S]) :- B #> A #<==> R, r_truth(R, T).
func(<,  [int(A), int(B)|S], [T|S]) :- B #< A #<==> R, r_truth(R, T).
func(=,  [int(A), int(B)|S], [T|S]) :- B #= A #<==> R, r_truth(R, T).
func(>=, [int(A), int(B)|S], [T|S]) :- B #>= A #<==> R, r_truth(R, T).
func(<=, [int(A), int(B)|S], [T|S]) :- B #=< A #<==> R, r_truth(R, T).
func(<>, [int(A), int(B)|S], [T|S]) :- B #\= A #<==> R, r_truth(R, T).

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
              [  Else, Then, If|S], E, [ifte|E]) :-
    append(R0, [[If, Then, R0, R1, symbol(genrec)]|R1], Else).

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

joy_def --> joy_parse([symbol(Name)|Body]), { assert_def(Name, Body) }.

assert_defs(DefsFile) :-
    read_file_to_codes(DefsFile, Codes, []),
    lines(Codes, Lines),
    maplist(phrase(joy_def), Lines).

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
  _         ___         _           
 | |_ ___  | _ \_ _ ___| |___  __ _ 
 |  _/ _ \ |  _/ '_/ _ \ / _ \/ _` |
  \__\___/ |_| |_| \___/_\___/\__, |
                              |___/ 

This is an experimental compiler from Joy expressions to Prolog code.
As you will see it's also doing type inference and type checking.

For many Joy expressions the existing code is enough to "compile" them to
Prolog code.  E.g. the definition of 'third' is 'rest rest first' and
that's enough for the code to generate the "type" of the expression:

    ?- joy(`third`, Si, So).
    Si = [list([_32906, _32942, _32958|_32960])|_32898],
    So = [_32958|_32898] .

Because 'third' is just manipulating lists (the stack is a list too) the
type signature is the whole of the (Prolog) implementation of the
function:

    ?- sjc(third, `third`).
    func(third, [list([_, _, A|_])|B], [A|B]).

So that's nice.

Functions that involve just math require capturing the constraints
recorded by the CLP(FD) subsystem.  SWI Prolog provide a predicate
call_residue_vars/2 to do just that.  Together with copy_term/3 it's
possible to collect all the information needed to capture functions
made out of math and stack/list manipulation.  (I do not understand the
details of how they work.  Markus Triska said they would do the trick and
they did.)

https://www.swi-prolog.org/pldoc/doc_for?object=call_residue_vars/2

https://www.swi-prolog.org/pldoc/doc_for?object=copy_term/3

I think this is sort of like "gradual" or "dependent" types.  But the
formal theory there is beyond me.  In any event, it captures the integer
constraints established by the expressions as well as the "types" of
inputs and outputs.

    ?- sjc(fn, `* + * -`).
    func(fn, [int(H), int(I), int(F), int(D), int(C)|A], [int(B)|A]) :-
        maplist(call,
                
                [ clpfd:(B+E#=C),
                clpfd:(G*D#=E),
                clpfd:(J+F#=G),
                clpfd:(H*I#=J)
                ]).

For functions involving 'branch', compilation results in one rule for each
(reachable) path of the branch:

    ?- sjc(fn, `[+] [-] branch`).

    func(fn, [bool(true), int(C), int(D)|A], [int(B)|A]) :-
        maplist(call, [clpfd:(B+C#=D)]).

    func(fn, [bool(false), int(B), int(C)|A], [int(D)|A]) :-
        maplist(call, [clpfd:(B+C#=D)]).

(Note that in the subtraction case (bool(true)) the CLP(FD) constraints
are coded as addition but the meaning is the same (subtaction) because of
how the logic variables are named:  B + C #= D  <==>  B #= D - C.)

?- sjc(fn, `[[+] [-] branch] [pop *] branch`).

    func(fn, [bool(true), _, int(B), int(C)|A], [int(D)|A]) :-
        maplist(call, [clpfd:(B*C#=D)]).

    func(fn, [bool(false), bool(true), int(C), int(D)|A], [int(B)|A]) :-
        maplist(call, [clpfd:(B+C#=D)]).

    func(fn, [bool(false), bool(false), int(B), int(C)|A], [int(D)|A]) :-
        maplist(call, [clpfd:(B+C#=D)]).

Three paths, three rules.  Neat, eh?

That leaves loop, genrec, and x combinators...


*/

joy_compile(Name, Expression) :- jcmpl(Name, Expression, Rule), asserta(Rule).

show_joy_compile(Name, Expression) :- jcmpl(Name, Expression, Rule), portray_clause(Rule).

jcmpl(Name, Expression, Rule) :-
    call_residue_vars(thun(Expression, Si, So), Term),
    copy_term(Term, Term, Gs),
    Head =.. [func, Name, Si, So],
    rule(Head, Gs, Rule).

rule(Head, [],    Head).
rule(Head, [A|B], Head :- maplist(call, [A|B])).

sjc(Name, InputString) :- phrase(joy_parse(E), InputString), show_joy_compile(Name, E).


/*

Experiments with compilation.

?- sjc(fn, `[+ dup bool] loop`).

func(fn, [bool(false)|A], A).

func(fn, [bool(true), int(B), int(C)|A], [int(0)|A]) :-
    maplist(call, [clpfd:(B+C#=0)]).

func(fn, [bool(true), int(D), int(E), int(B)|A], [int(0)|A]) :-
    maplist(call,
            [ clpfd:(B in inf.. -1\/1..sup),
              clpfd:(C+B#=0),
              clpfd:(C in inf.. -1\/1..sup),
              clpfd:(D+E#=C)
            ]).

func(fn, [bool(true), int(F), int(G), int(D), int(B)|A], [int(0)|A]) :-
    maplist(call,
            [ clpfd:(B in inf.. -1\/1..sup),
              clpfd:(C+B#=0),
              clpfd:(C in inf.. -1\/1..sup),
              clpfd:(E+D#=C),
              clpfd:(E in inf.. -1\/1..sup),
              clpfd:(F+G#=E)
            ]).



?- sjc(fn, `[] loop`).

func(fn, [bool(false)|A], A).

func(fn, [bool(true), bool(false)|A], A).

func(fn, [bool(true), bool(true), bool(false)|A], A).

func(fn, [bool(true), bool(true), bool(true), bool(false)|A], A).

So...

    `[] loop` ::= true* false

sorta...


The quine '[[dup cons] dup cons]' works fine:

?- sjc(fn, `dup cons`).
func(fn, [list(A)|B], [list([list(A)|A])|B]).

?- sjc(fn, `[dup cons] dup cons`).
func(fn, A, [list([list([symbol(dup), symbol(cons)]), symbol(dup), symbol(cons)])|A]).

?- sjc(fn, `[dup cons] dup cons i`).
func(fn, A, [list([list([symbol(dup), symbol(cons)]), symbol(dup), symbol(cons)])|A]).

?- sjc(fn, `[dup cons] dup cons i i i i`).
func(fn, A, [list([list([symbol(dup), symbol(cons)]), symbol(dup), symbol(cons)])|A]).


In the right context the system will "hallucinate" programs:

?- sjc(fn, `x`).
func(fn, [list([])|A], [list([])|A]).

func(fn, [list([int(A)])|B], [int(A), list([int(A)])|B]).

func(fn, [list([bool(A)])|B], [bool(A), list([bool(A)])|B]).

func(fn, [list([list(A)])|B], [list(A), list([list(A)])|B]).

func(fn, [list([symbol(?)])|A], [bool(true), list([symbol(?)])|A]).

func(fn, [list([symbol(app1)]), list([]), A|B], [A, A|B]).

func(fn, [list([symbol(app1)]), list([int(A)]), B|C], [int(A), B|C]).

func(fn, [list([symbol(app1)]), list([bool(A)]), B|C], [bool(A), B|C]).

With iterative deepening this might be very interesting...


Infinite loops are infinite:

?- sjc(fn, `[x] x`).
ERROR: Out of global-stack.


?- sjc(fn, `sum`).
func(fn, [list([])|A], [int(0)|A]).

func(fn, [list([int(A)])|B], [int(A)|B]) :-
    maplist(call, [clpfd:(A in inf..sup)]).

func(fn, [list([int(C), int(B)])|A], [int(D)|A]) :-
    maplist(call, [clpfd:(B+C#=D)]).

func(fn, [list([int(E), int(D), int(B)])|A], [int(C)|A]) :-
    maplist(call,
            
            [ clpfd:(B+F#=C),
              clpfd:(D+E#=F)
            ]).

func(fn, [list([int(G), int(F), int(D), int(B)])|A], [int(C)|A]) :-
    maplist(call,
            
            [ clpfd:(B+E#=C),
              clpfd:(D+H#=E),
              clpfd:(F+G#=H)
            ]).


TODO: genrec, fix points.

 ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗██╗     ███████╗██████╗ 
██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║██║     ██╔════╝██╔══██╗
██║     ██║   ██║██╔████╔██║██████╔╝██║██║     █████╗  ██████╔╝
██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║██║     ██╔══╝  ██╔══██╗
╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║███████╗███████╗██║  ██║
 ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
  _         __  __         _    _                       _     
 | |_ ___  |  \/  |__ _ __| |_ (_)_ _  ___   __ ___  __| |___ 
 |  _/ _ \ | |\/| / _` / _| ' \| | ' \/ -_) / _/ _ \/ _` / -_)
  \__\___/ |_|  |_\__,_\__|_||_|_|_||_\___| \__\___/\__,_\___|
                                                              
This is an experimental compiler from Joy expressions to machine code.

One interesting twist is that Joy doesn't mention variables, just the
operators, so they have to be inferred from the ops.

So let's take e.g. '+'?

It seems we want to maintain a mapping from stack locations to registers,
and maybe from locations in lists on the stack, and to memory locations as
well as registers?

But consider 'pop', the register pointed to by stack_0 is put back in an
available register pool, but then all the stack_N mappings have to point
to stack_N+1 (i.e. stack_0 must now point to what stack_1 pointed to and
stack_1 must point to stack_2, and so on...)

What if we keep a stack of register/RAM locations in the same order as
the Joy stack?

Reference counting for registers?  Can it be avoided?

*/

get_reg(R, _) --> {gensym(r, R)}, [].
assoc_reg(_, _, _) --> [].

thun_compile([], S, S) --> [].
thun_compile([Term|Rest], Si, So) --> thun_compile(Term, Rest, Si, So).

thun_compile(int(I), E, Si, So) -->
    [mov_imm(R, int(I))],
    get_reg(R, _), assoc_reg(R, int(I), _),
    thun_compile(E, [R|Si], So).

thun_compile(bool(B), E, Si, So) -->
    get_reg(R, _), assoc_reg(R, bool(B), _),
    thun_compile(E, [R|Si], So).

thun_compile(list(L), E, Si, So) -->
    % encode_list(_), ???
    get_reg(R, _), assoc_reg(R, list(L), _),
    thun_compile(E, [R|Si], So).

thun_compile(symbol(Name), E, Si, So) -->   {def(Name, _)}, !,         def_compile(Name, E, Si, So).
thun_compile(symbol(Name), E, Si, So) -->  {func(Name, _, _)}, !,     func_compile(Name, E, Si, So).
thun_compile(symbol(Name), E, Si, So) --> {combo(Name, _, _, _, _)}, combo_compile(Name, E, Si, So).

% I'm going to assume that any defs that can be compiled to funcs already
% have been.  Defs that can't be pre-compiled shove their body expression
% onto the pending expression (continuation) to be compiled "inline".

def_compile(Def, E, Si, So) -->
    {def(Def, Body),
    append(Body, E, Eo)},
    thun_compile(Eo, Si, So).

% Functions delegate to a per-function compilation relation.

func_compile(+, E, [A, B|S], So) --> !,
    [add(B, A, B)],
    thun_compile(E, [B|S], So).


func_compile(Func, E, Si, So) -->
    % look up function, compile it...
    {Si = S},
    thun_compile(E, S, So).

combo_compile(Combo, E, Si, So) -->
    % look up combinator, compile it...
    {Si = S, E = Eo},
    thun_compile(Eo, S, So).

compiler(InputString, MachineCode, StackIn, StackOut) :-
    reset_gensym(r),
    phrase(joy_parse(Expression), InputString), !,
    phrase(thun_compile(Expression, StackIn, StackOut), MachineCode, []).


/* 

?- compiler(`1 2 +`, MachineCode, StackIn, StackOut).
MachineCode = [mov_imm(_18272, int(1)), mov_imm(_18298, int(2))],
StackOut = [_18298, _18272|StackIn].


- - - -


?- compiler(`1 2 +`, MachineCode, StackIn, StackOut).
MachineCode = [mov_imm(r1, int(1)), mov_imm(r2, int(2)), add(r1, r2, r1)],
StackOut = [r1|StackIn].

?- compiler(`1 2 +`, MachineCode, StackIn, StackOut).
MachineCode = [mov_imm(r1, int(1)), mov_imm(r2, int(2)), add(r1, r2, r1)],
StackOut = [r1|StackIn].

?- compiler(`1 2 + 3 +`, MachineCode, StackIn, StackOut).
MachineCode = [mov_imm(r1, int(1)), mov_imm(r2, int(2)), add(r1, r2, r1), mov_imm(r3, int(3)), add(r1, r3, r1)],
StackOut = [r1|StackIn].

?- compiler(`1 2 + +`, MachineCode, StackIn, StackOut).
MachineCode = [mov_imm(r1, int(1)), mov_imm(r2, int(2)), add(r1, r2, r1), add(_37848, r1, _37848)],
StackIn = StackOut, StackOut = [_37848|_37850].

?- compiler(`+ +`, MachineCode, StackIn, StackOut).
MachineCode = [add(_37270, _37264, _37270), add(_37688, _37270, _37688)],
StackIn = [_37264, _37270, _37688|_37690],
StackOut = [_37688|_37690].

?- compiler(`+ +`, MachineCode, [r1, r2, r3], StackOut).
MachineCode = [add(r2, r1, r2), add(r3, r2, r3)],
StackOut = [r3].

?- compiler(`+ +`, MachineCode, [r1, r2, r3, r4, r5, r6, r7], StackOut).
MachineCode = [add(r2, r1, r2), add(r3, r2, r3)],
StackOut = [r3, r4, r5, r6, r7].








███╗   ███╗███████╗████████╗ █████╗       ██████╗ ██████╗  ██████╗  ██████╗ ██████╗  █████╗ ███╗   ███╗███╗   ███╗██╗███╗   ██╗ ██████╗ 
████╗ ████║██╔════╝╚══██╔══╝██╔══██╗      ██╔══██╗██╔══██╗██╔═══██╗██╔════╝ ██╔══██╗██╔══██╗████╗ ████║████╗ ████║██║████╗  ██║██╔════╝ 
██╔████╔██║█████╗     ██║   ███████║█████╗██████╔╝██████╔╝██║   ██║██║  ███╗██████╔╝███████║██╔████╔██║██╔████╔██║██║██╔██╗ ██║██║  ███╗
██║╚██╔╝██║██╔══╝     ██║   ██╔══██║╚════╝██╔═══╝ ██╔══██╗██║   ██║██║   ██║██╔══██╗██╔══██║██║╚██╔╝██║██║╚██╔╝██║██║██║╚██╗██║██║   ██║
██║ ╚═╝ ██║███████╗   ██║   ██║  ██║      ██║     ██║  ██║╚██████╔╝╚██████╔╝██║  ██║██║  ██║██║ ╚═╝ ██║██║ ╚═╝ ██║██║██║ ╚████║╚██████╔╝
╚═╝     ╚═╝╚══════╝   ╚═╝   ╚═╝  ╚═╝      ╚═╝     ╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝     ╚═╝╚═╝╚═╝  ╚═══╝ ╚═════╝ 







███████╗██╗  ██╗██████╗  █████╗ ███╗   ██╗██████╗         ██╗     ██████╗ ██████╗ ███╗   ██╗████████╗██████╗  █████╗  ██████╗████████╗
██╔════╝╚██╗██╔╝██╔══██╗██╔══██╗████╗  ██║██╔══██╗       ██╔╝    ██╔════╝██╔═══██╗████╗  ██║╚══██╔══╝██╔══██╗██╔══██╗██╔════╝╚══██╔══╝
█████╗   ╚███╔╝ ██████╔╝███████║██╔██╗ ██║██║  ██║      ██╔╝     ██║     ██║   ██║██╔██╗ ██║   ██║   ██████╔╝███████║██║        ██║   
██╔══╝   ██╔██╗ ██╔═══╝ ██╔══██║██║╚██╗██║██║  ██║     ██╔╝      ██║     ██║   ██║██║╚██╗██║   ██║   ██╔══██╗██╔══██║██║        ██║   
███████╗██╔╝ ██╗██║     ██║  ██║██║ ╚████║██████╔╝    ██╔╝       ╚██████╗╚██████╔╝██║ ╚████║   ██║   ██║  ██║██║  ██║╚██████╗   ██║   
╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝     ╚═╝         ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝   ╚═╝   

*/

% Simple DCGs to expand/contract definitions.

expando,    Body --> [Def], {def(Def, Body)}.
contracto, [Def] --> {def(Def, Body)}, Body.

% Apply expando/contracto more than once, and descend into sub-lists.
% The K term is one of expando or contracto, and the J term is used
% on sub-lists, i.e. expando/grow and contracto/shrink.
% BTW, "rebo" is a meaningless name, don't break your brain
% trying to figure it out.

rebo(K, J)      -->    K   ,                         rebo(K, J).
rebo(K, J), [E] --> [[H|T]], !, {call(J, [H|T], E)}, rebo(K, J).
rebo(K, J), [A] --> [  A  ], !,                      rebo(K, J).
rebo(_, _)      --> [].

to_fixed_point(DCG, Ei, Eo) :-
    phrase(DCG, Ei, E),  % Apply DCG...
    (Ei=E -> Eo=E ; to_fixed_point(DCG, E, Eo)).  % ...until a fixed-point is reached.

grow   --> to_fixed_point(rebo(expando,   grow  )).
shrink --> to_fixed_point(rebo(contracto, shrink)).

% ?- phrase(grow, [third], Out).
% Out = [rest, rest, first] ;
% Out = [rest, rest, first] ;
% Out = [rest, second] ;
% Out = [third].

% ?- phrase(shrink, [rest, rest, first], Out).
% Out = [rrest, first] ;
% Out = [third] ;
% Out = [rest, second] ;
% Out = [rest, rest, first].

/* 

███████╗ ██████╗ ██████╗ ███╗   ███╗ █████╗ ████████╗████████╗███████╗██████╗ 
██╔════╝██╔═══██╗██╔══██╗████╗ ████║██╔══██╗╚══██╔══╝╚══██╔══╝██╔════╝██╔══██╗
█████╗  ██║   ██║██████╔╝██╔████╔██║███████║   ██║      ██║   █████╗  ██████╔╝
██╔══╝  ██║   ██║██╔══██╗██║╚██╔╝██║██╔══██║   ██║      ██║   ██╔══╝  ██╔══██╗
██║     ╚██████╔╝██║  ██║██║ ╚═╝ ██║██║  ██║   ██║      ██║   ███████╗██║  ██║
╚═╝      ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝   ╚═╝      ╚═╝   ╚══════╝╚═╝  ╚═╝


?- phrase(joy_parse(E), `22 18 true [false] [1[2[3]]]`), !, format_joy_terms(E, A, []), string_codes(S, A).
E = [int(22), int(18), bool(true), list([bool(false)]), list([int(1), list([...|...])])],
A = [50, 50, 32, 49, 56, 32, 116, 114, 117|...],
S = "22 18 true [false] [1 [2 [3]]]".

*/

format_joy_expression(   int(I)) --> { number_codes(I, Codes) }, Codes.
format_joy_expression(  bool(B)) --> {   atom_codes(B, Codes) }, Codes.
format_joy_expression(symbol(S)) --> {   atom_codes(S, Codes) }, Codes.
format_joy_expression(  list(J)) --> "[", format_joy_terms(J), "]".

format_joy_terms(    []) --> [].
format_joy_terms(   [T]) --> format_joy_expression(T), !.
format_joy_terms([T|Ts]) --> format_joy_expression(T), " ", format_joy_terms(Ts).

joy_terms_to_string(Expr, String) :-
    format_joy_terms(Expr, Codes, []),
    string_codes(String, Codes).


/* 

██████╗  █████╗ ██████╗ ████████╗██╗ █████╗ ██╗          
██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██║██╔══██╗██║          
██████╔╝███████║██████╔╝   ██║   ██║███████║██║          
██╔═══╝ ██╔══██║██╔══██╗   ██║   ██║██╔══██║██║          
██║     ██║  ██║██║  ██║   ██║   ██║██║  ██║███████╗     
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝╚═╝  ╚═╝╚══════╝     
                                                         
██████╗ ███████╗██████╗ ██╗   ██╗ ██████╗███████╗██████╗ 
██╔══██╗██╔════╝██╔══██╗██║   ██║██╔════╝██╔════╝██╔══██╗
██████╔╝█████╗  ██║  ██║██║   ██║██║     █████╗  ██████╔╝
██╔══██╗██╔══╝  ██║  ██║██║   ██║██║     ██╔══╝  ██╔══██╗
██║  ██║███████╗██████╔╝╚██████╔╝╚██████╗███████╗██║  ██║
╚═╝  ╚═╝╚══════╝╚═════╝  ╚═════╝  ╚═════╝╚══════╝╚═╝  ╚═╝

Partial Reducer from "The Art of Prolog" by Sterling and Shapiro
Program 18.3, pg. 362 */

process(Program, ReducedProgram) :-
    findall(PC1, (member(C1, Program), preduce(C1, PC1)), ReducedProgram).

preduce( (A :- B), (Pa :- Pb) ) :- !, preduce(B, Pb), preduce(A, Pa).
preduce(     true,       true ) :- !.
preduce(   (A, B),    Residue ) :- !, preduce(A, Pa), preduce(B, Pb), combine(Pa, Pb, Residue).
% preduce(        A,          B ) :- should_fold(A, B), !.
preduce(        A,    Residue ) :- should_unfold(A), !, clause(A, B), preduce(B, Residue).
preduce(        A,          A ).

combine(true, B, B) :- !.
combine(A, true, A) :- !.
combine(A,    B, (A, B)).

/*

Partial reduction of thun/3 in the thun/4 relation gives a new
version of thun/4 that is tail-recursive.  You generate the new
relation rules like so:

    ?- thunder(C), process(C, R), maplist(portray_clause, R).

I just cut-n-paste from the SWI terminal and rearrange it.

*/

should_unfold(thun(_, _, _)).
% should_unfold(func(_, _, _)).
% should_unfold(def(_, _)).

thunder([  % Source code for thun/4.
    (thun( int(I), E, Si, So) :- thun(E, [ int(I)|Si], So)),
    (thun(bool(B), E, Si, So) :- thun(E, [bool(B)|Si], So)),
    (thun(list(L), E, Si, So) :- thun(E, [list(L)|Si], So)),
    (thun(symbol(Def),   E, Si, So) :- def(Def, Body), append(Body, E, [T|Eo]), thun(T, Eo, Si, So)),
    (thun(symbol(Func),  E, Si, So) :- func(Func, Si, S), thun(E, S, So)),
    (thun(symbol(Combo), E, Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So))
]).

/*

N.B.: in 'thun(symbol(Def)...' the last clause has changed from thun/3 to thun/4.
The earlier version doesn't transform into correct code:

    thun(symbol(B), D, A, A) :- def(B, C), append(C, D, []).
    thun(symbol(A), C, F, G) :- def(A, B), append(B, C, [D|E]), thun(D, E, F, G).

With the change to thun/4 it doesn't transform under reduction w/ thun/3.

You can also unfold def/2 and func/3 (but you need to check for bugs!)

Functions become clauses like these:

    thun(symbol(rolldown),    [], [C, A, B|D], [A, B, C|D]).
    thun(symbol(rolldown), [A|B], [E, C, D|F], G) :- thun(A, B, [C, D, E|F], G).

    thun(symbol(dupd),    [], [A, B|C], [A, B, B|C]).
    thun(symbol(dupd), [A|B], [C, D|E], F) :- thun(A, B, [C, D, D|E], F).

    thun(symbol(over),    [], [B, A|C], [A, B, A|C]).
    thun(symbol(over), [A|B], [D, C|E], F) :- thun(A, B, [C, D, C|E], F).

Definitions become

    thun(symbol(of), A, D, E) :-
        append([symbol(swap), symbol(at)], A, [B|C]),
        thun(B, C, D, E).

    thun(symbol(pam), A, D, E) :-
        append([list([symbol(i)]), symbol(map)], A, [B|C]),
        thun(B, C, D, E).

    thun(symbol(popd), A, D, E) :-
        append([list([symbol(pop)]), symbol(dip)], A, [B|C]),
        thun(B, C, D, E).

These are tail-recursive and allow for better indexing so I would expect
them to be more efficient than the originals.  Ii would be even nicer to
get them looking like this:

    thun(symbol(of), A, D, E) :- thun(symbol(swap), [symbol(at)|A], D, E).

And then if 'swap' was a definition you could push it out even further,
you could pre-expand definitions and functions (and maybe even some
combinators!)

*/