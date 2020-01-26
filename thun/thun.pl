%
%    Copyright © 2018, 2019, 2020 Simon Forman
%
%    This file is part of Thun
%
%    Thun is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    Thun is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with Thun.  If not see <http://www.gnu.org/licenses/>.
%
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

Parser

The grammar of Joy is very simple.  A Joy expression is zero or more Joy
terms separated by blanks and terms can be either integers, Booleans,
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
Interpreter
thun(Expression, InputStack, OutputStack)
*/

thun([], S, S).
thun([Term|E], Si, So) :- thun(Term, E, Si, So).

/*  Original code.  Partial reduction is used to generate the
    actual relations, see below.

    thun( int(I), E, Si, So) :- thun(E, [ int(I)|Si], So).
    thun(bool(B), E, Si, So) :- thun(E, [bool(B)|Si], So).
    thun(list(L), E, Si, So) :- thun(E, [list(L)|Si], So).
    thun(symbol(Def),   E, Si, So) :- def(Def, Body), append(Body, E, Eo), thun(Eo, Si, So).
    thun(symbol(Func),  E, Si, So) :- func(Func, Si, S), thun(E, S, So).
    thun(symbol(Combo), E, Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So).

 */

% Machine-generated thun/4 rules.
% Literals okay.

thun(int(A), [], B, [int(A)|B]).
thun(int(C), [A|B], D, E) :- thun(A, B, [int(C)|D], E).

thun(bool(A), [], B, [bool(A)|B]).
thun(bool(C), [A|B], D, E) :- thun(A, B, [bool(C)|D], E).

thun(list(A), [], B, [list(A)|B]).
thun(list(C), [A|B], D, E) :- thun(A, B, [list(C)|D], E).

% What is wrong here?
% thun(symbol(B), D, A, A) :- def(B, C), append(C, D, []).
% thun(symbol(A), C, F, G) :- def(A, B), append(B, C, [D|E]), thun(D, E, F, G).

% We want something like...
thun(symbol(B), [], A, D) :- def(B, [H|C]), thun(H, C, A, D).
thun(symbol(A), [H|E0], Si, So) :-
    def(A, [DH|DE]),
    append(DE, [H|E0], E),
    thun(DH, E, Si, So).

% And func/3 works too,
thun(symbol(A), [], B, C) :- func(A, B, C).
thun(symbol(A), [C|D], B, F) :- func(A, B, E), thun(C, D, E, F).

% Combo is all messed up.
% thun(symbol(A), D, B, C) :- combo(A, B, C, D, []).
% thun(symbol(A), C, B, G) :- combo(A, B, F, C, [D|E]), thun(D, E, F, G).

thun(symbol(Combo), [], Si, So) :- combo(Combo, Si, S, [], Eo), thun(Eo, S, So).
thun(symbol(Combo), [Term|Expr0], Si, So) :-
    combo(Combo, Si, S, [Term|Expr0], Eo),
    thun(Eo, S, So).

% Some error handling.
thun(symbol(Unknown), _, _, _) :-
    \+ def(Unknown, _),
    \+ func(Unknown, _, _),
    \+ combo(Unknown, _, _, _, _),
    write("Unknown: "),
    writeln(Unknown),
    fail.

/*
Functions
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
func(first, [list([X|_])|S],   [X|S]).
func(rest,  [list([_|X])|S],   [X|S]).
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

func(bool, [_|S], [bool(true)|S]).
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
Combinators
*/

combo(i,          [list(P)|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [list(P), X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [list(P), X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).

combo(dupdip, [list(P), X|S], [X|S], Ei, Eo) :- append(P, [X|Ei], Eo).

combo(branch, [list(T), list(_),  bool(true)|S], S, Ei, Eo) :- append(T, Ei, Eo).
combo(branch, [list(_), list(F), bool(false)|S], S, Ei, Eo) :- append(F, Ei, Eo).

combo(loop, [list(_), bool(false)|S], S, E,  E ).
combo(loop, [list(B),  bool(true)|S], S, Ei, Eo) :- append(B, [list(B), symbol(loop)|Ei], Eo).

combo(step, [list(_),    []|S],    S,  E,  E ).
combo(step, [list(P),   [X]|S], [X|S], Ei, Eo) :- !, append(P, Ei, Eo).
combo(step, [list(P), [X|Z]|S], [X|S], Ei, Eo) :-    append(P, [Z, list(P), symbol(step)|Ei], Eo).

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
Definitions
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

words(Words) :-
    findall(Name, clause(func(Name, _, _), _), Funcs),
    findall(Name, clause(combo(Name, _, _, _, _), _), Combos, Funcs),
    findall(Name, clause(def(Name, _), _), Words0, Combos),
    list_to_set(Words0, Words1),
    sort(Words1, Words).


/*
Compiler
*/

joy_compile(Name, Expression) :- jcmpl(Name, Expression, Rule), asserta(Rule).

show_joy_compile(Name, Expression) :- jcmpl(Name, Expression, Rule), portray_clause(Rule).

jcmpl(Name, Expression, Rule) :-
    call_residue_vars(thun(Expression, Si, So), Term),
    copy_term(Term, Term, Gs),
    Head =.. [func, Name, Si, So],
    rule(Head, Gs, Rule).

rule(Head, [],    Head                        ).
rule(Head, [A|B], Head :- maplist(call, [A|B])).

sjc(Name, InputString) :- phrase(joy_parse(E), InputString), show_joy_compile(Name, E).


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

% ?- phrase(grow, In, [rest, rest, first]).
% Action (h for help) ? abort
% % Execution Aborted
% ?- phrase(shrink, [rest, rest, first], Out).
% Out = [rrest, first] ;
% Out = [third] ;
% Out = [rest, second] ;
% Out = [rest, rest, first].


% format_n(N) --> {number(N), !, number_codes(N, Codes)}, Codes.
% format_n(N) --> signed_digits(Codes), !, {number_codes(N, Codes)}.

% signed_digits([45|Codes]) --> [45], !, digits(Codes).
% signed_digits(    Codes ) -->          digits(Codes).

% digits([Ch|Chars]) --> [Ch], {code_type(Ch, digit)}, digits(Chars).
% digits([]), [Ch]   --> [Ch], {code_type(Ch, space) ; Ch=0'] }.
% digits([], [], _).  % Match if followed by space, ], or nothing.


%-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
/*  Partial Reducer from "The Art of Prolog" by Sterling and Shapiro
    Program 18.3, pg. 362 */

process(Program, ReducedProgram) :-
    findall(PC1, (member(C1, Program), preduce(C1, PC1)), ReducedProgram).

preduce( (A :- B), (Pa :- Pb) ) :- !, preduce(B, Pb), preduce(A, Pa).
preduce(     true,       true ) :- !.
preduce(   (A, B),    Residue ) :- !, preduce(A, Pa), preduce(B, Pb), combine(Pa, Pb, Residue).
preduce(        A,          B ) :- should_fold(A, B), !.
preduce(        A,    Residue ) :- should_unfold(A), !, clause(A, B), preduce(B, Residue).
preduce(        A,          A ).

combine(true, B, B) :- !.
combine(A, true, A) :- !.
combine(A,    B, (A, B)).

should_fold(z, a).  % Just a "No Op" appease the linter.

%-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
/*  Partial reduction of thun/3 in the thun/4 relation gives a new
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
    (thun(symbol(Def),   E, Si, So) :- def(Def, Body), append(Body, E, Eo), thun(Eo, Si, So)),
    (thun(symbol(Func),  E, Si, So) :- func(Func, Si, S), thun(E, S, So)),
    (thun(symbol(Combo), E, Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So))
]).

