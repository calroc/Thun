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
terms separated by blanks and terms can be either integers, quoted Joy
expressions, or symbols (names of functions.)

    joy ::= ( blanks term blanks )*

    term ::= integer | '[' joy ']' | symbol

    integer ::= [ '-' | '+' ] ('0'...'9')+
    symbol ::= char+

    char ::= <Any non-space other than '[' and ']'.>
    blanks ::= <Zero or more whitespace characters.>

For integer//1 and blanks//0 I delegate to SWI's dcg/basics library.  The
blank//0 matches and discards space and newline characters and integer//1
"processes an optional sign followed by a non-empty sequence of digits
into an integer." (https://www.swi-prolog.org/pldoc/man?section=basics)

Symbols can be made of any non-blank characters except '['and ']' which
are fully reserved for list literals ("quotes"), and '==' is reserved as
a kind of meta-logical punctuation for definitions (it's not a symbol,
you can't use it in code, it only appears in the defs.txt file as a
visual aid to humans.  The rule of one definition per line with the
name as the first symbol in the definition would suffice, but I tried it
and it looked ugly to me.  Any number of '=' characters can appear as
part of a symbol, and any number of them other than two can be a symbol.)

Symbols 'true' and 'false' are treated as literals for Boolean values.

For now strings are neglected in favor of lists of numbers.  (But there's
no support for parsing string notation and converting to lists of ints.)

One wrinkle of the grammar is that numbers do not need to be followed by
blanks before the next match, which is nice when the next match is a
square bracket but a little weird when it's a symbol term.  E.g. "2[3]"
parses as [2, [3]] but "23x" parses as [23, x].  It's a minor thing not
worth disfiguring the grammar to change IMO.

Integers are converted to Prolog integers, symbols to Prolog atoms, and
list literals to Prolog lists.

*/

joy_parse([T|J]) --> blanks, joy_term(T), blanks, joy_parse(J).
joy_parse([]) --> [].

joy_term(J) --> integer(J), !.
joy_term(J) --> "[", !, joy_parse(J), "]".
joy_term(J) --> symbol(J).

symbol(_) --> "==", !, {fail}.  % prevents '==' parsing as [= =].
symbol(C) --> chars(Chars), !, {atom_string(C, Chars)}.

chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> [Ch], {Ch \== 91, Ch \== 93, code_type(Ch, graph)}.


/*
Interpreter
thun(Expression, InputStack, OutputStack)
*/

thun([], S, S).
thun(  [Lit|E], Si, So) :- literal(Lit), !, thun(E, [Lit|Si], So).
thun(  [Def|E], Si, So) :- def(Def, Body), !, append(Body, E, Eo), thun(Eo, Si, So).
thun( [Func|E], Si, So) :- func(Func, Si, S), thun(E, S, So).
thun([Combo|E], Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So).

% Some error handling.

thun([Unknown|E], Si, So) :-
    damned_thing(Unknown),
    write("wtf? "),
    writeln(Unknown),
    So = [Unknown|E]-Si.

damned_thing(It) :-
    \+ literal(It),
    \+ def(It, _),
    \+ func(It, _, _),
    \+ combo(It, _, _, _, _).


/*
Literals
*/

literal(V) :- var(V).
literal(I) :- number(I).
literal([]).
literal([_|_]).
literal(true).
literal(false).


/*
Functions
*/

func(words, S, [Words|S]) :- words(Words).

func(cons, [A, B|S], [[B|A]|S]).
func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).

func(concat, [A, B|S],   [C|S]) :- append(B, A, C).
func(flatten,   [A|S],   [B|S]) :- flatten(A, B).
func(swaack,    [R|S],   [S|R]).
func(stack,        S ,   [S|S]).
func(clear,        _ ,      []).
func(first, [[X|_]|S],   [X|S]).
func(rest,  [[_|X]|S],   [X|S]).
func(unit,      [X|S], [[X]|S]).

func(rolldown, [A, B, C|S], [B, C, A|S]).
func(dupd,        [A, B|S], [A, B, B|S]).
func(over,        [A, B|S], [B, A, B|S]).
func(tuck,        [A, B|S], [A, B, A|S]).

func(shift, [[B|A], C|D], [A, [B|C]|D]).

func(rollup, Si, So) :- func(rolldown, So, Si).
func(uncons, Si, So) :- func(cons, So, Si).

func(bool, [    0|S], [false|S]) :- !.
func(bool, [  0.0|S], [false|S]) :- !.
func(bool, [   []|S], [false|S]) :- !.
func(bool, [   ""|S], [false|S]) :- !.
func(bool, [false|S], [false|S]) :- !.

func(bool, [_|S], [true|S]).

func( + ,  [A, B|S], [C|S]) :- C #= A + B.
func( - ,  [A, B|S], [C|S]) :- C #= B - A.
func( * ,  [A, B|S], [C|S]) :- C #= A * B.
func( / ,  [A, B|S], [C|S]) :- C #= B div A.
func('%',  [A, B|S], [C|S]) :- C #= B mod A.

func('/%', [A, B|S], [C, D|S]) :- C #= A div B, D #= A mod B.
func( pm , [A, B|S], [C, D|S]) :- C #= A + B,   D #= B - A.

func(>,  [A, B|S], [T|S]) :- B #> A #<==> R, r_truth(R, T).
func(<,  [A, B|S], [T|S]) :- B #< A #<==> R, r_truth(R, T).
func(=,  [A, B|S], [T|S]) :- B #= A #<==> R, r_truth(R, T).
func(>=, [A, B|S], [T|S]) :- B #>= A #<==> R, r_truth(R, T).
func(<=, [A, B|S], [T|S]) :- B #=< A #<==> R, r_truth(R, T).
func(<>, [A, B|S], [T|S]) :- B #\= A #<==> R, r_truth(R, T).

r_truth(0, false).
r_truth(1, true).


/*
Combinators
*/

combo(i,          [P|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [P, X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [P, X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).

combo(dupdip, [P, X|S], [X|S], Ei, Eo) :- append(P, [X|Ei], Eo).

combo(branch, [T, _,  true|S], S, Ei, Eo) :- append(T, Ei, Eo).
combo(branch, [_, F, false|S], S, Ei, Eo) :- append(F, Ei, Eo).

combo(loop, [_, false|S], S, E,  E ).
combo(loop, [B,  true|S], S, Ei, Eo) :- append(B, [B, loop|Ei], Eo).

combo(step, [_,    []|S],    S,  E,  E ).
combo(step, [P,   [X]|S], [X|S], Ei, Eo) :- !, append(P, Ei, Eo).
combo(step, [P, [X|Z]|S], [X|S], Ei, Eo) :-    append(P, [Z, P, step|Ei], Eo).

combo(times, [_, 0|S], S, E,  E ).
combo(times, [P, 1|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(times, [P, N|S], S, Ei, Eo) :- N #>= 2, M #= N - 1, append(P, [M, P, times|Ei], Eo).
combo(times, [_, N|S], S, _,  _ ) :- N #< 0, fail.

combo(genrec, [R1, R0, Then, If|S],
              [  Else, Then, If|S], E, [ifte|E]) :-
    Quoted = [If, Then, R0, R1, genrec],
    append(R0, [Quoted|R1], Else).

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

combo(map, [_,   []|S],         [[]|S], E,        E ) :- !.
combo(map, [P, List|S], [Mapped, []|S], E, [infra|E]) :-
    prepare_mapping(P, S, List, Mapped).

% Set up a program for each term in ListIn
%
%     [term S] [P] infrst
%
% prepare_mapping(P, S, ListIn, ListOut).

prepare_mapping(P, S, In, Out) :- prepare_mapping(P, S, In, [], Out).

prepare_mapping(    _, _,     [],                   Out,  Out) :- !.
prepare_mapping(    P, S, [T|In],                   Acc,  Out) :-
    prepare_mapping(P, S,    In,  [[T|S], P, infrst|Acc], Out).


/*
Definitions
*/

joy_def(def(Def, Body)) --> symbol(Def), blanks, "==", joy_parse(Body).

joy_defs([Def|Rest]) --> blanks, joy_def(Def), blanks, joy_defs(Rest).
joy_defs([]) --> [].

assert_defs(DefsFile) :-
    read_file_to_codes(DefsFile, Codes, []),
    phrase(joy_defs(Defs), Codes),
    maplist(assert_def, Defs).

assert_def(def(Def, Body)) :-
    (  % Don't let Def "shadow" functions or combinators.
        \+ func(Def, _, _),
        \+ combo(Def, _, _, _, _)
    ) -> (
        retractall(def(Def, _)),
        assertz(def(Def, Body))
    ) ; true.  % Otherwise it's okay.

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
