%
%    Copyright © 2018 Simon Forman
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
:- op(990, xfy, ≡).  % for Joy definitions.
:- dynamic func/3.

/*
An entry point.
*/

joy(InputString, StackIn, StackOut) :-
    phrase(joy_parse(Expression), InputString), !,
    thun(Expression, StackIn, StackOut).

/*
Parser

    joy :== number | '[' joy* ']' | atom

*/

joy_parse([T|J]) --> blanks, joy_term(T), blanks, joy_parse(J).
joy_parse([]) --> [].

joy_term(N) --> number(N), !.
joy_term(J) --> "[", !, joy_parse(J), "]".
joy_term(C) --> chars(Chars), !, {atom_string(C, Chars)}.

chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> [Ch], {Ch \== 91, Ch \== 93, code_type(Ch, graph)}.

/*
Interpreter
thun(Expression, InputStack, OutputStack)
*/

thun([], S, S).
thun(  [Lit|E], Si, So) :- literal(Lit), !, thun(E, [Lit|Si], So).
thun(  [Def|E], Si, So) :- Def ≡ Body, !, append(Body, E, Eo), thun(Eo, Si, So).
thun( [Func|E], Si, So) :- func(Func, Si, S), thun(E, S, So).
thun([Combo|E], Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So).

% Some error handling.
thun([Unknown|E], Si, So) :- write("wtf? "), writeln(Unknown), So = [[Unknown|E]|Si].

/*
Literals
*/

literal(V) :- var(V).
literal(I) :- number(I).
literal([]).
literal([_|_]).
literal(true).
literal(false).

% Symbolic math expressions are literals.
literal(_+_).
literal(_-_).
literal(_*_).
literal(_/_).

/*
Functions
*/

func(app1,     [P, Xi|S],     [Xo|S]) :- thun(P, [Xi|S], [Xo|_]).  % Combinator.
func(app2, [P, Xi, Yi|S], [Xo, Yo|S]) :- thun(P, [Xi|S], [Xo|_]),  % Combinator.
                                         thun(P, [Yi|S], [Yo|_]).

func(nullary,   [P|S],   [X|S]) :- thun(P, S, [X|_]).  % Combinator.
func(infra,  [P, R|S],   [Q|S]) :- thun(P, R, Q).  % Combinator.

func(cons, [A, B|S], [[B|A]|S]).
func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).
% func(+,    [A, B|S],     [C|S]) :- C #= A + B.
% func(-,    [A, B|S],     [C|S]) :- C #= B - A.
% func(*,    [A, B|S],     [C|S]) :- C #= A * B.
% func(/,    [A, B|S],     [C|S]) :- C #= B div A.

% Symbolic math.  Compute the answer, or derivative, or whatever, later.
func(+, [A, B|S], [B + A|S]).
func(-, [A, B|S], [B - A|S]).
func(*, [A, B|S], [B * A|S]).
func(/, [A, B|S], [B / A|S]).

func(=, [A|S], [B|S]) :- B is A.

% func(pm, [A, B|S],    [C, D|S]) :- C #= A + B, D #= B - A.
% func(pm, [A, B|S],    [B + A, B - A|S]).

% func(sqrt, [A|S], [B|S]) :- B^2 #= A.
func(sqrt, [A|S], [sqrt(A)|S]).

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

func(rollup, Si, So) :- func(rolldown, So, Si).
func(uncons, Si, So) :- func(cons, So, Si).

func(>,  [A, B|S], [T|S]) :- B #> A #<==> R, r_truth(R, T).
func(<,  [A, B|S], [T|S]) :- B #< A #<==> R, r_truth(R, T).
func(=,  [A, B|S], [T|S]) :- B #= A #<==> R, r_truth(R, T).
func(>=, [A, B|S], [T|S]) :- B #>= A #<==> R, r_truth(R, T).
func(<=, [A, B|S], [T|S]) :- B #=< A #<==> R, r_truth(R, T).
func(<>, [A, B|S], [T|S]) :- B #\= A #<==> R, r_truth(R, T).

r_truth(0, false).
r_truth(1, true).


/*
Definitions
*/

at ≡ [drop, first].
b ≡ [[i], dip, i].
binary ≡ [unary, popd].
ccons ≡ [cons, cons].
cleave ≡ [fork, [popd], dip].
codireco ≡ [cons, dip, rest, cons].
drop ≡ [[rest], times].
dupd ≡ [[dup], dip].
dupdd ≡ [[dup], dipd].
fork ≡ [[i], app2].
fourth ≡ [rest, third].
ifte ≡ [[nullary], dipd, swap, branch].
make_generator ≡ [[codireco], ccons].
neg ≡ [0, swap, -].
of ≡ [swap, at].
pm ≡ [[+], [-], cleave, popdd].
popd ≡ [[pop], dip].
popdd ≡ [[pop], dipd].
popop ≡ [pop, pop].
popopd ≡ [[popop], dip].
popopdd ≡ [[popop], dipd].
product ≡ [1, swap, [*], step].
rrest ≡ [rest, rest].
second ≡ [rest, first].
size ≡ [0, swap, [pop, 1, +], step].
sqr ≡ [dup, *].
sum ≡ [0, swap, [+], step].
swons ≡ [swap, cons].
third ≡ [rest, second].
trinary ≡ [binary, popd].
unary ≡ [nullary, popd].
unit ≡ [[], cons].
unswons ≡ [uncons, swap].
while ≡ [swap, [nullary], cons, dup, dipd, concat, loop].
x ≡ [dup, i].


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


% Simple DCGs to expand/contract definitions.

expando,    Body --> [Def], {Def ≡ Body}.
contracto, [Def] --> {Def ≡ Body}, Body.

% phrase(expando, ExprIn, ExprOut).

