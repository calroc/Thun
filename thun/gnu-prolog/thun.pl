/*
    Copyright  2018, 2019 Simon Forman

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

*/
% :- dynamic(func/3).
% :- discontiguous(func/3).
:- multifile(func/3).

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
    write(`huh? `),
    write(Unknown), nl,
    So = [[Unknown|E]|Si].

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

% Symbolic math expressions are literals.
literal(_+_).
literal(_-_).
literal(_*_).
literal(_/_).
literal(_ mod _).

% Symbolic comparisons are literals.
literal(_>_).
literal(_<_).
literal(_>=_).
literal(_=<_).
literal(_=:=_).
literal(_=\=_).


/*
Functions
*/

func(cons, [A, B|S], [[B|A]|S]).
func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).
func(concat, [A, B|S],   [C|S]) :- append(B, A, C).
func(flatten,   [A|S],   [B|S]) :- flatten(A, B).
func(swaack,    [R|S],   [S|R]).
func(stack,        S ,   [S|S]).
func(unstack,   [S|_],      S ).
func(clear,        _ ,      []).
func(first, [[X|_]|S],   [X|S]).
func(rest,  [[_|X]|S],   [X|S]).
func(unit,      [X|S], [[X]|S]).
func(thunk,        S ,   [X|S]).

func(rolldown, [A, B, C|S], [B, C, A|S]).
func(swapd,    [A, B, C|S], [A, C, B|S]).
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

func(sqrt, [A|S], [B|S]) :- B is sqrt(A).


/*
Combinators
*/

combo(i,          [P|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [P, X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [P, X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).

combo(dupdip, [P, X|S], [X|S], Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dupdip, [P, X|S], [X|S], Ei, Eo) :- append(P, [X|Ei], Eo).

combo(branch, [T, _,  true|S], S, Ei, Eo) :- append(T, Ei, Eo).
combo(branch, [_, F, false|S], S, Ei, Eo) :- append(F, Ei, Eo).
combo(branch, [T, F,  Expr|S], S, Ei, Eo) :-
    \+ Expr = true, \+ Expr = false,
    catch(  % Try Expr and do one or the other,
        (Expr -> append(T, Ei, Eo) ; append(F, Ei, Eo)),
        _,  % If Expr don't grok, try both branches.
        (append(T, Ei, Eo) ; append(F, Ei, Eo))
        ).

combo(loop, [_, false|S], S, E,  E ).
combo(loop, [B,  true|S], S, Ei, Eo) :- append(B, [B, loop|Ei], Eo).
combo(loop, [B,  Expr|S], S, Ei, Eo) :-
    \+ Expr = true, \+ Expr = false,
    catch(  % Try Expr and do one or the other,
        (Expr -> append(B, [B, loop|Ei], Eo) ; Ei=Eo),
        _,  % If Expr don't grok, try both branches.
        (Ei=Eo ; append(B, [B, loop|Ei], Eo))
        ).

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

