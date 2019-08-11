:- dynamic(func/3).
:- discontiguous(func/3).

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


/*
Combinators
*/

combo(i,          [P|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [P, X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [P, X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).

combo(dupdip, [P, X|S], [X|S], Ei, Eo) :- append(P, [X|Ei], Eo).



/*
Definitions
*/

def(x, [dup, i]).





