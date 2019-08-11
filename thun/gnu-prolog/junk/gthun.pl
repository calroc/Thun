
:- op(990, xfy, =-).
:- dynamic((=-)/2).

:- initialization(loop).


/*
Parser
*/

joy_parse([T|S]) --> blanks, joy_term(T), blanks, joy_parse(S).
joy_parse([]) --> [].

joy_term(N) --> num(N), !.
joy_term(S) --> [0'[], !, joy_parse(S), [0']].
joy_term(A) --> chars(Chars), !, {atom_codes(A, Chars)}.


/*
Interpreter.
*/

thun([], S, S).

thun(  [Lit|E], Si, So) :- literal(Lit), !, thun(E, [Lit|Si], So).
thun( [Func|E], Si, So) :- func(Func, Si, S), !, thun(E, S, So).
thun([Combo|E], Si, So) :- combo(Combo, Si, S, E, Eo), !, thun(Eo, S, So).

thun(Err, S, [Err|S]) :- write('Unknown term!'), nl.


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

func(cons, [A, B|S], [[B|A]|S]).
func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).

func(uncons, Si, So) :- func(cons, So, Si).

func(+, [A, B|S], [B+A|S]).
func(=, [A|S], [B|S]) :- B is A.

func(clear, _,    []).
func(stack, S, [S|S]).


/*
Definitions
*/

% This is NOT the Continuation-Passing Style
%
% func(Name, Si, So) :- Name =- Body, thun(Body, Si, So).

func(inscribe, [Definition|S], S) :-
  Definition = [Name|Body],
  atom(Name),
  assertz(Name =- Body).

swons =- [swap, cons].
x =- [dup, i].
unit =- [[], cons].
enstacken =- [stack, [clear], dip].

% This IS the Continuation-Passing Style
%
combo(Name, S, S, Ei, Eo) :- Name =- Body, append(Body, Ei, Eo).

/*
Combinators
*/

combo(i,          [P|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [P, X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [P, X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).

combo(branch, [T, _,  true|S], S, Ei, Eo) :- append(T, Ei, Eo).
combo(branch, [_, F, false|S], S, Ei, Eo) :- append(F, Ei, Eo).

combo(loop, [_, false|S], S, E,  E ).
combo(loop, [B,  true|S], S, Ei, Eo) :- append(B, [B, loop|Ei], Eo).

combo(step, [_,    []|S],    S,  E,  E ).
combo(step, [P,   [X]|S], [X|S], Ei, Eo) :- !, append(P, Ei, Eo).
combo(step, [P, [X|Z]|S], [X|S], Ei, Eo) :-    append(P, [Z, P, step|Ei], Eo).


/*
Main Loop
*/

loop :- line(Line), loop(Line, [], _Out).

loop([eof],  S,   S) :- !.
loop( Line, In, Out) :-
  do_line(Line, In, S),
  write(S), nl,
  line(NextLine), !,
  loop(NextLine, S, Out).


do_line(Line, In, Out) :- phrase(joy_parse(E), Line), thun(E, In, Out).
do_line(_Line, S,   S) :- write('Err'), nl.

