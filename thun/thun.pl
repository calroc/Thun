%
%    Copyright © 2018, 2019 Simon Forman
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

To handle comparision operators the possibility of exceptions due to
insufficiently instantiated arguments must be handled.  First try to make
the comparison and set the result to a Boolean atom.  If an exception
happens just leave the comparison expression as the result and some other
function or combinator will deal with it.  Example:

    func(>,  [A, B|S], [C|S]) :- catch(
            (B > A -> C=true ; C=false),
            _,
            C=(B>A)  % in case of error.
            ).

To save on conceptual overhead I've defined a term_expansion/2 that sets
up the func/3 for each op.
*/

term_expansion(comparison_operator(X), (func(X, [A, B|S], [C|S]) :-
    F =.. [X, B, A], catch((F -> C=true ; C=false), _, C=F))).

% I don't use Prolog-compatible op symbols in all cases.
term_expansion(comparison_operator(X, Y), (func(X, [A, B|S], [C|S]) :-
    F =.. [Y, B, A], catch((F -> C=true ; C=false), _, C=F))).

% Likewise for math operators, try to evaluate, otherwise use the
% symbolic form.

term_expansion(math_operator(X), (func(X, [A, B|S], [C|S]) :-
    F =.. [X, B, A], catch(C is F, _, C=F))).

term_expansion(math_operator(X, Y), (func(X, [A, B|S], [C|S]) :-
    F =.. [Y, B, A], catch(C is F, _, C=F))).


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
joy_term(C) --> symbol(C).

symbol(C) --> chars(Chars), !, {Chars \= [61, 61], atom_string(C, Chars)}.

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

% Symbolic math.  Compute the answer, or derivative, or whatever, later.
math_operator(+).
math_operator(-).
math_operator(*).
math_operator(/).
math_operator(mod).

% Attempt to calculate the value of a symbolic math expression.
func(calc, [A|S], [B|S]) :- B is A.

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

func(shift, [[B|A], C|D], [A, [B|C]|D]).

func(rollup, Si, So) :- func(rolldown, So, Si).
func(uncons, Si, So) :- func(cons, So, Si).

func(bool, [    0|S], [false|S]) :- !.
func(bool, [  0.0|S], [false|S]) :- !.
func(bool, [   []|S], [false|S]) :- !.
func(bool, [   ""|S], [false|S]) :- !.
func(bool, [false|S], [false|S]) :- !.

func(bool, [_|S], [true|S]).

comparison_operator(>).
comparison_operator(<).
comparison_operator(>=).
comparison_operator(<=, =<).
comparison_operator(=, =:=).
comparison_operator(<>, =\=).


/*
Combinators
*/

combo(i,          [P|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [P, X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [P, X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).

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


/*
Definitions
*/

joy_def(def(Def, Body)) --> symbol(Def), blanks, "==", joy_parse(Body).

joy_def --> joy_def(Def), {ignore(assert_def(Def))}.

joy_defs --> blanks, joy_def, blanks, joy_defs.
joy_defs --> [].

assert_defs(DefsFile) :-
    read_file_to_codes(DefsFile, Codes, []),
    phrase(joy_defs, Codes).

assert_def(def(Def, Body)) :-
    \+ func(Def, _, _),
    \+ combo(Def, _, _, _, _),
    retractall(def(Def, _)),
    assertz(def(Def, Body)).

:- assert_defs("defs.txt").


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
