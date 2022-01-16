:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- dynamic func/3.
:- dynamic def/2.
/*

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

A version of joy with just lists and symbols, data structures are
logical expressions in LoF notation, optionally organised in lists.
No distinction is made syntactically or semantically between lists-as-forumla
and lists-as-containers, 'tis done by usage.

    [] as zero / false
    [[]] as true (1 in Peano arith)

    ((A)(B)) OR
      A  B   AND
    ((A) B)   B IMPLIES A

     (A(B)) ((A)B)  EQUIV   (A IMPLIES B) AND (B IMPLIES A)
    ((A(B)) ((A)B)) XOR




    _ _     ( ( )) (( ) ) _
    o _     (o( )) ((o) ) o
    _ o     ( (o)) (( )o) o
    o o     (o(o)) ((o)o) _

    _ _     ( )    o
    o _     (o)    _
    _ o     ( )o   o
    o o     (o)o   o

    _ _     (( ) )   _
    o _     ((o) )   o
    _ o     (( )o)   _
    o o     ((o)o)   _



*/

joy(InputString, StackIn, StackOut) :-
    phrase(joy_parse(Expression), InputString), !,
    thun(Expression, StackIn, StackOut).

joy_parse([J|Js]) --> blanks, joy_term(J), blanks, joy_parse(Js).
joy_parse([]) --> blanks.
joy_term(list(J)) --> "[", !, joy_parse(J), "]".
joy_term(symbol(S)) --> symbol(S).
symbol(C) --> chars(Chars), !, {atom_string(C, Chars)}.
chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).
char(Ch) --> [Ch], {Ch \== 91, Ch \== 93, code_type(Ch, graph)}.

thun([], S, S).
% thun(E, Si, _) :- show_it(E, Si), fail.  % To visualize the evaluation.
thun([Term|E], S0, S) :- thun(Term, E, S0, S).

thun(list(L), Expr, S0, S) :- thun(Expr, [list(L)|S0], S).
thun(symbol(Name), Expr0, S0, S) :- 
    ( def(Name, Body), append(Body, Expr0, Expr), S1=S0
    ; func(Name, S0, S1), Expr0=Expr
    ; combo(Name, S0, S1, Expr0, Expr)
    ), thun(Expr, S1, S).

show_it(E, Si) :-
    joy_terms_to_string(E, Es),
    is_list(Si), reverse(Si, Is),    
    joy_terms_to_string(Is, Sis),
    write(Sis), write(' . '), writeln(Es).

% joy_terms_to_string(So, S)

func(void, [A|S], [B|S]) :- void(A, B).
% func(or,  [A, B|S], [[[B], [A]]|S]).
% func(and, [A, B|S], [[[B,   A]]|S]).
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

combo(i,          [list(P)|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [list(P), X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [list(P), X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).
combo(dupdip, [list(P), X|S], [X|S], Ei, Eo) :- append(P, [X|Ei], Eo).
combo(branch, [list(T), list(_),  list([list([])])|S], S, Ei, Eo) :- append(T, Ei, Eo).
combo(branch, [list(_), list(F),        list([])  |S], S, Ei, Eo) :- append(F, Ei, Eo).
combo(loop, [list(_),        list([])  |S], S, E,  E ).
combo(loop, [list(B),  list([list([])])|S], S, Ei, Eo) :- append(B, [list(B), symbol(loop)|Ei], Eo).
combo(step, [list(_),    list([])|S],    S,  E,  E ).
combo(step, [list(P), list([X|Z])|S], [X|S], Ei, Eo) :- append(P, [list(Z), list(P), symbol(step)|Ei], Eo).
combo(times, [list(_),       list([])  |S], S, E,  E ).
combo(times, [list(P), list([list([])])|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(times, [list(P), list([list(L )])|S], S, Ei, Eo) :-
    L \= [], append(P, [list(L), list(P), symbol(times)|Ei], Eo).
combo(genrec, [R1, R0, Then, If|S], [Else, Then, If|S], E, [symbol(ifte)|E]) :-
    append(R0, [list([If, Then, R0, R1, symbol(genrec)])|R1], Else).
combo(map, [list(_),   list([])|S],               [list([])|S], E,                E ) :- !.
combo(map, [list(P), list(List)|S], [list(Mapped), list([])|S], E, [symbol(infra)|E]) :-
    prepare_mapping(list(P), S, List, Mapped).

prepare_mapping(Pl, S, In, Out) :- prepare_mapping(Pl, S, In, [], Out).

prepare_mapping(    _,  _,     [],                                  Out,  Out) :- !.
prepare_mapping(    Pl, S, [T|In],                                  Acc,  Out) :-
    prepare_mapping(Pl, S,    In,  [list([T|S]), Pl, symbol(infrst)|Acc], Out).


term_expansion(def(Def), def(Name, Body)) :-
    phrase(joy_parse([symbol(Name)|Body]), Def),
    % Don't let defs "shadow" functions or combinators.
    \+ ( func(Name, _, _) ; combo(Name, _, _, _, _) ).

% def(``).
def(`and duo unit`).
def(`app2 [grba swap grba swap] dip [infrst] cons ii`).
def(`b [i] dip i`).
def(`cleave fork popdd`).
def(`clop cleave popdd`).
def(`duo unit cons`).
def(`fba [xor xor void] [[and] [xor and] fork or void] clop popdd`).
def(`fork [i] app2`).
def(`grba [stack popd] dip`).
def(`ii [dip] dupdip i`).
def(`infra swons swaack [i] dip swaack`).
def(`infrst infra first`).
def(`or [unit] ii duo`).
def(`popd [pop] dip`).
def(`popdd [pop] dipd`).
def(`popop pop pop`).
def(`swons swap cons`).
def(`uncons-pair [uncons] dip unswons rolldown`).
def(`unswons uncons swap`).
def(`xor [unit] ii [cons] [swap cons] clop duo`).


format_joy_expression(    V    ) --> { var(V), ! }, "...".
format_joy_expression(symbol(S)) --> !, {   atom_codes(S, Codes) }, Codes.
format_joy_expression(  list(J)) --> "[", format_joy_terms(J), "]".
format_joy_terms(    []) --> [].
format_joy_terms(   [T]) --> format_joy_expression(T), !.
format_joy_terms([T|Ts]) --> format_joy_expression(T), " ", format_joy_terms(Ts).
joy_terms_to_string(Expr, String) :-
    format_joy_terms(Expr, Codes, []),
    string_codes(String, Codes).

/* Reduce arithmetic formula to Mark or Void */

void(      list([]),         list([])  ) :- !.
void(list([list([])]), list([list([])])) :- !.
void(list([   A  |_]), list([list([])])) :- void(A,       list([])  ), !.
void(list([   A  |S]), V               ) :- void(A, list([list([])])), void(list(S), V).


symbols(E, S) :- symbols(E, [], S).

symbols(symbol(S))      --> seen_sym(S), !.
symbols(symbol(S)), [S] --> [].
symbols( list([]))      --> [].
symbols(list([T|Tail])) --> symbols(T), symbols(list(Tail)).

seen_sym(Term, List, List) :- member(Term, List).

fooooo :- forall(def(Symbol, Body),
    (
        symbols(list(Body), Deps),
        forall(member(Dep, Deps),
            (
                write(Symbol),
                write(" -> "),
                write(Dep),
                writeln(";")
            )
        )
    )
).


/* 



ᴀ?- joy(`[] [ [] [[]] [] ] [or] step void`, Si, So), !, joy_terms_to_string(So, S).
Si = [],
So = [list([list([])])],
S = "[[]]".

?- joy(`[[]] [ [[]] [[]] [[]] [[]] ] [and] step void`, Si, So), !, joy_terms_to_string(So, S).
Si = [],
So = [list([list([])])],
S = "[[]]".


 */