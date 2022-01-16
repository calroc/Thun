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


Run with e.g.:

    $ swipl -g fooooo -g halt source/joy2dot.pl  > jd.dot


*/
:- use_module(library(dcg/basics)).
:- dynamic def/2.


joy_lex([tok(Token)|Ls]) --> chars(Token), !, joy_lex(Ls).
joy_lex([  lbracket|Ls]) --> "[",          !, joy_lex(Ls).
joy_lex([  rbracket|Ls]) --> "]",          !, joy_lex(Ls).

joy_lex(Ls) --> [Space], {code_type(Space, space)}, !, joy_lex(Ls).

joy_lex([]) --> [].


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



joy_def(Codes) :-
    text_to_expression(Codes, [symbol(Name)|Body]),
    % writeln(Name),
    assert_def(Name, Body).

assert_defs(DefsFile) :-
    read_file_to_codes(DefsFile, Codes, []),
    lines(Codes, Lines),
    maplist(joy_def, Lines).

assert_def(Symbol, Body) :-
        retractall(def(Symbol, _)),
        assertz(def(Symbol, Body)).

% Split on newline chars a list of codes into a list of lists of codes
% one per line.  Helper function.
lines([], []) :- !.
lines(Codes, [Line|Lines]) :- append(Line, [0'\n|Rest], Codes), !, lines(Rest, Lines).
lines(Codes, [Codes]).

:- assert_defs("defs.txt").

/*

term_expansion(def(Def), def(Name, Body)) :-
    text_to_expression(Def, [symbol(Name)|Body]).


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
% def(`uncons-pair [uncons] dip unswons rolldown`).
def(`unswons uncons swap`).
def(`xor [unit] ii [cons] [swap cons] clop duo`).

*/

symbols(E, S) :- symbols(E, [], S).

symbols(symbol(S))      --> seen_sym(S), !.
symbols(symbol(S)), [S] --> [].
symbols(  bool(_))      --> [].
symbols(   int(_))      --> [].
symbols(  list(L))      --> symbols(L).

symbols([])             --> [].
symbols([T|Tail])       --> symbols(T), symbols(Tail).

seen_sym(Term, List, List) :- member(Term, List).

write_sym(Symbol) :- write('"'), write(Symbol), write('"').

fooooo :- 
    writeln("digraph joy_defs {"),
    % writeln("    rankdir=LR;"),
    forall(
        def(Symbol, Body),
        (
            symbols(list(Body), Deps),
            forall(
                member(Dep, Deps),
                (
                    write("    "),
                    write_sym(Symbol),
                    write(" -> "),
                    write_sym(Dep),
                    writeln(";")
                )
            )
        )
    ),
    writeln("}"). 

