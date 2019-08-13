/*
    Copyright  2019 Simon Forman

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


Parser

*/

:- set_prolog_flag(double_quotes, codes).

joy_parse([T|J]) --> blanks, joy_term(T), blanks, joy_parse(J).
joy_parse([]) --> [].

joy_term(N) --> num(N), !.
joy_term(J) --> "[", !, joy_parse(J), "]".
joy_term(C) --> symbol(C).

symbol(C) --> chars(Chars), !, {Chars \= "==", atom_codes(C, Chars)}.


% TODO: negative numbers, floats, scientific notation.

num(N) --> signed_digits(Codes), !, { number_codes(N, Codes) }.

% Groups of characters.

chars(Chars)   --> one_or_more(char, Chars).
blanks         --> blank, !, blanks | [].
digits(Digits) --> one_or_more(digit, Digits).

signed_digits([45|Codes]) --> "-", !, digits(Codes).
signed_digits(    Codes ) -->         digits(Codes).

% Character types.

char(Ch)  --> [Ch], { nonvar(Ch), Ch =\= 0'[, Ch =\= 0'], between(33, 126, Ch) }.
blank     --> [Ch], { nonvar(Ch),(Ch =:= 32 ; between(9, 13, Ch)) }.
digit(Ch) --> [Ch], { nonvar(Ch), between(48, 57, Ch) }.


% Line is the next new-line delimited line from standard input stream as
% a list of character codes.

line(Line) :- get_code(X), line(X, Line).

line(10,      []) :- !.  % break on new-lines.
line(-1,   [eof]) :- !.  % break on EOF
line(X, [X|Line]) :- get_code(Y), !, line(Y, Line).

one_or_more(E, List) --> one_or_more_(List, E).

one_or_more_([Ch|Rest], P) --> call(P, Ch), one_or_more_(Rest, P).
one_or_more_([Ch],      P) --> call(P, Ch).

/*

Print state.

*/

format_state(Stack, Expression, Codes) :-
    reverse(Stack, RStack),
    phrase(format_joy(RStack), RStackCodes),
    phrase(format_joy(Expression), ExpressionCodes),
    append(RStackCodes, [32, 46, 32|ExpressionCodes], Codes).


frump(Stack, Expression) :-
    format_state(Stack, Expression, Codes),
    maplist(put_code, Codes), nl.

print_stack(Stack) :-
    reverse(Stack, RStack),
    phrase(format_joy(RStack), Codes),
    maplist(put_code, Codes).



% Print Joy expressions as text.

format_joy(Tail)  --> {var(Tail)}, !, [46, 46, 46].
format_joy([T])   --> format_term(T), !.
format_joy([T|S]) --> format_term(T), " ", format_joy(S).
format_joy([])    --> [].

format_term(N) --> {number(N),   number_codes(N, Codes)}, Codes.
format_term(A) --> {  atom(A),     atom_codes(A, Codes)}, Codes.
format_term(V) --> {   var(V), write_to_codes(Codes, V)}, Codes.
format_term([A|As]) --> "[", format_joy([A|As]), "]".
format_term(F) --> {    write_to_codes(Codes, F)}, Codes.


