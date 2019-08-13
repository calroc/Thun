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


DCG basics.  For some of this I cribbed the source from SWI library code
and adapted it for GNU Prolog Compiler.

N.B. is_glyph//1 excludes '[' and ']' characters.  D'oh!  FIXME

*/


:- set_prolog_flag(double_quotes, codes).


% TODO: scientific notation.

signed_float_or_integer(Codes) --> signed_digits(J), ".", !, digits(I),
    { append(J, [0'.|I], Codes) }.
signed_float_or_integer(Codes) --> signed_digits(Codes).

signed_digits([0'-|Codes]) --> "-", !, digits(Codes).
signed_digits(     Codes ) -->         digits(Codes).

% Groups of characters.

chars(Chars)   --> one_or_more(char, Chars).
blanks         --> blank, !, blanks | [].
digits(Digits) --> one_or_more(digit, Digits).

% Character types.

char(Ch)  --> [Ch], { nonvar(Ch), is_glyph(Ch) }.
blank     --> [Ch], { nonvar(Ch), is_space(Ch) }.
digit(Ch) --> [Ch], { nonvar(Ch), between(0'0, 0'9, Ch) }.

is_glyph(Ch) :- Ch =\= 0'[, Ch =\= 0'], between(0'!, 0'~, Ch).
is_space(Ch) :- Ch =:= 32 ; between(9, 13, Ch).

one_or_more(P, [Ch|Rest]) --> call(P, Ch), one_or_more(P, Rest).
one_or_more(P, [Ch]     ) --> call(P, Ch).

