/*
    Copyright 2019 Simon Forman

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

Main Loop

*/

% :- debug.
% :- spy(thun).

:- initialization(loop).

loop :- prompt(Line), loop(Line, [], _Out).

loop([eof],  S,   S) :- !.
loop( Line, In, Out) :-
  do_line(Line, In, S),
  show_stack(S),
  prompt(NextLine), !,
  loop(NextLine, S, Out).

do_line(Line, In, Out) :- phrase(joy_parse(E), Line), thun(E, In, Out).
do_line(_Line, S,   S) :- write('Err'), nl.

prompt(Line) :- write(`joy? `), get_line(Line).
show_stack(S) :- nl, print_stack(S), write(` <-top`), nl, nl.


% Line is the next newget_line-delimited line from standard input stream as
% a list of character codes.

get_line(Line) :- get_code(X), line(X, Line).

line(10,      []) :- !.  % break on new-lines.
line(-1,   [eof]) :- !.  % break on EOF
line(X, [X|Line]) :- get_code(Y), !, line(Y, Line).

