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

:- initialization(loop).

loop :- line(Line), loop(Line, [], _Out).

loop([eof],  S,   S) :- !.
loop( Line, In, Out) :-
  do_line(Line, In, S),
  write(S), nl,
  line(NextLine), !,
  loop(NextLine, S, Out).


do_line(Line, In, Out) :- phrase(joy_parse(E), Line), thun(E, In, Out).
do_line(_Line, S,   S) :- write('Err'), nl.

