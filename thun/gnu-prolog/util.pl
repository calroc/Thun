

% Line is the next new-line delimited line from standard input stream as
% a list of character codes.

line(Line) :- get_code(X), line(X, Line).

line(10,      []) :- !.  % break on new-lines.
line(-1,   [eof]) :- !.  % break on EOF
line(X, [X|Line]) :- get_code(Y), !, line(Y, Line).


chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> [Ch], { Ch \== 0'[, Ch \== 0'], Ch >= 33, Ch =< 126 }.


blanks --> blank, !, blanks.
blanks --> [].

blank --> [32].


% TODO: negative numbers, floats, scientific notation.

num(N) --> digits(Codes), !, { num(N, Codes) }.

num(_, []) :- fail, !.
num(N, [C|Codes]) :- number_codes(N, [C|Codes]).


digits([H|T]) --> digit(H), !, digits(T).
digits([]) --> [].

digit(C) --> [C], { nonvar(C), C =< 57, C >= 48 }.

