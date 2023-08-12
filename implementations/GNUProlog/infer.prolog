

text_to_expression(Text, Expression) :-
    phrase(joy_lex(Tokens), Text), !,
    phrase(joy_parse(Expression), Tokens).


joy_lex([tok(Token)|Ls]) --> chars(Token), !, joy_lex(Ls).
joy_lex([  lbracket|Ls]) --> "[",          !, joy_lex(Ls).
joy_lex([  rbracket|Ls]) --> "]",          !, joy_lex(Ls).
joy_lex(            Ls ) --> blank,        !, joy_lex(Ls).

joy_lex([]) --> [].


joy_parse([J|Js]) --> joy_term(J), !, joy_parse(Js).
joy_parse([]) --> [].


joy_term(list(J)) --> [lbracket], !, joy_parse(J), [rbracket].
joy_term(Term) --> [tok(Codes)], { joy_term(Term, Codes) }.


joy_term(  int,   Codes) :- numeric(Codes), !, atom_codes(_I, Codes).
joy_term( bool,  "true") :- !.
joy_term( bool, "false") :- !.
joy_term(symbol(S),   Codes) :- atom_codes(S, Codes).


% Apologies for all the (green, I hope) cuts.  The strength of the Joy
% syntax is that it's uninteresting.

chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> \+ blank, [Ch], { Ch \== 0'[, Ch \== 0'] }.

numeric(Codes) :- digits(Codes, []), !.
numeric([45,FirstDigit|Codes]) :- digit(FirstDigit), digits(Codes, []), !.
% ASCII 45 is '-'.

digits --> digit, digits.
digits --> [].

digit --> [Code], { digit(Code) }.

digit(Code) :- between(0'0, 0'9, Code).


% TODO: code golf this into something more efficient.
blank --> [9].
blank --> [10].
blank --> [11].
blank --> [12].
blank --> [13].
blank --> [32].
blank --> [194, 133].
blank --> [194, 160].
blank --> [225, 154, 128].
blank --> [226, 128, 128].
blank --> [226, 128, 129].
blank --> [226, 128, 130].
blank --> [226, 128, 131].
blank --> [226, 128, 132].
blank --> [226, 128, 133].
blank --> [226, 128, 134].
blank --> [226, 128, 135].
blank --> [226, 128, 136].
blank --> [226, 128, 137].
blank --> [226, 128, 138].
blank --> [226, 128, 168].
blank --> [226, 128, 169].
blank --> [226, 128, 175].
blank --> [226, 129, 159].
blank --> [227, 128, 128].


thun([], S, S).
thun([Term|E], Si, So) :- thun(Term, E, Si, So).

thun(A,    [], S, [A|S]) :- var(A), !.
thun(A, [T|E], S,   So)  :- var(A), !, thun(T, E, [A|S], So).

thun(int,    [], B, [int|B]).
thun(int, [A|B], D, E) :- thun(A, B, [int|D], E).

thun(bool,    [], B, [bool|B]).
thun(bool, [A|B], D, E) :- thun(A, B, [bool|D], E).

thun(list(A),    [], B, [list(A)|B]).
thun(list(C), [A|B], D, E) :- thun(A, B, [list(C)|D], E).

thun(symbol(A),    [], B, C) :- func(A, B, C).
thun(symbol(A), [C|D], B, F) :- func(A, B, E), thun(C, D, E, F).

thun(symbol(Combo), E, Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So).


func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).

func(cons,   [list(A),      B |S], [list([B|A])|S]).

func(swaack,    [list(R)|S],   [list(S)|R]).
func(stack,              S ,   [list(S)|S]).
func(clear,              _ ,            []).
func(first, [list([X|_])|S],   [     X |S]).
func(rest,  [list([_|X])|S],   [list(X)|S]).

func(bool, [_|S], [bool|S]).

func( + ,  [int, int|S], [int|S]).
func( - ,  [int, int|S], [int|S]).
func( * ,  [int, int|S], [int|S]).
func( / ,  [int, int|S], [int|S]).
func('%',  [int, int|S], [int|S]).

func( add ,  [int, int|S], [int|S]).
func( sub ,  [int, int|S], [int|S]).
func( mul ,  [int, int|S], [int|S]).
func( div ,  [int, int|S], [int|S]).
func( mod ,  [int, int|S], [int|S]).


combo(i,          [list(P)|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [list(P), X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).

combo(branch, [list(T), list(F), bool|S], StackOut, E, E) :-
    thun(T, S, StackOut),
    thun(F, S, StackOut).

combo(loop, [list(_), bool(false)|S], S, E,  E ).
combo(loop, [list(B),  bool(true)|S], S, Ei, Eo) :- append(B, [list(B), symbol(loop)|Ei], Eo).


stdin_to_codes(Codes) :- stdin_to_codes(code, [code|Codes]).
% Pass in and discard atom 'code' to prime stdin_to_codes/2.    

stdin_to_codes(-1, []) :- !.
stdin_to_codes(Code, [Code|Codes]) :-
    get_code(NextCode),
    stdin_to_codes(NextCode, Codes).


:- initialization((
    stdin_to_codes(Codes),
    text_to_expression(Codes, Expr),
    %read_term(AST, []),
    thun(Expr, StackIn, StackOut),
    write_term(StackIn, [quoted(true), max_depth(5)]), print('.\n'),
    write_term(StackOut, [quoted(true), max_depth(5)]), print('.\n')
    )).

