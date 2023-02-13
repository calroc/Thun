
:- dynamic(def/2).

% For number_codes/2 we want to just fail if the codes do not represent an integer.
% gprolog.html#number-atom%2F2
% > Number is a variable, Atom (or Chars or Codes) cannot be parsed as a number and the value of the syntax_error Prolog flag is error (section 8.22.1)
:- set_prolog_flag(syntax_error, fail).


joy(InputString, StackIn, StackOut) :-
    text_to_expression(InputString, Expression),
    !,
    thun(Expression, StackIn, StackOut).


joy_lex([tok(Token)|Ls]) --> chars(Token), !, joy_lex(Ls).
joy_lex([  lbracket|Ls]) --> "[",          !, joy_lex(Ls).
joy_lex([  rbracket|Ls]) --> "]",          !, joy_lex(Ls).

joy_lex(Ls) --> blank, !, joy_lex(Ls).

joy_lex([]) --> [].


% Then parse the tokens converting them to Prolog values and building up
% the list structures (if any.)

joy_parse([J|Js]) --> joy_term(J), !, joy_parse(Js).
joy_parse([]) --> [].

joy_term(list(J)) --> [lbracket], !, joy_parse(J), [rbracket].
joy_term(Token) --> [tok(Codes)], {joy_token(Token, Codes)}.

joy_token(int(I), Codes) :- number_codes(I, Codes), !.
joy_token(bool(true), "true") :- !.
joy_token(bool(false), "false") :- !.
joy_token(symbol(S), Codes) :- atom_codes(S, Codes).

text_to_expression(Text, Expression) :-
    phrase(joy_lex(Tokens), Text), !,
    phrase(joy_parse(Expression), Tokens).

% Apologies for all the (green, I hope) cuts.  The strength of the Joy
% syntax is that it's uninteresting.

chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> \+ blank, [Ch], { Ch \== 0'[, Ch \== 0'] }.


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

thun(int(A),    [], B, [int(A)|B]).
thun(int(C), [A|B], D, E) :- thun(A, B, [int(C)|D], E).

thun(bool(A),    [], B, [bool(A)|B]).
thun(bool(C), [A|B], D, E) :- thun(A, B, [bool(C)|D], E).

thun(list(A),    [], B, [list(A)|B]).
thun(list(C), [A|B], D, E) :- thun(A, B, [list(C)|D], E).

thun(symbol(A),    [], B, C) :- func(A, B, C).
thun(symbol(A), [C|D], B, F) :- func(A, B, E), thun(C, D, E, F).

thun(symbol(Combo), E, Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So).

thun(symbol(D),     [], Si, So) :- def(D, [DH| E]), thun(DH, E, Si, So).
thun(symbol(D), [H|E0], Si, So) :- def(D, [DH|DE]),
     append(DE, [H|E0], E), /* ................. */ thun(DH, E, Si, So).

% Some error handling.

thun(symbol(Unknown), _, _, _) :-
    \+ def(Unknown, _),
    \+ func(Unknown, _, _),
    \+ combo(Unknown, _, _, _, _),
    write('Unknown: '),
    write(Unknown),
    fail.


func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).

func(cons,   [list(A),      B |S], [list([B|A])|S]).
func(concat, [list(A), list(B)|S],     [list(C)|S]) :- append(B, A, C).

func(swaack,    [list(R)|S],   [list(S)|R]).
func(stack,              S ,   [list(S)|S]).
func(clear,              _ ,            []).
func(first, [list([X|_])|S],   [     X |S]).
func(rest,  [list([_|X])|S],   [list(X)|S]).

func(bool, [     int(0)|S], [bool(false)|S]).
func(bool, [   list([])|S], [bool(false)|S]).
func(bool, [bool(false)|S], [bool(false)|S]).

func(bool, [     int(N)|S], [bool(true)|S]) :- N #\= 0.
func(bool, [list([_|_])|S], [bool(true)|S]).
func(bool, [ bool(true)|S], [bool(true)|S]).

func( + ,  [int(A), int(B)|S], [int(A + B)|S]).
func( - ,  [int(A), int(B)|S], [int(B - A)|S]).
func( * ,  [int(A), int(B)|S], [int(A * B)|S]).
func( / ,  [int(A), int(B)|S], [int(B div A)|S]).
func('%',  [int(A), int(B)|S], [int(B mod A)|S]).

func( add ,  [int(A), int(B)|S], [int(A + B)|S]).
func( sub ,  [int(A), int(B)|S], [int(B - A)|S]).
func( mul ,  [int(A), int(B)|S], [int(A * B)|S]).
func( div ,  [int(A), int(B)|S], [int(B div A)|S]).
func( mod,  [int(A), int(B)|S], [int(B mod A)|S]).


combo(i,          [list(P)|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [list(P), X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).

combo(branch, [list(T), list(_),  bool(true)|S], S, Ei, Eo) :- append(T, Ei, Eo).
combo(branch, [list(_), list(F), bool(false)|S], S, Ei, Eo) :- append(F, Ei, Eo).

combo(loop, [list(_), bool(false)|S], S, E,  E ).
combo(loop, [list(B),  bool(true)|S], S, Ei, Eo) :- append(B, [list(B), symbol(loop)|Ei], Eo).


joy_def(Codes) :-
    text_to_expression(Codes, [symbol(Name)|Body]),
    assert_def(Name, Body).

assert_def(Symbol, Body) :-
    (  % Don't let this "shadow" functions or combinators.
        \+ func(Symbol, _, _),
        \+ combo(Symbol, _, _, _, _)
    ) -> (  % Replace any existing defs of this name.
        retractall(def(Symbol, _)),
        assertz(def(Symbol, Body))
    ) ; true.

:- initialization(joy_def("enstacken stack [clear] dip")).
