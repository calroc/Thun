

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


func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).

func(cons,   [list(A),      B |S], [list([B|A])|S]).

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



%:- initialization((
%    read_term(AST, []),
%    thun(AST, [], Stack),
%    write_term(Stack, [quoted(true)]), print('.\n')
%    )).

