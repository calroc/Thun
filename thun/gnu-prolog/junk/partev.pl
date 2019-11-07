:- use_module(library(clpfd)).

%-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
%
%    Program 18.3 from "Art of Prolog"
%

process(Program, ReducedProgram) :-
    findall(PC1, (member(C1, Program), preduce(C1, PC1), portray_clause(PC1)), ReducedProgram).

preduce( (A :- B), (Pa :- Pb) ) :- !, preduce(B, Pb), preduce(A, Pa).
preduce(     true,       true ) :- !.
preduce(   (A, B),    Residue ) :- !, preduce(A, Pa), preduce(B, Pb), combine(Pa, Pb, Residue).
preduce(        A,          B ) :- should_fold(A, B), !.
preduce(        A,    Residue ) :- should_unfold(A), !, clause(A, B), preduce(B, Residue).
preduce(        A,          A ).

combine(true, B, B) :- !.
combine(A, true, A) :- !.
combine(A,    B, (A, B)).

test(Name, Program) :- program(Name, Clauses), process(Clauses, Program).

%-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

program(tundra, [
    ( thun([], S, S) ),
    ( thun(  [Lit|E], Si, So) :- literal(Lit), thun(E, [Lit|Si], So) ),
    ( thun(  [Def|E], Si, So) :- def(Def, Body), append(Body, E, Eo), thun(Eo, Si, So) ),
    ( thun( [Func|E], Si, So) :- func(Func, Si, S), thun(E, S, So) ),
    ( thun([Combo|E], Si, So) :- combo(Combo, Si, S, E, Eo), thun(Eo, S, So) )
    ]).

should_unfold(literal(Lit)).
should_unfold(def(Def, Body)).
should_unfold(func(Func, Si, So)).
should_unfold(combo(A, B, C, D, E)).
should_fold(sam, bill).

%-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

literal(V) :- var(V).
literal(I) :- number(I).
literal([]).
literal([_|_]).
literal(true).
literal(false).

func(cons, [A, B|S], [[B|A]|S]).
func(swap, [A, B|S],  [B, A|S]).
func(dup,     [A|S],  [A, A|S]).
func(pop,     [_|S],        S ).
func(+,    [A, B|S],     [C|S]) :- C #= A + B.
func(-,    [A, B|S],     [C|S]) :- C #= B - A.
func(*,    [A, B|S],     [C|S]) :- C #= A * B.
func(/,    [A, B|S],     [C|S]) :- C #= B div A.

func(nullary,   [P|S],   [X|S]) :- thun(P, S, [X|_]).  % Combinator.
func(infra,  [P, R|S],   [Q|S]) :- thun(P, R, Q).  % Combinator.

func(concat, [A, B|S],   [C|S]) :- append(B, A, C).
func(flatten,   [A|S],   [B|S]) :- flatten(A, B).
func(swaack,    [R|S],   [S|R]).
func(stack,        S ,   [S|S]).
func(clear,        _ ,      []).
func(first, [[X|_]|S],   [X|S]).
func(rest,  [[_|X]|S],   [X|S]).
func(unit,      [X|S], [[X]|S]).

combo(i,          [P|S], S, Ei, Eo) :- append(P, Ei, Eo).
combo(dip,     [P, X|S], S, Ei, Eo) :- append(P, [X|Ei], Eo).
combo(dipd, [P, X, Y|S], S, Ei, Eo) :- append(P, [Y, X|Ei], Eo).

def(at,[drop,first]).
def(b,[[i],dip,i]).
def(binary,[unary,popd]).

% thun([binary|A], C, D) :- thun([unary, popd|A], C, D).

%-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

% ?- test(tundra, _).
% thun([], A, A).
% thun([A|B], C, D) :- var(A), thun(B, [A|C], D).
% thun([A|B], C, D) :- number(A), thun(B, [A|C], D).
% thun([[]|A], B, C) :- thun(A, [[]|B], C).
% thun([[B|C]|A], D, E) :- thun(A, [[B|C]|D], E).
% thun([true|A], B, C) :- thun(A, [true|B], C).
% thun([false|A], B, C) :- thun(A, [false|B], C).
% thun([cons|A], [C, B|D], E) :- thun(A, [[B|C]|D], E).
% thun([swap|A], [C, B|D], E) :- thun(A, [B, C|D], E).
% thun([dup|A], [B|C], D) :- thun(A, [B, B|C], D).
% thun([pop|A], [_|B], C) :- thun(A, B, C).
% thun([+|E], [A, B|F], G) :-
%     (   integer(C)
%     ->  (   integer(A),
%             integer(B)
%         ->  C=:=A+B
%         ;   D=C,
%             clpfd:clpfd_equal(D, A+B)
%         )
%     ;   integer(A),
%         integer(B)
%     ->  (   var(C)
%         ->  C is A+B
%         ;   D is A+B,
%             clpfd:clpfd_equal(C, D)
%         )
%     ;   clpfd:clpfd_equal(C, A+B)
%     ),
%     thun(E, [C|F], G).
% thun([-|E], [B, A|F], G) :-
%     (   integer(C)
%     ->  (   integer(A),
%             integer(B)
%         ->  C=:=A-B
%         ;   D=C,
%             clpfd:clpfd_equal(D, A-B)
%         )
%     ;   integer(A),
%         integer(B)
%     ->  (   var(C)
%         ->  C is A-B
%         ;   D is A-B,
%             clpfd:clpfd_equal(C, D)
%         )
%     ;   clpfd:clpfd_equal(C, A-B)
%     ),
%     thun(E, [C|F], G).
% thun([*|E], [A, B|F], G) :-
%     (   integer(C)
%     ->  (   integer(A),
%             integer(B)
%         ->  C=:=A*B
%         ;   D=C,
%             clpfd:clpfd_equal(D, A*B)
%         )
%     ;   integer(A),
%         integer(B)
%     ->  (   var(C)
%         ->  C is A*B
%         ;   D is A*B,
%             clpfd:clpfd_equal(C, D)
%         )
%     ;   clpfd:clpfd_equal(C, A*B)
%     ),
%     thun(E, [C|F], G).
% thun([/|C], [B, A|E], F) :- D#=A div B, thun(C, [D|E], F).
% thun([nullary|C], [A|B], E) :- thun(A, B, [D|_]), thun(C, [D|B], E).
% thun([infra|C], [A, B|E], F) :- thun(A, B, D), thun(C, [D|E], F).
% thun([concat|C], [B, A|E], F) :- append(A, B, D), thun(C, [D|E], F).
% thun([flatten|B], [A|D], E) :- flatten(A, C), thun(B, [C|D], E).
% thun([swaack|A], [C|B], D) :- thun(A, [B|C], D).
% thun([stack|A], B, C) :- thun(A, [B|B], C).
% thun([clear|A], _, B) :- thun(A, [], B).
% thun([first|A], [[B|_]|C], D) :- thun(A, [B|C], D).
% thun([rest|A], [[_|B]|C], D) :- thun(A, [B|C], D).
% thun([unit|A], [B|C], D) :- thun(A, [[B]|C], D).
% thun([i|B], [A|D], E) :- append(A, B, C), thun(C, D, E).
% thun([dip|C], [A, B|E], F) :- append(A, [B|C], D), thun(D, E, F).
% thun([dipd|D], [A, C, B|F], G) :- append(A, [B, C|D], E), thun(E, F, G).
% true.





