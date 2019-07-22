
:- use_module(library(clpfd)).


% relly(PC, I, PCnext, Inext).

initial_state :- relly(start, 0, _, _).

relly(start, _I, middle, Inext) :- Inext in 0..1000.
relly(middle, I,   done, Inext) :- Inext #= I + 1.


% next(PC, I) :- 
next(done, _).
next(PC, I) :- PC \= done, relly(PC, I, PCnext, Inext), next(PCnext, Inext).


% ?- relly(start, 17, middle, 534).
% true.

% ?- relly(middle, 534, 77, done).
% false.

% ?- initial_state.
% true.

% ?- relly(start, 0, PC, I).
% PC = middle,
% I in 0..1000.

% ?- relly(start, 0, PC, I), relly(PC, I, PCnext, Inext).
% PC = middle,
% PCnext = done,
% I in 0..1000,
% I+1#=Inext,
% Inext in 1..1001.

























type_ok(Small, Big) :- Small in 0..3, Big in 0..5.


next_dh(Moves) :- next_dh(0, 0, Moves).

next_dh(Small, Big, [[Move, Si, Bi]|Moves]) :-
    type_ok(Small, Big),
    die_hard(Move, Small, Big, Si, Bi),
    (Bi = 4 -> Moves = [] ; next_dh(Si, Bi, Moves)).


die_hard( fill_small, Small, Big, 3, Big) :- Small #< 3.
die_hard(   fill_big, Small, Big, Small, 5) :- Big #< 5.
die_hard(empty_small, Small, Big, 0, Big) :- Small #> 0.
die_hard(  empty_big, Small, Big, Small, 0) :- Big #> 0.

die_hard(small_to_big, Small, Big, S, B) :-
    Big #< 5, Small #> 0,
    small_to_big(Small, Big, S, B).

die_hard(big_to_small, Small, Big, S, B) :-
    Small #< 3, Big #> 0,
    big_to_small(Small, Big, S, B).


big_to_small(Small, Big, S, 0) :-
    Small + Big #=< 3,
    S #= Small + Big.

big_to_small(Small, Big, 3, B) :-
    Small + Big #> 3,
    B #= Big - (3 - Small).


small_to_big(Small, Big, 0, B) :-
    Small + Big #=< 5,
    B #= Small + Big.

small_to_big(Small, Big, S, 5) :-
    Small + Big #> 5,
    S #= Small - (5 - Big).


































/*

With some manual reflow of the list for presentation...

?- call_with_depth_limit(next_dh(Moves), 11, _).
Moves = [
    [fill_big, 0, 5],
    [big_to_small, 3, 2],
    [empty_small, 0, 2],
    [big_to_small, 2, 0],
    [fill_big, 2, 5],
    [big_to_small, 3, 4]
    ] ;
true.





























call_with_depth_limit(next_dh(0, 0, S, B), 12, REsult).

find...

fill big
0 0 -> 0 5
big to small
0 5 -> 3 2
empty small
3 2 -> 0 2
big to small
0 2 -> 2 0
fill big
2 0 -> 2 5
big to small
2 5 -> 3 4


*/