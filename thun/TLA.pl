
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


next_dh(Small, Big, S, B, Mi, Mo) :-
    B #\= 4, type_ok(Small, Big),
    die_hard(Move, Small, Big, Si, Bi),
    State = [Move, Si, Bi],
    (Bi = 4 -> Mo=[State|Mi] ; next_dh(Si, Bi, S, B, [State|Mi], Mo)).


% die_hard(Small, Big, S, B).
die_hard(fill_small, Small, Big, 3, Big) :- Small #\= 3.
die_hard(  fill_big, Small, Big, Small, 5) :- Big #\= 5.

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
    S is Small + Big.

big_to_small(Small, Big, 3, B) :-  % 
    Small + Big #> 3,
    B is Big - (3 - Small).

small_to_big(Small, Big, 0, B) :-
    Small + Big #=< 5,
    B is Small + Big.

small_to_big(Small, Big, S, 5) :-
    Small + Big #> 5,
    S is Small - (5 - Big).

/*


call_with_depth_limit(next_dh(0, 0, S, B, Mi, Mo), 12, REsult).































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