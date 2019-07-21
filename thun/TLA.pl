
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

initi :- die_hard(0, 0, _, _).

next_dh(Small, Big, S, B) :-
    B #\= 4,
    type_ok(Small, Big),
    die_hard(Small, Big, Si, Bi),
    write(Small), write(" "), write(Big), write(" -> "), write(Si), write(" "), writeln(Bi),
    (Bi = 4 -> true ; next_dh(Si, Bi, S, B)).
next_dh(_, _, _, 4).



% die_hard(Small, Big, S, B).
die_hard(Small, Big, 3, Big) :- Small #\= 3, writeln("fill small").  % Fill small.
die_hard(Small, Big, Small, 5) :- Big #\= 5, writeln("fill big").  % Fill big.

die_hard(Small, Big, 0, Big) :- Small #> 0, writeln("empty small").  % empty small.
die_hard(Small, Big, Small, 0) :- Big #> 0, writeln("empty big").  % empty big.

die_hard(Small, Big, 0, B) :-  % small to big.
    Big #< 5, Small #> 0,
    Small + Big #=< 5,
    B is Small + Big, writeln("small to big").    
die_hard(Small, Big, S, 5) :-  % small to big.
    Big #< 5, Small #> 0,
    Small + Big #> 5,
    N is 5 - Big,
    (N #< Small -> S is Small - N ; S=0), writeln("small to big").    

die_hard(Small, Big, S, 0) :-  % big to small.
    Small #< 3, Big #> 0,
    Small + Big #=< 3,
    S is Small + Big, writeln("big to small").    

die_hard(Small, Big, 3, B) :-  % big to small.
    Small #< 3, Big #> 0,
    Small + Big #> 3,
    N is 3 - Small,
    (N #< Big -> B is Big - N ; B=0), writeln("big to small").    

/*

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