% A Tracing Meta-Interpreter for Thun

tmi(true).
tmi(!).
tmi((A, B)) :- tmi(A), tmi(B).
tmi(number(A)) :- number(A).
tmi(var(A)) :- var(A).

% Meta-logical print trace.
% (Could also be captured in a list or something instead.)
tmi(thun(E, Si, _)) :- portray_clause(Si-E), fail.

tmi(Goal) :-
    checky(Goal),
    clause(Goal, Body),  % doesn't work for e.g. +
    tmi(Body).

checky(Goal) :-
    Goal \= true,
    Goal \= (_,_),
    Goal \= var(_),
    Goal \= number(_),
    Goal \= !.

/*

[debug]  ?- tmi(thun([1, 2, swap], Si, So)).
_-[1, 2, swap].
[1|_]-[2, swap].
[2, 1|_]-[swap].
[1, 2|_]-[].
So = [1, 2|Si] ;
false.

[debug]  ?- tmi(thun([[1], 2, swons], Si, So)).
_-[[1], 2, swons].
[[1]|_]-[2, swons].
[2, [1]|_]-[swons].
[2, [1]|_]-[swap, cons].
[[1], 2|_]-[cons].
[[2, 1]|_]-[].
So = [[2, 1]|Si] .

[debug]  ?- tmi(thun([[1], 2, [swons], i], Si, So)).
_-[[1], 2, [swons], i].
[[1]|_]-[2, [swons], i].
[2, [1]|_]-[[swons], i].
[[swons], 2, [1]|_]-[i].
[2, [1]|_]-[swons].
[2, [1]|_]-[swap, cons].
[[1], 2|_]-[cons].
[[2, 1]|_]-[].
So = [[2, 1]|Si] .

*/