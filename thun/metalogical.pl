% A Tracing Meta-Interpreter for Thun

alpha(true).
alpha((A, B)) :- alpha(A), alpha(B).
alpha(number(A)) :- !, number(A).
alpha(var(A)) :- !, var(A).
alpha(!) :- !.

% Meta-logical print trace.
% (Could also be captured in a list or something instead.)
alpha(thun(E, Si, _)) :- portray_clause(Si-E), fail.

alpha(Goal) :-
    checky(Goal),
    clause(Goal, Body),  % doesn't work for e.g. +
    alpha(Body).

checky(Goal) :-
    Goal \= true,
    Goal \= (_,_),
    Goal \= var(_),
    Goal \= number(_),
    Goal \= !.

/*

[debug]  ?- alpha(thun([1, 2, swap], Si, So)).
_-[1, 2, swap].
[1|_]-[2, swap].
[2, 1|_]-[swap].
[1, 2|_]-[].
So = [1, 2|Si] ;
false.

[debug]  ?- alpha(thun([[1], 2, swons], Si, So)).
_-[[1], 2, swons].
[[1]|_]-[2, swons].
[2, [1]|_]-[swons].
[2, [1]|_]-[swap, cons].
[[1], 2|_]-[cons].
[[2, 1]|_]-[].
So = [[2, 1]|Si] .

[debug]  ?- alpha(thun([[1], 2, [swons], i], Si, So)).
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