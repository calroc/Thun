

do(DCG) :-
    fd_domain(X, 0, 9),
    fd_labeling(X),
    number_codes(X, [C]),
    DCG = `-->`(digit(C), [C]).



