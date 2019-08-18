:- multifile(func/3).

func(fork, [F, G|S], [X, Y|S]) :-
    fork(F, S, X, ChildPID),
    thun(G, S, [Y|_]),
    wait(ChildPID, Status).  % FIXME check status!!!

fork(Expr, Stack, Result, ChildPID) :-
    mkpipe(In, Out),
    fork_prolog(ChildPID),
    bar(ChildPID, In, Out, Expr, Stack, Result).

bar(0, In, Out, Expr, Stack, Result) :-
    close(In),
    thun(Expr, Stack, [Result|_]),
    w(Out, Result),
    close(Out),
    halt.

bar(P, In, Out, Expr, Stack, Result) :-
    integer(P), P =\= 0,
    close(Out),
    select([In], R, [], _, 1500),
    (R=[In] ->
        read(In, Result)
    ;
        Result=timeout
    ),
    close(In).

mkpipe(In, Out) :-
    create_pipe(In, Out),
    set_stream_buffering(Out, none),
    set_stream_buffering(In, none).

w(Out, Term) :-          % To get a term written out and recognized,
    write(Out, Term),    % you write it to the stream
    put_code(Out, 0'.),  % add a period at the end
    nl(Out),             % and a newline even if you set buffering
    flush_output(Out).   % to none, then flush for good measure.

