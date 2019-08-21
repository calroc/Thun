/*
    Copyright 2019 Simon Forman

    This file is part of Thun

    Thun is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Thun is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Thun.  If not see <http://www.gnu.org/licenses/>.

A fork function that actually forks.  Experimental.

*/
:- multifile(func/3).

func(fork, [F, G|S], [X, Y|S]) :-
    fork(F, S, R, ChildPID), % Send F off to the child,
    thun(G, S, [Y|_]),       % Run G locally,
    read_pipe(R, X),         % Collect the result from F,
    % FIXME deal with X=timeout...
    wait(ChildPID, Status).  % FIXME check status!!!

fork(Expr, Stack, In, ChildPID) :-
    mkpipe(In, Out),
    fork_prolog(ChildPID),
    korf(ChildPID, In, Out, Expr, Stack).

korf(0, In, Out, Expr, Stack) :- close(In),  % In the child.
    thun(Expr, Stack, [Result|_]),
    w(Out, Result), close(Out),
    halt.

korf(PID, _, Out, _, _) :-  % In the parent.
    integer(PID), PID =\= 0,
    close(Out).

read_pipe(In, Result) :-  % select/5, read the pipe or timeout.
    select([In], R, [], _, 1500),
    read_pipe_(R, In, Result),
    close(In).

read_pipe_([In], In,  Result) :- read(In, Result).
read_pipe_(  [],  _, timeout).

mkpipe(In, Out) :-
    create_pipe(In, Out),
    set_stream_buffering(Out, none),
    set_stream_buffering(In, none).

w(Out, Term) :-          % To get a term written out and recognized,
    write(Out, Term),    % you write it to the stream
    put_code(Out, 0'.),  % add a period at the end
    nl(Out),             % and a newline even if you set buffering
    flush_output(Out).   % to none, then flush for good measure.

