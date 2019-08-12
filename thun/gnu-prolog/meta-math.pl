/*

To handle comparision operators the possibility of exceptions due to
insufficiently instantiated arguments must be handled.  First try to make
the comparison and set the result to a Boolean atom.  If an exception
happens just leave the comparison expression as the result and some other
function or combinator will deal with it.  Example:

    func(>,  [A, B|S], [C|S]) :- catch(
            (B > A -> C=true ; C=false),
            _,
            C=(B>A)  % in case of error.
            ).

To save on conceptual overhead I've defined a term_expansion/2 that sets
up the func/3 for each op.
*/

term_expansion(comparison_operator(X), (func(X, [A, B|S], [C|S]) :-
    F =.. [X, B, A], catch((F -> C=true ; C=false), _, C=F))).

% I don't use Prolog-compatible op symbols in all cases.
term_expansion(comparison_operator(X, Y), (func(X, [A, B|S], [C|S]) :-
    F =.. [Y, B, A], catch((F -> C=true ; C=false), _, C=F))).

% Likewise for math operators, try to evaluate, otherwise use the
% symbolic form.

term_expansion(math_operator(X), (func(X, [A, B|S], [C|S]) :-
    F =.. [X, B, A], catch(C is F, _, C=F))).

term_expansion(math_operator(X, Y), (func(X, [A, B|S], [C|S]) :-
    F =.. [Y, B, A], catch(C is F, _, C=F))).


% Symbolic math.  Compute the answer, or derivative, or whatever, later.
math_operator(+).
math_operator(-).
math_operator(*).
math_operator(/).
math_operator(mod).


comparison_operator(>).
comparison_operator(<).
comparison_operator(>=).
comparison_operator(<=, =<).
comparison_operator(=, =:=).
comparison_operator(<>, =\=).


expand_op(Op, Term) :- Op, expand_term(Op, Term).

print_o(Stream, Op) :-
    findall(Term, expand_op(Op, Term), List),
    maplist(writeln(Stream), List).

writeln(Stream, Thing) :-
    portray_clause(Stream, Thing),
    nl(Stream).

do :-
    open(`math.pl`, write, Stream),
    print_o(Stream, math_operator(Op)),
    print_o(Stream, comparison_operator(Op)),
    print_o(Stream, comparison_operator(Op, Po)),
    close(Stream),
    halt.


