thun(int(A), [], B, [int(A)|B]).
thun(int(C), [A|B], D, E) :-
    thun(A, B, [int(C)|D], E).
thun(bool(A), [], B, [bool(A)|B]).
thun(bool(C), [A|B], D, E) :-
    thun(A, B, [bool(C)|D], E).
thun(list(A), [], B, [list(A)|B]).
thun(list(C), [A|B], D, E) :-
    thun(A, B, [list(C)|D], E).
thun(symbol(--), A, C, D) :-
    append([symbol(-)], A, B),
    thun(int(1), B, C, D).
thun(symbol(?), A, C, D) :-
    append([symbol(bool)], A, B),
    thun(symbol(dup), B, C, D).
thun(symbol(&&), A, C, D) :-
    append(
           [ symbol(cons),
             list([symbol(nullary), list([bool(false)])]),
             symbol(dip),
             symbol(branch)
           ],
           A,
           B),
    thun(list([symbol(nullary)]), B, C, D).
thun(symbol(++), A, C, D) :-
    append([symbol(+)], A, B),
    thun(int(1), B, C, D).
thun(symbol('||'), A, C, D) :-
    append(
           [ symbol(cons),
             list([symbol(nullary)]),
             symbol(dip),
             list([bool(true)]),
             symbol(branch)
           ],
           A,
           B),
    thun(list([symbol(nullary)]), B, C, D).
thun(symbol('!-'), A, C, D) :-
    append([symbol(>=)], A, B),
    thun(int(0), B, C, D).
thun(symbol(abs), A, C, D) :-
    append([int(0), symbol(<), list([]), list([symbol(neg)]), symbol(branch)],
           A,
           B),
    thun(symbol(dup), B, C, D).
thun(symbol(anamorphism), A, C, D) :-
    append([symbol(swap), list([symbol(dip), symbol(swons)]), symbol(genrec)],
           A,
           B),
    thun(list([symbol(pop), list([])]), B, C, D).
thun(symbol(app1), A, C, D) :-
    append([symbol(infrst)], A, B),
    thun(symbol(grba), B, C, D).
thun(symbol(app2), A, C, D) :-
    append([symbol(dip), list([symbol(infrst)]), symbol(cons), symbol(ii)],
           A,
           B),
    thun(list([symbol(grba), symbol(swap), symbol(grba), symbol(swap)]),
         B,
         C,
         D).
thun(symbol(app3), A, C, D) :-
    append([symbol(appN)], A, B),
    thun(int(3), B, C, D).
thun(symbol(appN), A, C, D) :-
    append([symbol(cons), symbol(dip), symbol(map), symbol(disenstacken)],
           A,
           B),
    thun(list([symbol(grabN)]), B, C, D).
thun(symbol(at), A, C, D) :-
    append([symbol(first)], A, B),
    thun(symbol(drop), B, C, D).
thun(symbol(average), A, C, D) :-
    append([list([symbol(size)]), symbol(cleave), symbol(/)], A, B),
    thun(list([symbol(sum), int(1), symbol('.0'), symbol(*)]),
         B,
         C,
         D).
thun(symbol(b), A, C, D) :-
    append([symbol(dip), symbol(i)], A, B),
    thun(list([symbol(i)]), B, C, D).
thun(symbol(binary), A, C, D) :-
    append([symbol(popd)], A, B),
    thun(symbol(unary), B, C, D).
thun(symbol(ccons), A, C, D) :-
    append([symbol(cons)], A, B),
    thun(symbol(cons), B, C, D).
thun(symbol(cleave), A, C, D) :-
    append([symbol(popdd)], A, B),
    thun(symbol(fork), B, C, D).
thun(symbol(clop), A, C, D) :-
    append([symbol(popdd)], A, B),
    thun(symbol(cleave), B, C, D).
thun(symbol(codireco), A, C, D) :-
    append([symbol(dip), symbol(rest), symbol(cons)], A, B),
    thun(symbol(cons), B, C, D).
thun(symbol(dinfrirst), A, C, D) :-
    append([symbol(infrst)], A, B),
    thun(symbol(dip), B, C, D).
thun(symbol(disenstacken), A, C, D) :-
    append([list([symbol(uncons), symbol(?)]), symbol(loop), symbol(pop)],
           A,
           B),
    thun(symbol(?), B, C, D).
thun(symbol(down_to_zero), A, C, D) :-
    append([list([symbol(dup), symbol(--)]), symbol(while)], A, B),
    thun(list([int(0), symbol(>)]), B, C, D).
thun(symbol(drop), A, C, D) :-
    append([symbol(times)], A, B),
    thun(list([symbol(rest)]), B, C, D).
thun(symbol(dupdd), A, C, D) :-
    append([symbol(dipd)], A, B),
    thun(list([symbol(dup)]), B, C, D).
thun(symbol(dupdipd), A, C, D) :-
    append([symbol(dipd)], A, B),
    thun(symbol(dup), B, C, D).
thun(symbol(enstacken), A, C, D) :-
    append([list([symbol(clear)]), symbol(dip)], A, B),
    thun(symbol(stack), B, C, D).
thun(symbol(fork), A, C, D) :-
    append([symbol(app2)], A, B),
    thun(list([symbol(i)]), B, C, D).
thun(symbol(fourth), A, C, D) :-
    append([symbol(third)], A, B),
    thun(symbol(rest), B, C, D).
thun(symbol(gcd), A, C, D) :-
    append(
           [ list([symbol(tuck), symbol(mod), symbol(dup), int(0), symbol(>)]),
             symbol(loop),
             symbol(pop)
           ],
           A,
           B),
    thun(bool(true), B, C, D).
thun(symbol(grabN), A, C, D) :-
    append([symbol(swap), list([symbol(cons)]), symbol(times)], A, B),
    thun(list([]), B, C, D).
thun(symbol(grba), A, C, D) :-
    append([symbol(dip)], A, B),
    thun(list([symbol(stack), symbol(popd)]), B, C, D).
thun(symbol(hypot), A, C, D) :-
    append([symbol(ii), symbol(+), symbol(sqrt)], A, B),
    thun(list([symbol(sqr)]), B, C, D).
thun(symbol(ifte), A, C, D) :-
    append([symbol(dipd), symbol(swap), symbol(branch)], A, B),
    thun(list([symbol(nullary)]), B, C, D).
thun(symbol(ii), A, C, D) :-
    append([symbol(dupdip), symbol(i)], A, B),
    thun(list([symbol(dip)]), B, C, D).
thun(symbol(infra), A, C, D) :-
    append([symbol(swaack), list([symbol(i)]), symbol(dip), symbol(swaack)],
           A,
           B),
    thun(symbol(swons), B, C, D).
thun(symbol(infrst), A, C, D) :-
    append([symbol(first)], A, B),
    thun(symbol(infra), B, C, D).
thun(symbol(make_generator), A, C, D) :-
    append([symbol(ccons)], A, B),
    thun(list([symbol(codireco)]), B, C, D).
thun(symbol(neg), A, C, D) :-
    append([symbol(swap), symbol(-)], A, B),
    thun(int(0), B, C, D).
thun(symbol(not), A, C, D) :-
    append([list([bool(false)]), symbol(branch)], A, B),
    thun(list([bool(true)]), B, C, D).
thun(symbol(nullary), A, C, D) :-
    append([symbol(dinfrirst)], A, B),
    thun(list([symbol(stack)]), B, C, D).
thun(symbol(of), A, C, D) :-
    append([symbol(at)], A, B),
    thun(symbol(swap), B, C, D).
thun(symbol(pam), A, C, D) :-
    append([symbol(map)], A, B),
    thun(list([symbol(i)]), B, C, D).
thun(symbol(popd), A, C, D) :-
    append([symbol(dip)], A, B),
    thun(list([symbol(pop)]), B, C, D).
thun(symbol(popdd), A, C, D) :-
    append([symbol(dipd)], A, B),
    thun(list([symbol(pop)]), B, C, D).
thun(symbol(popop), A, C, D) :-
    append([symbol(pop)], A, B),
    thun(symbol(pop), B, C, D).
thun(symbol(popopd), A, C, D) :-
    append([symbol(dip)], A, B),
    thun(list([symbol(popop)]), B, C, D).
thun(symbol(popopdd), A, C, D) :-
    append([symbol(dipd)], A, B),
    thun(list([symbol(popop)]), B, C, D).
thun(symbol(primrec), A, C, D) :-
    append([symbol(genrec)], A, B),
    thun(list([symbol(i)]), B, C, D).
thun(symbol(product), A, C, D) :-
    append([symbol(swap), list([symbol(*)]), symbol(step)], A, B),
    thun(int(1), B, C, D).
thun(symbol(quoted), A, C, D) :-
    append([symbol(dip)], A, B),
    thun(list([symbol(unit)]), B, C, D).
thun(symbol(range), A, C, D) :-
    append([list([int(1), symbol(-), symbol(dup)]), symbol(anamorphism)],
           A,
           B),
    thun(list([int(0), symbol(<=)]), B, C, D).
thun(symbol(range_to_zero), A, C, D) :-
    append([list([symbol(down_to_zero)]), symbol(infra)], A, B),
    thun(symbol(unit), B, C, D).
thun(symbol(reverse), A, C, D) :-
    append([symbol(swap), symbol(shunt)], A, B),
    thun(list([]), B, C, D).
thun(symbol(rrest), A, C, D) :-
    append([symbol(rest)], A, B),
    thun(symbol(rest), B, C, D).
thun(symbol(run), A, C, D) :-
    append([symbol(swap), symbol(infra)], A, B),
    thun(list([]), B, C, D).
thun(symbol(second), A, C, D) :-
    append([symbol(first)], A, B),
    thun(symbol(rest), B, C, D).
thun(symbol(shunt), A, C, D) :-
    append([symbol(step)], A, B),
    thun(list([symbol(swons)]), B, C, D).
thun(symbol(size), A, C, D) :-
    append([symbol(swap), list([symbol(pop), symbol(++)]), symbol(step)],
           A,
           B),
    thun(int(0), B, C, D).
thun(symbol(spiral_next), A, C, D) :-
    append(
           [ list(
                  [ list([symbol('!-')]),
                    list([list([symbol(++)])]),
                    list([list([symbol(--)])]),
                    symbol(ifte),
                    symbol(dip)
                  ]),
             list(
                  [ list([symbol(pop), symbol('!-')]),
                    list([symbol(--)]),
                    list([symbol(++)]),
                    symbol(ifte)
                  ]),
             symbol(ifte)
           ],
           A,
           B),
    thun(list(
              [ list([list([symbol(abs)]), symbol(ii), symbol(<=)]),
                list(
                     [ list([symbol(<>)]),
                       list([symbol(pop), symbol('!-')]),
                       symbol('||')
                     ]),
                symbol(&&)
              ]),
         B,
         C,
         D).
thun(symbol(split_at), A, C, D) :-
    append([list([symbol(take)]), symbol(clop)], A, B),
    thun(list([symbol(drop)]), B, C, D).
thun(symbol(sqr), A, C, D) :-
    append([symbol(*)], A, B),
    thun(symbol(dup), B, C, D).
thun(symbol(step_zero), A, C, D) :-
    append([symbol('roll>'), symbol(step)], A, B),
    thun(int(0), B, C, D).
thun(symbol(sum), A, C, D) :-
    append([symbol(swap), list([symbol(+)]), symbol(step)], A, B),
    thun(int(0), B, C, D).
thun(symbol(swons), A, C, D) :-
    append([symbol(cons)], A, B),
    thun(symbol(swap), B, C, D).
thun(symbol(take), A, C, D) :-
    append([symbol(rolldown), list([symbol(shift)]), symbol(times), symbol(pop)],
           A,
           B),
    thun(list([]), B, C, D).
thun(symbol(ternary), A, C, D) :-
    append([symbol(popd)], A, B),
    thun(symbol(binary), B, C, D).
thun(symbol(third), A, C, D) :-
    append([symbol(second)], A, B),
    thun(symbol(rest), B, C, D).
thun(symbol(unary), A, C, D) :-
    append([symbol(popd)], A, B),
    thun(symbol(nullary), B, C, D).
thun(symbol(unquoted), A, C, D) :-
    append([symbol(dip)], A, B),
    thun(list([symbol(i)]), B, C, D).
thun(symbol(unswons), A, C, D) :-
    append([symbol(swap)], A, B),
    thun(symbol(uncons), B, C, D).
thun(symbol(while), A, C, D) :-
    append(
           [ list([symbol(nullary)]),
             symbol(cons),
             symbol(dup),
             symbol(dipd),
             symbol(concat),
             symbol(loop)
           ],
           A,
           B),
    thun(symbol(swap), B, C, D).
thun(symbol(x), A, C, D) :-
    append([symbol(i)], A, B),
    thun(symbol(dup), B, C, D).
thun(symbol(words), [], A, [B|A]) :-
    words(B).
thun(symbol(words), [A|B], D, E) :-
    words(C),
    thun(A, B, [C|D], E).
thun(symbol(swap), [], [B, A|C], [A, B|C]).
thun(symbol(swap), [A|B], [D, C|E], F) :-
    thun(A, B, [C, D|E], F).
thun(symbol(dup), [], [A|B], [A, A|B]).
thun(symbol(dup), [A|B], [C|D], E) :-
    thun(A, B, [C, C|D], E).
thun(symbol(pop), [], [_|A], A).
thun(symbol(pop), [A|B], [_|C], D) :-
    thun(A, B, C, D).
thun(symbol(cons), [], [list(B), A|C], [list([A|B])|C]).
thun(symbol(cons), [A|B], [list(D), C|E], F) :-
    thun(A, B, [list([C|D])|E], F).
thun(symbol(concat), [], [list(C), list(B)|A], [list(D)|A]) :-
    append(B, C, D).
thun(symbol(concat), [C|D], [list(B), list(A)|F], G) :-
    append(A, B, E),
    thun(C, D, [list(E)|F], G).
thun(symbol(flatten), [], [list(B)|A], [list(C)|A]) :-
    flatten(B, C).
thun(symbol(flatten), [B|C], [list(A)|E], F) :-
    flatten(A, D),
    thun(B, C, [list(D)|E], F).
thun(symbol(swaack), [], [list(B)|A], [list(A)|B]).
thun(symbol(swaack), [A|B], [list(D)|C], E) :-
    thun(A, B, [list(C)|D], E).
thun(symbol(stack), [], A, [list(A)|A]).
thun(symbol(stack), [A|B], C, D) :-
    thun(A, B, [list(C)|C], D).
thun(symbol(clear), [], _, []).
thun(symbol(clear), [A|B], _, C) :-
    thun(A, B, [], C).
thun(symbol(first), [], [list([A|_])|B], [A|B]).
thun(symbol(first), [A|B], [list([C|_])|D], E) :-
    thun(A, B, [C|D], E).
thun(symbol(rest), [], [list([_|A])|B], [list(A)|B]).
thun(symbol(rest), [A|B], [list([_|C])|D], E) :-
    thun(A, B, [list(C)|D], E).
thun(symbol(unit), [], [A|B], [list([A])|B]).
thun(symbol(unit), [A|B], [C|D], E) :-
    thun(A, B, [list([C])|D], E).
thun(symbol(rolldown), [], [C, A, B|D], [A, B, C|D]).
thun(symbol(rolldown), [A|B], [E, C, D|F], G) :-
    thun(A, B, [C, D, E|F], G).
thun(symbol(dupd), [], [A, B|C], [A, B, B|C]).
thun(symbol(dupd), [A|B], [C, D|E], F) :-
    thun(A, B, [C, D, D|E], F).
thun(symbol(over), [], [B, A|C], [A, B, A|C]).
thun(symbol(over), [A|B], [D, C|E], F) :-
    thun(A, B, [C, D, C|E], F).
thun(symbol(tuck), [], [A, B|C], [A, B, A|C]).
thun(symbol(tuck), [A|B], [C, D|E], F) :-
    thun(A, B, [C, D, C|E], F).
thun(symbol(shift), [], [list([B|A]), list(C)|D], [list(A), list([B|C])|D]).
thun(symbol(shift), [A|B], [list([D|C]), list(E)|F], G) :-
    thun(A,
         B,
         [list(C), list([D|E])|F],
         G).
thun(symbol(rollup), [], [B, C, A|D], [A, B, C|D]).
thun(symbol(rollup), [A|B], [D, E, C|F], G) :-
    thun(A, B, [C, D, E|F], G).
thun(symbol(uncons), [], [list([B|A])|C], [list(A), B|C]).
thun(symbol(uncons), [A|B], [list([D|C])|E], F) :-
    thun(A, B, [list(C), D|E], F).
thun(symbol(bool), [], [int(0)|A], [bool(false)|A]).
thun(symbol(bool), [A|B], [int(0)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol(bool), [], [list([])|A], [bool(false)|A]).
thun(symbol(bool), [A|B], [list([])|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol(bool), [], [bool(false)|A], [bool(false)|A]).
thun(symbol(bool), [A|B], [bool(false)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol(bool), [], [int(B)|A], [bool(true)|A]) :-
    B#\=0.
thun(symbol(bool), [B|C], [int(A)|D], E) :-
    A#\=0,
    thun(B, C, [bool(true)|D], E).
thun(symbol(bool), [], [list([_|_])|A], [bool(true)|A]).
thun(symbol(bool), [A|B], [list([_|_])|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol(bool), [], [bool(true)|A], [bool(true)|A]).
thun(symbol(bool), [A|B], [bool(true)|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol('empty?'), [], [list([])|A], [bool(true)|A]).
thun(symbol('empty?'), [A|B], [list([])|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol('empty?'), [], [list([_|_])|A], [bool(false)|A]).
thun(symbol('empty?'), [A|B], [list([_|_])|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol('list?'), [], [list(_)|A], [bool(true)|A]).
thun(symbol('list?'), [A|B], [list(_)|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol('list?'), [], [bool(_)|A], [bool(false)|A]).
thun(symbol('list?'), [A|B], [bool(_)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol('list?'), [], [int(_)|A], [bool(false)|A]).
thun(symbol('list?'), [A|B], [int(_)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol('list?'), [], [symbol(_)|A], [bool(false)|A]).
thun(symbol('list?'), [A|B], [symbol(_)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol('one-or-more?'), [], [list([_|_])|A], [bool(true)|A]).
thun(symbol('one-or-more?'), [A|B], [list([_|_])|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol('one-or-more?'), [], [list([])|A], [bool(false)|A]).
thun(symbol('one-or-more?'), [A|B], [list([])|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol(and), [], [bool(true), bool(true)|A], [bool(true)|A]).
thun(symbol(and), [A|B], [bool(true), bool(true)|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol(and), [], [bool(true), bool(false)|A], [bool(false)|A]).
thun(symbol(and), [A|B], [bool(true), bool(false)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol(and), [], [bool(false), bool(true)|A], [bool(false)|A]).
thun(symbol(and), [A|B], [bool(false), bool(true)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol(and), [], [bool(false), bool(false)|A], [bool(false)|A]).
thun(symbol(and), [A|B], [bool(false), bool(false)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol(or), [], [bool(true), bool(true)|A], [bool(true)|A]).
thun(symbol(or), [A|B], [bool(true), bool(true)|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol(or), [], [bool(true), bool(false)|A], [bool(true)|A]).
thun(symbol(or), [A|B], [bool(true), bool(false)|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol(or), [], [bool(false), bool(true)|A], [bool(true)|A]).
thun(symbol(or), [A|B], [bool(false), bool(true)|C], D) :-
    thun(A, B, [bool(true)|C], D).
thun(symbol(or), [], [bool(false), bool(false)|A], [bool(false)|A]).
thun(symbol(or), [A|B], [bool(false), bool(false)|C], D) :-
    thun(A, B, [bool(false)|C], D).
thun(symbol(+), [], [int(C), int(D)|A], [int(B)|A]) :-
    (   integer(B)
    ->  (   integer(C),
            integer(D)
        ->  B=:=C+D
        ;   E=B,
            clpfd:clpfd_equal(E, C+D)
        )
    ;   integer(C),
        integer(D)
    ->  (   var(B)
        ->  B is C+D
        ;   E is C+D,
            clpfd:clpfd_equal(B, E)
        )
    ;   clpfd:clpfd_equal(B, C+D)
    ).
thun(symbol(+), [E|F], [int(A), int(B)|G], H) :-
    (   integer(C)
    ->  (   integer(A),
            integer(B)
        ->  C=:=A+B
        ;   D=C,
            clpfd:clpfd_equal(D, A+B)
        )
    ;   integer(A),
        integer(B)
    ->  (   var(C)
        ->  C is A+B
        ;   D is A+B,
            clpfd:clpfd_equal(C, D)
        )
    ;   clpfd:clpfd_equal(C, A+B)
    ),
    thun(E, F, [int(C)|G], H).
thun(symbol(-), [], [int(D), int(C)|A], [int(B)|A]) :-
    (   integer(B)
    ->  (   integer(C),
            integer(D)
        ->  B=:=C-D
        ;   E=B,
            clpfd:clpfd_equal(E, C-D)
        )
    ;   integer(C),
        integer(D)
    ->  (   var(B)
        ->  B is C-D
        ;   E is C-D,
            clpfd:clpfd_equal(B, E)
        )
    ;   clpfd:clpfd_equal(B, C-D)
    ).
thun(symbol(-), [E|F], [int(B), int(A)|G], H) :-
    (   integer(C)
    ->  (   integer(A),
            integer(B)
        ->  C=:=A-B
        ;   D=C,
            clpfd:clpfd_equal(D, A-B)
        )
    ;   integer(A),
        integer(B)
    ->  (   var(C)
        ->  C is A-B
        ;   D is A-B,
            clpfd:clpfd_equal(C, D)
        )
    ;   clpfd:clpfd_equal(C, A-B)
    ),
    thun(E, F, [int(C)|G], H).
thun(symbol(*), [], [int(C), int(D)|A], [int(B)|A]) :-
    (   integer(B)
    ->  (   integer(C),
            integer(D)
        ->  B=:=C*D
        ;   E=B,
            clpfd:clpfd_equal(E, C*D)
        )
    ;   integer(C),
        integer(D)
    ->  (   var(B)
        ->  B is C*D
        ;   E is C*D,
            clpfd:clpfd_equal(B, E)
        )
    ;   clpfd:clpfd_equal(B, C*D)
    ).
thun(symbol(*), [E|F], [int(A), int(B)|G], H) :-
    (   integer(C)
    ->  (   integer(A),
            integer(B)
        ->  C=:=A*B
        ;   D=C,
            clpfd:clpfd_equal(D, A*B)
        )
    ;   integer(A),
        integer(B)
    ->  (   var(C)
        ->  C is A*B
        ;   D is A*B,
            clpfd:clpfd_equal(C, D)
        )
    ;   clpfd:clpfd_equal(C, A*B)
    ),
    thun(E, F, [int(C)|G], H).
thun(symbol(/), [], [int(D), int(C)|A], [int(B)|A]) :-
    B#=C div D.
thun(symbol(/), [C|D], [int(B), int(A)|F], G) :-
    E#=A div B,
    thun(C, D, [int(E)|F], G).
thun(symbol('%'), [], [int(D), int(C)|A], [int(B)|A]) :-
    (   integer(B)
    ->  (   integer(C),
            integer(D),
            D=\=0
        ->  B=:=C mod D
        ;   E=B,
            clpfd:clpfd_equal(E, C mod D)
        )
    ;   integer(C),
        integer(D),
        D=\=0
    ->  (   var(B)
        ->  B is C mod D
        ;   E is C mod D,
            clpfd:clpfd_equal(B, E)
        )
    ;   clpfd:clpfd_equal(B, C mod D)
    ).
thun(symbol('%'), [E|F], [int(B), int(A)|G], H) :-
    (   integer(C)
    ->  (   integer(A),
            integer(B),
            B=\=0
        ->  C=:=A mod B
        ;   D=C,
            clpfd:clpfd_equal(D, A mod B)
        )
    ;   integer(A),
        integer(B),
        B=\=0
    ->  (   var(C)
        ->  C is A mod B
        ;   D is A mod B,
            clpfd:clpfd_equal(C, D)
        )
    ;   clpfd:clpfd_equal(C, A mod B)
    ),
    thun(E, F, [int(C)|G], H).
thun(symbol('/%'), [], [int(D), int(C)|A], [int(B), int(E)|A]) :-
    B#=C div D,
    (   integer(E)
    ->  (   integer(C),
            integer(D),
            D=\=0
        ->  E=:=C mod D
        ;   F=E,
            clpfd:clpfd_equal(F, C mod D)
        )
    ;   integer(C),
        integer(D),
        D=\=0
    ->  (   var(E)
        ->  E is C mod D
        ;   F is C mod D,
            clpfd:clpfd_equal(E, F)
        )
    ;   clpfd:clpfd_equal(E, C mod D)
    ).
thun(symbol('/%'), [E|F], [int(B), int(A)|H], I) :-
    ( G#=A div B,
      (   integer(C)
      ->  (   integer(A),
              integer(B),
              B=\=0
          ->  C=:=A mod B
          ;   D=C,
              clpfd:clpfd_equal(D, A mod B)
          )
      ;   integer(A),
          integer(B),
          B=\=0
      ->  (   var(C)
          ->  C is A mod B
          ;   D is A mod B,
              clpfd:clpfd_equal(C, D)
          )
      ;   clpfd:clpfd_equal(C, A mod B)
      )
    ),
    thun(E, F, [int(G), int(C)|H], I).
thun(symbol(pm), [], [int(C), int(D)|A], [int(B), int(F)|A]) :-
    (   integer(B)
    ->  (   integer(C),
            integer(D)
        ->  B=:=C+D
        ;   E=B,
            clpfd:clpfd_equal(E, C+D)
        )
    ;   integer(C),
        integer(D)
    ->  (   var(B)
        ->  B is C+D
        ;   E is C+D,
            clpfd:clpfd_equal(B, E)
        )
    ;   clpfd:clpfd_equal(B, C+D)
    ),
    (   integer(F)
    ->  (   integer(D),
            integer(C)
        ->  F=:=D-C
        ;   G=F,
            clpfd:clpfd_equal(G, D-C)
        )
    ;   integer(D),
        integer(C)
    ->  (   var(F)
        ->  F is D-C
        ;   G is D-C,
            clpfd:clpfd_equal(F, G)
        )
    ;   clpfd:clpfd_equal(F, D-C)
    ).
thun(symbol(pm), [G|H], [int(A), int(B)|I], J) :-
    ( (   integer(C)
      ->  (   integer(A),
              integer(B)
          ->  C=:=A+B
          ;   D=C,
              clpfd:clpfd_equal(D, A+B)
          )
      ;   integer(A),
          integer(B)
      ->  (   var(C)
          ->  C is A+B
          ;   D is A+B,
              clpfd:clpfd_equal(C, D)
          )
      ;   clpfd:clpfd_equal(C, A+B)
      ),
      (   integer(E)
      ->  (   integer(B),
              integer(A)
          ->  E=:=B-A
          ;   F=E,
              clpfd:clpfd_equal(F, B-A)
          )
      ;   integer(B),
          integer(A)
      ->  (   var(E)
          ->  E is B-A
          ;   F is B-A,
              clpfd:clpfd_equal(E, F)
          )
      ;   clpfd:clpfd_equal(E, B-A)
      )
    ),
    thun(G, H, [int(C), int(E)|I], J).
thun(symbol(>), [], [int(C), int(B)|A], [E|A]) :-
    B#>C#<==>D,
    r_truth(D, E).
thun(symbol(>), [D|E], [int(B), int(A)|G], H) :-
    ( A#>B#<==>C,
      r_truth(C, F)
    ),
    thun(D, E, [F|G], H).
thun(symbol(<), [], [int(C), int(B)|A], [E|A]) :-
    B#<C#<==>D,
    r_truth(D, E).
thun(symbol(<), [D|E], [int(B), int(A)|G], H) :-
    ( A#<B#<==>C,
      r_truth(C, F)
    ),
    thun(D, E, [F|G], H).
thun(symbol(=), [], [int(C), int(B)|A], [E|A]) :-
    B#=C#<==>D,
    r_truth(D, E).
thun(symbol(=), [D|E], [int(B), int(A)|G], H) :-
    ( A#=B#<==>C,
      r_truth(C, F)
    ),
    thun(D, E, [F|G], H).
thun(symbol(>=), [], [int(C), int(B)|A], [E|A]) :-
    B#>=C#<==>D,
    r_truth(D, E).
thun(symbol(>=), [D|E], [int(B), int(A)|G], H) :-
    ( A#>=B#<==>C,
      r_truth(C, F)
    ),
    thun(D, E, [F|G], H).
thun(symbol(<=), [], [int(C), int(B)|A], [E|A]) :-
    B#=<C#<==>D,
    r_truth(D, E).
thun(symbol(<=), [D|E], [int(B), int(A)|G], H) :-
    ( A#=<B#<==>C,
      r_truth(C, F)
    ),
    thun(D, E, [F|G], H).
thun(symbol(<>), [], [int(C), int(B)|A], [E|A]) :-
    B#\=C#<==>D,
    r_truth(D, E).
thun(symbol(<>), [D|E], [int(B), int(A)|G], H) :-
    ( A#\=B#<==>C,
      r_truth(C, F)
    ),
    thun(D, E, [F|G], H).
thun(symbol(A), D, B, C) :-
    combo(A, B, C, D, []).
thun(symbol(A), C, B, G) :-
    combo(A, B, F, C, [D|E]),
    thun(D, E, F, G).
