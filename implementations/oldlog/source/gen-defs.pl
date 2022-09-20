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
thun(symbol(A), [], B, C) :-
    func(A, B, C).
thun(symbol(A), [C|D], B, F) :-
    func(A, B, E),
    thun(C, D, E, F).
thun(symbol(A), D, B, C) :-
    combo(A, B, C, D, []).
thun(symbol(A), C, B, G) :-
    combo(A, B, F, C, [D|E]),
    thun(D, E, F, G).
