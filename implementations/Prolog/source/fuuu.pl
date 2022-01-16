f, [foo] --> [bar].  % f([bar|A], [foo|A]).
g, [bas] --> [quo].  % g([quo|A], [bas|A]).

end, [end] --> [].   % end(A, [end|A]).


k --> f, g, end.
% k(A, B) :- f(A, C), g(C, D), end(D, B).

/* So I DON'T know what I was doing.

f replaces foo with bar and then passes the whole enchilada on to the
next predicate.  I guess I somehow thought it was building an output list
or something?

 */

/*

?- gronk("fn", `[swap] [] branch `).

def fn(stack, expression, dictionary):
    (v1, (v2, (v3, stack))) = stack
    if v1:
        stack = (v2, (v3, stack))
    else:
        stack = (v3, (v2, stack))
    return stack, expression, dictionary



?- gronk("fn", `[swap] [] branch pop`).

def fn(stack, expression, dictionary):
    (v1, (v2, (v3, stack))) = stack
    if v1:
        (v4, stack) = (v2, (v3, stack))
    else:
        (v4, stack) = (v3, (v2, stack))
    return stack, expression, dictionary



?- gronk("fn", `over over > [swap] [] branch pop`).

def fn(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    v3 = v2 > v1
    if v3:
        (v4, stack) = (v1, (v2, stack))
    else:
        (v4, stack) = (v2, (v1, stack))
    return stack, expression, dictionary



Here's a case where factoring the pop to after the branch results in
inefficient code.  (Compare the function below to the versions above.  It
doesn't create and then immediately discard a v4 variable.)

?- gronk("fn", `[swap pop] [pop] branch`).

def fn(stack, expression, dictionary):
    (v1, (v2, (v3, stack))) = stack
    if v1:
        stack = (v3, stack)
    else:
        stack = (v2, stack)
    return stack, expression, dictionary


 */
/*

gronk_fn_list([symbol(*)], [int(A),int(A)|B], StackOut, [tab,"return ",stack_to_python(StackOut),", expression, dictionary",nl], CGTail, 1)


def fn(stack, expression, dictionary):
    tos = True
    while tos:
        (v1, (v2, stack)) = stack
        v3 = v2 % v1
        tos = v3 > 0
        stack = (v3, (v1, stack))
    (v4, stack) = stack
    return stack, expression, dictionary


Close, but broken.  THe boundaries between blocks are too permeable.

?- gronk("fn", `true [>] loop`).

def fn(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    tos = True
    while tos:
        v3 = v1 > v2
        tos = v3
    return stack, expression, dictionary




gronk_fn_list(
    [symbol(*)],
    [int(A),int(A)|B],
    StackOut,
    [tab,"return ",stack_to_python(StackOut),", expression, dictionary",nl],
    CGTail,
    1
    ).







?- gronk("fn", `stack`).

def fn(stack, expression, dictionary):
    stack = stack
    return ((), stack), expression, dictionary

SHould be

?- gronk("fn", `stack`).

def fn(stack, expression, dictionary):
    return (stack, stack), expression, dictionary



Okay then...

?- gronk("fn", `over over + stack dup`).

def fn(stack, expression, dictionary):
    (i1, (i2, stack)) = stack
    v1 = i2 + i1
    (v2, stack) = ((v1, (i1, (i2, stack))), (v1, (i1, (i2, stack))))
    return (v2, (v2, stack)), expression, dictionary


*/

/*


gronk_fn_body([int(A), int(B)|S], StackOut, IndentLevel, [symbol(Sym)|D], E) :-
    [symbol(Sym)|D]=[symbol(Sym)|F],
    bin_math_op(Sym, Op),
    G=F,
    gronk_fn_body([int(C)|S],
                  StackOut,
                  IndentLevel,
                  G,
                  H),
    E=[tabs(IndentLevel), term_to_python(C), " = ", term_to_python(A), Op, term_to_python(B), nl|H].

gronk_fn_body([int(A), int(B)|S], StackOut, IndentLevel, [symbol(Sym)|D], E) :-
    [symbol(Sym)|D]=[symbol(Sym)|F],
    bin_bool_op(Sym, Op),
    G=F,
    gronk_fn_body([bool(C)|S],
                  StackOut,
                  IndentLevel,
                  G,
                  H),
    E=[tabs(IndentLevel), term_to_python(C), " = ", term_to_python(A), Op, term_to_python(B), nl|H].

gronk_fn_body(S, S, _, A, [tab, "return ", stack_to_python(S), ", expression, dictionary", nl|A]).


Yeah, that can't be right...  I'm basically in "How did this ever work?" territory.








?- gronk("fn", `+ +`).

def fn(stack, expression, dictionary):
    (v1, (v2, (v3, stack))) = stack
    v4 = v1 + v2
    v5 = v4 + v3
    return (v5, stack), expression, dictionary


?- gronk("fn", `+ * - div mod`).

def fn(stack, expression, dictionary):
    (v1, (v2, (v3, (v4, (v5, (v6, stack)))))) = stack
    v7 = v1 + v2
    v8 = v7 * v3
    v9 = v8 - v4
    v10 = v9 // v5
    v11 = v10 % v6
    return (v11, stack), expression, dictionary








?- gronk_fn("name", [symbol(+)], Out), code_gen(Out, A, []), string_codes(S, A), writeln(""), writeln(S).

def name(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    stack = (v3, stack)
    return stack, expression, dictionary
    v3 = v1 + v2

Reversing the order reversed the output...  I wish i knew what I was
doing... :)

?- gronk_fn("name", [symbol(+)], Out), code_gen(Out, A, []), string_codes(S, A), writeln(""), writeln(S).

def name(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    v3 = v1 + v2
    stack = (v3, stack)
    return stack, expression, dictionary


?- gronk_fn("name", [symbol(+), symbol(+)], Out), code_gen(Out, A, []), !, string_codes(S, A), writeln(""), writeln(S).

def name(stack, expression, dictionary):
    (v1, (v2, (v3, stack))) = stack
    v4 = v1 + v2
    v5 = v4 + v3
    stack = (v5, stack)
    return stack, expression, dictionary

Whatever, it works now.

 */



/*

?- gronk_fn("name", [], [], Out), code_gen(Out, In, []).
Out = ["def ", "name", "(stack, expression, dictionary):", nl, tab, "return stack, expression, dictionary", nl],
In = "def name(stack, expressio...nary
".

?- listing(cg).
cg(A, D) :-
    A=[C|B],
    cg(B, E),
    phrase(C, D, E).
cg(A, A).

?- gronk_fn("name", [], [], Out), cg(Out,C).
Out = ["def ", "name", "(stack, expression, dictionary):", nl, tab, "return stack, expression, dictionary", nl],
C = "def name(stack, expressio...nary
" ;
Out = ["def ", "name", "(stack, expression, dictionary):", nl, tab, "return stack, expression, dictionary", nl],
C = [100, 101, 102, 32, 110, 97, 109, 101, 40|...] .

?- phrase((gronk_fn("name", []), cg), [], Out).
Out = "def name(stack, expressio...nary
" ;
Out = [100, 101, 102, 32, 110, 97, 109, 101, 40|...] ;
Out = [100, 101, 102, 32, 110, 97, 109, 101, 40|...] ;
Out = [100, 101, 102, 32, 110, 97, 109, 101, 40|...] ;
Out = [100, 101, 102, 32, 110, 97, 109, 101, 40|...] ;
Out = [100, 101, 102, 32, 110, 97, 109, 101, "(stack, expression, dictionary):"|...] ;
Out = [100, 101, 102, 32, "name", "(stack, expression, dictionary):", nl, tab, "return stack, expression, dictionary"|...] .

Bleah.














?- gronk_fn("name", [symbol(+)], Out), code_gen(Out, A, []), string_codes(S, A), writeln(""), writeln(S).

def name(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    stack = (v3, stack)
    return stack, expression, dictionary
    v3 = v1 + v2


Almost, but not quite.  The assignment is happening after the return call!



=-=-=-=--=-=-=-=-==-=-

?- gronk_fn("name", [], Out), code_gen(Out, A, []), string_codes(S, A), writeln(""), writeln(S).

def name(stack, expression, dictionary):
    stack = stack
    stack = stack
    return stack, expression, dictionary

Out = ["def ", "name", "(stack, expression, dictionary):", nl, tab, stack_to_python([]), " = stack", nl, tab|...],
A = "def name(stack, expressio...nary
",
S = "def name(stack, expression, dictionary):\n    stack = stack\n    stack = stack\n    return stack, expression, dictionary\n" .

?- gronk_fn("name", [symbol(+)], Out), writeln(Out).
[def ,name,(stack, expression, dictionary):,nl,tab,stack_to_python([int(_274090),int(_274100)|_274096]), = stack,nl,tab,stack = ,stack_to_python([int(_274110)|_274096]),nl,tab,return stack, expression, dictionary,nl,tabs(1),term_to_python(_274110), = ,term_to_python(_274090), + ,term_to_python(_274100),nl]
Out = ["def ", "name", "(stack, expression, dictionary):", nl, tab, stack_to_python([int(_274090), int(...)|...]), " = stack", nl, tab|...] .

?- gronk_fn("name", [symbol(+)], Out), code_gen(Out, A, []), string_codes(S, A), writeln(""), writeln(S).

def name(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    stack = (v3, stack)
    return stack, expression, dictionary
    v3 = v1 + v2

Out = ["def ", "name", "(stack, expression, dictionary):", nl, tab, stack_to_python([int(v1), int(...)]), " = stack", nl, tab|...],
A = "def name(stack, expressio...+ v2
",
S = "def name(stack, expression, dictionary):\n    (v1, (v2, stack)) = stack\n    stack = (v3, stack)\n    return stack, expression, dictionary\n    v3 = v1 + v2\n" .




=-=-=-=--=-=-=-=-==-=-

There we go...

?- gronk_fn("name", [symbol(+)], Out), code_gen(Out, A, []), string_codes(S, A), writeln(""), writeln(S).

def name(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    v3 = v1 + v2
    stack = (v3, stack)
    return stack, expression, dictionary






















?- do(`dup dup +`).

(v5, stack) = stack
stack = ((v5 + v5), (v5, stack))

true .

That's better.

?- do(`[* / - + dup] [dup + over *] branch * * `).

tos, stack = stack
if tos:
    (v16, (v17, stack)) = stack
    stack = ((v17 * (v16 + v16)), (v17, stack))
else:
    (v18, (v19, (v20, (v21, (v22, stack))))) = stack
    stack = (((v21 - (v20 // (v18 * v19))) + v22), (((v21 - (v20 // (v18 * v19))) + v22), stack))
(v23, (v24, (v25, stack))) = stack
stack = (((v23 * v24) * v25), stack)

true .

That's beautiful.


Of course, if we carried through the expression for the stack...


    tos, stack = stack
    if tos:
        (v16, (v17, stack)) = stack
        (v23, (v24, (v25, stack))) = ((v17 * (v16 + v16)), (v17, stack))
    else:
        (v18, (v19, (v20, (v21, (v22, stack))))) = stack
        (v23, (v24, (v25, stack))) = (((v21 - (v20 // (v18 * v19))) + v22), (((v21 - (v20 // (v18 * v19))) + v22), stack))
    stack = (((v23 * v24) * v25), stack)

we could assign the new variables directly from the previous stage,
saving the packing and unpacking of the "stack" tuple.

"Something to think about."


With symbolic Booleans this works now (there were a lot of bugs but I
don't know what they were.)

?- do(`<= [+] [-] branch`).

(v1, (v2, stack)) = stack
stack = ((v2 <= v1), stack)
tos, stack = stack
if tos:
    (v3, (v4, stack)) = stack
    stack = ((v4 - v3), stack)
else:
    (v5, (v6, stack)) = stack
    stack = ((v5 + v6), stack)

true.



Now we can compile GCD:

?- do(`true [tuck % dup 0 >] loop pop`).

stack = True, stack
tos, stack = stack
while tos:
    (v9, (v10, stack)) = stack
    stack = ((v10 % v9), ((v10 % v9), (v9, stack)))
    stack = 0, stack
    (v11, (v12, stack)) = stack
    stack = ((v12 > v11), stack)
    tos, stack = stack
(v13, stack) = stack
stack = stack

true.


It's not ideal, for example, it computes v10 % v9 twice.  :(

We would like, e.g.:

tos = True
while tos:
    (v9, (v10, stack)) = stack
    vN = v10 % v9
    stack = ((vN), ((vN), (v9, stack)))
    (v11, (v12, stack)) = 0, stack
    stack = ((v12 > v11), stack)
    tos, stack = stack
(v13, stack) = stack
stack = stack


tos = True
while tos:
    (v9, (v10, stack)) = stack
    vN = v10 % v9
    stack = ((vN), ((vN), (v9, stack)))
    (v12, stack) = stack
    stack = ((v12 > 0), stack)
    tos, stack = stack
(v13, stack) = stack


tos = True
while tos:
    (v9, (v10, stack)) = stack
    vN = v10 % v9
    stack = ((vN), ((vN), (v9, stack)))
    (v12, stack) = stack
    tos = (v12 > 0)
(v13, stack) = stack



tos = True
while tos:
    (v9, (v10, stack)) = stack
    vN = v10 % v9
    (v12, stack) = ((vN), ((vN), (v9, stack)))
    tos = (v12 > 0)
(v13, stack) = stack




tos = True
while tos:
    (v9, (v10, stack)) = stack
    vN = v10 % v9
    stack = (vN, (v9, stack))
    tos = (vN > 0)
(v13, stack) = stack

Anyhow...  I could keep going but you get the idea.  The simple
mechanical translation results in correct but inefficient code.
I'm not too worried about it, this is great progress nonetheless, but it
would be nice to tighten up that code gen.

What's that "stack = stack" doing in there?





do(`[[dup dup] [dup] branch dup [dup] loop dup] loop dup`).

do(`[dup] [[dup dup dup] [dup dup] branch] branch`).


*/
