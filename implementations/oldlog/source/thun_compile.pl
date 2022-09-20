:- use_module(library(clpfd)).
:- [thun].
/*

    Copyright © 2018, 2019, 2020 Simon Forman

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


The enviroment or context is a predicate reggy/4:

    reggy(FreePool, References, Values, Code)

The FreePool is a list of atoms that each denote a free register; 
References is a list of register atoms that keeps track of how many times
a register is used (it is in lieu of reference counting);  Values is an
assoc list mapping register atoms to their current values; and lastly
Code is a list of machine code predicates emitted by the compiler.

 */

% just to hush the linter, which won't respect consult/1.
% def(Name, _).
% func(Name, _, _).
% combo(Name, _, _, _, _).
% joy_parse(_, _, _).


encode_list(List, addr(list(List))) --> [].

% Retrieve the next free register.
get_reggy([], _, _) :- writeln('Out of Registers'), fail.
get_reggy([Reg|FreePool], Reg, FreePool).

% free one reference and de-allocate if it was the last.
free_reg(Reg, Value, reggy(FreePool0, References0, V0, Code),
                     reggy(FreePool,  References,  V,  Code)) :-
    select(Reg, References0, References),
    get_assoc(Reg, V0, Value),
    (  member(Reg, References)  % If reg is still in use
    -> FreePool=     FreePool0, V0=V % we can't free it yet
    ;  FreePool=[Reg|FreePool0], % otherwise we put it back in the pool.
       del_assoc(Reg, V0, _, V)
    ).

add_ref(Reg, reggy(FreePool,      References,  V, Code),
             reggy(FreePool, [Reg|References], V, Code)).

assoc_reg(Reg, Value, reggy(FreePool0,      References,  V0, Code),
                      reggy(FreePool,  [Reg|References], V,  Code)) :-
    get_reggy(FreePool0, Reg, FreePool),
    put_assoc(Reg, V0, Value, V).

fresh_env(reggy(  % Create a fresh new env/context with...
    [r0, r1, r2, r3,  % Available registers
     r4, r5, r6, r7,
     r8, r9, rA, rB,
     rC, rD, rE, rF],
    [],               % References.
    V,                % Register to value assoc list.
    []                % List of (pseudo-)machine code.
    )) :-
        empty_assoc(V).


emit([]) --> [].
emit([A|Rest]) --> emit(A), emit(Rest).
emit(A) --> { A \= [], A \= [_|_] }, emit_code(A).

emit_code(C, reggy(FreePool, References, V, [C|Code]),
             reggy(FreePool, References, V,    Code )).


/* Compiling

THread through the env/context as DCG dif-lists

*/

thun_compile(E, Si, So, Env) :-
    fresh_env(Env0),
    thun_compile(E, Si, So, Env0, Env).

thun_compile([], S, S) --> [].
thun_compile([Term|Rest], Si, So) --> thun_compile(Term, Rest, Si, So).

thun_compile(int(I), E, Si, So) -->
    emit(mov_imm(R, int(I))),
    assoc_reg(R, int(I)),
    thun_compile(E, [R|Si], So).

thun_compile(bool(B), E, Si, So) -->
    assoc_reg(R, bool(B)),
    thun_compile(E, [R|Si], So).

thun_compile(list(L), E, Si, So) -->
    encode_list(L, Addr),
    assoc_reg(R, Addr),
    emit(load_imm(R, Addr)),
    thun_compile(E, [R|Si], So).

thun_compile(symbol(Name), E, Si, So) -->
    {   def(Name, _)          } ->   def_compile(Name, E, Si, So) ;
    {  func(Name, _, _)       } ->  func_compile(Name, E, Si, So) ;
    { combo(Name, _, _, _, _) } -> combo_compile(Name, E, Si, So).


% I'm going to assume that any defs that can be compiled to funcs already
% have been.  Defs that can't be pre-compiled shove their body expression
% onto the pending expression (continuation) to be compiled "inline".

def_compile(Def, E, Si, So) -->
    { def(Def, Body), append(Body, E, Eo) },
    thun_compile(Eo, Si, So).


% swap (et. al.) doesn't change register refs nor introspect values
% so we can delegate its effect to the semantic relation.
non_alloc(swap).
non_alloc(rollup).
non_alloc(rolldown).

% Functions delegate to a per-function compilation relation.

func_compile(+, E, [A, B|S], So) --> !,
    free_reg(A, int(N)),
    free_reg(B, int(M)),
    assoc_reg(R, int(K)),
    emit(add(R, A, B)),
    { K #= N + M },
    % Update value in the context?
    thun_compile(E, [R|S], So).

func_compile(dup, E, [A|S], So) --> !,
    add_ref(A),
    thun_compile(E, [A, A|S], So).

func_compile(pop, E, [A|S], So) --> !,
    free_reg(A, _),
    thun_compile(E, S, So).

func_compile(cons, E, [List, Item|S], So) --> !,
    % Assume list is already stored in RAM
    % and item ...
    % allocate a cons cell
    emit(alloc_cons(list(Item, List))),
    % https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-33.html#%_sec_5.3
    thun_compile(E, S, So).

func_compile(Func, E, Si, So) --> { non_alloc(Func), !,
    func(Func, Si, S) },
    thun_compile(E, S, So).

func_compile(_Func, E, Si, So) -->
    % look up function, compile it...
    {Si = S},
    thun_compile(E, S, So).


combo_compile(_Combo, E, Si, So) -->
    % look up combinator, compile it...
    {Si = S, E = Eo},
    thun_compile(Eo, S, So).


compiler(InputString, StackIn, StackOut, FreePool0, References0, Values0, MachineCode0, FreePool,  References,  Values,  MachineCode) :-
    phrase(joy_parse(Expression), InputString), !,
    thun_compile(Expression, StackIn, StackOut,
        reggy(FreePool0, References0, Values0, MachineCode0),
        reggy(FreePool,  References,  Values,  MachineCode )
    ).

    % phrase(thun_compile(Expression, StackIn, StackOut, _), MachineCode, []).



compiler(InputString, StackIn, StackOut, FreePool,  References,  Values,  MachineCode) :-
    [r0, r1, r2, r3,  % Available registers
     r4, r5, r6, r7,
     r8, r9, rA, rB,
     rC, rD, rE, rF]=FreePool0,
    empty_assoc(Values0),
    compiler(InputString, StackIn, StackOut,
        FreePool0, [], Values0, MachineCode,
        FreePool,  References,  Values,  []).


% compiler(`3 +`, [r0|StackIn], StackOut, [r1, r2, r3, r4, ], [r0], Values0, MachineCode0, FreePool,  References,  Values,  MachineCode).

/* 

compiler(`2`, StackIn, Stack1,           FreePool0, References0, Values0, MachineCode0),
compiler(`3 +`,        Stack1, StackOut, FreePool0, References0, Values0, MachineCode1, FreePool,  References,  Values,  []).

?- compiler(`2`, StackIn, Stack1,           FreePool0, References0, Values0, MachineCode0),
   compiler(`3 +`,        Stack1, StackOut, FreePool0, References0, Values0, MachineCode1, FreePool,  References,  Values,  []).|    compiler(`3 +`,        Stack1, StackOut, FreePool0, References0, Values0, MachineCode1, FreePool,  References,  Values,  []).
Stack1 = StackOut, StackOut = [r0|StackIn],
FreePool0 = FreePool, FreePool = [r1, r2, r3, r4, r5, r6, r7, r8, r9|...],
References0 = References, References = [r0],
Values0 = t(r0, int(2), -, t, t),
MachineCode0 = [mov_imm(r0, int(2))],
MachineCode1 = [mov_imm(r1, int(3)), add(r0, r1, r0)],
Values = t(r0, int(_19548), -, t, t) .

 */


% show_compiler(InputString, StackIn, StackOut) :-
%     phrase(joy_parse(Expression), InputString), !,
%     phrase(thun_compile(Expression, StackIn, StackOut, reggy(_, _, V)), MachineCode, []),
%     maplist(portray_clause, MachineCode),
%     assoc_to_list(V, VP),
%     portray_clause(VP).



/*

So what happens when you compile just an integer literal?

?- thun_compile([int(23)], Si, So, reggy(FreePool, References, Values, Code)).
So = [r0|Si],
FreePool = [r1, r2, r3, r4, r5, r6, r7, r8, r9|...],
References = [r0],
Values = t(r0, int(23), -, t, t),
Code = [mov_imm(r0, int(23))].

The int is put onto the next available register, which is returned on the stack.


?- compiler(`2 3 +`, MachineCode, StackIn, StackOut).
MachineCode = [mov_imm(r0, int(2)), mov_imm(r1, int(3)), add(r0, r1, r0)],
StackOut = [r0|StackIn] ;
false.














?- phrase(grow, [symbol('&&')], Out), writeln(Out).


[
    list([
        list([symbol(stack)]),
        symbol(dip),
        symbol(swap),
        symbol(cons),
        symbol(swaack),
        list([symbol(i)]),
        symbol(dip),
        symbol(swaack),
        symbol(first)
        ]),
    msymbol(cons),
    list([
        list([symbol(stack)]),
        symbol(dip),
        symbol(swap),
        symbol(cons),
        symbol(swaack),
        list([symbol(i)]),
        symbol(dip),
        symbol(swaack),
        symbol(first),
        list([bool(false)])
    ]),
    symbol(dip),
    symbol(branch)
]


 */