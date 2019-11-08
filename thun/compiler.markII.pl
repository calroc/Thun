/*

Copyright © 2018-2019 Simon Forman

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



The Joy interpreter that this implements is pretty crude.  the only types
are 16-bit integers and linked lists.  The lists are 32-bit words divided
into two 16-bit fields.  The high half is the node value and the low half
points directly (not offset) to the next cell, zero terminates the list.

The expression is expected to be already written in RAM as a linked list at
the time the mainloop starts.  As yet there is no support for actually doing
this.  Both the new stack and expression cells are written to the same heap
intermixed.  The stack and expression pointers never decrease, the whole
history of the computation is recorded in RAM.  If the computation of the
expression overruns the end of RAM (or 16-bits whichever comes first) the
machine crashes.

At the moment, functions are recognized by setting high bit, but I don't
think I remembered to set the bits during compilation, so it won't work
at all right now.  Er...  Boo.  Anyhow, the whole thing is very crude and
not at all what I am hoping eventually to build.

But it's a start, and I feel good about emitting machine code (even if the
program doesn't do anything useful yet.)

*/
:- use_module(library(assoc)).
:- use_module(library(clpfd)).


do :- Program = [
    ヲ,∅,⟴,ヵ,メ,ョ,
    [グ,ケ,ゲ,ド,ゴ,サ],ヮ(cons),
    [ザ,シ],ヮ(dup),
    [グ,ス,[],[ジ,ス,[ズ,セ,ス,[ゼ,ソ],[タ,ゾ],ヰ,ヂ],ヱ],ヰ,チ],ヮ(i),
    [ヶ,ペ],ワ(new),
    [ナ,ズ,セ,ネ,ヒ,ド,ャ,ペ],ワ(swap),
    [new,cons],≡(unit),
    [dup,i],≡(x),
    [swap,cons],≡(swons)
    ],
compile_program(Program, Binary),
write_binary('joy_asm.bin', Binary).


compile_program(Program, Binary) :-
    phrase((init, ⦾(Program, IR)), [], [Context]),
    phrase(⟐(IR), ASM),
    phrase(linker(ASM), EnumeratedASM),
    % writeln(EnumeratedASM),
    foo(Context),
    phrase(asm(EnumeratedASM), Binary).

foo(Context) :-
    get_assoc(dictionary, Context, D),
    assoc_to_list(D, Dictionary),
    portray_clause(Dictionary).


/*

This first stage ⦾//2 converts the Joy description into a kind of intermediate
representation that models the Joy interpreter on top of the machine but doesn't
actually use assembly instructions.  It also manages the named registers and
memory locations so thet don't appear in the program.

The idea here is to extract the low-level "primitives" needed to define the Joy
interpreter to make it easier to think about (and maybe eventually retarget other
CPUs.)

 */

⦾([], []) --> [].

⦾([ヲ|Terms], Ts) -->  % Preamble.
    % Initialize context/state/symbol table.
    set(dict_ptr, 11),  % Reg 11 is a pointer used during func lookup.
    set(dict_top, 12),  % Reg 12 points to top of dictionary.
    set(dict, 0),  % Address of top of dict during compilation.
    set(done, _DONE),  % DONE label (logic variable.)
    set(expr, 4),  % Reg 4 points to expression.
    set(halt, _HALT),  % HALT label (logic variable.)
    set(main, _MAIN),  % MAIN label (logic variable.)
    set(reset, _Reset),  % Reset label (logic variable.)
    set(sp, 2),  % Reg 2 points to just under top of stack.
    set(temp0, 6),  % Reg 6 is a temp var.
    set(temp1, 7),  % Reg 7 is a temp var.
    set(temp2, 8),  % Reg 8 is a temp var.
    set(temp3, 9),  % Reg 9 is a temp var.
    set(term, 5),  % Reg 4 holds current term.
    set(tos, 3),  % Reg 3 holds Top of Stack.
    ⦾(Terms, Ts).

⦾([ヵ|Terms], [  % Initialization.
    jump(Over),  % Oberon bootloader writes MemLim to RAM[12] and
    asm(allocate(_, 16)),  % stackOrg to RAM[24], we don't need these
    label(Over),  % but they must not be allowed to corrupt our code.
    set_reg_const(0, 0),  % zero out the root cell.
    write_ram(0, 0),
    set_reg_const(SP, 0x1000),
    set_reg_const(EXPR, 0x500),
    set_reg_label(DICT_TOP, LastWord),
    set_reg_const(TOS, 0),
    set_reg_const(TERM, 0),
    asm(store_word(TOS, SP, 0))  % RAM[SP] := 0
    |Ts]) -->
    get([dict_top, DICT_TOP, expr, EXPR, sp, SP, term, TERM, tos, TOS]),
    ⦾(Terms, Ts), get(dict, LastWord).

⦾([メ|Terms], [  % Mainloop.
    label(MAIN),
    if_zero(EXPR, HALT),
    deref(EXPR, TEMP0),
    % At this point EXPR holds the record word of the expression and TEMP0 
    % has a copy of the address of the record.
    split_pair(TERM, TEMP1, EXPR, TEMP0),
    % Now Term has the term's record data and temp1 has the address of the term.
    if_literal(TERM, PUSH),
    % if it is a symbol the rest of it is the pointer to the machine code.
    lookup(TERM),  % Jump to command.
    label(PUSH), push(TOS, TERM, SP),  % stack = TERM, stack
    label(DONE), write_ram(SP, TOS),   % RAM[SP] := TOS
    jump(MAIN)
    |Ts]) -->
    get([dict_ptr, DICT_PTR, dict_top, DICT_TOP, done, DONE, expr, EXPR,
         halt, HALT, main, MAIN, sp, SP, term, TERM, tos, TOS,
         temp0, TEMP0, temp1, TEMP1]),
    ⦾(Terms, Ts).

⦾([Body, ≡(NameAtom)|Terms], [defi(Name, B, Prev, I, SP, TOS)|Ts]) -->
    get(dict, Prev), set(dict, Name), get([sp, SP, tos, TOS]),
    inscribe(NameAtom, Name), ⦾(Terms, Ts), lookup(i, I), lookup(Body, B).

⦾([Body, ヮ(NameAtom)|Terms], [definition(Name, DONE, B, Prev)|Ts]) -->
    get(dict, Prev), set(dict, Name), inscribe(NameAtom, Name),
    get(done, DONE), ⦾([Body, Terms], [B, Ts]).

⦾([Body, ワ(NameAtom)|Terms], [definition(Name, MAIN, B, Prev)|Ts]) -->
    get(dict, Prev), set(dict, Name), inscribe(NameAtom, Name),
    get(main, MAIN), ⦾([Body, Terms], [B, Ts]).

⦾([P, T, E, ヰ|Terms], [br(Predicate, Then, Else)|Ts]) -->
    ⦾([P, T, E, Terms], [Predicate, Then, Else, Ts]).

⦾([P, B, ヱ|Terms], [repeat_until(Predicate, Body)|Ts]) -->
    ⦾([P, B, Terms], [Predicate, Body, Ts]).

⦾([Term|Terms], [T|Ts]) --> ⦾(Term, T), ⦾(Terms, Ts).

⦾(∅, dw(0))                    --> [].
⦾(⟴, label(Reset))             --> get(reset, Reset).
⦾(ョ, halt(HALT))               --> get(halt, HALT).
⦾(グ, pop(TEMP0, TOS))          --> get(temp0, TEMP0), get(tos, TOS).
⦾(シ, push(TOS, TOS, SP))       --> get(tos, TOS), get(sp, SP).
⦾(ケ, high_half(TEMP1, TOS))    --> get(temp1, TEMP1), get(tos, TOS).
⦾(サ, merge(SP, TOS))           --> get(tos, TOS), get(sp, SP).
⦾(ザ, swap_halves(TOS))         --> get(tos, TOS).
⦾(ズ, deref(TEMP0))             --> get(temp0, TEMP0).
⦾(ス, if_zero(TEMP0))           --> get(temp0, TEMP0).
⦾(ソ, asm(mov(EXPR, TEMP3)))    --> get(expr, EXPR), get(temp3, TEMP3).
⦾(ャ, asm(ior(TOS, TEMP1, SP))) --> get(tos, TOS), get(temp1, TEMP1), get(sp, SP).
⦾(タ, add_const(TEMP2, SP, 8))  --> get(temp2, TEMP2), get(sp, SP).
⦾(ジ, add_const(TEMP3, SP, 4))  --> get(temp3, TEMP3), get(sp, SP).
⦾(チ, add_const(SP, SP, 4))     --> get(sp, SP).
⦾(セ, chop_word(TEMP1, TEMP0))  --> get(temp0, TEMP0), get(temp1, TEMP1).
⦾(ト, chop_word(TEMP0, TOS))    --> get(temp0, TEMP0), get(tos, TOS).
⦾(ネ, chop_word(TEMP2, TOS))    --> get(temp2, TEMP2), get(tos, TOS).
⦾(ゼ, or_inplace(TEMP1,  EXPR)) --> get(expr, EXPR), get(temp1, TEMP1).
⦾(ゲ, or_inplace(TEMP0, TEMP1)) --> get(temp0, TEMP0), get(temp1, TEMP1).
⦾(ヒ, or_inplace(TEMP0, TEMP2)) --> get(temp0, TEMP0), get(temp2, TEMP2).
⦾(ゾ, or_inplace(TEMP1, TEMP2)) --> get(temp1, TEMP1), get(temp2, TEMP2).
⦾(ド, write_cell(TEMP0, SP))    --> get(temp0, TEMP0), get(sp, SP).
⦾(ヂ, write_cell(TEMP1, SP))    --> get(temp1, TEMP1), get(sp, SP).
⦾(ペ, write_cell(TOS,   SP))    --> get(tos, TOS), get(sp, SP).
⦾(ゴ, low_half(TOS))            --> get(tos, TOS).
⦾(ナ, low_half(TEMP0, TOS))     --> get(temp0, TEMP0), get(tos, TOS).
⦾(ヶ, low_half(TOS, SP))        --> get(sp, SP), get(tos, TOS).


/* 

Context (state) manipulation for the ⦾//2 relation.

Association lists are used to keep a kind of symbol table as well as a dictionary
of name atoms to address logic variables for defined Joy functions.

*/

init, [Context] -->
    {empty_assoc(C), empty_assoc(Dictionary),
     put_assoc(dictionary, C, Dictionary, Context)}.

get([]) --> !.
get([Key, Value|Ts]) --> !, get(Key, Value), get(Ts).

get(Key, Value) --> state(Context), {get_assoc(Key, Context, Value)}.
set(Key, Value) --> state(ContextIn, ContextOut),
    {put_assoc(Key, ContextIn, Value, ContextOut)}.

inscribe(NameAtom, Label) --> state(ContextIn, ContextOut),
    {get_assoc(dictionary, ContextIn, Din),
     put_assoc(NameAtom, Din, Label, Dout),
     put_assoc(dictionary, ContextIn, Dout, ContextOut)}.

lookup([], []) --> !.
lookup([T|Ts], [V|Vs]) --> !, lookup(T, V), lookup(Ts, Vs).
lookup(NameAtom, Label) --> state(Context),
    {get_assoc(dictionary, Context, D), get_assoc(NameAtom, D, Label)}.

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].


/*

This second stage ⟐//1 converts the intermediate representation to assembly
language.

*/

⟐([]) --> [].
⟐([Term|Terms]) --> ⟐(Term), ⟐(Terms).

⟐(if_literal(Reg, Label)) -->
    [ior_imm(0, Reg, -30),  % get just the two tag bits.
     sub_imm(0, 0, 2),  % subtract 2 to check if result is zero.
     ne_offset(Label)].

% if reg = 0 jump to label.
⟐(if_zero(Reg, Label)) --> [sub_imm(Reg, Reg, 0), eq_offset(Label)].

⟐(set_reg_const(Reg, Immediate)) --> {Immediate >= -(2^15), Immediate < 2^16}, !,
    [mov_imm(Reg, Immediate)].

⟐(set_reg_const(Reg, Immediate)) --> {Immediate >= 0, Immediate < 2^33}, !,  % FIXME: handle negative numbers.
    {high_half_word(Immediate, HighHalf), low_half_word(Immediate,  LowHalf)},
    [  mov_imm_with_shift(Reg, HighHalf),         ior_imm(Reg, Reg, LowHalf)].

⟐(set_reg_label(Reg, Label)) --> [mov_imm(Reg, Label)].

⟐(        noop) --> [mov(0, 0)].
⟐(  halt(Halt)) --> [label(Halt), do_offset(Halt)].
⟐(    asm(ASM)) --> [ASM].
⟐(label(Label)) --> [label(Label)].
⟐( jump(Label)) --> [do_offset(Label)].
⟐(     dw(Int)) --> [word(Int)].

⟐(      low_half(Reg)) --> [and_imm(Reg, Reg, 0xffff)].
⟐( low_half(To, From)) --> [and_imm(To, From, 0xffff)].
⟐(     high_half(Reg)) --> [mov_imm_with_shift(0, 0xffff), and(Reg, Reg, 0)].
⟐(high_half(To, From)) --> [mov_imm_with_shift(0, 0xffff), and(To, From, 0)].

⟐(swap_halves(Register)) --> [ror_imm(Register, Register, 16)].
⟐(swap_halves(To, From)) --> [ror_imm(      To,     From, 16)].

⟐(high_half_to(To, From)) --> ⟐([swap_halves(To, From), low_half(To)]).

⟐(split_word(To, From)) --> ⟐([high_half_to(To, From), low_half(From)]).

⟐(chop_word(To, From)) --> ⟐([high_half(To, From), low_half(From)]).

⟐(merge(SP, TOS)) -->
    [lsl_imm(0, SP, 16),
     ior(TOS, TOS, 0),
     add_imm(SP, SP, 4)].

⟐(push(TOS, TERM, SP)) -->
    [lsl_imm(TOS, TERM, 16),  %  TOS := TERM << 16
     ior(TOS, TOS, SP),       %  TOS := TOS | SP
     add_imm(SP, SP, 4)].     % SP += 1 (word, not byte)

⟐( write_ram(To, From)) -->                     [store_word(From, To, 0)].
⟐(write_cell(From, SP)) --> [add_imm(SP, SP, 4), store_word(From, SP, 0)].

⟐(deref(Reg)) --> [load_word(Reg, Reg, 0)].

⟐(deref(Reg, Temp)) -->
    [mov(Temp, Reg),  % Save the address for adding it to offsets later.
     load_word(Reg, Reg, 0)].

⟐(or_inplace(To, From)) --> [ior(To, To, From)].

⟐(definition(Label, Exit, Body, Prev)) -->
    ⟐([
        dw(Prev),
        label(Label),
        Body,
        jump(Exit)
    ]).

⟐(defi(Label, Body, Prev, I, SP, TOS)) -->
    ⟐([dw(Prev),
       label(Label),
       defi_def(BodyLabel, SP, TOS),
       jump(I)]),
    dexpr(Body, BodyLabel).

⟐(defi_def(Label, SP, TOS)) -->
    [mov_imm_with_shift(TOS, Label),
     ior(TOS, TOS, SP)],
    ⟐(write_cell(TOS, SP)).

⟐(lookup(TERM)) -->
    [mov_imm_with_shift(0, 0x3fff),
     ior_imm(0, 0, 0xffff),
     and(0, 0, TERM),
     eq(0)].  % Jump to term's machine code.

⟐(repeat_until(Condition, Body)) -->
    {add_label(Condition, End, ConditionL)},
    ⟐([
        label(Loop),
        Body,
        ConditionL,
        jump(Loop),
        label(End)
    ]).

⟐(br(Condition, [], Else)) --> !,
    {add_label(Condition, END, ConditionL)},
    ⟐([ConditionL, Else, label(END)]).

⟐(br(Condition, Then, Else)) -->
    {add_label(Condition, THEN, ConditionL)},
    ⟐([
        ConditionL, Else, jump(END),
        label(THEN), Then, label(END)
    ]).

⟐(add_const(To, From, Immediate)) --> [add_imm(To, From, Immediate)].

⟐(pop(Reg, TOS)) --> ⟐([split_word(Reg, TOS), deref(TOS)]).

% From is a register containing a pair record
% FromAddr is a register containing the address of the record in From
% after,
% To is a register that will contain the record from the head
% ToAddr holds the address of the record in To.
% From is a register containing a pair record
% FromAddr is a register containing the address of the record in From
⟐(split_pair(To, ToAddr, From, FromAddr)) -->
    [ior_imm(ToAddr, From, -15),  % roll right 15 bits
     % No need to mask off high bits as the type tag for pairs is 00
     add(ToAddr, ToAddr, FromAddr),
     load_word(To, ToAddr, 0),  % Bring the record in from RAM.
     and_imm(From, From, 0x7fff),  % Mask off  lower 15 bits.
     add(From, From, FromAddr)  % Add the address to the offset.
    ].


/* 

Support for ⟐//1 second stage.

The dexpr//2 DCG establishes a sequence of labeled expr_cell/2 pseudo-assembly
memory locations as a linked list that encodes a Prolog list of Joy function
labels comprising e.g. the body of some Joy definition.

*/

dexpr([], 0) --> [].
dexpr([Func|Rest], ThisCell) -->
    [label(ThisCell), expr_cell(Func, NextCell)],
    dexpr(Rest, NextCell).

/*

The add_label/3 relation is a meta-logical construct that accepts a comparision
predicate (e.g. if_zero/2) and "patches" it by adding the Label logic variable
to the end.

*/

add_label(CmpIn, Label, CmpOut) :-
    CmpIn =.. F,
    append(F, [Label], G),
    CmpOut =.. G.

/*

Two simple masking predicates.

*/

high_half_word(I, HighHalf) :- HighHalf is I >> 16 /\ 0xFFFF.
low_half_word( I,  LowHalf) :-  LowHalf is I       /\ 0xFFFF.


/*

Linker

*/

linker(ASM) --> enumerate_asm(ASM, 0, _).

enumerate_asm(                [], N, N) --> !, [].
enumerate_asm(      [Term|Terms], N, M) --> !, enumerate_asm(Term, N, O), enumerate_asm(Terms, O, M).
enumerate_asm(   label(N)       , N, N) --> !, [].
enumerate_asm(allocate(N, Bytes), N, M) --> !, {Bits is 8 * Bytes}, [skip(Bits)], {align(N, Bytes, M)}.
enumerate_asm(             Instr, N, M) -->    [(Z, Instr)], {align(N, 0, Z), align(Z, 4, M)}.

align(_, Bytes, _) :- (Bytes < 0 -> write('Align negative number? No!')), !, fail.
align(N,     1, M) :- !, M is N + 1.
align(N, Bytes, M) :- N mod 4 =:= 0, !, M is N + Bytes.
align(N, Bytes, M) :- Padding is 4 - (N mod 4), M is N + Bytes + Padding.


/*

Assembler

*/

asm([]) --> !, [].
asm([      skip(Bits)|Rest]) --> !, skip(Bits),          asm(Rest).
asm([(N, Instruction)|Rest]) --> !, asm(N, Instruction), asm(Rest).

asm(_, expr_cell(Func, NextCell)) --> !,
    {Data is (Func << 16) \/ NextCell}, asm(_, word(Data)).

asm(_, word(Word)) --> !, {binary_number(Bits, Word)}, collect(32, Bits).

asm(_,  load_word(A, B, Offset)) --> !, instruction_format_F2(0, 0, A, B, Offset).
asm(_,  load_byte(A, B, Offset)) --> !, instruction_format_F2(0, 1, A, B, Offset).
asm(_, store_word(A, B, Offset)) --> !, instruction_format_F2(1, 0, A, B, Offset).
asm(_, store_byte(A, B, Offset)) --> !, instruction_format_F2(1, 1, A, B, Offset).

asm(_, mov(A, C))            --> instruction_format_F0(0, A, 0, mov, C).
asm(_, mov_with_shift(A, C)) --> instruction_format_F0(1, A, 0, mov, C).

asm(_, mov_imm_with_shift(A, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(1, 0, A, 0, mov, Imm).
asm(_, mov_imm_with_shift(A, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(1, 0, A, 0, mov, Imm).
asm(_, mov_imm_with_shift(_,   _)) --> {write('Immediate value out of bounds'), fail}.

asm(_, mov_imm(A, Imm)           ) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, 0, mov, Imm).
asm(_, mov_imm(A, Imm)           ) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, 0, mov, Imm).
asm(_, mov_imm(_,   _)           ) --> {write('Immediate value out of bounds'), fail}.

asm(_, add(A, B, C))       --> instruction_format_F0(0, A, B, add, C).
asm(_, add_carry(A, B, C)) --> instruction_format_F0(1, A, B, add, C).
asm(_, sub(A, B, C))       --> instruction_format_F0(0, A, B, sub, C).
asm(_, sub_carry(A, B, C)) --> instruction_format_F0(1, A, B, sub, C).

asm(_, add_imm(A, B, Imm))       --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, add, Imm).
asm(_, add_imm(A, B, Imm))       --> {pos_int15(Imm)}, !, instruction_format_F1(0, 0, A, B, add, Imm).
asm(_, add_imm_carry(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(1, 1, A, B, add, Imm).
asm(_, add_imm_carry(A, B, Imm)) --> {pos_int15(Imm)}, !, instruction_format_F1(1, 0, A, B, add, Imm).
asm(_, sub_imm(A, B, Imm))       --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, sub, Imm).
asm(_, sub_imm(A, B, Imm))       --> {pos_int15(Imm)}, !, instruction_format_F1(0, 0, A, B, sub, Imm).
asm(_, sub_imm_carry(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(1, 1, A, B, sub, Imm).
asm(_, sub_imm_carry(A, B, Imm)) --> {pos_int15(Imm)}, !, instruction_format_F1(1, 0, A, B, sub, Imm).

asm(_, mul(A, B, C))          --> instruction_format_F0(0, A, B, mul, C).
asm(_, mul_unsigned(A, B, C)) --> instruction_format_F0(1, A, B, mul, C).
asm(_, mul_imm(A, B, Imm, U)) --> {neg_int15(Imm)}, !, instruction_format_F1(U, 1, A, B, mul, Imm).
asm(_, mul_imm(A, B, Imm, U)) --> {pos_int15(Imm)}, !, instruction_format_F1(U, 0, A, B, mul, Imm).

asm(_, and(A, B, C)) --> instruction_format_F0(0, A, B, and, C).
asm(_, ann(A, B, C)) --> instruction_format_F0(0, A, B, ann, C).
asm(_, asr(A, B, C)) --> instruction_format_F0(0, A, B, asr, C).
asm(_, div(A, B, C)) --> instruction_format_F0(0, A, B, div, C).
asm(_, ior(A, B, C)) --> instruction_format_F0(0, A, B, ior, C).
asm(_, lsl(A, B, C)) --> instruction_format_F0(0, A, B, lsl, C).
asm(_, ror(A, B, C)) --> instruction_format_F0(0, A, B, ror, C).
asm(_, xor(A, B, C)) --> instruction_format_F0(0, A, B, xor, C).

asm(_, and_imm(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, and, Imm).
asm(_, and_imm(A, B, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, B, and, Imm).
asm(_, ann_imm(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, ann, Imm).
asm(_, ann_imm(A, B, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, B, ann, Imm).
asm(_, asr_imm(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, asr, Imm).
asm(_, asr_imm(A, B, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, B, asr, Imm).
asm(_, div_imm(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, div, Imm).
asm(_, div_imm(A, B, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, B, div, Imm).
asm(_, ior_imm(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, ior, Imm).
asm(_, ior_imm(A, B, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, B, ior, Imm).
asm(_, lsl_imm(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, lsl, Imm).
asm(_, lsl_imm(A, B, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, B, lsl, Imm).
asm(_, ror_imm(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, ror, Imm).
asm(_, ror_imm(A, B, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, B, ror, Imm).
asm(_, xor_imm(A, B, Imm)) --> {neg_int15(Imm)}, !, instruction_format_F1(0, 1, A, B, xor, Imm).
asm(_, xor_imm(A, B, Imm)) --> {pos_int16(Imm)}, !, instruction_format_F1(0, 0, A, B, xor, Imm).

asm(_, cc(C))                 --> instruction_format_F3a(0, cc, C).
asm(N, cc_offset(Label))      --> instruction_format_F3b(0, cc, Label, N).
asm(_, cc_link(C))            --> instruction_format_F3a(1, cc, C).
asm(N, cc_link_offset(Label)) --> instruction_format_F3b(1, cc, Label, N).
asm(_, cs(C))                 --> instruction_format_F3a(0, cs, C).
asm(N, cs_offset(Label))      --> instruction_format_F3b(0, cs, Label, N).
asm(_, cs_link(C))            --> instruction_format_F3a(1, cs, C).
asm(N, cs_link_offset(Label)) --> instruction_format_F3b(1, cs, Label, N).
asm(_, do(C))                 --> instruction_format_F3a(0, do, C).
asm(N, do_offset(Label))      --> instruction_format_F3b(0, do, Label, N).
asm(_, do_link(C))            --> instruction_format_F3a(1, do, C).
asm(N, do_link_offset(Label)) --> instruction_format_F3b(1, do, Label, N).
asm(_, eq(C))                 --> instruction_format_F3a(0, eq, C).
asm(N, eq_offset(Label))      --> instruction_format_F3b(0, eq, Label, N).
asm(_, eq_link(C))            --> instruction_format_F3a(1, eq, C).
asm(N, eq_link_offset(Label)) --> instruction_format_F3b(1, eq, Label, N).
asm(_, ge(C))                 --> instruction_format_F3a(0, ge, C).
asm(N, ge_offset(Label))      --> instruction_format_F3b(0, ge, Label, N).
asm(_, ge_link(C))            --> instruction_format_F3a(1, ge, C).
asm(N, ge_link_offset(Label)) --> instruction_format_F3b(1, ge, Label, N).
asm(_, gt(C))                 --> instruction_format_F3a(0, gt, C).
asm(N, gt_offset(Label))      --> instruction_format_F3b(0, gt, Label, N).
asm(_, gt_link(C))            --> instruction_format_F3a(1, gt, C).
asm(N, gt_link_offset(Label)) --> instruction_format_F3b(1, gt, Label, N).
asm(_, hi(C))                 --> instruction_format_F3a(0, hi, C).
asm(N, hi_offset(Label))      --> instruction_format_F3b(0, hi, Label, N).
asm(_, hi_link(C))            --> instruction_format_F3a(1, hi, C).
asm(N, hi_link_offset(Label)) --> instruction_format_F3b(1, hi, Label, N).
asm(_, le(C))                 --> instruction_format_F3a(0, le, C).
asm(N, le_offset(Label))      --> instruction_format_F3b(0, le, Label, N).
asm(_, le_link(C))            --> instruction_format_F3a(1, le, C).
asm(N, le_link_offset(Label)) --> instruction_format_F3b(1, le, Label, N).
asm(_, ls(C))                 --> instruction_format_F3a(0, ls, C).
asm(N, ls_offset(Label))      --> instruction_format_F3b(0, ls, Label, N).
asm(_, ls_link(C))            --> instruction_format_F3a(1, ls, C).
asm(N, ls_link_offset(Label)) --> instruction_format_F3b(1, ls, Label, N).
asm(_, lt(C))                 --> instruction_format_F3a(0, lt, C).
asm(N, lt_offset(Label))      --> instruction_format_F3b(0, lt, Label, N).
asm(_, lt_link(C))            --> instruction_format_F3a(1, lt, C).
asm(N, lt_link_offset(Label)) --> instruction_format_F3b(1, lt, Label, N).
asm(_, mi(C))                 --> instruction_format_F3a(0, mi, C).
asm(N, mi_offset(Label))      --> instruction_format_F3b(0, mi, Label, N).
asm(_, mi_link(C))            --> instruction_format_F3a(1, mi, C).
asm(N, mi_link_offset(Label)) --> instruction_format_F3b(1, mi, Label, N).
asm(_, ne(C))                 --> instruction_format_F3a(0, ne, C).
asm(N, ne_offset(Label))      --> instruction_format_F3b(0, ne, Label, N).
asm(_, ne_link(C))            --> instruction_format_F3a(1, ne, C).
asm(N, ne_link_offset(Label)) --> instruction_format_F3b(1, ne, Label, N).
asm(_, nv(C))                 --> instruction_format_F3a(0, nv, C).  % NeVer.
asm(N, nv_offset(Label))      --> instruction_format_F3b(0, nv, Label, N).
asm(_, nv_link(C))            --> instruction_format_F3a(1, nv, C).
asm(N, nv_link_offset(Label)) --> instruction_format_F3b(1, nv, Label, N).
asm(_, pl(C))                 --> instruction_format_F3a(0, pl, C).
asm(N, pl_offset(Label))      --> instruction_format_F3b(0, pl, Label, N).
asm(_, pl_link(C))            --> instruction_format_F3a(1, pl, C).
asm(N, pl_link_offset(Label)) --> instruction_format_F3b(1, pl, Label, N).
asm(_, vc(C))                 --> instruction_format_F3a(0, vc, C).
asm(N, vc_offset(Label))      --> instruction_format_F3b(0, vc, Label, N).
asm(_, vc_link(C))            --> instruction_format_F3a(1, vc, C).
asm(N, vc_link_offset(Label)) --> instruction_format_F3b(1, vc, Label, N).
asm(_, vs(C))                 --> instruction_format_F3a(0, vs, C).
asm(N, vs_offset(Label))      --> instruction_format_F3b(0, vs, Label, N).
asm(_, vs_link(C))            --> instruction_format_F3a(1, vs, C).
asm(N, vs_link_offset(Label)) --> instruction_format_F3b(1, vs, Label, N).

% This is the core of the assembler where the instruction formats are specified.

instruction_format_F0(U,    A, B, Op, C ) --> [0, 0, U, 0], reg(A), reg(B), operation(Op), skip(12), reg(C).
instruction_format_F1(U, V, A, B, Op, Im) --> [0, 1, U, V], reg(A), reg(B), operation(Op), immediate(Im).
instruction_format_F2(U, V, A, B, Offset) --> [1, 0, U, V], reg(A), reg(B), offset(Offset).
instruction_format_F3a(V, Cond, C       ) --> [1, 1, 0, V], cond(Cond), skip(20), reg(C).
instruction_format_F3b(V, Cond, To, Here) --> [1, 1, 1, V], cond(Cond), encode_jump_offset(To, Here).

immediate(Imm) --> encode_int(16, Imm),    !.
offset(Offset) --> encode_int(20, Offset), !.

skip(N) --> collect(N, Zeros), {Zeros ins 0..0}.

encode_jump_offset(To, Here) --> {Offset is ((To - Here) >> 2) - 1}, encode_int(24, Offset).

encode_int(Width, I) --> {I >= 0}, !, collect(Width, Bits), {  binary_number(Bits, I)       }, !.
encode_int(Width, I) --> {I < 0},  !, collect(Width, Bits), {twos_compliment(Bits, I, Width)}, !.

collect(N,       []) --> {N =< 0}.
collect(N, [X|Rest]) --> {N > 0, N0 is N - 1}, [X], collect(N0, Rest).

reg( 0) --> [0, 0, 0, 0].
reg( 1) --> [0, 0, 0, 1].
reg( 2) --> [0, 0, 1, 0].
reg( 3) --> [0, 0, 1, 1].
reg( 4) --> [0, 1, 0, 0].
reg( 5) --> [0, 1, 0, 1].
reg( 6) --> [0, 1, 1, 0].
reg( 7) --> [0, 1, 1, 1].
reg( 8) --> [1, 0, 0, 0].
reg( 9) --> [1, 0, 0, 1].
reg(10) --> [1, 0, 1, 0].
reg(11) --> [1, 0, 1, 1].
reg(12) --> [1, 1, 0, 0].
reg(13) --> [1, 1, 0, 1].
reg(14) --> [1, 1, 1, 0].
reg(15) --> [1, 1, 1, 1].

operation(mov) --> [0, 0, 0, 0].
operation(lsl) --> [0, 0, 0, 1].
operation(asr) --> [0, 0, 1, 0].
operation(ror) --> [0, 0, 1, 1].
operation(and) --> [0, 1, 0, 0].
operation(ann) --> [0, 1, 0, 1].
operation(ior) --> [0, 1, 1, 0].
operation(xor) --> [0, 1, 1, 1].
operation(add) --> [1, 0, 0, 0].
operation(sub) --> [1, 0, 0, 1].
operation(mul) --> [1, 0, 1, 0].
operation(div) --> [1, 0, 1, 1].
operation(fad) --> [1, 1, 0, 0].
operation(fsb) --> [1, 1, 0, 1].
operation(fml) --> [1, 1, 1, 0].
operation(fdv) --> [1, 1, 1, 1].

cond(mi) --> [0, 0, 0, 0].
cond(eq) --> [0, 0, 0, 1].
cond(cs) --> [0, 0, 1, 0].
cond(vs) --> [0, 0, 1, 1].
cond(ls) --> [0, 1, 0, 0].
cond(lt) --> [0, 1, 0, 1].
cond(le) --> [0, 1, 1, 0].
cond(do) --> [0, 1, 1, 1].
cond(pl) --> [1, 0, 0, 0].
cond(ne) --> [1, 0, 0, 1].
cond(cc) --> [1, 0, 1, 0].
cond(vc) --> [1, 0, 1, 1].
cond(hi) --> [1, 1, 0, 0].
cond(ge) --> [1, 1, 0, 1].
cond(gt) --> [1, 1, 1, 0].
cond(nv) --> [1, 1, 1, 1].

pos_int16(I) :- I >= 0, I <    2^16.
pos_int15(I) :- I >= 0, I <    2^15.
neg_int15(I) :- I <  0, I >= -(2^15).
int15(I) :- pos_int15(I) ; neg_int15(I).


invert([], []).
invert([1|Tail], [0|Lait]) :- invert(Tail, Lait).
invert([0|Tail], [1|Lait]) :- invert(Tail, Lait).

twos_compliment(Bits, Number, Width) :-
    X is abs(Number),
    binary_number(B, X),
    length(B, Width),
    invert(B, Antibits),
    binary_number(Antibits, Y),
    Z is Y+1,
    length(Bits, Width),
    binary_number(Bits, Z).

% https://stackoverflow.com/a/28015816

canonical_binary_number([0], 0).
canonical_binary_number([1], 1).
canonical_binary_number([1|Bits], Number):-
    when(ground(Number),
         (Number > 1,
          Pow is floor(log(Number) / log(2)),
          Number1 is Number - 2 ^ Pow,
          (   Number1 > 1
           -> Pow1 is floor(log(Number1) / log(2)) + 1
           ;  Pow1 = 1
         ))),
    length(Bits, Pow),
    between(1, Pow, Pow1),
    length(Bits1, Pow1),
    append(Zeros, Bits1, Bits),
    maplist(=(0), Zeros),
    canonical_binary_number(Bits1, Number1),
    Number is Number1 + 2 ^ Pow.

binary_number(   Bits , Number) :- canonical_binary_number(Bits, Number).
binary_number([0|Bits], Number) :-           binary_number(Bits, Number).


% Helper code to write the list of bits as a binary file.

for_serial(Binary, Ser) :-
    length(Binary, LengthInBits),
    LengthInBytes is LengthInBits >> 3,
    skip(32, Caboose, []),  % zero word to signal EOF to bootloader.
    append(Binary, Caboose, B),
    skip(32, G, B),  % Address is zero.
    binary_number(Bits, LengthInBytes),
    collect(32, Bits, Ser, G).

write_binary(Name, Binary) :-
    open(Name, write, Stream, [type(binary)]),
    for_serial(Binary, Ser),
    phrase(write_binary_(Stream), Ser),
    close(Stream).

write_binary_(Stream) -->
    % Handle "Endian-ness".
    collect(8, Bits3), collect(8, Bits2), collect(8, Bits1), collect(8, Bits0), !,
    {wb(Bits0, Stream), wb(Bits1, Stream), wb(Bits2, Stream), wb(Bits3, Stream)},
    write_binary_(Stream).
write_binary_(_) --> [].

wb(Bits, Stream) :- binary_number(Bits, Byte), put_byte(Stream, Byte).
