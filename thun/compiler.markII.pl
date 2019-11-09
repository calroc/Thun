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

Mark II

*/
:- use_module(library(assoc)).
:- use_module(library(clpfd)).

% Just do it in assembler.

⟐(program) -->
    { [SP, EXPR_addr, TOS, TERM, EXPR, TermAddr, TEMP0, TEMP1, TEMP2, TEMP3]
    = [0,  1,         2,   3,    4,    5,        6,     7,     8,     9    ]
    },
    [  % Mainloop.
    word(0),  % Zero root cell.
    do_offset(Reset),  % Oberon bootloader writes MemLim to RAM[12] and
    allocate(_, 20),  % stackOrg to RAM[24], we don't need these
    label(Reset),  % but they must not be allowed to corrupt our code.

    mov_imm(0, 0),  % zero out the root cell.  (After reset.)
    store_word(0, 0, 0),

    mov_imm(SP, 0x1000),
    mov_imm(EXPR_addr, Expression),
    mov_imm(TOS, 0),
    mov_imm(TERM, 0),
    store_word(TOS, SP, 0),  % RAM[SP] := 0

    label(Main)],

    ⟐(if_zero(EXPR_addr, HALT)),

    % deref(EXPR_addr, EXPR),
    [load_word(EXPR, EXPR_addr, 0),  % Load expr pair record into EXPR

    % At this point EXPR holds the record word of the expression.

    lsl_imm(TermAddr, EXPR, 2),  % Trim off the type tag 00 bits.
    asr_imm(TermAddr, TermAddr, 17),  % preserve sign of offset.
    eq_offset(Foo),  % if the offset is zero don't add the address. it's empty list.
    add(TermAddr, TermAddr, EXPR_addr), 
    label(Foo),

    % TermAddr has the address of the term record.

    load_word(TERM, TermAddr, 0),  % Bring the record in from RAM.

    % Now Term has the term's record data and TermAddr has the address of the term.

    lsl_imm(TEMP0, EXPR, 17),  % Get the offset of the tail of the expr
    asr_imm(TEMP0, TEMP0, 17),  % while preserving the sign.
    eq_offset(Foo0),  % if the offset is zero don't add the address. it's empty list.
    add(TEMP0, TEMP0, EXPR_addr),  % Add the address to the offset.
    label(Foo0),
    mov(EXPR_addr, TEMP0),

    % EXPR_addr now holds the address of the next cell of the expression list.

    % if_literal(TERM, PUSH),
    asr_imm(TEMP0, TERM, 30),  % get just the two tag bits.
    and_imm(TEMP0, TEMP0, 2),  % mask the two tag bits.
    sub_imm(TEMP0, TEMP0, 2),  % if this is a symbol result is zero.
    ne_offset(PUSH),

    % if it is a symbol the rest of it is the pointer to the machine code.
    % lookup(TERM),  % Jump to command.
    mov_imm_with_shift(TEMP0, 0x3fff),  % TEMP0 = 0x3fffffff
    ior_imm(TEMP0, TEMP0, 0xffff),
    and(TEMP0, TEMP0, TERM),
    do(TEMP0),

    % going into push we have the term
    label(PUSH),
    % push2(TOS, TEMP1, SP),  % stack = TERM, stack

    sub_imm(SP, SP, 4),     % SP -= 1 (word, not byte)
    % SP points to the future home of the new stack cell.
    sub(TOS, TermAddr, SP), % TOS := &temp - sp
    % Er, what if it's negative?
    hi_offset(Bar0),
    and_imm(TOS, TOS, 0x7fff),  % Mask off high bits so
    label(Bar0),  % they won't interfere with making a record cell.
    % TOS has the offset from new stack cell to term cell.
    % Combine with the offset to the previous stack cell.
    lsl_imm(TOS, TOS, 15),  % TOS := TOS << 15
    ior_imm(TOS, TOS, 4),   % TOS := TOS | 4

    label(Done),
    store_word(TOS, SP, 0),   % RAM[SP] := TOS
    do_offset(Main),

    label(HALT),  % This is a HALT loop, the emulator detects and traps
    do_offset(HALT),  % on this "10 goto 10" instruction.

% ======================================

    label(Cons)],  % Let's cons.

    ⟐(unpack_pair(TOS, TEMP0, TOS)),
    % TEMP0 = Address of the list to which to append.
    % TOS = Address of the second stack cell.
    [load_word(TEMP1, TOS, 0)],
    % TEMP1 contains the record of the second stack cell.
    ⟐(unpack_pair(TEMP1, TEMP2, TEMP3)),
    % TEMP2 contains the address of the second item on the stack
    % TEMP3 = TOS +  TEMP1[15:0]  the address of the third stack cell

    % Build and write the new list cell.
    [
    sub_imm(SP, SP, 4)

    ],⟐([

    sub_base_from_offset(TEMP2, SP),
    sub_base_from_offset(TEMP0, SP)

    ]),[

    lsl_imm(TEMP2, TEMP2, 15),  % TEMP2 := TEMP2 << 15
    ior(TEMP2, TEMP2, TEMP0),
    store_word(TEMP2, SP, 0),

    sub_imm(SP, SP, 4)               ],⟐(
    sub_base_from_offset(TEMP3, SP)  ),[
    mov_imm_with_shift(TOS, 2),  % TOS := 4 << 15
    ior(TOS, TOS, TEMP3),
    do_offset(Done),  % Rely on mainloop::Done to write TOS to RAM.

    label(Expression),
    expr_cell(ConsSym, 0),
    label(ConsSym), symbol(Cons)

].

/*

This stage ⟐//1 converts the intermediate representation to assembly
language.

*/

⟐([]) --> [].
⟐([Term|Terms]) --> ⟐(Term), ⟐(Terms).

⟐(if_zero(Reg, Label)) --> [sub_imm(Reg, Reg, 0), eq_offset(Label)].

⟐(sub_base_from_offset(Reg, Base)) -->
    ⟐(if_zero(Reg, Label)),  % if the offset is zero don't subtract 
    [sub(Reg, Reg, Base),  % the Base address, it's the empty list.
     and_imm(Reg, Reg, 0x7fff),  % Mask off high bits.
     label(Label)].

⟐(unpack_pair(From, HeadAddr, TailAddr)) -->
    [
    lsl_imm(HeadAddr, From, 2),  % Trim off the type tag 00 bits.
    asr_imm(HeadAddr, HeadAddr, 17),  % HeadAddr := From >> 15
    eq_offset(Label0),  % if the offset is zero don't add the address. it's empty list.
    add(HeadAddr, HeadAddr, TOS),
    label(Label0),
    % HeadAddr contains the address of the second item on the stack
    lsl_imm(TailAddr, From, 17),  % Get the offset of the third stack cell
    asr_imm(TailAddr, TailAddr, 17),  % while preserving the sign.
    eq_offset(Label1),  % if the offset is zero don't add the address. it's empty list.
    add(TailAddr, TailAddr, TOS),
    label(Label1)
    % TailAddr = TOS +  From[15:0]  the address of the third stack cell
    ].


do :-
    compile_program(Binary),
    write_binary('joy_asmii.bin', Binary).


compile_program(Binary) :-
    phrase(⟐(program), ASM),
    writeln(ASM),
    phrase(linker(ASM), EnumeratedASM),
    phrase(asm(EnumeratedASM), Binary).


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

asm(Here, expr_cell(Func, NextCell)) --> !,
    {Data is ((Func - Here) << 15) \/ NextCell}, asm(Here, word(Data)).

asm(_, symbol(Sym)) --> !, {Data is Sym \/ 0x80000000}, asm(_, word(Data)).

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
