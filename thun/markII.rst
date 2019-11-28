

Mark II
=========================



This (and the previous incarnation) is really more of a "macro assembler"
than a compiler, and nothing like what I want it to be.  It should be
tracking the "types" of registers in some enviroment that gets carried
along and picking primitives and making optimizations based on that
information.


TO replace the crude first draft I want to expand the representation of
data types.

At first I thought I might use the COLA object model but when I reviewed
it I realized that it was way too flexible for what I needed, so I"m
going to use a simple tagged record kind of a thing.  There are three
"types": symbols, lists (cons lists, pairs), and integers.  In order to
deal with large numbers and do double duty as strings, I'm going to let
them be made up of more than one word of memory.

Preliminary design:  A record is one or more 32-bit words, the two most
signifigant bits are a type tag:

    00 - Pair of pointers to other records, 30 bits left so 15 each?
    10 - Symbol, the remaining 30 bits are the address of the func.
    01 - Integer, the next, hmm, 6? bits are the length in words.
    11 - escape hatch to COLA maybe?

Deets:  For pairs, the empty list is still 0 and by leaving 0 in RAM[0]
it's "safe" to deref it.  Each half of the rest of the word (15 bits) is
an offset (not a direct pointer) from the pair to the member record.

For symbols, the rest of the word is the direct pointer to the machine
code of the function denoted by the symbol.  I might add some additional
data to the head of the record because the CPU doesn't have 30 address
lines.  I'm assuming that the (as yet unwritten) parser will take care of
looking up the symbols at parse time, but it would also be possible to
point to a integer that represents the string name of the function and do
lookup during evaluation, or during some intermediate stage.

For ints, I'm putting a little length field in the record, 0 length means
the integer's bits all fit in the rest of the record word.  If the length
is 1 the integer is in the following word (but what if the rest of the
record word was a pointer to the data word?  Could save space for popular
integers, eh?)  If the length is greater than 1 the rest of the bytes in
the record word are included in the intger(?)

    01000000|byte|byte|byte  <- three bytes of integer.

    01000001|0000|0000|0000  <- (Pointer to data maybe?)
        byte|byte|byte|byte  <- four bytes of integer.

Or how about...

    010nnnnn|byte|byte|byte  <- 29 bits of immediate integer
    011nnnnn|byte|byte|byte  <- length and offset packed in 29 bits?
                                pointing to a stretch of words in RAM

If the offset is limited to 16 bits that leaves 13 bits for the length.
8K 32-bit words is 262144 bits, and 2^262144 is a pretty big number.

It doesn't matter yet because I'm not about to implement math yet.

So let's see how bad it is to rewrite the compiler to make it implement
this new stuff.

main loop
-------------------------------------


if_zero(EXPR, HALT)

No change to the iplementation is needed.

deref(EXPR) loads the record at the address in expr register into that
register, but now we are going to need to remember that address to add
it to the offset in the record to find the records of the head and tail
records.

Change it to deref(EXPR, TEMP0) and keep the address around in TEMP0.

split_word(TERM, EXPR)  puts the record pointed to by head of the expr
record into term register and leave the address of the tail record in
expr.  THe address of the tail record is just the last 15 bits plus the
address in TEMP0.

The address of the head record is bits [30:15] of the record plus the
address in TEMP0.

SO, load the head record address bits into ToAddr and then add FromAddr

    ior_imm(ToAddr, From, -15),  % roll right 15 bits
    % No need to mask off high bits as the type tag for pairs is 00
    add(ToAddr, ToAddr, FromAddr),

    load_word(To, ToAddr, 0),  % Bring the record in from RAM.

    and_imm(From, From, 0x7fff),  % Mask off  lower 15 bits.
    add(From, From, FromAddr),  % Add the address to the offset.


If a record can only be created after its parts and the parts are being
allocated in strictly ascending (or descending) order of addresses then
the offsets will always be negative (or positive).  SInce it's easier to
deal with positive offsets and it's just as easy to allocate up as down,
I'm going to do that.


Next, we must check if the term is a literal (not a symbol)

That involves rolling the value to get the top two bits and then checking
whether they are 10 or not.

lookup(DICT_PTR, DICT_TOP, TERM, HALT)

It turns out that you don;t need anything but the record since it's
already been looked up.  The symbol contains the jump address.



Next, push

push(TOS, TERM, SP)

There's a note saying

    set(sp, 2),  % Reg 2 points to just under top of stack.

But I've forgotten if that's exactly true.  I want to just point to TOS
pair record in SP.  If that's the case, then the PUSH operation is:

Since the record we are constructing is going to have the rest of the
stack as its tail and it's going to be written to the next word in RAM
after (before) the SP (the address of the stack/tail) the offset is +4.

The address of the term is in a register, the address of the pair record
that we are contrsucting will be SP - 4, to get the offset we have to
subtract that from the term's address.  Since we need it anyway, and
we're going to do it sooner or later, let's subtract 4 from the SP right
off:

    sp = sp - 4

Then we can use it to calculate the offset and put it in tos:

    tos = &temp - sp
    tos = tos << 15

If I was as slick as I like to pretend I would insert a check here that
the two high bits are indeed 00. ...

With the offset of the term in the tos register already we just have to
OR 4:

    tos = tos | 4

And write it to the (already decremented) sp.

    ram[sp] = tos


cons
------------------------------

    [グ,ケ,ゲ,ド,ゴ,サ],ヮ(cons),

    グ pop(TEMP0, TOS)           split_word(TEMP0, TOS),        high_half_to(To, From),     swap_halves(To, From),    ror_imm(To, From, 16)
                                                                                            low_half(To)              and_imm(Reg, Reg, 0xffff)
                                                                low_half(From)                                        and_imm(Reg, Reg, 0xffff)

                                                                   Puts the term on tos (the list to cons onto) into temp0 and points tos
                                 deref(TOS)                        to the value under tos (the item to cons onto the list.)

    ケ high_half(TEMP1, TOS)     mov_imm_with_shift(0, 0xffff),    Mask off the high half of (new) tos to isolate value.
                                 and(TEMP1, TOS, 0)
    ゲ or_inplace(TEMP0, TEMP1)  ior(TEMP0, TEMP0, TEMP1)          Combines value with the list in a new pair record.
    ド write_cell(TEMP0, SP)     add_imm(SP, SP, 4),               Writes the new pair cell word to ++sp.
                                 store_word(TEMP0, SP, 0)
    ゴ low_half(TOS)             and_imm(TOS, TOS, 0xffff)         Delete the reference to second item down.
    サ merge(SP, TOS)            lsl_imm(0, SP, 16),               Make a new pair record from the SP which points to the new cons'd list
                                 ior(TOS, TOS, 0),                 and TOS which points to the rest of the stack.  This record is then the
                                 add_imm(SP, SP, 4)                new TOS pair cell word, and we let the mainloop write it to RAM for us.

So now that i've recreated it, what is it doing?

⦾(グ, pop(TEMP0, TOS))
⦾(ケ, high_half(TEMP1, TOS))
⦾(ゲ, or_inplace(TEMP0, TEMP1))
⦾(ド, write_cell(TEMP0, SP))
⦾(ゴ, low_half(TOS))
⦾(サ, merge(SP, TOS))

⟐(pop(Reg, TOS)) --> ⟐([split_word(Reg, TOS), deref(TOS)]).
⟐(high_half(To, From)) --> [mov_imm_with_shift(0, 0xffff), and(To, From, 0)].
⟐(or_inplace(To, From)) --> [ior(To, To, From)].
⟐(write_cell(From, SP)) --> [add_imm(SP, SP, 4), store_word(From, SP, 0)].
⟐(       low_half(Reg)) --> [and_imm(Reg, Reg, 0xffff)].
⟐(merge(SP, TOS)) -->
    [lsl_imm(0, SP, 16),
     ior(TOS, TOS, 0),
     add_imm(SP, SP, 4)].


This blows, just write it in assembly already.


-------------------------------

TO review, at this point, when we jump to the machine code of a
definition, the following registers hold:

    EXPR - the record word of the expression.
    EXPR_addr - the address of the next cell of the expression list.
    TERM - the term's record word.
    TermAddr - the address of the term.
    SP - points to TOS record in RAM
    TOS - the record word of TOS

    address of the list to append to is SP + TOS[30:15]
    the address of the second stack cell is SP + TOS[15:0]
    the address of the second item on the stack is (SP + TOS[15:0]) + ram[SP + TOS[15:0]][30:15]
    the address of the third stack cell         is (SP + TOS[15:0]) + ram[SP + TOS[15:0]][15: 0]

we need to create

    [SP - 4] -> 00:(address of the second item on the stack):(address of the list to append to)
    [SP - 8] -> 00:(address of the record above)            :(address of the third stack cell)

Each of the addresses above must be converted to offsets from their
respective records.

ror_imm(TEMP0, TOS, 15), % TEMP0 := TOS >> 15
add(TEMP0, TEMP0, SP)
% TEMP0 = SP + TOS[30:15] Address of the list to which to append.

and_imm(TOS, TOS, 0x7fff),  % get the offset of the tail of the stack
add(TOS, TOS, SP)
% TOS = SP + TOS[15:0] Address of the second stack cell.

% the address of the second item on the stack is (TOS) + ram[TOS][30:15]
% the address of the third stack cell         is (TOS) + ram[TOS][15: 0]

load_word(TEMP1, TOS, 0),  % TOS := TOS << 15
% TEMP1 contains the record of the second stack cell.

% the address of the second item on the stack is (TOS) + TEMP1[30:15]
% the address of the third stack cell         is (TOS) + TEMP1[15: 0]

ror_imm(TEMP2, TEMP1, 15),  % TEMP2 := TEMP1 >> 15
add(TEMP2, TEMP2, TOS)
% TEMP2 contains the address of the second item on the stack

and_imm(TEMP3, TEMP1, 0x7fff),  % get the offset of the third stack cell
add(TEMP3, TEMP1, TOS)
% TEMP3 = TOS +  TEMP1[15:0]  the address of the third stack cell


we need to create

    [SP - 4] -> 00:(address of the second item on the stack):(address of the list to append to)
    [SP - 8] -> 00:(address of the record above)            :(address of the third stack cell)
                     4 << 15
Each of the addresses above must be converted to offsets from their
respective records.

sub_imm(SP, SP, 4),
sub(TEMP2, TEMP2, SP),
sub(TEMP0, TEMP0, SP),
lsl_imm(TEMP2, TEMP2, 15),  % TEMP2 := TEMP2 << 15
ior(TEMP2, TEMP2, TEMP0),
store_word(TEMP2, SP, 0),


sub_imm(SP, SP, 4),
sub(TEMP3, TEMP3, SP),
mov_imm(TEMP2, 4),
lsl_imm(TEMP2, TEMP2, 15),  % TEMP2 := 4 << 15
ior(TEMP2, TEMP2, TEMP3),
store_word(TEMP2, SP, 0),

I had some bugs because I forgot that when the offset is zero the base
should not be added/subtracted: zero offset means the value is the empty
list.

If offsets can only be positive (as I have it now) then record pairs on
the stack can only point to cells above them.  That implies that symbols
must be allocated above the stack in RAM.  However, I am constructing the
code library from low mem upwards, and if I include the symbol cells in
the function machine code as a kind of header (as I intend to) they'll be
below the stack.  I could put the symbol cells together in a clump just
above the stack but then I would need to implement org() and modify the
for_serial/2 relation to emit more than one "patch" for the bootloader.

I could modify the implementation to allow for negative offsets.  That
would be a little tricky but probably easier at the moment than adding
org() and changing for_serial/2.  It would also facilitate writing lists
in "reverse" order (the head cell is above the tail in RAM) which is
useful for appending lists to other lists.

Yeah, I think that's the way to go...

---------------------------------------------

There's soething fishey with the symbols now that they are in the header
of the machine code.  Each symbol's pointer field points to the next cell,
which seems really redundant.  We need some symbol record to
differentiate from lists and ints.  Maybe if more information was in the
header? Like the name of the function?  It would make more sense?

Right now I'm assuming that the eventual parser would be looking up
symbols at parse-time and reusing the header symbols rather than
allocating cells for new ones.  If the symbols were kept apart from the
machine code then it makes sense for them to have pointers?

Maybe I can dispense with symbol records by modifying the
is-this-a-symbol code to just check if the address is below the end of
the library code.

------------------------------------

 
    [ザ,シ],ヮ(dup),

    ザ swap_halves(TOS)
    シ push(TOS, TOS, SP)


------------------------------------


    [グ,ス,[],[ジ,ス,[ズ,セ,ス,[ゼ,ソ],[タ,ゾ],ヰ,ヂ],ヱ],ヰ,チ],ヮ(i),

    グ, pop(TEMP0, TOS)
    ス, if_zero(TEMP0)
    ジ, add_const(TEMP3, SP, 4)
    ズ, deref(TEMP0)
    セ, chop_word(TEMP1, TEMP0)
    ゼ, or_inplace(TEMP1,  EXPR)
    ソ, asm(mov(EXPR, TEMP3))
    タ, add_const(TEMP2, SP, 8)
    ゾ, or_inplace(TEMP1, TEMP2)
    ヂ, write_cell(TEMP1, SP)
    チ, add_const(SP, SP, 4)



⦾([P, T, E, ヰ|Terms], [br(Predicate, Then, Else)|Ts]) -->
    ⦾([P, T, E, Terms], [Predicate, Then, Else, Ts]).

⦾([P, B, ヱ|Terms], [repeat_until(Predicate, Body)|Ts]) -->
    ⦾([P, B, Terms], [Predicate, Body, Ts]).



    [
        グ, pop(TEMP0, TOS)
        ス, if_zero(TEMP0)
        [], Then
        [   Else
            ジ, add_const(TEMP3, SP, 4)
            ス, if_zero(TEMP0)
            [ Body
                ズ, deref(TEMP0)
                セ, chop_word(TEMP1, TEMP0)
                ス, if_zero(TEMP0)
                [ Then
                    ゼ, or_inplace(TEMP1,  EXPR)
                    ソ  asm(mov(EXPR, TEMP3))
                ],
                [ Else
                    タ, add_const(TEMP2, SP, 8)
                    ゾ  or_inplace(TEMP1, TEMP2)
                ],
                ヰ, br(Predicate, Then, Else)
                ヂ  write_cell(TEMP1, SP)
            ],
            ヱ repeat_until(Predicate, Body)
        ],
        ヰ, br(Predicate, Then, Else)
        チ  add_const(SP, SP, 4)
    ],
    ヮ(i),


------------------------------------------

    [ヶ,ペ],ワ(new),

    ヶ, low_half(TOS, SP)
    ペ, write_cell(TOS,   SP)


------------------------------------------


    [ナ,ズ,セ,ネ,ヒ,ド,ャ,ペ],ワ(swap),

    ナ, low_half(TEMP0, TOS)
    ズ, deref(TEMP0)
    セ, chop_word(TEMP1, TEMP0)
    ネ, chop_word(TEMP2, TOS)
    ヒ, or_inplace(TEMP0, TEMP2)
    ド, write_cell(TEMP0, SP)
    ャ, asm(ior(TOS, TEMP1, SP))
    ペ, write_cell(TOS,   SP)


--------------------------------------

Debugging definitions

     swons 0xd2 load R[0] <- ram[R[0] + 0x34c]
>          0xd3 mov R[2] <- 0x354
           0xd4 BR T 0x2 immediate 
           0xd5 BR F -0x69fffc immediate  and R[15] <- PC + 1
           0xd6 BR GT -0x4c0000 immediate  and R[15] <- PC + 1
     dodef 0xd7 mov R[7] <- 0x4
           0xd8 sub R[0] <- R[0] 0x4 immediate

Q: Is 0x354 >> 2 == 0xd5 ?

In [1]: 0x354 >> 2 == 0xd5
Out[1]: True

Okay then.


>    dodef 0xd7 mov R[7] <- 0x4
           0xd8 sub R[0] <- R[0] 0x4 immediate
           0xd9 sub R[2] <- R[2] 0x0 immediate
           0xda BR EQ 0x2 immediate 
           0xdb sub R[2] <- R[2] R[0]
           0xdc and R[2] <- R[2] 0x7fff immediate


0xff4  r0
0x354  r2


>          0xde ior R[2] <- R[2] R[7]
           0xdf store R[2] -> ram[R[0]]
           0xe0 mov R[7] <- 0x168
           0xe1 add R[7] <- R[7] 0x4 immediate
           0xe2 BR T R[7]
  expressi 0xe3 BR F 0x740004 immediate  and R[15] <- PC + 1

0x39b00004  r2

           0xe0 mov R[7] <- 0x168
>          0xe1 add R[7] <- R[7] 0x4 immediate
           0xe2 BR T R[7]
  expressi 0xe3 BR F 0x740004 immediate  and R[15] <- PC + 1


0x16c  r7   = 364 decimal

0x5b I machine code in words

In [8]: 0x16c >> 2 == 0x5b
Out[8]: True

         i 0x5a load R[0] <- ram[R[0] + 0x16c]
>          0x5b lsl R[6] <- R[2] 0x2 immediate
           0x5c asr R[6] <- R[6] 0x11 immediate
           0x5d BR EQ 0x1 immediate 
           0x5e add R[6] <- R[6] R[0]
           0x5f lsl R[2] <- R[2] 0x11 immediate
           0x60 asr R[2] <- R[2] 0x11 immediate

and then...

         i 0x5a load R[0] <- ram[R[0] + 0x16c]
           0x5b lsl R[6] <- R[2] 0x2 immediate
           0x5c asr R[6] <- R[6] 0x11 immediate
           0x5d BR EQ 0x1 immediate 
>          0x5e add R[6] <- R[6] R[0]
           0x5f lsl R[2] <- R[2] 0x11 immediate
           0x60 asr R[2] <- R[2] 0x11 immediate
           0x61 BR EQ 0x1 immediate 
           0x62 add R[2] <- R[2] R[0]
           0x63 sub R[6] <- R[6] 0x0 immediate

0x354  r6  check

           0x5e add R[6] <- R[6] R[0]
           0x5f lsl R[2] <- R[2] 0x11 immediate
           0x60 asr R[2] <- R[2] 0x11 immediate
           0x61 BR EQ 0x1 immediate 
>          0x62 add R[2] <- R[2] R[0]
           0x63 sub R[6] <- R[6] 0x0 immediate
           0x64 BR EQ 0x22 immediate 
           0x65 sub R[10] <- R[0] 0x4 immediate

0xff8  r2

           0x61 BR EQ 0x1 immediate 
           0x62 add R[2] <- R[2] R[0]
           0x63 sub R[6] <- R[6] 0x0 immediate
#117       0x64 BR EQ 0x22 immediate 
>          0x65 sub R[10] <- R[0] 0x4 immediate
           0x66 mov R[9] <- 0x4
           0x67 load R[3] <- ram[R[6]]
     iball 0x68 lsl R[7] <- R[3] 0x2 immediate
           0x69 asr R[7] <- R[7] 0x11 immediate
           0x6a BR EQ 0x1 immediate 

line 120

#120       0x65 sub R[10] <- R[0] 0x4 immediate
           0x66 mov R[9] <- 0x4
>          0x67 load R[3] <- ram[R[6]]
     iball 0x68 lsl R[7] <- R[3] 0x2 immediate
           0x69 asr R[7] <- R[7] 0x11 immediate
           0x6a BR EQ 0x1 immediate 
           0x6b add R[7] <- R[7] R[6]
           0x6c lsl R[8] <- R[3] 0x11 immediate

line 123

0xff960004  r3

     iball 0x68 lsl R[7] <- R[3] 0x2 immediate
           0x69 asr R[7] <- R[7] 0x11 immediate
           0x6a BR EQ 0x1 immediate 
>          0x6b add R[7] <- R[7] R[6]
           0x6c lsl R[8] <- R[3] 0x11 immediate
           0x6d asr R[8] <- R[8] 0x11 immediate
           0x6e BR EQ 0x1 immediate 
           0x6f add R[8] <- R[8] R[6]
           0x70 mov R[6] <- R[8]

0x280  r7  Address of swons def list?  of swap symbol?

    In [4]: w.cpu.R[7]
    Out[4]: 640L

    In [5]: hex(_)
    Out[5]: '0x280L'

    In [6]: w.cpu.ram[w.cpu.R[7]]
    Out[6]: 2147484292L

    In [7]: bin(_)
    Out[7]: '0b10000000000000000000001010000100'

    In [8]: w.syms.keys()
    Out[8]: [160, 35, 7, 104, 44, 45, 14, 205, 48, 200, 210, 227, 86, 215, 90, 157]

    In [9]: 640 >> 2
    Out[9]: 160

    In [10]: w.syms[160]
    Out[10]: 'swap'

0x280  r7  points to swap symbol record.
===================================================

           0x6b add R[7] <- R[7] R[6]
           0x6c lsl R[8] <- R[3] 0x11 immediate
           0x6d asr R[8] <- R[8] 0x11 immediate
           0x6e BR EQ 0x1 immediate 
>          0x6f add R[8] <- R[8] R[6]
#127       0x70 mov R[6] <- R[8]
           0x71 sub R[0] <- R[0] 0x4 immediate
           0x72 sub R[8] <- R[8] 0x0 immediate
           0x73 BR EQ 0x4 immediate 

0x358  r8

>          0x70 mov R[6] <- R[8]

line 127

           0x70 mov R[6] <- R[8]
           0x71 sub R[0] <- R[0] 0x4 immediate
           0x72 sub R[8] <- R[8] 0x0 immediate
>          0x73 BR EQ 0x4 immediate 
           0x74 lsl R[7] <- R[7] 0xf immediate
           0x75 ior R[7] <- R[7] R[9]
           0x76 store R[7] -> ram[R[0]]
           0x77 BR T 0xb immediate 
           0x78 sub R[7] <- R[7] 0x0 immediate

line 129

           0x73 BR EQ 0x4 immediate 
           0x74 lsl R[7] <- R[7] 0xf immediate
           0x75 ior R[7] <- R[7] R[9]
>          0x76 store R[7] -> ram[R[0]]
           0x77 BR T 0xb immediate 

0x1400004 r7  -> ram[r[0]]  w/ r0 = 0xff0

saved foobar1

I think I found it.  Not subtracting SP from r7 address
before merge_and_store(TEMP1, TEMP3, SP) on line #134.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

jmp @ 0x77 to 0xb  -> 0x83
repeat
>          0x83 sub R[6] <- R[6] 0x0 immediate
           0x84 BR EQ 0x1 immediate 
           0x85 BR T -0x1f immediate 
           0x86 mov R[1] <- R[10]
           0x87 load R[7] <- ram[R[2]]
           0x88 lsl R[6] <- R[7] 0x2 immediate


0x85 +  -0x1f

>          0x67 load R[3] <- ram[R[6]]
     iball 0x68 lsl R[7] <- R[3] 0x2 immediate
           0x69 asr R[7] <- R[7] 0x11 immediate
           0x6a BR EQ 0x1 immediate 
           0x6b add R[7] <- R[7] R[6]
           0x6c lsl R[8] <- R[3] 0x11 immediate

back to line 123

     iball 0x68 lsl R[7] <- R[3] 0x2 immediate
           0x69 asr R[7] <- R[7] 0x11 immediate
           0x6a BR EQ 0x1 immediate 
           0x6b add R[7] <- R[7] R[6]
           0x6c lsl R[8] <- R[3] 0x11 immediate
           0x6d asr R[8] <- R[8] 0x11 immediate
>          0x6e BR EQ 0x1 immediate 
           0x6f add R[8] <- R[8] R[6]
           0x70 mov R[6] <- R[8]
           0x71 sub R[0] <- R[0] 0x4 immediate
           0x72 sub R[8] <- R[8] 0x0 immediate
           0x73 BR EQ 0x4 immediate 

0xc0  r7
0x0   r8

           0x6e BR EQ 0x1 immediate 
           0x6f add R[8] <- R[8] R[6]
           0x70 mov R[6] <- R[8]
           0x71 sub R[0] <- R[0] 0x4 immediate
           0x72 sub R[8] <- R[8] 0x0 immediate
>          0x73 BR EQ 0x4 immediate 
           0x74 lsl R[7] <- R[7] 0xf immediate
           0x75 ior R[7] <- R[7] R[9]
           0x76 store R[7] -> ram[R[0]]
           0x77 BR T 0xb immediate 
           0x78 sub R[7] <- R[7] 0x0 immediate

We take the high road this time

           0x73 BR EQ 0x4 immediate 
           0x74 lsl R[7] <- R[7] 0xf immediate
           0x75 ior R[7] <- R[7] R[9]
           0x76 store R[7] -> ram[R[0]]
           0x77 BR T 0xb immediate 
>          0x78 sub R[7] <- R[7] 0x0 immediate
           0x79 BR EQ 0x2 immediate 
           0x7a sub R[7] <- R[7] R[0]
           0x7b and R[7] <- R[7] 0x7fff immediate
           0x7c sub R[1] <- R[1] 0x0 immediate
           0x7d BR EQ 0x2 immediate 


r7 = 0xc0
r0 = 0xfec

so, @0x7a, r7 <- r7 - r0  -> -3884 or -0xf2c
Hmm...

           0x78 sub R[7] <- R[7] 0x0 immediate
           0x79 BR EQ 0x2 immediate 
           0x7a sub R[7] <- R[7] R[0]
>          0x7b and R[7] <- R[7] 0x7fff immediate
           0x7c sub R[1] <- R[1] 0x0 immediate
           0x7d BR EQ 0x2 immediate 
           0x7e sub R[1] <- R[1] R[0]
           0x7f and R[1] <- R[1] 0x7fff immediate
           0x80 lsl R[7] <- R[7] 0xf immediate



           0x7b and R[7] <- R[7] 0x7fff immediate
           0x7c sub R[1] <- R[1] 0x0 immediate
           0x7d BR EQ 0x2 immediate 
>          0x7e sub R[1] <- R[1] R[0]
           0x7f and R[1] <- R[1] 0x7fff immediate
           0x80 lsl R[7] <- R[7] 0xf immediate
           0x81 ior R[7] <- R[7] R[1]
           0x82 store R[7] -> ram[R[0]]

0xfffff3ac  r1  ... 0x73ac

           0x7e sub R[1] <- R[1] R[0]
           0x7f and R[1] <- R[1] 0x7fff immediate
           0x80 lsl R[7] <- R[7] 0xf immediate
           0x81 ior R[7] <- R[7] R[1]
>          0x82 store R[7] -> ram[R[0]]
           0x83 sub R[6] <- R[6] 0x0 immediate
           0x84 BR EQ 0x1 immediate 
           0x85 BR T -0x1f immediate 
           0x86 mov R[1] <- R[10]
           0x87 load R[7] <- ram[R[2]]

0x386a73ac  r7 !?

           0x82 store R[7] -> ram[R[0]]
           0x83 sub R[6] <- R[6] 0x0 immediate
           0x84 BR EQ 0x1 immediate 
           0x85 BR T -0x1f immediate 
>          0x86 mov R[1] <- R[10]
           0x87 load R[7] <- ram[R[2]]
           0x88 lsl R[6] <- R[7] 0x2 immediate
           0x89 asr R[6] <- R[6] 0x11 immediate
           0x8a BR EQ 0x1 immediate 
           0x8b add R[6] <- R[6] R[2]

line 141

>          0x87 load R[7] <- ram[R[2]]

line 146

0x4  r7  "the record of the second stack cell"
empty list followed by the record one cell above

           0x87 load R[7] <- ram[R[2]]
*          0x88 lsl R[6] <- R[7] 0x2 immediate
*          0x89 asr R[6] <- R[6] 0x11 immediate
*          0x8a BR EQ 0x1 immediate 
*          0x8b add R[6] <- R[6] R[2]
*          0x8c lsl R[7] <- R[7] 0x11 immediate
*          0x8d asr R[7] <- R[7] 0x11 immediate
*          0x8e BR EQ 0x1 immediate 
*          0x8f add R[7] <- R[7] R[2]
>          0x90 sub R[0] <- R[0] 0x4 immediate
           0x91 sub R[6] <- R[6] 0x0 immediate
           0x92 BR EQ 0x2 immediate 
           0x93 sub R[6] <- R[6] R[0]
           0x94 and R[6] <- R[6] 0x7fff immediate
           0x95 sub R[7] <- R[7] 0x0 immediate

at line 150
0x0   r6
0xffc r7

TEMP0 = HeadAddr, TEMP1 = TailAddr

mkII.2.2 saved here.

           0x90 sub R[0] <- R[0] 0x4 immediate
           0x91 sub R[6] <- R[6] 0x0 immediate
           0x92 BR EQ 0x2 immediate 
           0x93 sub R[6] <- R[6] R[0]
           0x94 and R[6] <- R[6] 0x7fff immediate
           0x95 sub R[7] <- R[7] 0x0 immediate
           0x96 BR EQ 0x2 immediate 
           0x97 sub R[7] <- R[7] R[0]
           0x98 and R[7] <- R[7] 0x7fff immediate
           0x99 lsl R[6] <- R[6] 0xf immediate
           0x9a ior R[6] <- R[6] R[7]
>          0x9b store R[6] -> ram[R[0]]
           0x9c BR T -0x8f immediate 
       new 0x9d load R[0] <- ram[R[0] + 0x278]
           0x9e mov R[5] <- 0x0
           0x9f BR T -0x7d immediate 
      swap 0xa0 load R[0] <- ram[R[0] + 0x284]


lie 151-152 end of I machine code

0x14  r6  20 decimal  5 words/cells above
0xfe8 r0

>     main 0xe sub R[1] <- R[1] 0x0 immediate
           0xf BR EQ 0x1f immediate 
           0x10 load R[4] <- ram[R[1]]
           0x11 lsl R[5] <- R[4] 0x2 immediate
           0x12 asr R[5] <- R[5] 0x11 immediate
           0x13 BR EQ 0x1 immediate 

0xff0  r1



saved mkII.2.3

      main 0xe sub R[1] <- R[1] 0x0 immediate
           0xf BR EQ 0x1f immediate 
>          0x10 load R[4] <- ram[R[1]]
           0x11 lsl R[5] <- R[4] 0x2 immediate
           0x12 asr R[5] <- R[5] 0x11 immediate
           0x13 BR EQ 0x1 immediate 
           0x14 add R[5] <- R[5] R[1]
           0x15 lsl R[6] <- R[4] 0x11 immediate

line 50 
0x1400004  r4

      main 0xe sub R[1] <- R[1] 0x0 immediate
           0xf BR EQ 0x1f immediate 
           0x10 load R[4] <- ram[R[1]]
           0x11 lsl R[5] <- R[4] 0x2 immediate
           0x12 asr R[5] <- R[5] 0x11 immediate
           0x13 BR EQ 0x1 immediate 
>          0x14 add R[5] <- R[5] R[1]
           0x15 lsl R[6] <- R[4] 0x11 immediate
           0x16 asr R[6] <- R[6] 0x11 immediate
           0x17 BR EQ 0x1 immediate 
           0x18 add R[6] <- R[6] R[1]
           0x19 load R[3] <- ram[R[5]]

0x1270  r5

           0x14 add R[5] <- R[5] R[1]
           0x15 lsl R[6] <- R[4] 0x11 immediate
           0x16 asr R[6] <- R[6] 0x11 immediate
           0x17 BR EQ 0x1 immediate 
>          0x18 add R[6] <- R[6] R[1]
           0x19 load R[3] <- ram[R[5]]
           0x1a mov R[1] <- R[6]
           0x1b asr R[6] <- R[3] 0x1e immediate
           0x1c and R[6] <- R[6] 0x2 immediate
           0x1d sub R[6] <- R[6] 0x2 immediate

0xff4  r6

>          0x19 load R[3] <- ram[R[5]]

line 53
0x0  r3   ram[0x1270]  

>          0x1a mov R[1] <- R[6]

line 56

0xff4  r1


           0x1d sub R[6] <- R[6] 0x2 immediate
           0x1e BR NE 0x4 immediate 
           0x1f mov R[6] <- 0x3fff0000
           0x20 ior R[6] <- R[6] 0xffff immediate
           0x21 and R[6] <- R[6] R[3]
           0x22 BR T R[6]
>     push 0x23 sub R[0] <- R[0] 0x4 immediate
           0x24 sub R[5] <- R[5] 0x0 immediate
           0x25 BR EQ 0x6 immediate 
           0x26 sub R[2] <- R[5] R[0]
           0x27 BR HI 0x1 immediate 
           0x28 and R[2] <- R[2] 0x7fff immediate

Not a literal, 0x0 (but it should be, it should point to swap)

      push 0x23 sub R[0] <- R[0] 0x4 immediate
           0x24 sub R[5] <- R[5] 0x0 immediate
           0x25 BR EQ 0x6 immediate 
>          0x26 sub R[2] <- R[5] R[0]
           0x27 BR HI 0x1 immediate 
           0x28 and R[2] <- R[2] 0x7fff immediate
           0x29 lsl R[2] <- R[2] 0xf immediate
           0x2a ior R[2] <- R[2] 0x4 immediate
           0x2b BR T 0x1 immediate 

line 68

0xfe4  r0  SP
0x28c  r2  TOS
0x1270 r5  TermAddr

      push 0x23 sub R[0] <- R[0] 0x4 immediate
           0x24 sub R[5] <- R[5] 0x0 immediate
           0x25 BR EQ 0x6 immediate 
           0x26 sub R[2] <- R[5] R[0]
           0x27 BR HI 0x1 immediate 
           0x28 and R[2] <- R[2] 0x7fff immediate
>          0x29 lsl R[2] <- R[2] 0xf immediate
           0x2a ior R[2] <- R[2] 0x4 immediate
           0x2b BR T 0x1 immediate 
      jpel 0x2c mov R[2] <- 0x4
      done 0x2d store R[2] -> ram[R[0]]
           0x2e BR T -0x21 immediate 

line 75




==================================================

iball * 2

0xfeac0000  r2


0xc0  r7

           0x67 load R[3] <- ram[R[6]]
     iball 0x68 lsl R[7] <- R[3] 0x2 immediate
           0x69 asr R[7] <- R[7] 0x11 immediate
           0x6a BR EQ 0x1 immediate 
>          0x6b add R[7] <- R[7] R[6]
           0x6c lsl R[8] <- R[3] 0x11 immediate
           0x6d asr R[8] <- R[8] 0x11 immediate
           0x6e BR EQ 0x1 immediate 
           0x6f add R[8] <- R[8] R[6]
           0x70 mov R[6] <- R[8]

0x0  r8

           0x70 mov R[6] <- R[8]
>          0x71 sub R[0] <- R[0] 0x4 immediate
           0x72 sub R[8] <- R[8] 0x0 immediate
           0x73 BR EQ 0x8 immediate 
           0x74 sub R[7] <- R[7] 0x0 immediate
           0x75 BR EQ 0x2 immediate 
           0x76 sub R[7] <- R[7] R[0]
           0x77 and R[7] <- R[7] 0x7fff immediate
           0x78 lsl R[7] <- R[7] 0xf immediate
           0x79 ior R[7] <- R[7] R[9]
           0x7a store R[7] -> ram[R[0]]
           0x7b BR T 0xb immediate 
>          0x7c sub R[7] <- R[7] 0x0 immediate
           0x7d BR EQ 0x2 immediate 
           0x7e sub R[7] <- R[7] R[0]
           0x7f and R[7] <- R[7] 0x7fff immediate
           0x80 sub R[1] <- R[1] 0x0 immediate
           0x81 BR EQ 0x2 immediate 



===================================================

Looksgood so far...

but we just loaded
>          0x67 load R[3] <- ram[R[6]]

and the value in r3 is malformed:

In [14]: x = 0xfeac0000

In [15]: bin(x)
Out[15]: '0b11111110101011000000000000000000'

In [16]: len(_)-2
Out[16]: 32

We just loaded that at line 123
load(TERM, TEMP0),

Ah!

I think it's that expr_cell/2 doesn't zero out the top two bits for
records that have negative head offsets.









PC == 0
PC == 0x25

ram[R[0]]
ram[R[1]]