

Mark II
=========================


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

PC == 0
PC == 0x25

ram[R[0]]
ram[R[1]]