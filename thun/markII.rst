

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

