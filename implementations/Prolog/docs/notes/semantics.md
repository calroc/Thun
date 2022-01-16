# list-structured memory

[In SICP, section 5.3, "Storage Allocation and Garbage Collection"](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-33.html#%_sec_5.3):

> In order to simplify the discussion, we will assume that our register
  machines can be equipped with a list-structured memory, in which the
  basic operations for manipulating list-structured data are primitive.

So they bunt to an abstraction and then implement that abstraction as a
separate problem.  Makes sense.  I see no reason not to adopt the design
described here.

------------------

# Machine Ints vs BigNums

Already there is a problem in the semantics.  SWI Prolog integers can be
larger than machine words, which in the RISC CPU are thirty-two bits.
(GNU Prolog uses machine words for its integers).  THe main options are:

1. Implements "BigNums" for Wirth RISC.

2. Adjust the semantics of Thun to reflect the modular arithmetic of
   machine words and native machine integer math operations.

3. ... something else.

------------------

# specialized versions of `branch` and `ifte`

THere's another semantic wrinkle with branches and Boolean values.
Namely, the CPU provides the condition and the offset in one instruction
whereas Joy has them separated.  I have been thinking about introducing
specialized versions of `branch` as primitives:

    =branch
    >branch
    <branch
    <=branch
    >=branch
    <>branch

Or maybe:

    =?
    >?
    <?
    <=?
    >=?
    <>?

Anyway, it would be pretty easy to detect simple cases of the split
pattern and convert them automatically, but the programmer could use them
directly whenever it made sense.

    > [F] [T] branch ==> [F] [T] >branch

Probably specialized versions of `ifte` would be useful as well.

------------------
