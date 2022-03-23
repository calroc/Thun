------------------------------------------------------------------------

## genrec

Basis Function Combinator

General Recursion Combinator. :

    [if] [then] [rec1] [rec2] genrec
    ---------------------------------------------------------------------
    [if] [then] [rec1 [[if] [then] [rec1] [rec2] genrec] rec2] ifte

From \"Recursion Theory and Joy\" (j05cmp.html) by Manfred von Thun:
\"The genrec combinator takes four program parameters in addition to
whatever data parameters it needs. Fourth from the top is an if-part,
followed by a then-part. If the if-part yields true, then the then-part
is executed and the combinator terminates. The other two parameters are
the rec1-part and the rec2-part. If the if-part yields false, the
rec1-part is executed. Following that the four program parameters and
the combinator are again pushed onto the stack bundled up in a quoted
form. Then the rec2-part is executed, where it will find the bundled
form. Typically it will then execute the bundled form, either with i or
with app2, or some other combinator.\"

The way to design one of these is to fix your base case \[then\] and the
test \[if\], and then treat rec1 and rec2 as an else-part
\"sandwiching\" a quotation of the whole function.

For example, given a (general recursive) function \'F\': :

    F == [I] [T] [R1] [R2] genrec

If the \[I\] if-part fails you must derive R1 and R2 from: :

    ... R1 [F] R2

Just set the stack arguments in front, and figure out what R1 and R2
have to do to apply the quoted \[F\] in the proper way. In effect, the
genrec combinator turns into an ifte combinator with a quoted copy of
the original definition in the else-part: :

    F == [I] [T] [R1]   [R2] genrec
      == [I] [T] [R1 [F] R2] ifte

Primitive recursive functions are those where R2 == i. :

    P == [I] [T] [R] tailrec
      == [I] [T] [R [P] i] ifte
      == [I] [T] [R P] ifte

Gentzen diagram.

### Definition

if not basis.

### Derivation

if not basis.

### Source

if basis

### Discussion

Lorem ipsum.

### Crosslinks

Lorem ipsum.
