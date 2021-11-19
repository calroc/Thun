
Type Inference of Joy Expressions
=================================

UPDATE: May 2020 - I removed the type inference code in `joy.utils.types`
but you can find it in the `v0.4.0` tag here:
https://osdn.net/projects/joypy/scm/hg/Joypy/tags


Two kinds of type inference are provided, a simple inferencer that can
handle functions that have a single stack effect (aka "type signature")
and that can generate Python code for a limited subset of those
functions, and a more complex inferencer/interpreter hybrid that can
infer the stack effects of most Joy expressions, including multiple stack
effects, unbounded sequences of values, and combinators (if enough
information is available.)


``joy.utils.types``
-------------------


Curently (asterix after name indicates a function that can be
auto-compiled to Python)::

    _Tree_add_Ee = ([a4 a5 ...1] a3 a2 a1 -- [a2 a3 ...1]) *
    _Tree_delete_R0 = ([a2 ...1] a1 -- [a2 ...1] a2 a1 a1) *
    _Tree_delete_clear_stuff = (a3 a2 [a1 ...1] -- [...1]) *
    _Tree_get_E = ([a3 a4 ...1] a2 a1 -- a4) *
    add = (n1 n2 -- n3)  
    and = (b1 b2 -- b3)  
    bool = (a1 -- b1)  
    ccons = (a2 a1 [...1] -- [a2 a1 ...1]) *
    cons = (a1 [...0] -- [a1 ...0]) *
    div = (n1 n2 -- n3)  
    divmod = (n2 n1 -- n4 n3)  
    dup = (a1 -- a1 a1) *
    dupd = (a2 a1 -- a2 a2 a1) *
    dupdd = (a3 a2 a1 -- a3 a3 a2 a1) *
    eq = (n1 n2 -- b1)  
    first = ([a1 ...1] -- a1) *
    first_two = ([a1 a2 ...1] -- a1 a2) *
    floordiv = (n1 n2 -- n3)  
    fourth = ([a1 a2 a3 a4 ...1] -- a4) *
    ge = (n1 n2 -- b1)  
    gt = (n1 n2 -- b1)  
    le = (n1 n2 -- b1)  
    lshift = (n1 n2 -- n3)  
    lt = (n1 n2 -- b1)  
    modulus = (n1 n2 -- n3)  
    mul = (n1 n2 -- n3)  
    ne = (n1 n2 -- b1)  
    neg = (n1 -- n2)  
    not = (a1 -- b1)  
    over = (a2 a1 -- a2 a1 a2) *
    pm = (n2 n1 -- n4 n3)  
    pop = (a1 --) *
    popd = (a2 a1 -- a1) *
    popdd = (a3 a2 a1 -- a2 a1) *
    popop = (a2 a1 --) *
    popopd = (a3 a2 a1 -- a1) *
    popopdd = (a4 a3 a2 a1 -- a2 a1) *
    pow = (n1 n2 -- n3)  
    pred = (n1 -- n2)  
    rest = ([a1 ...0] -- [...0]) *
    rolldown = (a1 a2 a3 -- a2 a3 a1) *
    rollup = (a1 a2 a3 -- a3 a1 a2) *
    rrest = ([a1 a2 ...1] -- [...1]) *
    rshift = (n1 n2 -- n3)  
    second = ([a1 a2 ...1] -- a2) *
    sqrt = (n1 -- n2)  
    stack = (... -- ... [...]) *
    stuncons = (... a1 -- ... a1 a1 [...]) *
    stununcons = (... a2 a1 -- ... a2 a1 a1 a2 [...]) *
    sub = (n1 n2 -- n3)  
    succ = (n1 -- n2)  
    swaack = ([...1] -- [...0]) *
    swap = (a1 a2 -- a2 a1) *
    swons = ([...1] a1 -- [a1 ...1]) *
    third = ([a1 a2 a3 ...1] -- a3) *
    truediv = (n1 n2 -- n3)  
    tuck = (a2 a1 -- a1 a2 a1) *
    uncons = ([a1 ...0] -- a1 [...0]) *
    unit = (a1 -- [a1 ]) *
    unswons = ([a1 ...1] -- [...1] a1) *


Example output of the ``infer()`` function.  The first number on each
line is the depth of the Python stack.  It goes down when the function
backtracks.  The next thing on each line is the currently-computed stack
effect so far.  It starts with the empty "identity function" and proceeds
through the expression, which is the rest of each line.  The function
acts like an interpreter but instead of executing the terms of the
expression it composes them, but for combinators it *does* execute them,
using the output side of the stack effect as the stack.  This seems to
work fine.  With proper definitions for the behavior of the combinators
that can have more than one effect (like ``branch`` or ``loop``) the
``infer()`` function seems to be able to handle anything I throw at it so
far.

::

      7 (--) ∘ pop swap rolldown rest rest cons cons
     10 (a1 --) ∘ swap rolldown rest rest cons cons
     13 (a3 a2 a1 -- a2 a3) ∘ rolldown rest rest cons cons
     16 (a4 a3 a2 a1 -- a2 a3 a4) ∘ rest rest cons cons
     19 ([a4 ...1] a3 a2 a1 -- a2 a3 [...1]) ∘ rest cons cons
     22 ([a4 a5 ...1] a3 a2 a1 -- a2 a3 [...1]) ∘ cons cons
     25 ([a4 a5 ...1] a3 a2 a1 -- a2 [a3 ...1]) ∘ cons
     28 ([a4 a5 ...1] a3 a2 a1 -- [a2 a3 ...1]) ∘ 
    ----------------------------------------
    ([a4 a5 ...1] a3 a2 a1 -- [a2 a3 ...1])


Here's another example (implementing ``ifte``) using some combinators::

      7 (--) ∘ [pred] [mul] [div] [nullary bool] dipd branch
      8 (-- [pred ...2]) ∘ [mul] [div] [nullary bool] dipd branch
      9 (-- [pred ...2] [mul ...3]) ∘ [div] [nullary bool] dipd branch
     10 (-- [pred ...2] [mul ...3] [div ...4]) ∘ [nullary bool] dipd branch
     11 (-- [pred ...2] [mul ...3] [div ...4] [nullary bool ...5]) ∘ dipd branch
     15 (-- [pred ...5]) ∘ nullary bool [mul] [div] branch
     19 (-- [pred ...2]) ∘ [stack] dinfrirst bool [mul] [div] branch
     20 (-- [pred ...2] [stack ]) ∘ dinfrirst bool [mul] [div] branch
     22 (-- [pred ...2] [stack ]) ∘ dip infra first bool [mul] [div] branch
     26 (--) ∘ stack [pred] infra first bool [mul] [div] branch
     29 (... -- ... [...]) ∘ [pred] infra first bool [mul] [div] branch
     30 (... -- ... [...] [pred ...1]) ∘ infra first bool [mul] [div] branch
     34 (--) ∘ pred s1 swaack first bool [mul] [div] branch
     37 (n1 -- n2) ∘ [n1] swaack first bool [mul] [div] branch
     38 (... n1 -- ... n2 [n1 ...]) ∘ swaack first bool [mul] [div] branch
     41 (... n1 -- ... n1 [n2 ...]) ∘ first bool [mul] [div] branch
     44 (n1 -- n1 n2) ∘ bool [mul] [div] branch
     47 (n1 -- n1 b1) ∘ [mul] [div] branch
     48 (n1 -- n1 b1 [mul ...1]) ∘ [div] branch
     49 (n1 -- n1 b1 [mul ...1] [div ...2]) ∘ branch
     53 (n1 -- n1) ∘ div
     56 (f2 f1 -- f3) ∘ 
     56 (i1 f1 -- f2) ∘ 
     56 (f1 i1 -- f2) ∘ 
     56 (i2 i1 -- f1) ∘ 
     53 (n1 -- n1) ∘ mul
     56 (f2 f1 -- f3) ∘ 
     56 (i1 f1 -- f2) ∘ 
     56 (f1 i1 -- f2) ∘ 
     56 (i2 i1 -- i3) ∘ 
    ----------------------------------------
    (f2 f1 -- f3)
    (i1 f1 -- f2)
    (f1 i1 -- f2)
    (i2 i1 -- f1)
    (i2 i1 -- i3)

