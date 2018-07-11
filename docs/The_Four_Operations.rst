
The Four Fundamental Operations of Definite Action
==================================================

All definite actions (computer program) can be defined by four
fundamental patterns of combination:

1. Sequence
2. Branch
3. Loop
4. Parallel

Sequence
--------

Do one thing after another. In joy this is represented by putting two
symbols together, juxtaposition:

::

    foo bar

Operations have inputs and outputs. The outputs of ``foo`` must be
compatible in "arity", type, and shape with the inputs of ``bar``.

Branch
------

Do one thing or another.

::

    boolean [F] [T] branch


       t [F] [T] branch
    ----------------------
              T


       f [F] [T] branch
    ----------------------
          F


    branch == unit cons swap pick i

    boolean [F] [T] branch
    boolean [F] [T] unit cons swap pick i
    boolean [F] [[T]] cons swap pick i
    boolean [[F] [T]] swap pick i
    [[F] [T]] boolean pick i
    [F-or-T] i

Given some branch function ``G``:

::

    G == [F] [T] branch

Used in a sequence like so:

::

    foo G bar

The inputs and outputs of ``F`` and ``T`` must be compatible with the
outputs for ``foo`` and the inputs of ``bar``, respectively.

::

    foo F bar

    foo T bar

``ifte``
~~~~~~~~

Often it will be easier on the programmer to write branching code with
the predicate specified in a quote. The ``ifte`` combinator provides
this (``T`` for "then" and ``E`` for "else"):

::

    [P] [T] [E] ifte

Defined in terms of ``branch``:

::

    ifte == [nullary not] dip branch

In this case, ``P`` must be compatible with the stack and return a
Boolean value, and ``T`` and ``E`` both must be compatible with the
preceeding and following functions, as described above for ``F`` and
``T``. (Note that in the current implementation we are depending on
Python for the underlying semantics, so the Boolean value doesn't *have*
to be Boolean because Python's rules for "truthiness" will be used to
evaluate it. I reflect this in the structure of the stack effect comment
of ``branch``, it will only accept Boolean values, and in the definition
of ``ifte`` above by including ``not`` in the quote, which also has the
effect that the subject quotes are in the proper order for ``branch``.)

Loop
----

Do one thing zero or more times.

::

    boolean [Q] loop


       t [Q] loop
    ----------------
       Q [Q] loop


       ... f [Q] loop
    --------------------
       ...

The ``loop`` combinator generates a copy of itself in the true branch.
This is the hallmark of recursive defintions. In Thun there is no
equivalent to conventional loops. (There is, however, the ``x``
combinator, defined as ``x == dup i``, which permits recursive
constructs that do not need to be directly self-referential, unlike
``loop`` and ``genrec``.)

::

    loop == [] swap [dup dip loop] cons branch

    boolean [Q] loop
    boolean [Q] [] swap [dup dip loop] cons branch
    boolean [] [Q] [dup dip loop] cons branch
    boolean [] [[Q] dup dip loop] branch

In action the false branch does nothing while the true branch does:

::

    t [] [[Q] dup dip loop] branch
          [Q] dup dip loop
          [Q] [Q] dip loop
          Q [Q] loop

Because ``loop`` expects and consumes a Boolean value, the ``Q``
function must be compatible with the previous stack *and itself* with a
boolean flag for the next iteration:

::

    Q == G b

    Q [Q] loop
    G b [Q] loop
    G Q [Q] loop
    G G b [Q] loop
    G G Q [Q] loop
    G G G b [Q] loop
    G G G

``while``
~~~~~~~~~

Keep doing ``B`` *while* some predicate ``P`` is true. This is
convenient as the predicate function is made nullary automatically and
the body function can be designed without regard to leaving a Boolean
flag for the next iteration:

::

                [P] [B] while
    --------------------------------------
       [P] nullary [B [P] nullary] loop


    while == swap [nullary] cons dup dipd concat loop


    [P] [B] while
    [P] [B] swap [nullary] cons dup dipd concat loop
    [B] [P] [nullary] cons dup dipd concat loop
    [B] [[P] nullary] dup dipd concat loop
    [B] [[P] nullary] [[P] nullary] dipd concat loop
    [P] nullary [B] [[P] nullary] concat loop
    [P] nullary [B [P] nullary] loop

Parallel
--------

The *parallel* operation indicates that two (or more) functions *do not
interfere* with each other and so can run in parallel. The main
difficulty in this sort of thing is orchestrating the recombining
("join" or "wait") of the results of the functions after they finish.

The current implementaions and the following definitions *are not
actually parallel* (yet), but there is no reason they couldn't be
reimplemented in terms of e.g. Python threads. I am not concerned with
performance of the system just yet, only the elegance of the code it
allows us to write.

``cleave``
~~~~~~~~~~

Joy has a few parallel combinators, the main one being ``cleave``:

::

                   ... x [A] [B] cleave
    ---------------------------------------------------------
       ... [x ...] [A] infra first [x ...] [B] infra first
    ---------------------------------------------------------
                       ... a b

The ``cleave`` combinator expects a value and two quotes and it executes
each quote in "separate universes" such that neither can affect the
other, then it takes the first item from the stack in each universe and
replaces the quotes with their respective results.

(I'm not sure why it was specified to take that value, I may make a
combinator that does the same thing but without expecting a value.)

::

    cleavish  == unit cons pam uncons uncons pop

    [A] [B] cleavish
    [A] [B] unit cons pam uncons uncons pop
    [A] [[B]] cons pam uncons uncons pop
    [[A] [B]] pam uncons uncons pop
    [a b] uncons uncons pop
    a b

"Apply" Functions
~~~~~~~~~~~~~~~~~

There are also ``app2`` and ``app3`` which run a single quote on more
than one value:

::

                     ... y x [Q] app2
     ---------------------------------------------------------
        ... [y ...] [Q] infra first [x ...] [Q] infra first


            ... z y x [Q] app3
     ---------------------------------
        ... [z ...] [Q] infra first
            [y ...] [Q] infra first
            [x ...] [Q] infra first

Because the quoted program can be ``i`` we can define ``cleave`` in
terms of ``app2``:

::

    cleave == [i] app2 [popd] dip

``map``
~~~~~~~

The common ``map`` function in Joy should also be though of as a
*parallel* operator:

::

    [a b c ...] [Q] map

There is no reason why the implementation of ``map`` couldn't distribute
the ``Q`` function over e.g. a pool of worker CPUs.

``pam``
~~~~~~~

One of my favorite combinators, the ``pam`` combinator is just:

::

    pam == [i] map

This can be used to run any number of programs separately on the current
stack and combine their (first) outputs in a result list.

::

       [[A] [B] [C] ...] [i] map
    -------------------------------
       [ a   b   c  ...]

Handling Other Kinds of Join
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can imagine a few different potentially useful patterns of "joining"
results from parallel combinators.

first-to-finish
^^^^^^^^^^^^^^^

Thinking about variations of ``pam`` there could be one that only
returns the first result of the first-to-finish sub-program, or the
stack could be replaced by its output stack.

The other sub-programs would be cancelled.
