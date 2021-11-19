
*******************
Thun: Joy in Python
*******************

This implementation is meant as a tool for exploring the programming
model and method of Joy. Python seems like a great implementation
language for Joy for several reasons.

* We can lean on the Python immutable types for our basic semantics and types: ints, floats, strings, and tuples, which enforces functional purity.
* We get garbage collection for free.
* Compilation via Cython.
* Python is a "glue language" with loads of libraries which we can wrap in Joy functions.


`Read-Eval-Print Loop (REPL) <https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`__
====================================================================================================

The main way to interact with the Joy interpreter is through a simple
`REPL <https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`__
that you start by running the package:

::

    $ python3 -m joy
    Thun - Copyright © 2017 Simon Forman
    This program comes with ABSOLUTELY NO WARRANTY; for details type "warranty".
    This is free software, and you are welcome to redistribute it
    under certain conditions; type "sharing" for details.
    Type "words" to see a list of all words, and "[<name>] help" to print the
    docs for a word.


    <-top

    joy? _

The ``<-top`` marker points to the top of the (initially empty) stack.
You can enter Joy notation at the prompt and a :doc:`trace of evaluation <../pretty>` will
be printed followed by the stack and prompt again::

    joy? 23 sqr 18 +

    547 <-top

    joy? 

There is a `trace` combinator::

    joy? 23 [sqr 18 +] trace
        23 . sqr 18 +
        23 . dup mul 18 +
     23 23 . mul 18 +
       529 . 18 +
    529 18 . +
       547 . 

    547 <-top

    joy? 


The Stack
=============

In Joy, in addition to the types Boolean, integer, float, and string,
there is a :doc:`single sequence type <../stack>` represented by enclosing a sequence of
terms in brackets ``[...]``. This sequence type is used to represent
both the stack and the expression. It is a `cons
list <https://en.wikipedia.org/wiki/Cons#Lists>`__ made from Python
tuples.


Purely Functional Datastructures
=================================

Because Joy stacks are made out of Python tuples they are immutable, as are the other Python types we "borrow" for Joy, so all Joy datastructures are `purely functional <https://en.wikipedia.org/wiki/Purely_functional_data_structure>`__.


The ``joy()`` function
=======================

An Interpreter
~~~~~~~~~~~~~~~~~

The ``joy()`` interpreter function is extrememly simple. It accepts a stack, an
expression, and a dictionary, and it iterates through the expression
putting values onto the stack and delegating execution to functions which it
looks up in the dictionary.


`Continuation-Passing Style <https://en.wikipedia.org/wiki/Continuation-passing_style>`__
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One day I thought, What happens if you rewrite Joy to use
`CPS <https://en.wikipedia.org/wiki/Continuation-passing_style>`__? I
made all the functions accept and return the expression as well as the
stack and found that all the combinators could be rewritten to work by
modifying the expression rather than making recursive calls to the
``joy()`` function.


View function
~~~~~~~~~~~~~

The ``joy()`` function accepts an optional ``viewer`` argument that
is a function which it calls on
each iteration passing the current stack and expression just before
evaluation. This can be used for tracing, breakpoints, retrying after
exceptions, or interrupting an evaluation and saving to disk or sending
over the network to resume later. The stack and expression together
contain all the state of the computation at each step.


The ``TracePrinter``.
~~~~~~~~~~~~~~~~~~~~~

A ``viewer`` records each step of the evaluation of a Joy program. The
``TracePrinter`` has a facility for printing out a trace of the
evaluation, one line per step. Each step is aligned to the current
interpreter position, signified by a period separating the stack on the
left from the pending expression ("continuation") on the right.


Parser
======

The parser is extremely simple.  The undocumented ``re.Scanner`` class
does the tokenizing and then the parser builds the tuple
structure out of the tokens. There's no Abstract Syntax Tree or anything
like that.


Symbols
~~~~~~~~~~~~~

TODO: Symbols are just a string subclass; used by the parser to represent function names and by the interpreter to look up functions in the dictionary.  N.B.: Symbols are not looked up at parse-time.  You *could* define recursive functions, er, recusively, without ``genrec`` or other recursion combinators  ``foo == ... foo ...`` but don't do that.


Token Regular Expressions
~~~~~~~~~~~~~~~~~~~~~~~~~

::

    123   1.2   'single quotes'  "double quotes"   function

TBD (look in the :module: joy.parser  module.)


Examples
~~~~~~~~~~~

.. code:: ipython2

    joy.parser.text_to_expression('1 2 3 4 5')  # A simple sequence.


.. parsed-literal::

    (1, (2, (3, (4, (5, ())))))


.. code:: ipython2

    joy.parser.text_to_expression('[1 2 3] 4 5')  # Three items, the first is a list with three items


.. parsed-literal::

    ((1, (2, (3, ()))), (4, (5, ())))


.. code:: ipython2

    joy.parser.text_to_expression('1 23 ["four" [-5.0] cons] 8888')  # A mixed bag. cons is
                                                                     # a Symbol, no lookup at
                                                                     # parse-time.  Haiku docs.



.. parsed-literal::

    (1, (23, (('four', ((-5.0, ()), (cons, ()))), (8888, ()))))



.. code:: ipython2

    joy.parser.text_to_expression('[][][][][]')  # Five empty lists.




.. parsed-literal::

    ((), ((), ((), ((), ((), ())))))



.. code:: ipython2

    joy.parser.text_to_expression('[[[[[]]]]]')  # Five nested lists.




.. parsed-literal::

    ((((((), ()), ()), ()), ()), ())



Library
=======

The Joy library of functions (aka commands, or "words" after Forth
usage) encapsulates all the actual functionality (no pun intended) of
the Joy system. There are simple functions such as addition ``add`` (or
``+``, the library module supports aliases), and combinators which
provide control-flow and higher-order operations.

Many of the functions are defined in Python, like ``dip``:

.. code:: ipython2

    print inspect.getsource(joy.library.dip)


.. parsed-literal::

    def dip(stack, expression, dictionary):
      (quote, (x, stack)) = stack
      expression = x, expression
      return stack, concat(quote, expression), dictionary
    

Some functions are defined in equations in terms of other functions.
When the interpreter executes a definition function that function just
pushes its body expression onto the pending expression (the
continuation) and returns control to the interpreter.

.. code:: ipython2

    print joy.library.definitions


.. parsed-literal::

    second == rest first
    third == rest rest first
    product == 1 swap [*] step
    swons == swap cons
    swoncat == swap concat
    flatten == [] swap [concat] step
    unit == [] cons
    quoted == [unit] dip
    unquoted == [i] dip
    enstacken == stack [clear] dip
    disenstacken == ? [uncons ?] loop pop
    ? == dup truthy
    dinfrirst == dip infra first
    nullary == [stack] dinfrirst
    unary == [stack [pop] dip] dinfrirst
    binary == [stack [popop] dip] dinfrirst
    ternary == [stack [popop pop] dip] dinfrirst
    pam == [i] map
    run == [] swap infra
    sqr == dup mul
    size == 0 swap [pop ++] step
    cleave == [i] app2 [popd] dip
    average == [sum 1.0 *] [size] cleave /
    gcd == 1 [tuck modulus dup 0 >] loop pop
    least_fraction == dup [gcd] infra [div] concat map
    *fraction == [uncons] dip uncons [swap] dip concat [*] infra [*] dip cons
    *fraction0 == concat [[swap] dip * [*] dip] infra
    down_to_zero == [0 >] [dup --] while
    range_to_zero == unit [down_to_zero] infra
    anamorphism == [pop []] swap [dip swons] genrec
    range == [0 <=] [1 - dup] anamorphism
    while == swap [nullary] cons dup dipd concat loop
    dudipd == dup dipd
    primrec == [i] genrec
    


Currently, there's no function to add new definitions to the dictionary
from "within" Joy code itself. Adding new definitions remains a
meta-interpreter action. You have to do it yourself, in Python, and wash
your hands afterward.

It would be simple enough to define one, but it would open the door to
*name binding* and break the idea that all state is captured in the
stack and expression. There's an implicit *standard dictionary* that
defines the actual semantics of the syntactic stack and expression
datastructures (which only contain symbols, not the actual functions.
Pickle some and see for yourself.)

"There should be only one."
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Which brings me to talking about one of my hopes and dreams for this
notation: "There should be only one." What I mean is that there should
be one universal standard dictionary of commands, and all bespoke work
done in a UI for purposes takes place by direct interaction and macros.
There would be a *Grand Refactoring* biannually (two years, not six
months, that's semi-annually) where any new definitions factored out of
the usage and macros of the previous time, along with new algorithms and
such, were entered into the dictionary and posted to e.g. IPFS.

Code should not burgeon wildly, as it does today. The variety of code
should map more-or-less to the well-factored variety of human
computably-solvable problems. There shouldn't be dozens of chat apps, JS
frameworks, programming languages. It's a waste of time, a `fractal
"thundering herd"
attack <https://en.wikipedia.org/wiki/Thundering_herd_problem>`__ on
human mentality.

Literary Code Library
~~~~~~~~~~~~~~~~~~~~~

If you read over the other notebooks you'll see that developing code in
Joy is a lot like doing simple mathematics, and the descriptions of the
code resemble math papers. The code also works the first time, no bugs.
If you have any experience programming at all, you are probably
skeptical, as I was, but it seems to work: deriving code mathematically
seems to lead to fewer errors.

But my point now is that this great ratio of textual explanation to wind
up with code that consists of a few equations and could fit on an index
card is highly desirable. Less code has fewer errors. The structure of
Joy engenders a kind of thinking that seems to be very effective for
developing structured processes.

There seems to be an elegance and power to the notation.

