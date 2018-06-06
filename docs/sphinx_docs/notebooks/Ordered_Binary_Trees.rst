
Treating Trees I: Ordered Binary Trees
======================================

Although any expression in Joy can be considered to describe a
`tree <https://en.wikipedia.org/wiki/Tree_structure>`__ with the quotes
as compound nodes and the non-quote values as leaf nodes, in this page I
want to talk about `ordered binary
trees <https://en.wikipedia.org/wiki/Binary_search_tree>`__ and how to
make and use them.

The basic structure, in a `crude type
notation <https://en.wikipedia.org/wiki/Algebraic_data_type>`__, is:

::

    Tree :: [] | [key value Tree Tree]

That says that a Tree is either the empty quote ``[]`` or a quote with
four items: a key, a value, and two Trees representing the left and
right branches of the tree.

We're going to derive some recursive functions to work with such
datastructures:

::

    Tree-add
    Tree-delete
    Tree-get
    Tree-iter
    Tree-iter-order

Once these functions are defined we have a new "type" to work with, and
the Sufficiently Smart Compiler can be modified to use an optimized
implementation under the hood. (Where does the "type" come from? It has
a contingent existence predicated on the disciplined use of these
functions on otherwise undistinguished Joy datastructures.)

.. code:: ipython2

    from notebook_preamble import D, J, V, define, DefinitionWrapper

Adding Nodes to the Tree
------------------------

Let's consider adding nodes to a Tree structure.

::

       Tree value key Tree-add
    -----------------------------
                Tree′

Adding to an empty node.
~~~~~~~~~~~~~~~~~~~~~~~~

If the current node is ``[]`` then you just return
``[key value [] []]``:

::

    Tree-add == [popop not] [[pop] dipd Tree-new] [R0] [R1] genrec

``Tree-new``
^^^^^^^^^^^^

Where ``Tree-new`` is defined as:

::

       value key Tree-new
    ------------------------
       [key value [] []]

Example:

::

    value key swap [[] []] cons cons
    key value      [[] []] cons cons
    key      [value [] []]      cons
         [key value [] []]

Definition:

::

    Tree-new == swap [[] []] cons cons

.. code:: ipython2

    define('Tree-new == swap [[] []] cons cons')

.. code:: ipython2

    J('"v" "k" Tree-new')


.. parsed-literal::

    ['k' 'v' [] []]


(As an implementation detail, the ``[[] []]`` literal used in the
definition of ``Tree-new`` will be reused to supply the *constant* tail
for *all* new nodes produced by it. This is one of those cases where you
get amortized storage "for free" by using `persistent
datastructures <https://en.wikipedia.org/wiki/Persistent_data_structure>`__.
Because the tail, which is ``((), ((), ()))`` in Python, is immutable
and embedded in the definition body for ``Tree-new``, all new nodes can
reuse it as their own tail without fear that some other code somewhere
will change it.)

Adding to a non-empty node.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We now have to derive ``R0`` and ``R1``, consider:

::

    [key_n value_n left right] value key R0 [Tree-add] R1

In this case, there are three possibilites: the key can be greater or
less than or equal to the node's key. In two of those cases we will need
to apply a copy of ``Tree-add``, so ``R0`` is pretty much out of the
picture.

::

    [R0] == []

A predicate to compare keys.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    [key_n value_n left right] value key [BTree-add] R1

The first thing we need to do is compare the the key we're adding to the
node key and ``branch`` accordingly:

::

    [key_n value_n left right] value key [BTree-add] [P] [T] [E] ifte

That would suggest something like:

::

    [key_n value_n left right] value key [BTree-add] P
    [key_n value_n left right] value key [BTree-add] pop roll> pop first >
    [key_n value_n left right] value key                 roll> pop first >
    key [key_n value_n left right] value                 roll> pop first >
    key key_n                                                            >
    Boolean

Let's abstract the predicate just a little to let us specify the
comparison operator:

::

    P > == pop roll> pop first >
    P < == pop roll> pop first <
    P   == pop roll> pop first

.. code:: ipython2

    define('P == pop roll> pop first')

.. code:: ipython2

    J('["old_key" 23 [] []] 17 "new_key" ["..."] P')


.. parsed-literal::

    'new_key' 'old_key'


If the key we're adding is greater than the node's key.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here the parentheses are meant to signify that the expression is not
literal, the code in the parentheses is meant to have been evaluated:

::

       [key_n value_n left right] value key [Tree-add] T
    -------------------------------------------------------
       [key_n value_n left (Tree-add key value right)]

So how do we do this? We're going to want to use ``infra`` on some
function ``K`` that has the key and value to work with, as well as the
quoted copy of ``Tree-add`` to apply somehow. Considering the node as a
stack:

::

       right left value_n key_n value key [Tree-add] K
    -----------------------------------------------------
       right value key Tree-add left value_n key_n

Pretty easy:

::

    right left value_n key_n value key [Tree-add] cons cons dipdd
    right left value_n key_n [value key Tree-add]           dipdd
    right value key Tree-add left value_n key_n

So:

::

    K == cons cons dipdd

Looking at it from the point-of-view of the node as node again:

::

    [key_n value_n left right] [value key [Tree-add] K] infra

Expand ``K`` and evaluate a little:

::

    [key_n value_n left right] [value key [Tree-add] K] infra
    [key_n value_n left right] [value key [Tree-add] cons cons dipdd] infra
    [key_n value_n left right] [[value key Tree-add]           dipdd] infra

Then, working backwards:

::

    [key_n value_n left right] [[value key Tree-add]           dipdd]      infra
    [key_n value_n left right] [value key Tree-add]           [dipdd] cons infra
    [key_n value_n left right] value key [Tree-add] cons cons [dipdd] cons infra

And so ``T`` is just:

::

    T == cons cons [dipdd] cons infra

.. code:: ipython2

    define('T == cons cons [dipdd] cons infra')

.. code:: ipython2

    J('["old_k" "old_value" "left" "right"] "new_value" "new_key" ["Tree-add"] T')


.. parsed-literal::

    ['old_k' 'old_value' 'left' 'Tree-add' 'new_key' 'new_value' 'right']


If the key we're adding is less than the node's key.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is very very similar to the above:

::

    [key_n value_n left right] value key [Tree-add] E
    [key_n value_n left right] value key [Tree-add] [P <] [Te] [Ee] ifte

.. code:: ipython2

    define('E == [P <] [Te] [Ee] ifte')

In this case ``Te`` works that same as ``T`` but on the left child tree
instead of the right, so the only difference is that it must use
``dipd`` instead of ``dipdd``:

::

    Te == cons cons [dipd] cons infra

.. code:: ipython2

    define('Te == cons cons [dipd] cons infra')

.. code:: ipython2

    J('["old_k" "old_value" "left" "right"] "new_value" "new_key" ["Tree-add"] Te')


.. parsed-literal::

    ['old_k' 'old_value' 'Tree-add' 'new_key' 'new_value' 'left' 'right']


Else the keys must be equal.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This means we must find:

::

       [key old_value left right] new_value key [Tree-add] Ee
    ------------------------------------------------------------
       [key new_value left right]

This is another easy one:

::

    Ee == pop swap roll< rest rest cons cons

Example:

::

    [key old_value left right] new_value key [Tree-add] pop swap roll< rest rest cons cons
    [key old_value left right] new_value key                swap roll< rest rest cons cons
    [key old_value left right] key new_value                     roll< rest rest cons cons
    key new_value [key old_value left right]                           rest rest cons cons
    key new_value [              left right]                                     cons cons
                  [key new_value left right]

.. code:: ipython2

    define('Ee == pop swap roll< rest rest cons cons')

.. code:: ipython2

    J('["k" "old_value" "left" "right"] "new_value" "k" ["Tree-add"] Ee')


.. parsed-literal::

    ['k' 'new_value' 'left' 'right']


Now we can define ``Tree-add``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    Tree-add == [popop not] [[pop] dipd Tree-new] [] [[P >] [T] [E] ifte] genrec

Putting it all together:

::

    Tree-new == swap [[] []] cons cons
    P == pop roll> pop first
    T == cons cons [dipdd] cons infra
    Te == cons cons [dipd] cons infra
    Ee == pop swap roll< rest rest cons cons
    E == [P <] [Te] [Ee] ifte
    R == [P >] [T] [E] ifte

    Tree-add == [popop not] [[pop] dipd Tree-new] [] [R] genrec

.. code:: ipython2

    define('Tree-add == [popop not] [[pop] dipd Tree-new] [] [[P >] [T] [E] ifte] genrec')

Examples
~~~~~~~~

.. code:: ipython2

    J('[] 23 "b" Tree-add')  # Initial


.. parsed-literal::

    ['b' 23 [] []]


.. code:: ipython2

    J('["b" 23 [] []] 88 "c" Tree-add')  # Greater than


.. parsed-literal::

    ['b' 23 [] ['c' 88 [] []]]


.. code:: ipython2

    J('["b" 23 [] []] 88 "a" Tree-add')  # Less than


.. parsed-literal::

    ['b' 23 ['a' 88 [] []] []]


.. code:: ipython2

    J('["b" 23 [] []] 88 "b" Tree-add')  # Equal to


.. parsed-literal::

    ['b' 88 [] []]


.. code:: ipython2

    J('[] 23 "b" Tree-add 88 "a" Tree-add 44 "c" Tree-add')  # Series.


.. parsed-literal::

    ['b' 23 ['a' 88 [] []] ['c' 44 [] []]]


.. code:: ipython2

    J('[] [[23 "b"] [88 "a"] [44 "c"]] [i Tree-add] step')


.. parsed-literal::

    ['b' 23 ['a' 88 [] []] ['c' 44 [] []]]


Interlude: ``cmp`` combinator
-----------------------------

Instead of mucking about with nested ``ifte`` combinators let's use
``cmp`` which takes two values and three quoted programs on the stack
and runs one of the three depending on the results of comparing the two
values:

::

       a b [G] [E] [L] cmp
    ------------------------- a > b
            G

       a b [G] [E] [L] cmp
    ------------------------- a = b
                E

       a b [G] [E] [L] cmp
    ------------------------- a < b
                    L

.. code:: ipython2

    J("1 0 ['G'] ['E'] ['L'] cmp")


.. parsed-literal::

    'G'


.. code:: ipython2

    J("1 1 ['G'] ['E'] ['L'] cmp")


.. parsed-literal::

    'E'


.. code:: ipython2

    J("0 1 ['G'] ['E'] ['L'] cmp")


.. parsed-literal::

    'L'


Redefine ``Tree-add``
~~~~~~~~~~~~~~~~~~~~~

We need a new non-destructive predicate ``P``:

::

       [node_key node_value left right] value key [Tree-add] P
    ------------------------------------------------------------------------
       [node_key node_value left right] value key [Tree-add] key node_key

Let's start with ``over`` to get a copy of the key and then apply some
function ``Q`` with the ``nullary`` combinator so it can dig out the
node key (by throwing everything else away):

::

    P == over [Q] nullary

    [node_key node_value left right] value key [Tree-add] over [Q] nullary
    [node_key node_value left right] value key [Tree-add] key  [Q] nullary

And ``Q`` would be:

::

    Q == popop popop first

    [node_key node_value left right] value key [Tree-add] key Q
    [node_key node_value left right] value key [Tree-add] key popop popop first
    [node_key node_value left right] value key                      popop first
    [node_key node_value left right]                                      first
     node_key

Or just:

::

    P == over [popop popop first] nullary

.. code:: ipython2

    define('P == over [popop popop first] nullary')

Using ``cmp`` to simplify `our code above at
``R1`` <#Adding-to-a-non-empty-node.>`__:

::

    [node_key node_value left right] value key [Tree-add] R1
    [node_key node_value left right] value key [Tree-add] P [T] [E] [Te] cmp

The line above becomes one of the three lines below:

::

    [node_key node_value left right] value key [Tree-add] T
    [node_key node_value left right] value key [Tree-add] E
    [node_key node_value left right] value key [Tree-add] Te

The definition is a little longer but, I think, more elegant and easier
to understand:

::

    Tree-add == [popop not] [[pop] dipd Tree-new] [] [P [T] [Ee] [Te] cmp] genrec

.. code:: ipython2

    define('Tree-add == [popop not] [[pop] dipd Tree-new] [] [P [T] [Ee] [Te] cmp] genrec')

.. code:: ipython2

    J('[] 23 "b" Tree-add 88 "a" Tree-add 44 "c" Tree-add')  # Still works.


.. parsed-literal::

    ['b' 23 ['a' 88 [] []] ['c' 44 [] []]]


A Function to Traverse this Structure
-------------------------------------

Let's take a crack at writing a function that can recursively iterate or
traverse these trees.

Base case ``[]``
~~~~~~~~~~~~~~~~

The stopping predicate just has to detect the empty list:

::

    Tree-iter == [not] [E] [R0] [R1] genrec

And since there's nothing at this node, we just ``pop`` it:

::

    Tree-iter == [not] [pop] [R0] [R1] genrec

Node case ``[key value left right]``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we need to figure out ``R0`` and ``R1``:

::

    Tree-iter == [not] [pop] [R0]           [R1] genrec
              == [not] [pop] [R0 [Tree-iter] R1] ifte

Let's look at it *in situ*:

::

    [key value left right] R0 [Tree-iter] R1

Processing the current node.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``R0`` is almost certainly going to use ``dup`` to make a copy of the
node and then ``dip`` on some function to process the copy with it:

::

    [key value left right] [F] dupdip                 [Tree-iter] R1
    [key value left right]  F  [key value left right] [Tree-iter] R1

For example, if we're getting all the keys ``F`` would be ``first``:

::

    R0 == [first] dupdip

    [key value left right] [first] dupdip                 [Tree-iter] R1
    [key value left right]  first  [key value left right] [Tree-iter] R1
    key                            [key value left right] [Tree-iter] R1

Recur
^^^^^

Now ``R1`` needs to apply ``[Tree-iter]`` to ``left`` and ``right``. If
we drop the key and value from the node using ``rest`` twice we are left
with an interesting situation:

::

    key [key value left right] [Tree-iter] R1
    key [key value left right] [Tree-iter] [rest rest] dip
    key [key value left right] rest rest [Tree-iter]
    key [left right] [Tree-iter]

Hmm, will ``step`` do?

::

    key [left right] [Tree-iter] step
    key left Tree-iter [right] [Tree-iter] step
    key left-keys [right] [Tree-iter] step
    key left-keys right Tree-iter
    key left-keys right-keys

Neat. So:

::

    R1 == [rest rest] dip step

Putting it together
~~~~~~~~~~~~~~~~~~~

We have:

::

    Tree-iter == [not] [pop] [[F] dupdip] [[rest rest] dip step] genrec

When I was reading this over I realized ``rest rest`` could go in
``R0``:

::

    Tree-iter == [not] [pop] [[F] dupdip rest rest] [step] genrec

(And ``[step] genrec`` is such a cool and suggestive combinator!)

Parameterizing the ``F`` per-node processing function.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

                    [F] Tree-iter
    ------------------------------------------------------
       [not] [pop] [[F] dupdip rest rest] [step] genrec

Working backward:

::

    [not] [pop] [[F] dupdip rest rest]            [step] genrec
    [not] [pop] [F]       [dupdip rest rest] cons [step] genrec
    [F] [not] [pop] roll< [dupdip rest rest] cons [step] genrec

``Tree-iter``
~~~~~~~~~~~~~

::

    Tree-iter == [not] [pop] roll< [dupdip rest rest] cons [step] genrec

.. code:: ipython2

    define('Tree-iter == [not] [pop] roll< [dupdip rest rest] cons [step] genrec')

Examples
~~~~~~~~

.. code:: ipython2

    J('[] [foo] Tree-iter')  #  It doesn't matter what F is as it won't be used.


.. parsed-literal::

    


.. code:: ipython2

    J("['b' 23 ['a' 88 [] []] ['c' 44 [] []]] [first] Tree-iter")


.. parsed-literal::

    'b' 'a' 'c'


.. code:: ipython2

    J("['b' 23 ['a' 88 [] []] ['c' 44 [] []]] [second] Tree-iter")


.. parsed-literal::

    23 88 44


Interlude: A Set-like Datastructure
-----------------------------------

We can use this to make a set-like datastructure by just setting values
to e.g. 0 and ignoring them. It's set-like in that duplicate items added
to it will only occur once within it, and we can query it in
`:math:`O(\log_2 N)` <https://en.wikipedia.org/wiki/Binary_search_tree#cite_note-2>`__
time.

.. code:: ipython2

    J('[] [3 9 5 2 8 6 7 8 4] [0 swap Tree-add] step')


.. parsed-literal::

    [3 0 [2 0 [] []] [9 0 [5 0 [4 0 [] []] [8 0 [6 0 [] [7 0 [] []]] []]] []]]


.. code:: ipython2

    define('to_set == [] swap [0 swap Tree-add] step')

.. code:: ipython2

    J('[3 9 5 2 8 6 7 8 4] to_set')


.. parsed-literal::

    [3 0 [2 0 [] []] [9 0 [5 0 [4 0 [] []] [8 0 [6 0 [] [7 0 [] []]] []]] []]]


And with that we can write a little program ``unique`` to remove
duplicate items from a list.

.. code:: ipython2

    define('unique == [to_set [first] Tree-iter] cons run')

.. code:: ipython2

    J('[3 9 3 5 2 9 8 8 8 6 2 7 8 4 3] unique')  # Filter duplicate items.


.. parsed-literal::

    [7 6 8 4 5 9 2 3]


A Version of ``Tree-iter`` that does In-Order Traversal
-------------------------------------------------------

If you look back to the `non-empty case of the ``Tree-iter``
function <#Node-case-%5Bkey-value-left-right%5D>`__ we can design a
variant that first processes the left child, then the current node, then
the right child. This will allow us to traverse the tree in sort order.

::

    Tree-iter-order == [not] [pop] [R0] [R1] genrec

To define ``R0`` and ``R1`` it helps to look at them as they will appear
when they run:

::

    [key value left right] R0 [BTree-iter-order] R1

Process the left child.
~~~~~~~~~~~~~~~~~~~~~~~

Staring at this for a bit suggests ``dup third`` to start:

::

    [key value left right] R0        [Tree-iter-order] R1
    [key value left right] dup third [Tree-iter-order] R1
    [key value left right] left      [Tree-iter-order] R1

Now maybe:

::

    [key value left right] left [Tree-iter-order] [cons dip] dupdip
    [key value left right] left [Tree-iter-order]  cons dip [Tree-iter-order]
    [key value left right] [left Tree-iter-order]       dip [Tree-iter-order]
    left Tree-iter-order [key value left right]             [Tree-iter-order]

Process the current node.
~~~~~~~~~~~~~~~~~~~~~~~~~

So far, so good. Now we need to process the current node's values:

::

    left Tree-iter-order [key value left right] [Tree-iter-order] [[F] dupdip] dip
    left Tree-iter-order [key value left right] [F] dupdip [Tree-iter-order]
    left Tree-iter-order [key value left right] F [key value left right] [Tree-iter-order]

If ``F`` needs items from the stack below the left stuff it should have
``cons``'d them before beginning maybe? For functions like ``first`` it
works fine as-is.

::

    left Tree-iter-order [key value left right] first [key value left right] [Tree-iter-order]
    left Tree-iter-order key [key value left right] [Tree-iter-order]

Process the right child.
~~~~~~~~~~~~~~~~~~~~~~~~

First ditch the rest of the node and get the right child:

::

    left Tree-iter-order key [key value left right] [Tree-iter-order] [rest rest rest first] dip
    left Tree-iter-order key right [Tree-iter-order]

Then, of course, we just need ``i`` to run ``Tree-iter-order`` on the
right side:

::

    left Tree-iter-order key right [Tree-iter-order] i
    left Tree-iter-order key right Tree-iter-order

Defining ``Tree-iter-order``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The result is a little awkward:

::

    R1 == [cons dip] dupdip [[F] dupdip] dip [rest rest rest first] dip i

Let's do a little semantic factoring:

::

    fourth == rest rest rest first

    proc_left == [cons dip] dupdip
    proc_current == [[F] dupdip] dip
    proc_right == [fourth] dip i

    Tree-iter-order == [not] [pop] [dup third] [proc_left proc_current proc_right] genrec

Now we can sort sequences.

.. code:: ipython2

    #define('Tree-iter-order == [not] [pop] [dup third] [[cons dip] dupdip [[first] dupdip] dip [rest rest rest first] dip i] genrec')
    
    
    DefinitionWrapper.add_definitions('''
    
    fourth == rest rest rest first
    
    proc_left == [cons dip] dupdip
    proc_current == [[first] dupdip] dip
    proc_right == [fourth] dip i
    
    Tree-iter-order == [not] [pop] [dup third] [proc_left proc_current proc_right] genrec
    
    ''', D)
    
    


.. code:: ipython2

    J('[3 9 5 2 8 6 7 8 4] to_set Tree-iter-order')


.. parsed-literal::

    2 3 4 5 6 7 8 9


Parameterizing the ``[F]`` function is left as an exercise for the
reader.

Getting values by key
---------------------

Let's derive a function that accepts a tree and a key and returns the
value associated with that key.

::

       tree key Tree-get
    -----------------------
            value

But what do we do if the key isn't in the tree? In Python we might raise
a ``KeyError`` but I'd like to avoid exceptions in Joy if possible, and
here I think it's possible. (Division by zero is an example of where I
think it's probably better to let Python crash Joy. Sometimes the
machinery fails and you have to "stop the line", I think.)

Let's pass the buck to the caller by making the base case a given, you
have to decide for yourself what ``[E]`` should be.

::

       tree key [E] Tree-get
    ---------------------------- key in tree
               value

       tree key [E] Tree-get
    ---------------------------- key not in tree
             [] key E

The base case ``[]``
~~~~~~~~~~~~~~~~~~~~

As before, the stopping predicate just has to detect the empty list:

::

    Tree-get == [pop not] [E] [R0] [R1] genrec

So we define:

::

    Tree-get == [pop not] swap [R0] [R1] genrec

Note that this ``Tree-get`` creates a slightly different function than
itself and *that function* does the actual recursion. This kind of
higher-level programming is unusual in most languages but natural in
Joy.

::

    tree key [E] [pop not] swap [R0] [R1] genrec
    tree key [pop not] [E] [R0] [R1] genrec

The anonymous specialized recursive function that will do the real work.

::

    [pop not] [E] [R0] [R1] genrec

Node case ``[key value left right]``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we need to figure out ``R0`` and ``R1``:

::

    [key value left right] key R0 [BTree-get] R1

We want to compare the search key with the key in the node, and if they
are the same return the value, otherwise recur on one of the child
nodes. So it's very similar to the above funtion, with ``[R0] == []``
and ``R1 == P [T>] [E] [T<] cmp``:

::

    [key value left right] key [BTree-get] P [T>] [E] [T<] cmp

Predicate
^^^^^^^^^

::

    P == over [get-node-key] nullary
    get-node-key == pop popop first

The only difference is that ``get-node-key`` does one less ``pop``
because there's no value to discard.

Branches
^^^^^^^^

Now we have to derive the branches:

::

    [key_n value_n left right] key [BTree-get] T>
    [key_n value_n left right] key [BTree-get] E
    [key_n value_n left right] key [BTree-get] T<

Greater than and less than
^^^^^^^^^^^^^^^^^^^^^^^^^^

The cases of ``T>`` and ``T<`` are similar to above but instead of using
``infra`` we have to discard the rest of the structure:

::

       [key_n value_n left right] key [BTree-get] T>
    ---------------------------------------------------
                           right  key  BTree-get

And:

::

       [key_n value_n left right] key [BTree-get] T<
    ---------------------------------------------------
                      left        key  BTree-get

So:

::

    T> == [fourth] dipd i
    T< == [third] dipd i

E.g.:

::

    [key_n value_n left right]        key [BTree-get] [fourth] dipd i
    [key_n value_n left right] fourth key [BTree-get]               i
                        right         key [BTree-get]               i
                        right         key  BTree-get

Equal keys
^^^^^^^^^^

Return the node's value:

::

    [key_n value_n left right] key [BTree-get] E == value_n

    E == popop second

``Tree-get``
~~~~~~~~~~~~

So:

::

    fourth == rest rest rest first
    get-node-key == pop popop first
    P == over [get-node-key] nullary
    T> == [fourth] dipd i
    T< == [third] dipd i
    E == popop second

    Tree-get == [pop not] swap [] [P [T>] [E] [T<] cmp] genrec

.. code:: ipython2

    # I don't want to deal with name conflicts with the above so I'm inlining everything here.
    # The original Joy system has "hide" which is a meta-command which allows you to use named
    # definitions that are only in scope for a given definition.  I don't want to implement
    # that (yet) so...
    
    
    define('''
    Tree-get == [pop not] swap [] [
      over [pop popop first] nullary
      [[fourth] dipd i]
      [popop second]
      [[third] dipd i]
      cmp
      ] genrec
    ''')

.. code:: ipython2

    J('["gary" 23 [] []] "mike" [popd " not in tree" +] Tree-get')


.. parsed-literal::

    'mike not in tree'


.. code:: ipython2

    J('["gary" 23 [] []] "gary" [popop "err"] Tree-get')


.. parsed-literal::

    23


.. code:: ipython2

    J('''
    
        [] [[0 'a'] [1 'b'] [2 'c']] [i Tree-add] step
    
        'c' [popop 'not found'] Tree-get
    
    ''')


.. parsed-literal::

    2


.. code:: ipython2

    J('''
    
        [] [[0 'a'] [1 'b'] [2 'c']] [i Tree-add] step
    
        'd' [popop 'not found'] Tree-get
    
    ''')


.. parsed-literal::

    'not found'


Tree-delete
-----------

Now let's write a function that can return a tree datastructure with a
key, value pair deleted:

::

       tree key Tree-delete
    ---------------------------
              tree

If the key is not in tree it just returns the tree unchanged.

Base case
~~~~~~~~~

Same as above.

::

    Tree-Delete == [pop not] [pop] [R0] [R1] genrec

Recur
~~~~~

Now we get to figure out the recursive case. We need the node's key to
compare and we need to carry the key into recursive branches. Let ``D``
be shorthand for ``Tree-Delete``:

::

    D == Tree-Delete == [pop not] [pop] [R0] [R1] genrec

    [node_key node_value left right] key R0                   [D] R1
    [node_key node_value left right] key over  first swap dup [D] cons R1′
    [node_key node_value left right] key [...] first swap dup [D] cons R1′
    [node_key node_value left right] key node_key    swap dup [D] cons R1′
    [node_key node_value left right] node_key key         dup [D] cons R1′
    [node_key node_value left right] node_key key key         [D] cons R1′
    [node_key node_value left right] node_key key         [key D]      R1′

And then:

::

    [node_key node_value left right] node_key key [key D] R1′
    [node_key node_value left right] node_key key [key D] roll> [T>] [E] [T<] cmp
    [node_key node_value left right] node_key key [key D] roll> [T>] [E] [T<] cmp
    [node_key node_value left right] [key D] node_key key       [T>] [E] [T<] cmp

So:

::

    R0 == over first swap dup
    R1 == cons roll> [T>] [E] [T<] cmp

Compare Keys
~~~~~~~~~~~~

The last line above:

::

    [node_key node_value left right] [key D] node_key key [T>] [E] [T<] cmp

Then becomes one of these three:

::

    [node_key node_value left right] [key D] T>
    [node_key node_value left right] [key D] E
    [node_key node_value left right] [key D] T<

Greater than case and less than case
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

       [node_key node_value left right] [F] T>
    -------------------------------------------------
       [node_key node_value (left F) right]


       [node_key node_value left right] [F] T<
    -------------------------------------------------
       [node_key node_value left (right F)]

First, treating the node as a stack:

::

    right left       node_value node_key [key D] dipd
    right left key D node_value node_key
    right left'      node_value node_key

Ergo:

::

    [node_key node_value left right] [key D] [dipd] cons infra

So:

::

    T> == [dipd] cons infra
    T< == [dipdd] cons infra

The else case
~~~~~~~~~~~~~

We have found the node in the tree where ``key`` equals ``node_key``. We
need to replace the current node with something

::

       [node_key node_value left right] [key D] E
    ------------------------------------------------
                        tree

We have to handle three cases, so let's use ``cond``.

One or more child nodes are ``[]``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first two cases are symmetrical: if we only have one non-empty child
node return it. If both child nodes are empty return an empty node.

::

    E == [
        [[pop third not] pop fourth]
        [[pop fourth not] pop third]
        [default]
    ] cond

Both child nodes are non-empty.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If both child nodes are non-empty, we find the highest node in our lower
sub-tree, take its key and value to replace (delete) our own, then get
rid of it by recursively calling delete() on our lower sub-node with our
new key.

(We could also find the lowest node in our higher sub-tree and take its
key and value and delete it. I only implemented one of these two
symmetrical options. Over a lot of deletions this might make the tree
more unbalanced. Oh well.)

The initial structure of the default function:

::

    default == [E′] cons infra

    [node_key node_value left right] [key D] default
    [node_key node_value left right] [key D] [E′] cons infra
    [node_key node_value left right] [[key D] E′]      infra

    right left node_value node_key [key D] E′

First things first, we no longer need this node's key and value:

::

    right left node_value node_key [key D] roll> popop E″
    right left [key D] node_value node_key       popop E″
    right left [key D]                                 E″

We have to we find the highest (right-most) node in our lower (left) sub-tree:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    right left [key D] E″

Ditch the key:

::

    right left [key D] rest E‴
    right left     [D]      E‴

Find the right-most node:

::

    right left        [D] [dup W] dip E⁗
    right left dup  W [D]             E⁗
    right left left W [D]             E⁗

Consider:

::

    left W

We know left is not empty:

::

    [L_key L_value L_left L_right] W

We want to keep extracting the right node as long as it is not empty:

::

    W.rightmost == [P] [B] while

    left W.rightmost W′

The predicate:

::

    [L_key L_value L_left L_right] P
    [L_key L_value L_left L_right] fourth
                          L_right

This can run on ``[]`` so must be guarded:

::

    ?fourth ==  [] [fourth] [] ifte

( if\_not\_empty == [] swap [] ifte ?fourth == [fourth] if\_not\_empty )

The body is just ``fourth``:

::

    left [?fourth] [fourth] while W′
    rightest                      W′

So:

::

    W.rightmost == [?fourth] [fourth] while

Found right-most node in our left sub-tree
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We know rightest is not empty:

::

    [R_key R_value R_left R_right] W′
    [R_key R_value R_left R_right] W′
    [R_key R_value R_left R_right] uncons uncons pop
    R_key [R_value R_left R_right]        uncons pop
    R_key R_value [R_left R_right]               pop
    R_key R_value

So:

::

    W == [?fourth] [fourth] while uncons uncons pop

And:

::

    right left left W        [D] E⁗
    right left R_key R_value [D] E⁗

Replace current node key and value, recursively delete rightmost
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Final stretch. We want to end up with something like:

::

    right left [R_key D] i R_value R_key
    right left  R_key D    R_value R_key
    right left′            R_value R_key

If we adjust our definition of ``W`` to include ``over`` at the end:

::

    W == [fourth] [fourth] while uncons uncons pop over

That will give us:

::

    right left R_key R_value R_key [D] E⁗

    right left         R_key R_value R_key [D] cons dipd E⁗′
    right left         R_key R_value [R_key D]      dipd E⁗′
    right left R_key D R_key R_value                     E⁗′
    right left′        R_key R_value                     E⁗′
    right left′        R_key R_value                     swap
    right left′ R_value R_key

So:

::

    E′ == roll> popop E″

    E″ == rest E‴

    E‴ == [dup W] dip E⁗

    E⁗ == cons dipdd swap

Substituting:

::

    W == [fourth] [fourth] while uncons uncons pop over
    E′ == roll> popop rest [dup W] dip cons dipd swap
    E == [
        [[pop third not] pop fourth]
        [[pop fourth not] pop third]
        [[E′] cons infra]
    ] cond

Minor rearrangement, move ``dup`` into ``W``:

::

    W == dup [fourth] [fourth] while uncons uncons pop over
    E′ == roll> popop rest [W] dip cons dipd swap
    E == [
        [[pop third not] pop fourth]
        [[pop fourth not] pop third]
        [[E′] cons infra]
    ] cond

Refactoring
~~~~~~~~~~~

::

    W.rightmost == [fourth] [fourth] while
    W.unpack == uncons uncons pop
    W == dup W.rightmost W.unpack over
    E.clear_stuff == roll> popop rest
    E.delete == cons dipd
    E.0 == E.clear_stuff [W] dip E.delete swap
    E == [
        [[pop third not] pop fourth]
        [[pop fourth not] pop third]
        [[E.0] cons infra]
    ] cond
    T> == [dipd] cons infra
    T< == [dipdd] cons infra
    R0 == over first swap dup
    R1 == cons roll> [T>] [E] [T<] cmp
    BTree-Delete == [pop not] swap [R0] [R1] genrec

By the standards of the code I've written so far, this is a *huge* Joy
program.

.. code:: ipython2

    DefinitionWrapper.add_definitions('''
    first_two == uncons uncons pop
    fourth == rest rest rest first
    ?fourth == [] [fourth] [] ifte
    W.rightmost == [?fourth] [fourth] while
    E.clear_stuff == roll> popop rest
    E.delete == cons dipd
    W == dup W.rightmost first_two over
    E.0 == E.clear_stuff [W] dip E.delete swap
    E == [[[pop third not] pop fourth] [[pop fourth not] pop third] [[E.0] cons infra]] cond
    T> == [dipd] cons infra
    T< == [dipdd] cons infra
    R0 == over first swap dup
    R1 == cons roll> [T>] [E] [T<] cmp
    Tree-Delete == [pop not] [pop] [R0] [R1] genrec
    ''', D)

.. code:: ipython2

    J("['a' 23 [] ['b' 88 [] ['c' 44 [] []]]] 'c' Tree-Delete ")


.. parsed-literal::

    ['a' 23 [] ['b' 88 [] []]]


.. code:: ipython2

    J("['a' 23 [] ['b' 88 [] ['c' 44 [] []]]] 'b' Tree-Delete ")


.. parsed-literal::

    ['a' 23 [] ['c' 44 [] []]]


.. code:: ipython2

    J("['a' 23 [] ['b' 88 [] ['c' 44 [] []]]] 'a' Tree-Delete ")


.. parsed-literal::

    ['b' 88 [] ['c' 44 [] []]]


.. code:: ipython2

    J("['a' 23 [] ['b' 88 [] ['c' 44 [] []]]] 'der' Tree-Delete ")


.. parsed-literal::

    ['a' 23 [] ['b' 88 [] ['c' 44 [] []]]]


.. code:: ipython2

    J('[] [4 2 3 1 6 7 5 ] [0 swap Tree-add] step')


.. parsed-literal::

    [4 0 [2 0 [1 0 [] []] [3 0 [] []]] [6 0 [5 0 [] []] [7 0 [] []]]]


.. code:: ipython2

    J("[4 0 [2 0 [1 0 [] []] [3 0 [] []]] [6 0 [5 0 [] []] [7 0 [] []]]] 3 Tree-Delete ")


.. parsed-literal::

    [4 0 [2 0 [1 0 [] []] []] [6 0 [5 0 [] []] [7 0 [] []]]]


.. code:: ipython2

    J("[4 0 [2 0 [1 0 [] []] [3 0 [] []]] [6 0 [5 0 [] []] [7 0 [] []]]] 4 Tree-Delete ")


.. parsed-literal::

    [3 0 [2 0 [1 0 [] []] []] [6 0 [5 0 [] []] [7 0 [] []]]]


Appendix: The source code.
--------------------------

::

    fourth == rest_two rest first
    ?fourth == [] [fourth] [] ifte
    first_two == uncons uncons pop
    ccons == cons cons
    cinf == cons infra
    rest_two == rest rest

    _Tree_T> == [dipd] cinf
    _Tree_T< == [dipdd] cinf

    _Tree_add_P == over [popop popop first] nullary
    _Tree_add_T> == ccons _Tree_T<
    _Tree_add_T< == ccons _Tree_T>
    _Tree_add_Ee == pop swap roll< rest_two ccons
    _Tree_add_R == _Tree_add_P [_Tree_add_T>] [_Tree_add_Ee] [_Tree_add_T<] cmp
    _Tree_add_E == [pop] dipd Tree-new

    _Tree_iter_order_left == [cons dip] dupdip
    _Tree_iter_order_current == [[F] dupdip] dip
    _Tree_iter_order_right == [fourth] dip i
    _Tree_iter_order_R == _Tree_iter_order_left _Tree_iter_order_current _Tree_iter_order_right

    _Tree_get_P == over [pop popop first] nullary
    _Tree_get_T> == [fourth] dipd i
    _Tree_get_T< == [third] dipd i
    _Tree_get_E == popop second
    _Tree_get_R == _Tree_get_P [_Tree_get_T>] [_Tree_get_E] [_Tree_get_T<] cmp

    _Tree_delete_rightmost == [?fourth] [fourth] while
    _Tree_delete_clear_stuff == roll> popop rest
    _Tree_delete_del == dip cons dipd swap
    _Tree_delete_W == dup _Tree_delete_rightmost first_two over
    _Tree_delete_E.0 == _Tree_delete_clear_stuff [_Tree_delete_W] _Tree_delete_del
    _Tree_delete_E == [[[pop third not] pop fourth] [[pop fourth not] pop third] [[_Tree_delete_E.0] cinf]] cond
    _Tree_delete_R0 == over first swap dup
    _Tree_delete_R1 == cons roll> [_Tree_T>] [_Tree_delete_E] [_Tree_T<] cmp

    Tree-new == swap [[] []] ccons
    Tree-add == [popop not] [_Tree_add_E] [] [_Tree_add_R] genrec
    Tree-iter == [not] [pop] roll< [dupdip rest_two] cons [step] genrec
    Tree-iter-order == [not] [pop] [dup third] [_Tree_iter_order_R] genrec
    Tree-get == [pop not] swap [] [_Tree_get_R] genrec
    Tree-delete == [pop not] [pop] [_Tree_delete_R0] [_Tree_delete_R1] genrec
