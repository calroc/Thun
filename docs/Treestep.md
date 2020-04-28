# Treating Trees II: `treestep`
Let's consider a tree structure, similar to one described ["Why functional programming matters" by John Hughes](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf), that consists of a node value followed by zero or more child trees.  (The asterisk is meant to indicate the [Kleene star](https://en.wikipedia.org/wiki/Kleene_star).)

    tree = [] | [node tree*]

In the spirit of `step` we are going to define a combinator `treestep` which expects a tree and three additional items: a base-case function `[B]`, and two quoted programs `[N]` and `[C]`.

    tree [B] [N] [C] treestep

If the current tree node is empty then just execute `B`:

       [] [B] [N] [C] treestep
    ---------------------------
       []  B

Otherwise, evaluate `N` on the node value, `map` the whole function (abbreviated here as `K`) over the child trees recursively, and then combine the result with `C`.

       [node tree*] [B] [N] [C] treestep
    --------------------------------------- w/ K == [B] [N] [C] treestep
           node N [tree*] [K] map C

(Later on we'll experiment with making `map` part of `C` so you can use other combinators.)

## Derive the recursive function.
We can begin to derive it by finding the `ifte` stage that `genrec` will produce.

    K == [not] [B] [R0]   [R1] genrec
      == [not] [B] [R0 [K] R1] ifte

So we just have to derive `J`:

    J == R0 [K] R1

The behavior of `J` is to accept a (non-empty) tree node and arrive at the desired outcome.

           [node tree*] J
    ------------------------------
       node N [tree*] [K] map C

So `J` will have some form like:

    J == ... [N] ... [K] ... [C] ...

Let's dive in.  First, unquote the node and `dip` `N`.

    [node tree*] uncons [N] dip
    node [tree*]        [N] dip
    node N [tree*]

Next, `map` `K` over the child trees and combine with `C`.

    node N [tree*] [K] map C
    node N [tree*] [K] map C
    node N [K.tree*]       C

So:

    J == uncons [N] dip [K] map C

Plug it in and convert to `genrec`:

    K == [not] [B] [J                       ] ifte
      == [not] [B] [uncons [N] dip [K] map C] ifte
      == [not] [B] [uncons [N] dip]   [map C] genrec

## Extract the givens to parameterize the program.
Working backwards:

    [not] [B]          [uncons [N] dip]                  [map C] genrec
    [B] [not] swap     [uncons [N] dip]                  [map C] genrec
    [B]                [uncons [N] dip] [[not] swap] dip [map C] genrec
                                        ^^^^^^^^^^^^^^^^
    [B] [[N] dip]      [uncons] swoncat [[not] swap] dip [map C] genrec
    [B] [N] [dip] cons [uncons] swoncat [[not] swap] dip [map C] genrec
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^

Extract a couple of auxiliary definitions:

    TS.0 == [[not] swap] dip
    TS.1 == [dip] cons [uncons] swoncat

    [B] [N] TS.1 TS.0 [map C]                         genrec
    [B] [N]           [map C]         [TS.1 TS.0] dip genrec
    [B] [N] [C]         [map] swoncat [TS.1 TS.0] dip genrec

The givens are all to the left so we have our definition.

### (alternate) Extract the givens to parameterize the program.
Working backwards:

    [not] [B]           [uncons [N] dip]            [map C] genrec
    [not] [B] [N]       [dip] cons [uncons] swoncat [map C] genrec
    [B] [N] [not] roll> [dip] cons [uncons] swoncat [map C] genrec
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

## Define `treestep`


```python
from notebook_preamble import D, J, V, define, DefinitionWrapper
```


```python
DefinitionWrapper.add_definitions('''

    _treestep_0 == [[not] swap] dip
    _treestep_1 == [dip] cons [uncons] swoncat
    treegrind == [_treestep_1 _treestep_0] dip genrec
    treestep == [map] swoncat treegrind

''', D)
```

## Examples
Consider trees, the nodes of which are integers.  We can find the sum of all nodes in a tree with this function:

    sumtree == [pop 0] [] [sum +] treestep


```python
define('sumtree == [pop 0] [] [sum +] treestep')
```

Running this function on an empty tree value gives zero:

       [] [pop 0] [] [sum +] treestep
    ------------------------------------
               0


```python
J('[] sumtree')  # Empty tree.
```

    0


Running it on a non-empty node:

    [n tree*]  [pop 0] [] [sum +] treestep
    n [tree*] [[pop 0] [] [sum +] treestep] map sum +
    n [ ... ]                                   sum +
    n m                                             +
    n+m



```python
J('[23] sumtree')  # No child trees.
```

    23



```python
J('[23 []] sumtree')  # Child tree, empty.
```

    23



```python
J('[23 [2 [4]] [3]] sumtree')  # Non-empty child trees.
```

    32



```python
J('[23 [2 [8] [9]] [3] [4 []]] sumtree')  # Etc...
```

    49



```python
J('[23 [2 [8] [9]] [3] [4 []]] [pop 0] [] [cons sum] treestep')  # Alternate "spelling".
```

    49



```python
J('[23 [2 [8] [9]] [3] [4 []]] [] [pop 23] [cons] treestep')  # Replace each node.
```

    [23 [23 [23] [23]] [23] [23 []]]



```python
J('[23 [2 [8] [9]] [3] [4 []]] [] [pop 1] [cons] treestep')
```

    [1 [1 [1] [1]] [1] [1 []]]



```python
J('[23 [2 [8] [9]] [3] [4 []]] [] [pop 1] [cons] treestep sumtree')
```

    6



```python
J('[23 [2 [8] [9]] [3] [4 []]] [pop 0] [pop 1] [sum +] treestep')  # Combine replace and sum into one function.
```

    6



```python
J('[4 [3 [] [7]]] [pop 0] [pop 1] [sum +] treestep')  # Combine replace and sum into one function.
```

    3


## Redefining the Ordered Binary Tree in terms of `treestep`.

    Tree = [] | [[key value] left right]

What kind of functions can we write for this with our `treestep`?

The pattern for processing a non-empty node is:

    node N [tree*] [K] map C

Plugging in our BTree structure:

    [key value] N [left right] [K] map C

### Traversal
    [key value] first [left right] [K] map i
    key [value]       [left right] [K] map i
    key               [left right] [K] map i
    key               [lkey rkey ]         i
    key                lkey rkey

This doesn't quite work:


```python
J('[[3 0] [[2 0] [][]] [[9 0] [[5 0] [[4 0] [][]] [[8 0] [[6 0] [] [[7 0] [][]]][]]][]]] ["B"] [first] [i] treestep')
```

    3 'B' 'B'


Doesn't work because `map` extracts the `first` item of whatever its mapped function produces.  We have to return a list, rather than depositing our results directly on the stack.


    [key value] N     [left right] [K] map C

    [key value] first [left right] [K] map flatten cons
    key               [left right] [K] map flatten cons
    key               [[lk] [rk] ]         flatten cons
    key               [ lk   rk  ]                 cons
                      [key  lk   rk  ]

So:

    [] [first] [flatten cons] treestep


```python
J('[[3 0] [[2 0] [] []] [[9 0] [[5 0] [[4 0] [] []] [[8 0] [[6 0] [] [[7 0] [] []]] []]] []]]   [] [first] [flatten cons] treestep')
```

    [3 2 9 5 4 8 6 7]


There we go.

### In-order traversal

From here:

    key [[lk] [rk]] C
    key [[lk] [rk]] i
    key  [lk] [rk] roll<
    [lk] [rk] key swons concat
    [lk] [key rk]       concat
    [lk   key rk]

So:

    [] [i roll< swons concat] [first] treestep


```python
J('[[3 0] [[2 0] [] []] [[9 0] [[5 0] [[4 0] [] []] [[8 0] [[6 0] [] [[7 0] [] []]] []]] []]]   [] [uncons pop] [i roll< swons concat] treestep')
```

    [2 3 4 5 6 7 8 9]


## With `treegrind`?
The `treegrind` function doesn't include the `map` combinator, so the `[C]` function must arrange to use some combinator on the quoted recursive copy `[K]`.  With this function, the pattern for processing a non-empty node is:

    node N [tree*] [K] C

Plugging in our BTree structure:

    [key value] N [left right] [K] C


```python
J('[["key" "value"] ["left"] ["right"] ] ["B"] ["N"] ["C"] treegrind')
```

    ['key' 'value'] 'N' [['left'] ['right']] [[not] ['B'] [uncons ['N'] dip] ['C'] genrec] 'C'


## `treegrind` with `step`

Iteration through the nodes


```python
J('[[3 0] [[2 0] [] []] [[9 0] [[5 0] [[4 0] [] []] [[8 0] [[6 0] [] [[7 0] [] []]] []]] []]]   [pop] ["N"] [step] treegrind')
```

    [3 0] 'N' [2 0] 'N' [9 0] 'N' [5 0] 'N' [4 0] 'N' [8 0] 'N' [6 0] 'N' [7 0] 'N'


Sum the nodes' keys.


```python
J('0 [[3 0] [[2 0] [] []] [[9 0] [[5 0] [[4 0] [] []] [[8 0] [[6 0] [] [[7 0] [] []]] []]] []]]   [pop] [first +] [step] treegrind')
```

    44


Rebuild the tree using `map` (imitating `treestep`.)


```python
J('[[3 0] [[2 0] [] []] [[9 0] [[5 0] [[4 0] [] []] [[8 0] [[6 0] [] [[7 0] [] []]] []]] []]]   [] [[100 +] infra] [map cons] treegrind')
```

    [[103 0] [[102 0] [] []] [[109 0] [[105 0] [[104 0] [] []] [[108 0] [[106 0] [] [[107 0] [] []]] []]] []]]


## Do we have the flexibility to reimplement `Tree-get`?
I think we do:

    [B] [N] [C] treegrind

We'll start by saying that the base-case (the key is not in the tree) is user defined, and the per-node function is just the query key literal:

    [B] [query_key] [C] treegrind

This means we just have to define `C` from:

    [key value] query_key [left right] [K] C
 

Let's try `cmp`:

    C == P [T>] [E] [T<] cmp

    [key value] query_key [left right] [K] P [T>] [E] [T<] cmp

### The predicate `P`
Seems pretty easy (we must preserve the value in case the keys are equal):

    [key value] query_key [left right] [K] P
    [key value] query_key [left right] [K] roll<
    [key value] [left right] [K] query_key       [roll< uncons swap] dip

    [key value] [left right] [K] roll< uncons swap query_key
    [left right] [K] [key value]       uncons swap query_key
    [left right] [K] key [value]              swap query_key
    [left right] [K] [value] key                   query_key

    P == roll< [roll< uncons swap] dip

(Possibly with a swap at the end?  Or just swap `T<` and `T>`.)

So now:

    [left right] [K] [value] key query_key [T>] [E] [T<] cmp

Becomes one of these three:

    [left right] [K] [value] T>
    [left right] [K] [value] E
    [left right] [K] [value] T<


### `E`
Easy.

    E == roll> popop first

### `T<` and `T>`

    T< == pop [first] dip i
    T> == pop [second] dip i

## Putting it together


    T> == pop [first] dip i
    T< == pop [second] dip i
    E == roll> popop first
    P == roll< [roll< uncons swap] dip
    
    Tree-get == [P [T>] [E] [T<] cmp] treegrind

To me, that seems simpler than the `genrec` version.


```python
DefinitionWrapper.add_definitions('''

    T> == pop [first] dip i
    T< == pop [second] dip i
    E == roll> popop first
    P == roll< [roll< uncons swap] dip

    Tree-get == [P [T>] [E] [T<] cmp] treegrind

''', D)
```


```python
J('''\

[[3 13] [[2 12] [] []] [[9 19] [[5 15] [[4 14] [] []] [[8 18] [[6 16] [] [[7 17] [] []]] []]] []]]

[] [5] Tree-get

''')
```

    15



```python
J('''\

[[3 13] [[2 12] [] []] [[9 19] [[5 15] [[4 14] [] []] [[8 18] [[6 16] [] [[7 17] [] []]] []]] []]]

[pop "nope"] [25] Tree-get

''')
```

    'nope'

