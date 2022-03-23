--------------

cond
^^^^^^

Basis Function Combinator


This combinator works like a case statement.  It expects a single quote
on the stack that must contain zero or more condition quotes and a 
default quote.  Each condition clause should contain a quoted predicate
followed by the function expression to run if that predicate returns
true.  If no predicates return true the default function runs.

It works by rewriting into a chain of nested `ifte` expressions, e.g.::

          [[[B0] T0] [[B1] T1] [D]] cond
    -----------------------------------------
       [B0] [T0] [[B1] [T1] [D] ifte] ifte



Gentzen diagram.

Definition
~~~~~~~~~~

if not basis.

Derivation
~~~~~~~~~~

if not basis.

Source
~~~~~~~~~~

if basis

Discussion
~~~~~~~~~~

Crosslinks
~~~~~~~~~~

