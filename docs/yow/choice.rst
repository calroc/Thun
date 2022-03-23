--------------

choice
^^^^^^^^

Basis Function Combinator


Use a Boolean value to select one of two items.
::

       A B false choice
    ----------------------
       A


       A B true choice
    ---------------------
         B

Currently Python semantics are used to evaluate the "truthiness" of the
Boolean value (so empty string, zero, etc. are counted as false, etc.)


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

