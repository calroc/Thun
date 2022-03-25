------------------------------------------------------------------------

## dip

Basis Combinator

The `dip` combinator expects a quoted program on the stack and below it
some item, it hoists the item into the expression and runs the program
on the rest of the stack. 

       ... x [Q] . dip
    ---------------------
             ... . Q x

### Discussion

This along with [infra] are enough to update any datastructure.
See the ["Traversing Datastructures with Zippers" notebook](https://joypy.osdn.io/notebooks/Zipper.html).

Note that the item that was on the top of the stack (`x` in the example above)
will not be treated specially by the interpreter when it is reached
again.  This is something of a footgun.  My advice is to avoid putting
bare unquoted symbols onto the stack, but then you can't use symbols as
"atoms" and also use `dip` and `infra` to operate on compound
datastructures with atoms in them.  This is a kind of side-effect of the
Continuation-Passing Style.  The `dip` combinator could "set aside" the
item and replace it after running `Q` but that means that there is an
"extra space" where the item resides while `Q` runs.  One of the nice
things about CPS is that the whole state is recorded in the stack and
pending expression (not counting modifications to the dictionary.)

### Crosslinks

[dipd]
[dipdd]
[dupdip]
[dupdipd]
[infra]

