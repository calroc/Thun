------------------------------------------------------------------------

## inscribe

Create a new Joy function definition in the Joy dictionary. A definition
is given as a quote with a name followed by a Joy expression.

### Example

    [sqr dup mul] inscribe

### Discussion

This is the only function that modifies the dictionary.  It's provided as a 
convenience, for tinkering with new definitions before entering them into
the `defs.txt` file.  It can be abused, which you should avoid unless you
know what you're doing.

