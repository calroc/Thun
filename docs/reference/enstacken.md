------------------------------------------------------------------------

## enstacken

Function

Put the stack onto the stack replacing the contents of the stack.

       ... a b c enstacken
    -------------------------
           [c b a ...]


### Definition

> [stack] \[[clear]\] [dip]

### Discussion

This is a destructive version of [stack].  See the note under
[disenstacken] about the apparent but illusory reversal of the stack.

### Crosslinks

[stack]
[unstack]
[disenstacken]

