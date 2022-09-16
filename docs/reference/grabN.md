------------------------------------------------------------------------

## grabN

Function

Expect a number on the top of the stack and [cons] that many items from under it onto a new list.

### Example

       a b c d e 3 grabN
    -----------------------
          a b [c d e]

### Definition

> [\<\{\}] \[[cons]\] [times]
