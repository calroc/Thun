------------------------------------------------------------------------

## take

Function

Expects an integer `n` and a list on the stack and replace them with a list
with just the top `n` items in reverse order.

       [a b c d] 2 take
    ----------------------
            [b a]

### Definition

> [\<\<\{\}] \[[shift]\] [times] [pop]

