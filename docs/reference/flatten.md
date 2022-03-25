------------------------------------------------------------------------

## flatten

Function

Given a list of lists, concatinate them.

### Example

       [[1 2] [3 [4] 5] [6 7]] flatten
    -------------------------------------
              [1 2 3 [4] 5 6 7]

### Definition

> [\<\{\}] \[[concat]\] [step]

### Discussion

Note that only one "level" of lists is flattened.  In the example above
`[4]` is not unquoted.

### Crosslinks

[concat]
[first]
[first_two]
[fourth]
[getitem]
[remove]
[rest]
[reverse]
[rrest]
[second]
[shift]
[shunt]
[size]
[sort]
[split_at]
[split_list]
[swaack]
[third]
[zip]

