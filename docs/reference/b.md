--------------------

## b

(Combinator)

Run two quoted programs

       [P] [Q] b
    ---------------
          P Q

### Definition

    [i] dip i

### Derivation

    [P] [Q] b
    [P] [Q] [i] dip i
    [P] i [Q] i
     P    [Q] i
     P     Q

### Discussion

This combinator comes in handy.

### Crosslinks

[dupdip](#dupdip)
[ii](#ii)

