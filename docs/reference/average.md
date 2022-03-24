------------------------------------------------------------------------

## average

Function

Compute the average of a list of numbers.
(Currently broken until I can figure out what to do about "numeric tower"
in Thun.)

### Definition

> \[[sum]\] \[[size]\] [cleave] [/]

### Discussion

Theoretically this function would compute the sum and the size in two
separate threads, then divide.  This works but a compiled version would
probably do better to sum and count the list once, in one thread, eh?

As an exercise in Functional Programming in Joy it would be fun to
convert this into a catamorphism.
See the [Recursion Combinators notebook](https://joypy.osdn.io/notebooks/Recursion_Combinators.html).

