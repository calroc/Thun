------------------------------------------------------------------------

## loop

Basis Combinator

Expect a quoted program `Q` and a Boolean value on the stack.  If the value is false
discard the quoted program, otherwise run a copy of `Q` and `loop` again.

       false [Q] loop
    --------------------


       true [Q] . loop
    --------------------------
                . Q [Q] loop

### Discussion

This, along with [branch] and [fork], is one of the four main combinators
of all programming.  The fourth, sequence, is implied by juxtaposition.
That is to say, in Joy `F G` is like `G(F(...))` in a language bassed on
function application.  Or again, to quote the [Joy Wikipedia
entry](https://en.wikipedia.org/wiki/Joy_(programming_language)#Mathematical_purity),

> In Joy, the meaning function is a homomorphism from the syntactic monoid onto the semantic monoid. That is, the syntactic relation of concatenation of symbols maps directly onto the semantic relation of composition of functions.

Anyway, [branch], [fork], amd [loop] are the fundamental combinators in Joy.
Just as [branch] has it's more common and convenient form [ifte],
[loop] has [while].

### Crosslinks

[branch]
[fork]
[while]
