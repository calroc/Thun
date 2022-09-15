# Joy interpreter in Nim

> Simple pleasures are the best.

This interpreter written in Nim is part of the Thun project, which
includes interpreters written in Python and Prolog, and some explorations
towards compilers for Joy written in Prolog.


## Building

To build an executable just run:

    nim c joy.nim

To build and immediately run an executable use:

    nim c -r joy.nim

To build a smaller binary do:

    nim c -d:release joy.nim
    strip --strip-debug joy.exe


## Dependencies

- Nim functional programming library https://github.com/vegansk/nimfp
- Pure Nim (i.e. not GMP) BigInts library https://github.com/def-/nim-bigints
- ... 


## TODOs:

- link to other Thun sub-projects & info on Joy in general
- ctrl-c should exit cleanly
- graphics?
- `words` and `help`?
- How to integrate docs?


nim cc \
-d:release \
--stackTrace:off \
--lineTrace:off \
--checks:off \
--assertions:off \
--debugger:native \
joy.nim

nim doc --project --index:on --git.url:"https://git.sr.ht/~sforman/Bliss" --git.commit:10b5651ed242fb16c29f2c1f7340d77f65926ca4 --outdir:htmldocs joy.nim

https://git.sr.ht/~sforman/Bliss/tree/10b5651ed242fb16c29f2c1f7340d77f65926ca4/item/types.nim#L26
