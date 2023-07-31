# Joy interpreter in Nim

> Simple pleasures are the best.

This interpreter written in Nim is part of the Thun project.


## Dependencies

- Nim functional programming library https://github.com/vegansk/nimfp
- Pure Nim (i.e. not GMP) BigInts library https://github.com/def-/nim-bigints
- ... 

If you have `nimble` installed:

    nimble install bigints nimfp 


## Building

To build an executable just run:

    make

This will copy the `implementations/defs.txt` file into the Nim project and
compile the `joy` executable.


## TODOs:

- ctrl-c should exit cleanly
- graphics?
- `words` and `help`?
- How to integrate docs?

