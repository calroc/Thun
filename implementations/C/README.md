# Thun in C

This is my first attempt to write C in many years, I hope it's not too
embarrassing.

I use Gperf to create a static wordlist.  This make word lookup very
efficient, but there's no way currently to add definitions at runtime.

There's a janky script *convert\_defs.py* that generates *definitions.c*,
*definitions.h*, and *KEYWORDS.txt* from the *defs.txt* file.  I would
like to replace the dependency on Python with, say, Awk or something.

## Dependencies

- python
- gperf
- gmake
- boehm-gc
- gmp


    sudo apt install gperf 
