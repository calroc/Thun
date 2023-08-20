# Thun

This version is written in [Chicken Scheme](http://www.call-cc.org/).
This is my first Scheme code (first Lisp code, for that matter) so
there's probably a lot of non-idomatic stuff, but it seems to work.

It doesn't do the error messages yet.  Scheme code for list manipulation
is so elegant that it's kind of a shame to make clunky versions just to
customize the error messages.  But if I don't then this interpreter won't
conform to the test suite (as it exists as of this writing.)


I like Scheme.  It seems like an excellent target language for compiling
Joy code, which can then, of course, be compiled to C and thence to
machine code.  I don't know why I didn't try it sooner.


## Build

Build with `make` to generate a `joy` binary.

The `defs.txt` file is converted into a `defs.scm` Scheme source file by
a little utility script `generate_defs.scm`, and then that is included in
the main `joy.scm` compilation using the `-prologue` option of the `csc`
Chicken Scheme compiler.  It's a little clunky, but it seems to work.
Definitions are stored as string literals and converted to Joy
expressions during start up of the compiled binary. It would be nice to
push the conversion and even the entry into the hashtable into compile
time.
