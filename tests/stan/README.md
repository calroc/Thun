# Simple Test Set for Joy Programming Language

Each test consists of a snippet of Joy code along with the expected
output (stdout and stderr) for a (conforming) Joy implementation.  They
are run with make.

For the explanation of how this test system works see:
https://chrismorgan.info/blog/make-and-git-diff-test-harness/

Set the JOY environment variable to point to the joy interpreter to test,
e.g.:

    export JOY="python -m joy -q"

-or-

    setenv JOY "python -m joy -q"

or whatever for your shell.  Then run make.

The general command line is, e.g.:

    gmake -j

To make all the tests, e.g.:

    gmake -j --always-make

The the ``g-`` prefix indicates that this is GNU make (or compatible), and
the ``-j`` switch tells make to use multiple jobs to take advantage of
multi-core systems.
