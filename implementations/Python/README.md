# Joy interpreter in Python

> Simple pleasures are the best.

This interpreter written in Python is part of the Thun project.


## Building

To build an executable script just run:

    make

This will copy the `implementations/defs.txt` file into the Python project and
splice it into the `joy.py` file to create the `joy` executable script.


## Installation

I had the Python package set up to upload to PyPI as "Thun", but the
whole Python distribution story seems unsettled at the moment (2023) so
I've gone back to the *old ways*: there is a single script `joy.py`
that gets modified (``defs.txt`` is inserted) to create a `joy` script
that uses the "shebang" trick to pretend to be a binary.  In other words,
run ``make`` and put the resulting ``joy`` script in your PATH, if that's
what you want to do.  In a year or two the Python folks will have sorted
things out and we can go back to ``pip install Thun`` or whatever.

In the meantime, after `make` splices the `defs.txt` file into the `joy.py`
script to make the `joy` script you can start it as normal:

    ./joy

There is a "quiet" mode for e.g. using joy from a shell script:

    ./joy -q

This suppresses the initial banner output and the prompt text.


