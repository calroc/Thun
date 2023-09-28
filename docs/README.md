# Documentation for Thun

This directory contains the docs for Thun.  It's kind of a mess right now
(Aug 2023) but I'm working on it.  Originally the docs took the form of a
static website created with Sphinx (TODO: link to Sphinx project) and
uploaded to OSDN (which provides a static site hosting service as part of
your project there.)  This was great, but recently I've been having
little troubles with reaching OSDN.  It seems weird to have issues in
2023 (Japan's not that far away?) but there it is.

Now I'm trying a simple collection of Markdown files which get converted
to HTML by a simple Python script (the reference pages still use Pandoc.)
I want to have a few pages covering Thun project itself, but most of the
docs should be the notebooks and the function reference.  (Some of the
implementation languages have automatic doc generators and it might be
nice to imclude those in the docs, but getting them all to look like each
other might be bleah.)

## Directory

- `build_index.py` a crude script to convert Markdown to HTML
- `html/` output of the HTML creation process.  Serve the contents of
  this directly.
- `_joy_interpreter_flowchart_url.txt`  This is the URL for the SVG
  flowchart image generator service. 
- `Makefile`  uses `build_index.py` to convert `source/` to `html/`.
- `notebooks/` The original Jupyter notebooks (`*.ipynb` files).  I have
  a simple Joy kernel but I no longer know how to install and use it, so
  I'm converting these to Markdown files in the `source/notebooks`
  directory.
- `OSDN-Makefile` The old make file for OSDN, took care of uploading.
- `reference/` The function reference docs.  Some of these are out of
  date (e,g, some of the names of functions have changed.)  There's a
  script to convert the separate per-function Markdown files into a
  single-page HTML file.
- `source/`  The new source directory for generating the static site (in
  the `html/` directory.)

