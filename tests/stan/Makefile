# The general command line is, e.g.:
#
#     gmake -j
#
# To make all the tests, e.g.:
#
#     gmake -j --always-make
#
# (The the g- prefix indicates that this is GNU make or compatible,
# the ``-j`` switch tells make to use multiple jobs to take advantage of
# multi-core systems.)
#
# For the explanation of how this test system works see:
# https://chrismorgan.info/blog/make-and-git-diff-test-harness/

# Set JOY to point to the joy interpreter to test.
#JOY=joy

TESTS != ls *.joy
STDOUTS = $(TESTS:.joy=.stdout)

GIT_OPTS = --exit-code
GIT_OPTS += --src-prefix=expected/ --dst-prefix=actual/
GIT_OPTS += --name-only

# N.B. I'm using FreeBSD with GNU coreutils, so gtouch rather than touch
# (for that --date opt.)  It's not ideal to hard-code the g- prefix here,
# I know, but it was faster than rewriting the command to use FreeBSD's
# touch as I already had coreutils installed for other reasons.  If you
# are using GNU coreutils natively on your system then you can remove the
# g- prefix here.

TOUCH = touch --date=@0


.PHONY: test

test: $(STDOUTS)

%.stdout: %.joy
	@$(JOY) < $< > $@ 2> $*.stderr || ($(TOUCH) $@; false)
	@git diff $(GIT_OPTS) $@ $*.stderr || ($(TOUCH) $@; false)


