#GPLC_OPTIONS=--min-size
GPLC_OPTIONS=--no-top-level
#GPLC_OPTIONS=

THUN_DEPS=parser.pl defs.pl main.pl math.pl DCG_basics.pl fork.pl

thun: thun.pl $(THUN_DEPS) Makefile
	gplc $(GPLC_OPTIONS) -o thun thun.pl $(THUN_DEPS)

defs.pl: meta-defs.pl parser.pl defs.txt thun.pl DCG_basics.pl
	gprolog \
		--consult-file meta-defs.pl \
		--consult-file parser.pl \
		--consult-file fork.pl \
		--consult-file thun.pl \
		--consult-file DCG_basics.pl \
		--query-goal do

math.pl: meta-math.pl
	gprolog \
		--consult-file meta-math.pl \
		--query-goal do

