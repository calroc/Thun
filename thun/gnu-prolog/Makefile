#GPLC_OPTIONS=--min-size
GPLC_OPTIONS=--no-top-level

thun: thun.pl parser.pl defs.pl main.pl math.pl Makefile
	gplc $(GPLC_OPTIONS) -o thun thun.pl parser.pl defs.pl main.pl math.pl

defs.pl: meta-defs.pl parser.pl defs.txt thun.pl
	gprolog \
		--consult-file meta-defs.pl \
		--consult-file parser.pl \
		--consult-file thun.pl \
		--query-goal do

math.pl: meta-math.pl
	gprolog \
		--consult-file meta-math.pl \
		--query-goal do

