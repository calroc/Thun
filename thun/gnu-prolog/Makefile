GPLC_OPTIONS="--min-size"

thun: thun.pl parser.pl defs.pl main.pl
	gplc $(GPLC_OPTIONS) -o thun thun.pl parser.pl defs.pl main.pl

defs.pl: meta-defs.pl parser.pl defs.txt thun.pl
	gprolog \
		--consult-file meta-defs.pl \
		--consult-file parser.pl \
		--consult-file thun.pl \
		--query-goal do

