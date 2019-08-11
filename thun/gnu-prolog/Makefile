GPLC_OPTIONS="--min-size"

thun: thun.pl parser.pl
	gplc $(GPLC_OPTIONS) -o thun thun.pl parser.pl

defs.pl: meta-defs.pl defs.txt
	gprolog --consult-file meta-defs.pl defs.txt
