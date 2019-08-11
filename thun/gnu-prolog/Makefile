GPLC_OPTIONS="--min-size"

thun: thun.pl
	gplc $(GPLC_OPTIONS) -o thun thun.pl

defs.pl: meta-defs.pl defs.txt
	gprolog --consult-file meta-defs.pl defs.txt
