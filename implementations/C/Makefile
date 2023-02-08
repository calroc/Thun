OBJS=joy.o keywords.o definitions.o

LDFLAGS=-L/usr/local/lib -lgc -lgmp

# Hold my hand, in fact, hold both of them, and pat my head too please.
CFLAGS = -ansi -Wall -Wextra -Werror -pedantic -Wstrict-overflow -fno-strict-aliasing -pedantic-errors

CFLAGS += -I/usr/local/include

# TODO: move these to KEYWORDS.txt.
GPERF_OPTS = --readonly-tables --enum --includes --hash-function-name=keyword_hash
#--global-table


joy: ${OBJS} $(STATIC_GCLIB)

joy.o: joy.h definitions.h

definitions.c: defs.txt convert_defs.py
	python convert_defs.py > $@

definitions.h: defs.txt convert_defs.py
	python convert_defs.py --header > $@

definitions.o: definitions.h

KEYWORDS.txt: KEYWORDS.in defs.txt convert_defs.py
	python convert_defs.py --keywords > $@

# Gperf doesn't add anything to '{""}' unused entries in the wordlist.
# This causes "warning: missing field 'func' initializer [-Wmissing-field-initializers]"
# which is promoted to an error and holds up the show.
# But we can turn off that warning for the keyword code like so:
keywords.o: CFLAGS+=-Wno-missing-field-initializers
keywords.o: joy.h

keywords.c: KEYWORDS.txt
	gperf --output-file=$@ $(GPERF_OPTS) $<

clean:
	rm -vf *.o joy KEYWORDS.txt
