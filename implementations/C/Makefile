OBJS=joy.o keywords.o
STATIC_GCLIB=/usr/local/lib/libgc.a /usr/local/lib/libgmp.a

CFLAGS = -ansi -Wall -Wextra -Werror -pedantic -Wstrict-overflow -fno-strict-aliasing -pedantic-errors -I/usr/local/include

#COPTS=-I/usr/local/include
# $(STATIC_GCLIB)

GPERF_OPTS = --readonly-tables --enum --includes
#--global-table


joy: ${OBJS} $(STATIC_GCLIB)

# Gperf doesn't add anything to '{""}' unused entries in the worlist.
# This causes "warning: missing field 'func' initializer [-Wmissing-field-initializers]"
# But we can turn off that warning for this here:
keywords.o: CFLAGS+=-Wno-missing-field-initializers

keywords.c: KEYWORDS.txt
	gperf --output-file=$@ --hash-function-name=keyword_hash $(GPERF_OPTS) KEYWORDS.txt

clean:
	rm -vf *.o
