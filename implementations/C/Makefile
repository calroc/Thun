OBJS=joy.o keywords.o
STATIC_GCLIB=/usr/local/lib/libgc.a /usr/local/lib/libgmp.a
CFLAGS = -ansi -Wall -Wextra -Werror -pedantic -Wstrict-overflow -fno-strict-aliasing -pedantic-errors -I/usr/local/include
#COPTS=-I/usr/local/include
# $(STATIC_GCLIB)

GPERF_OPTS = --readonly-tables --enum --includes --global-table


joy: ${OBJS} $(STATIC_GCLIB)

keywords.c: KEYWORDS.txt keywords.h
	gperf --output-file=$@ --hash-function-name=keyword_hash $(GPERF_OPTS) KEYWORDS.txt

# keywords.c doesn't strictly depend on keywords.h, but I want make to
# remake if keywords.h is touched.
