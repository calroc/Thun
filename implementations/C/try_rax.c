#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "linenoise.h"
#include "rax.h"


rax *rt;

#define INSERT(key) raxInsert(rt, (unsigned char*)(key), sizeof((key)), NULL, NULL);


int
index_of_last_symbol_char(const char *buf)
{
	int n = strlen(buf);
	while (n) {
		if (strchr(" []", buf[n - 1])) {
			break;
		}
		n--;
	}
	return n;
}


int
longest_completion(const char *buf, int buffer_length)
{
	int longest = 0;
	raxIterator iter;
	raxStart(&iter, rt);
	raxSeek(&iter, ">=", (unsigned char*)buf, buffer_length);
	while(raxNext(&iter)) {
		if (strncmp((const char *)iter.key, buf, buffer_length))
			break;
		if (iter.key_len > longest) {
			longest = iter.key_len;
		};
	}
	raxStop(&iter);
	return longest;
}


void
prefixed_completion(const char *buf, linenoiseCompletions *lc, int prefix_length)
{
	raxIterator iter;

	/* Get a buffer large enough to hold the largest possible completion. */
	int buffer_length = strlen(buf);
	int completion_buffer_size = prefix_length + longest_completion(buf, buffer_length) + 1;
	char *completion_buffer = malloc(completion_buffer_size);
	if (NULL == completion_buffer)
		return;

	/* Prepare the prefix and trim it from the front of the buf. */
	memcpy(completion_buffer, buf, prefix_length);
	buf += prefix_length;
	buffer_length -= prefix_length;

	raxStart(&iter, rt);
	raxSeek(&iter, ">=", (unsigned char*)buf, buffer_length);
	while(raxNext(&iter)) {
		if (strncmp((const char *)iter.key, buf, buffer_length))
			break;
		completion_buffer[prefix_length] = 0;
		strlcat(completion_buffer + prefix_length, (const char *)iter.key, completion_buffer_size);
		linenoiseAddCompletion(lc, (const char *)completion_buffer);
	}
	raxStop(&iter);
	free(completion_buffer);
}


void
simple_completion(const char *buf, linenoiseCompletions *lc)
{
	int buffer_length = strlen(buf);
	raxIterator iter;
	raxStart(&iter, rt);
	raxSeek(&iter, ">=", (unsigned char*)buf, buffer_length);
	while(raxNext(&iter)) {
		if (strncmp((const char *)iter.key, buf, buffer_length))
			break;
		linenoiseAddCompletion(lc, (const char *)iter.key);
	}
	raxStop(&iter);
}


void
completion(const char *buf, linenoiseCompletions *lc)
{
	int prefix_length = index_of_last_symbol_char(buf);
	if (prefix_length) {
		prefixed_completion(buf, lc, prefix_length);
	} else {
		simple_completion(buf, lc);
	}
}


int
main()
{
	char *line;
	linenoiseSetCompletionCallback(completion);
	rt = raxNew();
	INSERT("=")
	INSERT(">")
	INSERT("<")
	INSERT("!=")
	INSERT("<>")
	INSERT("<=")
	INSERT(">=")
	INSERT("%")
	INSERT("mod")
	INSERT("*")
	INSERT("mul")
	INSERT("+")
	INSERT("add")
	INSERT("-")
	INSERT("sub")
	INSERT("/")
	INSERT("div")
	INSERT("lshift")
	INSERT("rshift")
	INSERT("bool")
	INSERT("branch")
	INSERT("clear")
	INSERT("cmp")
	INSERT("cons")
	INSERT("concat")
	INSERT("dip")
	INSERT("dup")
	INSERT("first")
	INSERT("i")
	INSERT("inscribe")
	INSERT("loop")
	INSERT("pop")
	INSERT("rest")
	INSERT("stack")
	INSERT("swaack")
	INSERT("swap")
	INSERT("fn")
	INSERT("eq")
	INSERT("gt")
	INSERT("lt")
	INSERT("neq")
	INSERT("le")
	INSERT("ge")
	INSERT("--")
	INSERT("?")
	INSERT("and")
	INSERT("++")
	INSERT("or")
	INSERT("!-")
	INSERT("<{}")
	INSERT("<<{}")
	INSERT("abs")
	INSERT("anamorphism")
	INSERT("app1")
	INSERT("app2")
	INSERT("app3")
	INSERT("appN")
	INSERT("at")
	INSERT("average")
	INSERT("b")
	INSERT("binary")
	INSERT("ccccons")
	INSERT("ccons")
	INSERT("clear")
	INSERT("cleave")
	INSERT("clop")
	INSERT("cmp")
	INSERT("codi")
	INSERT("codireco")
	INSERT("dinfrirst")
	INSERT("dipd")
	INSERT("disenstacken")
	INSERT("divmod")
	INSERT("down_to_zero")
	INSERT("drop")
	INSERT("dupd")
	INSERT("dupdd")
	INSERT("dupdip")
	INSERT("dupdipd")
	INSERT("enstacken")
	INSERT("first")
	INSERT("flatten")
	INSERT("fork")
	INSERT("fourth")
	INSERT("gcd")
	INSERT("genrec")
	INSERT("grabN")
	INSERT("grba")
	INSERT("hypot")
	INSERT("ifte")
	INSERT("ii")
	INSERT("infra")
	INSERT("infrst")
	INSERT("make_generator")
	INSERT("mod")
	INSERT("neg")
	INSERT("not")
	INSERT("nulco")
	INSERT("null")
	INSERT("nullary")
	INSERT("of")
	INSERT("pam")
	INSERT("pm")
	INSERT("popd")
	INSERT("popdd")
	INSERT("popop")
	INSERT("popopop")
	INSERT("popopd")
	INSERT("popopdd")
	INSERT("product")
	INSERT("quoted")
	INSERT("range")
	INSERT("range_to_zero")
	INSERT("reco")
	INSERT("rest")
	INSERT("reverse")
	INSERT("roll>")
	INSERT("roll<")
	INSERT("rollup")
	INSERT("rolldown")
	INSERT("rrest")
	INSERT("run")
	INSERT("second")
	INSERT("shift")
	INSERT("shunt")
	INSERT("size")
	INSERT("small")
	INSERT("spiral_next")
	INSERT("split_at")
	INSERT("split_list")
	INSERT("sqr")
	INSERT("stackd")
	INSERT("step_zero")
	INSERT("stuncons")
	INSERT("sum")
	INSERT("swapd")
	INSERT("swons")
	INSERT("swoncat")
	INSERT("tailrec")
	INSERT("take")
	INSERT("ternary")
	INSERT("third")
	INSERT("tuck")
	INSERT("unary")
	INSERT("uncons")
	INSERT("unit")
	INSERT("unquoted")
	INSERT("unstack")
	INSERT("unswons")
	INSERT("while")
	INSERT("x")
	INSERT("step")
	INSERT("_step0")
	INSERT("_step1")
	INSERT("_stept")
	INSERT("times")
	INSERT("_times0")
	INSERT("_times1")
	INSERT("_timest")
	INSERT("map")
	INSERT("_map?")
	INSERT("_mape")
	INSERT("_map0")
	INSERT("_map1")
	INSERT("_map2")
	INSERT("_\\/_")
	INSERT("/\\")
	INSERT("\\/")
	/*
	raxShow(rt);
	*/

	while (1) {
		line = linenoise("eh? ");
		if (NULL == line) {
			printf("\n");
			break;
		}
		linenoiseHistoryAdd(line);
	}
}
