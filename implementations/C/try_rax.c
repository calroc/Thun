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


void
completion(const char *buf, linenoiseCompletions *lc)
{
	char *prefix;
	int n = index_of_last_symbol_char(buf);
	int buffer_length = strlen(buf);
	if (n) {
		prefix = malloc(1024);  /* Just assume 1k is enough for now... TODO: fix! */
		if (prefix == NULL) return;
		memcpy(prefix, buf, n);
		buf += n;
		buffer_length -= n;
	}
	raxIterator iter;
	raxStart(&iter, rt);
	raxSeek(&iter, ">=", (unsigned char*)buf, buffer_length);
	while(raxNext(&iter)) {
		if (strncmp((const char *)iter.key, buf, buffer_length))
			break;
		if (n) {
			prefix[n] = 0;
			strlcat(prefix + n, (const char *)iter.key, 1024 - n);
			linenoiseAddCompletion(lc, (const char *)prefix);
		} else {
			linenoiseAddCompletion(lc, (const char *)iter.key);
		}
	}
	raxStop(&iter);
	if (n) {
		free(prefix);
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
