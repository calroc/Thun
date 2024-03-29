/*
Copyright © 2023 Simon Forman

This file is part of Thun

Thun is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Thun is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Thun.  If not see <http://www.gnu.org/licenses/>.

*/
#include <gmp.h>


enum JoyTypeType {
	joyInt,
	joyList,
	joySymbol,
	joyTrue, joyFalse
};

typedef struct list_node* JoyList;
typedef JoyList* JoyListPtr;

typedef struct {
	enum JoyTypeType kind;
	union {
		char *symbol;
		mpz_t i;
		JoyList el;
	} value;
} JoyType;

typedef JoyType* JoyTypePtr;

typedef struct list_node {
	JoyTypePtr head;
	JoyList tail;
} JoyListNode;

#define EMPTY_LIST (JoyList)NULL

#define newJoyList GC_malloc(sizeof(JoyListNode))
#define newJoyType GC_malloc(sizeof(JoyType))


typedef void JoyFunc(JoyListPtr, JoyListPtr);

JoyFunc add, branch, clear, cmp_joyfunc, cons, concat, dip, dup, first,
i_joyfunc, inscribe, loop, lshift, pop, rest, rshift, stack, swaack,
swap, mul, sub, fdiv_q, fdiv_r, truthy, fn;


struct dict_entry {
	char *name;
	JoyFunc *func;
};

const struct dict_entry *
in_word_set (register const char *str, register size_t len);

JoyList text_to_expression(char *text);
void push_quote_onto_expression(JoyList el, JoyListPtr expression);
void init_defs(void);
