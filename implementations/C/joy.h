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
	joySymbol,
	joyTrue,
	joyFalse,
	joyInt,
	joyList
};

typedef struct list_node* JoyList;
typedef JoyList* JoyListPtr;

typedef struct {
	enum JoyTypeType kind;
	union {
		int boolean;  /* TODO: Remoce this field, we don't use it. */
		mpz_t i;
		JoyList el;
		char *symbol;
	} value;
} JoyType;


struct list_node {
	JoyType head;  /* Should this be a pointer? */
	JoyList tail;
};

#define EMPTY_LIST (JoyList)NULL

#define newJoyList GC_malloc(sizeof(struct list_node))


typedef void (*JoyFunc)(struct list_node**, struct list_node**);


void add(JoyListPtr stack, JoyListPtr expression);
void branch(JoyListPtr stack, JoyListPtr expression);
void clear(JoyListPtr stack, JoyListPtr expression);
void div_joyfunc(JoyListPtr stack, JoyListPtr expression);
void eq(JoyListPtr stack, JoyListPtr expression);
void ge(JoyListPtr stack, JoyListPtr expression);
void gt(JoyListPtr stack, JoyListPtr expression);
void le(JoyListPtr stack, JoyListPtr expression);
void lt(JoyListPtr stack, JoyListPtr expression);
void mod(JoyListPtr stack, JoyListPtr expression);
void mul(JoyListPtr stack, JoyListPtr expression);
void neq(JoyListPtr stack, JoyListPtr expression);
void sub(JoyListPtr stack, JoyListPtr expression);
void truthy(JoyListPtr stack, JoyListPtr expression);


struct dict_entry  {
	char *name;
	JoyFunc func;
};

const struct dict_entry *
in_word_set (register const char *str, register size_t len);
