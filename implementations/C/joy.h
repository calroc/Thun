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
		int boolean;
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


void add(JoyList *stack, JoyList *expression);
void branch(JoyList *stack, JoyList *expression);
void clear(JoyList *stack, JoyList *expression);
void div_joyfunc(JoyList *stack, JoyList *expression);
void eq(JoyList *stack, JoyList *expression);
void ge(JoyList *stack, JoyList *expression);
void gt(JoyList *stack, JoyList *expression);
void le(JoyList *stack, JoyList *expression);
void lt(JoyList *stack, JoyList *expression);
void mod(JoyList *stack, JoyList *expression);
void mul(JoyList *stack, JoyList *expression);
void neq(JoyList *stack, JoyList *expression);
void sub(JoyList *stack, JoyList *expression);
void truthy(JoyList *stack, JoyList *expression);


struct dict_entry  { char *name; JoyFunc func; };

const struct dict_entry *
in_word_set (register const char *str, register size_t len);
