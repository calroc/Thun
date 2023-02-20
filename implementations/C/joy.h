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
		char *symbol;
		mpz_t i;
		JoyList el;
	} value;
} JoyType;

typedef JoyType* JoyTypePtr;

struct list_node {
	JoyTypePtr head;
	JoyList tail;
};

#define EMPTY_LIST (JoyList)NULL

#define newJoyList GC_malloc(sizeof(struct list_node))
#define newJoyType GC_malloc(sizeof(JoyType))


typedef void (*JoyFunc)(JoyListPtr, JoyListPtr);


JoyList text_to_expression(char *text);
void push_quote_onto_expression(JoyList el, JoyListPtr expression);
void init_defs(void);


void add(JoyListPtr stack, JoyListPtr expression);
void branch(JoyListPtr stack, JoyListPtr expression);
void clear(JoyListPtr stack, JoyListPtr expression);
void cmp_joyfunc(JoyListPtr stack, JoyListPtr expression);
void cons(JoyListPtr stack, JoyListPtr expression);
void concat(JoyListPtr stack, JoyListPtr expression);
void dip(JoyListPtr stack, JoyListPtr expression);
void dup(JoyListPtr stack, JoyListPtr expression);
void first(JoyListPtr stack, JoyListPtr expression);
void i_joyfunc(JoyListPtr stack, JoyListPtr expression);
void inscribe(JoyListPtr stack, JoyListPtr expression);
void loop(JoyListPtr stack, JoyListPtr expression);
void lshift(JoyListPtr stack, JoyListPtr expression);
void pop(JoyListPtr stack, JoyListPtr expression);
void rest(JoyListPtr stack, JoyListPtr expression);
void rshift(JoyListPtr stack, JoyListPtr expression);
void stack(JoyListPtr stack, JoyListPtr expression);
void swaack(JoyListPtr stack, JoyListPtr expression);
void swap(JoyListPtr stack, JoyListPtr expression);
void mul(JoyListPtr stack, JoyListPtr expression);
void sub(JoyListPtr stack, JoyListPtr expression);
void fdiv_q(JoyListPtr stack, JoyListPtr expression);
void fdiv_r(JoyListPtr stack, JoyListPtr expression);
void truthy(JoyListPtr stack, JoyListPtr expression);
void fn(JoyListPtr stack, JoyListPtr expression);


struct dict_entry  {
	char *name;
	JoyFunc func;
};

const struct dict_entry *
in_word_set (register const char *str, register size_t len);
