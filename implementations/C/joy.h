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
		mpz_t i;
		JoyList el;
		char *symbol;
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


void add(JoyListPtr stack, JoyListPtr expression);
void branch(JoyListPtr stack, JoyListPtr expression);
void clear(JoyListPtr stack, JoyListPtr expression);
void cmp_joyfunc(JoyListPtr stack, JoyListPtr expression);
void cons(JoyListPtr stack, JoyListPtr expression);
void dip(JoyListPtr stack, JoyListPtr expression);
void dup(JoyListPtr stack, JoyListPtr expression);
void i_joyfunc(JoyListPtr stack, JoyListPtr expression);
void pop(JoyListPtr stack, JoyListPtr expression);
void mul(JoyListPtr stack, JoyListPtr expression);
void sub(JoyListPtr stack, JoyListPtr expression);
void tdiv_q(JoyListPtr stack, JoyListPtr expression);
void tdiv_r(JoyListPtr stack, JoyListPtr expression);
void truthy(JoyListPtr stack, JoyListPtr expression);

void def_abs(JoyListPtr stack, JoyListPtr expression);
void def_anamorphism(JoyListPtr stack, JoyListPtr expression);
void def_app1(JoyListPtr stack, JoyListPtr expression);
void def_app2(JoyListPtr stack, JoyListPtr expression);
void def_app3(JoyListPtr stack, JoyListPtr expression);
void def_appN(JoyListPtr stack, JoyListPtr expression);
void def_at(JoyListPtr stack, JoyListPtr expression);
void def_average(JoyListPtr stack, JoyListPtr expression);
void def_b(JoyListPtr stack, JoyListPtr expression);
void def_binary(JoyListPtr stack, JoyListPtr expression);
void def_ccccons(JoyListPtr stack, JoyListPtr expression);
void def_ccons(JoyListPtr stack, JoyListPtr expression);
void def_clear(JoyListPtr stack, JoyListPtr expression);
void def_cleave(JoyListPtr stack, JoyListPtr expression);
void def_clop(JoyListPtr stack, JoyListPtr expression);
void def_cmp(JoyListPtr stack, JoyListPtr expression);
void def_codi(JoyListPtr stack, JoyListPtr expression);
void def_codireco(JoyListPtr stack, JoyListPtr expression);
void def_dinfrirst(JoyListPtr stack, JoyListPtr expression);
void def_dipd(JoyListPtr stack, JoyListPtr expression);
void def_disenstacken(JoyListPtr stack, JoyListPtr expression);


struct dict_entry  {
	char *name;
	JoyFunc func;
};

const struct dict_entry *
in_word_set (register const char *str, register size_t len);
