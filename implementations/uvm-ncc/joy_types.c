//    Copyright © 2023 Simon Forman
//
//    This file is part of Thun
//
//    Thun is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    Thun is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with Thun.  If not see <http://www.gnu.org/licenses/>.
//
#include <uvm/syscalls.h>
#include <string.h>

/*
 ██████╗ ██████╗ ███╗   ██╗███████╗    ██╗  ██╗███████╗ █████╗ ██████╗ 
██╔════╝██╔═══██╗████╗  ██║██╔════╝    ██║  ██║██╔════╝██╔══██╗██╔══██╗
██║     ██║   ██║██╔██╗ ██║███████╗    ███████║█████╗  ███████║██████╔╝
██║     ██║   ██║██║╚██╗██║╚════██║    ██╔══██║██╔══╝  ██╔══██║██╔═══╝ 
╚██████╗╚██████╔╝██║ ╚████║███████║    ██║  ██║███████╗██║  ██║██║     
 ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝    ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝     
Cons Heap

We don't have Unions, Enums, or Typedefs.  So how do we represent Joy types?
In SICP they use a pair of arrays of pointers, one for heads and one
for tails.

> A pointer to a pair is an index into the two vectors.

*/

#define HEAP_SIZE 1024

u32 heads[HEAP_SIZE];
u32 tails[HEAP_SIZE];


// cell 0 is reserved so that 0 can be the empty list.
u32 free = 1;

// > We also need a representation for objects other than pairs (such as
// > numbers and symbols) and a way to distinguish one kind of data from
// > another. There are many methods of accomplishing this, but they all
// > reduce to using typed pointers, that is, to extending the notion of
// > ``pointer'' to include information on data type. 

// Let's use u32 with the two MSB's for the type tag.

#define TYPE_OF(pointer) (pointer >> 30)
#define VALUE_OF(pointer) (pointer & 0x3fffffff)
#define JOY_VALUE(type, value) ((type & 3) << 30) | (value & 0x3fffffff)

/*
This means that our ints are restricted to 30 bits for now, until
I implement bignums.


In the Thun dialect of Joy we have four types of values:

Integers, Booleans, Symbols, and Lists.
*/
u8 joyList = 0;
u8 joyInt = 1;
u8 joySymbol = 2;
u8 joyBool = 3;

// Because the type tag for lists is 0 the empty list is just 0;
u32 empty_list = 0;

u32
cons(u32 head, u32 tail)
{
	if (free >= HEAP_SIZE)
		return -1;

	heads[free] = head;
	tails[free] = tail;
	u32 cell = JOY_VALUE(joyList, free);
	++free;
	return cell;
}

u32 head(u32 list) { return heads[VALUE_OF(list)]; }
u32 tail(u32 list) { return tails[VALUE_OF(list)]; }



/*
███████╗████████╗██████╗ ██╗███╗   ██╗ ██████╗ 
██╔════╝╚══██╔══╝██╔══██╗██║████╗  ██║██╔════╝ 
███████╗   ██║   ██████╔╝██║██╔██╗ ██║██║  ███╗
╚════██║   ██║   ██╔══██╗██║██║╚██╗██║██║   ██║
███████║   ██║   ██║  ██║██║██║ ╚████║╚██████╔╝
╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝ ╚═════╝ 
                                               
██╗  ██╗███████╗ █████╗ ██████╗                
██║  ██║██╔════╝██╔══██╗██╔══██╗               
███████║█████╗  ███████║██████╔╝               
██╔══██║██╔══╝  ██╔══██║██╔═══╝                
██║  ██║███████╗██║  ██║██║                    
╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝
Simple string storage heap.

We need a place to keep symbol strings.

*/

#define STRING_HEAP_SIZE 100000

char string_heap[STRING_HEAP_SIZE];
u32 string_heap_top = 0;

char*
allocate_string(char *buffer, u32 offset, u32 length)
{
	u64 end = string_heap_top + length + 1;
	if (end >= STRING_HEAP_SIZE)
		return 0;
	memcpy(string_heap + string_heap_top, buffer + offset, length);
	string_heap[end] = '\0';
	u32 new_string = string_heap_top;
	string_heap_top = (u32)end + 1;
	//print_str("allocating ");print_str(string_heap + new_string);print_endl();
	return string_heap + new_string;
}


/******************************************************************************/

// No setjmp/longjmp, so let's have a global error value and check it after ops.
u64 error;

#define NO_ERROR 0
#define UNKNOWN_WORD_ERROR 1
#define MISSING_CLOSING_BRACKET 2

/******************************************************************************/


/*
██████╗ ██████╗ ██╗███╗   ██╗████████╗███████╗██████╗
██╔══██╗██╔══██╗██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗
██████╔╝██████╔╝██║██╔██╗ ██║   ██║   █████╗  ██████╔╝
██╔═══╝ ██╔══██╗██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗
██║     ██║  ██║██║██║ ╚████║   ██║   ███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
Printer
*/

void
print_joy_value(u32 jv)
{
	u8 type = TYPE_OF(jv);
	if (type == joyInt) {
		print_i64(VALUE_OF(jv));
	} else if (type == joyBool) {
		print_str(VALUE_OF(jv) ? "true" : "false");
	} else if (type == joyList) {
		print_str("[");
		print_joy_list(jv);
		print_str("]");
	} else if (type == joySymbol) {
		char *str = ht_lookup(VALUE_OF(jv));
		if (error != NO_ERROR)
			return;
		print_str(str);
	}
}

void
print_joy_list(u32 list)
{
	while (list) {
		print_joy_value(head(list));
		if (error != NO_ERROR)
			return;
		list = tail(list);
		if (list) {
			print_str(" ");
		}
	}
}


/*
██╗  ██╗ █████╗ ███████╗██╗  ██╗         
██║  ██║██╔══██╗██╔════╝██║  ██║         
███████║███████║███████╗███████║         
██╔══██║██╔══██║╚════██║██╔══██║         
██║  ██║██║  ██║███████║██║  ██║         
╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝         
                                         
████████╗ █████╗ ██████╗ ██╗     ███████╗
╚══██╔══╝██╔══██╗██╔══██╗██║     ██╔════╝
   ██║   ███████║██████╔╝██║     █████╗  
   ██║   ██╔══██║██╔══██╗██║     ██╔══╝  
   ██║   ██║  ██║██████╔╝███████╗███████╗
   ╚═╝   ╚═╝  ╚═╝╚═════╝ ╚══════╝╚══════╝
And now for a hash table.

This table maps between hashes of symbol strings which are used in the tagged pointers in Joy values
and strings which are stored in the string heap.


FNV hash function.

https://benhoyt.com/writings/hash-table-in-c/#hash-tables
https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function

*/

#define FNV_OFFSET 0xcbf29ce484222325
#define FNV_PRIME 0x100000001b3

u64
hash_key(char* key)
{
    u64 hash = FNV_OFFSET;
    for (char* p = key; *p; ++p) {
        hash = hash ^ (u64)(unsigned char)(*p);
        hash = hash * FNV_PRIME;
    }
    return hash;
}

// Capacity is a power of two (10 for now.)
#define EXPONENT 10
#define CAPACITY 1024
#define HASH_MASK 1023

char* hash_table[CAPACITY];

u32
ht_insert(char *symbol)
{
	u64 hash = hash_key(symbol);
	u32 index = hash % CAPACITY;

	char *candidate = hash_table[index];
	if (!candidate) {
		hash_table[index] = symbol;
		return JOY_VALUE(joySymbol, VALUE_OF(hash));
	}

	// https://en.wikipedia.org/wiki/Double_hashing
	// Rather than use another hash function I'm going to try
	// using the extra bits of the same hash.
	u32 increment = ((VALUE_OF(hash) >> EXPONENT) | 1) % CAPACITY;
	// If I understand correctly, making the increment odd
	// means it will traverse the whole (even-sized) table.
	while (candidate) {
		// Compare pointers then hashes (since we already have
		// one hash I'm guessing that that's cheaper or at least
		// no more expensive than string comparision.)
		if (candidate == symbol || hash == hash_key(candidate))
			break;
		index = (index + increment) % CAPACITY;
		candidate = hash_table[index];
	}
	if (!candidate) {
		hash_table[index] = symbol;
	}
	return JOY_VALUE(joySymbol, VALUE_OF(hash));
}

char*
ht_lookup(u32 hash)
{
	u32 index = hash % CAPACITY;
	char *candidate = hash_table[index];
	// Note that hash will be truncated to N (N=30 as it happens) bits
	// by VALUE_OF().
	u32 increment = ((hash >> EXPONENT) | 1) % CAPACITY;
	while (candidate) {
		if (hash == VALUE_OF(hash_key(candidate)))
			return candidate;
		index = (index + increment) % CAPACITY;
		candidate = hash_table[index];
	}
	error = UNKNOWN_WORD_ERROR;
	return 0;
}

/******************************************************************************/

u32
push_symbol(char *symbol, u32 stack)
{
	return cons(JOY_VALUE(joySymbol, ht_insert(symbol)), stack);
}
u32
push_int(u32 n, u32 stack)
{
	return cons(JOY_VALUE(joyInt, n), stack);
}

/******************************************************************************/

bool
is_integer(char *str, u32 index, u32 length)
{
	for (;length; --length) {
		char ch = *(str + index + length - 1);
		if (!(ch == '0'
			|| ch == '1'
			|| ch == '2'
			|| ch == '3'
			|| ch == '4'
			|| ch == '5'
			|| ch == '6'
			|| ch == '7'
			|| ch == '8'
			|| ch == '9'))
		{
			return 0;
		}
	}
	return 1;
}

u32
convert_integer(char *str, u32 index, u32 length)
{
	u32 result = 0;
	length = length + index;
	for (; index < length; ++index) {
		char ch = *(str + index);
		u8 digit = (u8)ch - (u8)'0';
		result = result * 10 + digit;
	}
	//print_str("converted integer ");print_i64(result);print_endl();
	return JOY_VALUE(joyInt, result);
}

/******************************************************************************/

/*
████████╗ ██████╗ ██╗  ██╗███████╗███╗   ██╗██╗███████╗███████╗██████╗ 
╚══██╔══╝██╔═══██╗██║ ██╔╝██╔════╝████╗  ██║██║╚══███╔╝██╔════╝██╔══██╗
   ██║   ██║   ██║█████╔╝ █████╗  ██╔██╗ ██║██║  ███╔╝ █████╗  ██████╔╝
   ██║   ██║   ██║██╔═██╗ ██╔══╝  ██║╚██╗██║██║ ███╔╝  ██╔══╝  ██╔══██╗
   ██║   ╚██████╔╝██║  ██╗███████╗██║ ╚████║██║███████╗███████╗██║  ██║
   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═══╝╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
Tokenizer

*/

char* LEFT_BRACKET_symbol = "[";
char* RIGHT_BRACKET_symbol = "]";
// Filled in in main().
u32 LEFT_BRACKET;
u32 RIGHT_BRACKET;


u32
tokenate(char *str, u32 index, u32 length)
{
	if (4 == length
		&& *(str + index) == 't'
		&& *(str + index + 1) == 'r'
		&& *(str + index + 2) == 'u'
		&& *(str + index + 3) == 'e'
	) {
		//print_str("tokenate true");print_endl();
		return JOY_VALUE(joyBool, 1);
	}
	if (5 == length
		&& *(str + index) == 'f'
		&& *(str + index + 1) == 'a'
		&& *(str + index + 2) == 'l'
		&& *(str + index + 3) == 's'
		&& *(str + index + 4) == 'e'
	) {
		//print_str("tokenate false");print_endl();
		return JOY_VALUE(joyBool, 0);
	}
	if (is_integer(str, index, length)) {
		//print_str("tokenate integer");print_endl();
		return convert_integer(str, index, length);
	}
	// TODO: Use ht_insert to avoid multiple allocations of the same string!
	char *token = allocate_string(str, index, length);
	if (!token)
		return 0;  // OOM
	return JOY_VALUE(joySymbol, ht_insert(token));
}


u32
tokenize0(char *str, u32 str_length, u32 index, u32 acc)
{
	if (index >= str_length) {
		//print_i64(index);print_str(" : ");print_str("END tokenize");print_endl();
		//print_i64(acc);print_str("<");print_endl();
		return acc;
	}
	//print_i64(index);print_str(" : ");print_str(str + index);print_endl();
	char ch = str[index];
	if ('[' == ch) {
		acc = cons(LEFT_BRACKET, tokenize0(str, str_length, index + 1, acc));
		//print_i64(acc);print_str("<[");print_endl();
		return acc;
	}
	if (']' == ch) {
		acc = cons(RIGHT_BRACKET, tokenize0(str, str_length, index + 1, acc));
		//print_i64(acc);print_str("<]");print_endl();
		return acc;
	}
	if (' ' == ch) {
		return tokenize0(str, str_length, index + 1, acc);
	}
	u32 i = index + 1;
	for (; i < str_length; ++i) {
		if (str[i] == '[' || str[i] == ']' || str[i] == ' ') {
			break;
		}
	}
	// i == str_length OR str[i] is a delimiter char.
	return cons(tokenate(str, index, i - index), tokenize0(str, str_length, i, acc));
	
}


u32
tokenize(char *str)
{
	return tokenize0(str, strlen(str), 0, empty_list);
}



/*
██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
Parser

*/

u32
_reverse_list_in_place(u32 el, u32 end)
{
	u32 t = tail(el);
	tails[el] = end;
	return t ? _reverse_list_in_place(t, el) : el;
}

u32
reverse_list_in_place(u32 el)
{
	return el ? _reverse_list_in_place(el, empty_list) : el;
}

u32 t2e_stack[1000];
u32 t2e_stack_top = 0;

u32
text_to_expression(char *str)
{
	u32 frame = empty_list;
	u32 tokens = tokenize(str);
	//print_str("tokens: "); print_joy_list(tokens); print_endl();
	//return tokens;
	while (tokens) {
		u32 tok = head(tokens);
		tokens = tail(tokens);
		if (LEFT_BRACKET == tok) {
			//print_str("left bracket");print_endl();
			t2e_stack[t2e_stack_top] = frame;
			++t2e_stack_top;
			frame = empty_list;
			continue;
		}
		if (RIGHT_BRACKET == tok) {
			//print_str("right bracket");print_endl();
			tok = reverse_list_in_place(frame);
			//print_str("new list: "); print_joy_list(tok); print_endl();
			--t2e_stack_top;
			frame = t2e_stack[t2e_stack_top];
		}
		frame = cons(tok, frame);
		//print_str("t2e frame: "); print_joy_list(frame); print_endl();
	}
	return reverse_list_in_place(frame);
}


void
main()
{
	LEFT_BRACKET = JOY_VALUE(joySymbol, ht_insert(LEFT_BRACKET_symbol));
	RIGHT_BRACKET = JOY_VALUE(joySymbol, ht_insert(RIGHT_BRACKET_symbol));
	// TODO: these should be global.
	u32 joy_true = JOY_VALUE(joyBool, 1);
	u32 joy_false = JOY_VALUE(joyBool, 0);

	memset(hash_table, 0, sizeof(hash_table));
	memset(string_heap, 0, sizeof(string_heap));
	memset(t2e_stack, 0, sizeof(t2e_stack));
	error = NO_ERROR;

	/*
	u32 stack = empty_list;
	stack = push_int(23, stack);
	stack = cons(joy_true, stack);
	stack = push_int(42, stack);
	stack = push_symbol("cats", stack);
	u32 el = empty_list;
	el = push_int(48, el);
	el = cons(el, el);
	stack = cons(el, stack);
	stack = cons(joy_false, stack);
	stack = push_int(273, stack);
	print_joy_list(stack);
	print_endl();
	*/

	print_joy_list(text_to_expression(" 1[2[true 3][[]]bob]false[]bob 3[4]5"));
	print_endl();
}


