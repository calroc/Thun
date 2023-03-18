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
███████╗██████╗ ██████╗  ██████╗ ██████╗                        
██╔════╝██╔══██╗██╔══██╗██╔═══██╗██╔══██╗                       
█████╗  ██████╔╝██████╔╝██║   ██║██████╔╝                       
██╔══╝  ██╔══██╗██╔══██╗██║   ██║██╔══██╗                       
███████╗██║  ██║██║  ██║╚██████╔╝██║  ██║                       
╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═╝                       
																
██╗  ██╗ █████╗ ███╗   ██╗██████╗ ██╗     ██╗███╗   ██╗ ██████╗ 
██║  ██║██╔══██╗████╗  ██║██╔══██╗██║     ██║████╗  ██║██╔════╝ 
███████║███████║██╔██╗ ██║██║  ██║██║     ██║██╔██╗ ██║██║  ███╗
██╔══██║██╔══██║██║╚██╗██║██║  ██║██║     ██║██║╚██╗██║██║   ██║
██║  ██║██║  ██║██║ ╚████║██████╔╝███████╗██║██║ ╚████║╚██████╔╝
╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝ ╚══════╝╚═╝╚═╝  ╚═══╝ ╚═════╝ 

No setjmp/longjmp, so let's have a global error value and check it after ops.
*/

u64 error = 0;

#define NO_ERROR 0
#define UNKNOWN_WORD_ERROR 1
#define MISSING_CLOSING_BRACKET 2
#define EXTRA_CLOSING_BRACKET 3
#define CONS_HEAP_OOM 4
#define STRING_HEAP_OOM 5
#define NOT_ENOUGH_VALUES_ON_STACK 6
#define NOT_A_LIST 7

#define CHECK_ERROR if (error != NO_ERROR) return 0;


/*
char *error_messages[3] = {
	"",
	"Unknown word",
	"Missing closing bracket"
};
*/

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
	if (free >= HEAP_SIZE) {
		error = CONS_HEAP_OOM;
		return 0;
	}
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
	// I have already spent more time thinking about avoiding overflow
	// from the line above, going into the comparison below and goofing
	// up for "pathological" inputs (string_heap_top + length == MAX_INT_64
	// or whatever it's called.  In practice, this should never happen,
	// and we spell that with assert, eh?  I'm not going to add it now,
	// but it would be something like assert(string_heap_top + length < MAX_INT_64)
	if (end >= STRING_HEAP_SIZE) {
		error = STRING_HEAP_OOM;
		return 0;
	}
	memcpy(string_heap + string_heap_top, buffer + offset, length);
	string_heap[end] = '\0';
	u32 new_string = string_heap_top;
	string_heap_top = (u32)end + 1;
	//print_str("allocating ");print_str(string_heap + new_string);print_endl();
	return string_heap + new_string;
}


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

This table maps between hashes of symbol strings which are used in the
tagged pointers in Joy values and strings which are stored in the string
heap.

TODO: bool ht_has(char *str, u32 index, u32 length) to see if a fragment
	  of a string buffer is a symbol in the hash table.

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

u64
hash_fragment(char *str, u32 index, u32 length)
{
	u64 hash = FNV_OFFSET;
	for (char* p = (str + index); length; --length, ++p) {
		hash = hash ^ (u64)(unsigned char)(*p);
		hash = hash * FNV_PRIME;
	}
	return hash;
}

// Capacity is a power of two (10 for now.)
#define EXPONENT 10
#define CAPACITY 1024
#define HASH_MASK 1023

// Note that there's no checking for filling the table and expanding.
// For now, I'm just going to use a "large enough" table and hope
// for the best.  (We have thirty bits to work with so the obvious
// thing to do is make the exponent fifteen, half for the hash key
// and half for the increment.)

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
	// Note that hash must be truncated to N (N=30 as it happens) bits
	// by VALUE_OF().
	u32 index = hash % CAPACITY;
	char *candidate = hash_table[index];
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


u32
ht_has(char *str, u32 index, u32 length)
{
	u32 hash = VALUE_OF(hash_fragment(str, index, length));
	ht_lookup(hash);
	if (UNKNOWN_WORD_ERROR == error) {
		error = NO_ERROR;
		return 0;
	}
	return hash;
}





/******************************************************************************/


u32
pop_list(u32 stack)
{
	if (!stack) {
		error = NOT_ENOUGH_VALUES_ON_STACK;
		return 0;
	}
	u32 list = head(stack);
	if (TYPE_OF(list) != joyList) {
		error = NOT_A_LIST;
		return 0;
	}
	return list;
}


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
██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
Parser

*/


u32
intern(char *str, u32 index, u32 length)
{
	u32 symbol_hash = ht_has(str, index, length);
	if (!symbol_hash) {
		char *token = allocate_string(str, index, length);
		if (error != NO_ERROR) {
			//print_str("a. Error code: ");print_i64(error);print_endl();
			return 0;
		}
		symbol_hash = ht_insert(token);
	}
	return JOY_VALUE(joySymbol, symbol_hash);
}


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
	return intern(str, index, length);
}


int
is_delimiter(char ch)
{
	return ch == '[' || ch == ']' || ch == ' ';
}

// Store in-progress lists.  Here as in the hash table I'm not checking
// for capacity overload or anything like that.  If you think you're going
// to parse more than a five hundred '[' chars then increase the size of
// this array.
u32 t2e_stack[1000];
u32 t2e_stack_top = 0;

#define T2E_PUSH(thing) t2e_stack[t2e_stack_top] = (thing); ++t2e_stack_top; (thing) = empty_list;
#define T2E_POP(thing)  if (!t2e_stack_top) { error = EXTRA_CLOSING_BRACKET; return 0; }; --t2e_stack_top; (thing) = t2e_stack[t2e_stack_top];

u32
text_to_expression(char *str)
{
	u32 index = 0;
	u32 end = empty_list;
	u32 top = empty_list;
	u32 tok = empty_list;
	u64 str_length = strlen(str);  // TODO: rewrite so we don't iterate through the string twice.
	while (index < str_length) {
		char ch = str[index];
		if (' ' == ch) {
			++index;
			continue;
		}
		if ('[' == ch) {  // start new list
			++index;
			T2E_PUSH(end)
			T2E_PUSH(top)
			continue;
		}
		if (']' == ch) {  // finish last new list
			++index;
			tok = top;
			T2E_POP(top)
			T2E_POP(end)
		} else {
			u32 i = index + 1;
			for (; i < str_length && !is_delimiter(str[i]); ++i) {}
			tok = tokenate(str, index, i - index);
			index = i;
		}
		u32 cell = cons(tok, empty_list);
		if (end) tails[end] = cell;
		if (!top) top = cell;
		end = cell;
	}
	if (t2e_stack_top) {
		error = MISSING_CLOSING_BRACKET;
		return empty_list;
	}
	return top;
}

/*
In order to return two "pointers" I'm going to just OR them
into one u64 value.  It might be conceptually cleaner to define
an array of two u32 values, eh?

	u32 joy_state[2];

I'll figure it out later, I just want to get something going for now.
I also don't want to take the time at the moment to figure out if/how
to call function pointers in NCC C, so a chain of if..else is the
ticket.
*/

#define MATCH(name) if (!strcmp(symbol, (name)))

u64
joy_eval(char *symbol, u32 stack, u32 expression)
{
	MATCH("clear") return (u64)expression;
	MATCH("swaack") { stack = swaack(stack); }
	CHECK_ERROR
	//print_str(symbol);print_endl();
	return (u64)stack << 32 | expression;
}


u64
swaack(u32 stack)
{
	u32 list = pop_list(stack);
	CHECK_ERROR
	return cons(tail(stack), list);
}


u32
joy(u32 stack, u32 expression)
{
	u32 term;
	while (expression) {
		term = head(expression);
		expression = tail(expression);
		if (TYPE_OF(term) == joySymbol) {
			char *symbol = ht_lookup(VALUE_OF(term));
			CHECK_ERROR
			u64 new_state = joy_eval(symbol, stack, expression);
			CHECK_ERROR
			stack = new_state >> 32;
			expression = new_state & 0xffffffff;
		}
		else stack = cons(term, stack);
	}
	return stack;
}


void
main()
{
	memset(hash_table, 0, sizeof(hash_table));
	memset(string_heap, 0, sizeof(string_heap));
	memset(t2e_stack, 0, sizeof(t2e_stack));
	error = NO_ERROR;

	// TODO: these should be global.
	u32 joy_true = JOY_VALUE(joyBool, 1);
	u32 joy_false = JOY_VALUE(joyBool, 0);

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

	u32 expression = text_to_expression("1 2 3 [4 5 6] swaack");
	//u32 expression = text_to_expression("1 2 3 clear 4 5 6");
	//u32 expression = text_to_expression(" 1[2[true 3][aa[aa bb] aa bb cc]bob]false[]bob 3[4] ga[]ry");
	print_joy_list(expression);
	print_endl();
	u32 stack = joy(empty_list, expression);
	if (error) {
		print_str("error: ");
		print_i64(error);
		print_endl();
	} else {
		print_joy_list(stack);
		print_endl();
	}
}
