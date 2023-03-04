#include <uvm/syscalls.h>
#include <string.h>





//
// Simple cons list.
//

#define HEAP_SIZE 1024
u32 heads[HEAP_SIZE];
u32 tails[HEAP_SIZE];
u32 free = 1;  // cell 0 is reserved so that 0 can be the empty list.
#define TYPE_OF(pointer) (pointer >> 30)
#define VALUE_OF(pointer) (pointer & 0x3fffffff)
#define JOY_VALUE(type, value) ((type & 3) << 30) | (value & 0x3fffffff)
u32 empty_list = 0;
u8 joyList = 0;
u8 joyInt = 1;
u8 joySymbol = 2;
u8 joyBool = 3;
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


/*u32 cons(u32 head, u32 tail) { return tail; }*/







// And now for a hash table.
// https://benhoyt.com/writings/hash-table-in-c/#hash-tables
// https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function
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
		print_str("interning ");print_str(symbol);print_endl();
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
		print_str("interning ");print_str(symbol);print_endl();
		hash_table[index] = symbol;
	}
	return JOY_VALUE(joySymbol, VALUE_OF(hash));
}
char*
ht_lookup(u32 hash)
{
	// Note that hash will be truncated to N (N=30 as it happens) bits
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
	/*error = UNKNOWN_WORD_ERROR;*/
	return 0;
}







//
// Simple string storage heap.
//
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
	print_str("allocating ");print_str(string_heap + new_string);print_endl();
	return string_heap + new_string;
	
}


u32
push_symbol(char *symbol, u32 stack)
{
	return cons(JOY_VALUE(joySymbol, ht_insert(symbol)), stack);
}



char* LEFT_BRACKET_symbol = "[";
char* RIGHT_BRACKET_symbol = "]";
u32 LEFT_BRACKET;
u32 RIGHT_BRACKET;


u32
tokenize0(char *str, u32 str_length, u32 index, u32 acc)
{
	if (index >= str_length) {
		print_i64(index);print_str(" : ");print_str("END tokenize");print_endl();
		print_i64(acc);print_str("<");print_endl();
		return acc;
	}
	print_i64(index);print_str(" : ");print_str(str + index);print_endl();
	char ch = str[index];
	if ('[' == ch) {
		acc = cons(LEFT_BRACKET, tokenize0(str, str_length, index + 1, acc));
		print_i64(acc);print_str("<[");print_endl();
		return acc;
	}
	if (']' == ch) {
		acc = cons(RIGHT_BRACKET, tokenize0(str, str_length, index + 1, acc));
		print_i64(acc);print_str("<]");print_endl();
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

	// TODO: Convert bools and ints here?
	// Use ht_insert to avoid multiple allocations of the same string!
	char *token = allocate_string(str, index, i - index);
	if (!token)
		return 0;  // OOM
	return push_symbol(token, tokenize0(str, str_length, i, acc));
	
}


u32
tokenize(char *str)
{
	return tokenize0(str, strlen(str), 0, empty_list);
}

u32 stack[1000];
u32 stack_top = 0;

u32
text_to_expression(char *str)
{
	u32 frame = empty_list;
	u32 tokens = tokenize(str);
	print_str("tokens: ");
	print_joy_list(tokens);
	print_endl();
	return tokens;
	while (tokens) {
		u32 tok = head(tokens);
		tokens = tail(tokens);
		if (LEFT_BRACKET == tok) {
			print_str("left bracket");print_endl();
			stack[stack_top] = frame;
			++stack_top;
			frame = empty_list;
			continue;
		}
		if (RIGHT_BRACKET == tok) {
			print_str("right bracket");print_endl();
			tok = frame;  // as list
			frame = stack[stack_top];
			--stack_top;
		}
		frame = cons(tok, frame);
	}
	return frame;  // as list;
}


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
		/*if (error != NO_ERROR)*/
		/*	return;*/
		print_str(str);
	}
}

void
print_joy_list(u32 list)
{
	while (list) {
		print_joy_value(head(list));
		list = tail(list);
		if (list) {
			print_str(" ");
		}
	}
}

void
main()
{
	memset(string_heap, 0, sizeof(string_heap));
	LEFT_BRACKET = JOY_VALUE(joySymbol, ht_insert(LEFT_BRACKET_symbol));
	RIGHT_BRACKET = JOY_VALUE(joySymbol, ht_insert(RIGHT_BRACKET_symbol));

	//char *buffer = " 1[2[ 3  cats ]4] cats dogs bunnies";
	char *buffer = " 1[2]3 bunnies";
	/*print_str(allocate_string(buffer, 4, 4)); print_endl();*/
	/*print_str(allocate_string(buffer, 2, 4)); print_endl();*/
	/*print_str(allocate_string(buffer, 7, 5)); print_endl();*/
	u32 jv = text_to_expression(" 1 [2 3] 3[4]5");
	if (LEFT_BRACKET == jv) {
		print_str("boo");
	} else {
		//jv = JOY_VALUE(joyList, jv);
		print_joy_list(jv);
	}
	print_endl();
}
