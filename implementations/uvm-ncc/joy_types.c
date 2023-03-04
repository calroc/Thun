#include <uvm/syscalls.h>
// In the Thun dialect of Joy we have four types of values:
// Integers, Booleans, Symbols, and Lists.
// We don't have Unions, Enums, or Typedefs.
//
// So how do we represent Joy types?
//
// In SICP they use a pair of arrays of pointers, one for heads and one
// for tails.

#define HEAP_SIZE 1024

u32 heads[HEAP_SIZE];
u32 tails[HEAP_SIZE];

// > A pointer to a pair is an index into the two vectors.

u32 free = 0;

// > We also need a representation for objects other than pairs (such as
// numbers and symbols) and a way to distinguish one kind of data from
// another. There are many methods of accomplishing this, but they all
// reduce to using typed pointers, that is, to extending the notion of
// ``pointer'' to include information on data type. 

#define TYPE_OF(pointer) (pointer >> 30)
#define VALUE_OF(pointer) (pointer & 0x3fffffff)
#define JOY_VALUE(type, value) ((type & 3) << 30) | (value & 0x3fffffff)

u8 joyInt = 0;
u8 joyList = 1;
u8 joySymbol = 2;
u8 joyBool = 3;

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


// No setjmp/longjmp, so let's have a global error value and check it after ops.
u64 error;

#define NO_ERROR 0
#define UNKNOWN_WORD_ERROR 1
#define MISSING_CLOSING_BRACKET 2

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
	error = UNKNOWN_WORD_ERROR;
	return 0;
}

u32
push_symbol(char *symbol, u32 stack)
{
	return cons(JOY_VALUE(joySymbol, ht_insert(symbol)), stack);
}


void
main()
{
	u32 joy_true = JOY_VALUE(joyBool, 1);
	u32 joy_false = JOY_VALUE(joyBool, 0);

	memset(hash_table, 0, sizeof(hash_table));
	error = NO_ERROR;


	u32 stack = empty_list;

	stack = cons(23, stack);
	stack = cons(joy_true, stack);
	stack = cons(42, stack);

	stack = push_symbol("cats", stack);

	u32 el = empty_list;

	el = cons(48, el);
	el = cons(el, el);
	stack = cons(el, stack);

	stack = cons(joy_false, stack);
	stack = cons(273, stack);

	print_joy_list(stack);
	print_endl();
}



