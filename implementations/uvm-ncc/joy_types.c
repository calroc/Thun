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
		print_str(ht_lookup(VALUE_OF(jv)));
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
#define CAPACITY 1024
#define HASH_MASK 0x3ff

char* hash_table[CAPACITY];

u32
ht_insert(char *symbol)
{
	u32 index = hash_key(symbol) & HASH_MASK;
	// We're not checking for collisions yet.
	hash_table[index] = symbol;
	return index;
}

char*
ht_lookup(u64 hash)
{
	u64 index = hash & HASH_MASK;
	return hash_table[index];
}

void
main()
{
	u32 joy_true = JOY_VALUE(joyBool, 1);
	u32 joy_false = JOY_VALUE(joyBool, 0);

	memset(hash_table, 0, sizeof(hash_table));

	u32 stack = empty_list;

	stack = cons(23, stack);
	stack = cons(joy_true, stack);
	stack = cons(42, stack);

	u32 word = JOY_VALUE(joySymbol, ht_insert("cats"));
	stack = cons(word, stack);

	u32 el = empty_list;

	el = cons(48, el);
	el = cons(el, el);
	stack = cons(el, stack);

	stack = cons(joy_false, stack);
	stack = cons(273, stack);

	print_joy_list(stack);
	print_endl();
}



