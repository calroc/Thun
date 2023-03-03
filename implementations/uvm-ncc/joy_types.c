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

u32 empty_list = 0x3fffffff;

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
		// print_str(symbols[VALUE_OF(jv)]);
	}
}

void
print_joy_list(u32 list)
{
	if (TYPE_OF(list) != joyList) {
		1/0;
	}
	while (list != empty_list) {
		print_joy_value(head(list));
		list = tail(list);
		if (list != empty_list) {
			print_str(" ");
		}
	}
}

void
main()
{
	u32 joy_true = JOY_VALUE(joyBool, 1);
	u32 joy_false = JOY_VALUE(joyBool, 0);

	u32 el = empty_list;
	el = cons(48, el);
	el = cons(el, el);

	u32 stack = empty_list;

	stack = cons(23, stack);
	stack = cons(joy_true, stack);
	stack = cons(42, stack);
	stack = cons(el, stack);
	stack = cons(joy_false, stack);
	stack = cons(273, stack);
	print_joy_list(stack);
	print_endl();
}



