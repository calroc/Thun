// In the Thun dialect of Joy we have four types of values:
// Integers, Booleans, Symbols, and Lists.
// We don't have Unions, Enums, or Typedefs.
// So how do we represent Joy types?
// In SICP they use a pair of arrays of pointers, one for heads and one
// for tails.


// > A pointer to a pair is an index into the two vectors.
#define HEAP_SIZE 1024

u32 heads[HEAP_SIZE];
u32 tails[HEAP_SIZE];
u32 free = 0;

// > We also need a representation for objects other than pairs (such as
// numbers and symbols) and a way to distinguish one kind of data from
// another. There are many methods of accomplishing this, but they all
// reduce to using typed pointers, that is, to extending the notion of
// ``pointer'' to include information on data type. 

#define TYPE_OF(pointer) (pointer >> 30)
#define VALUE_OF(pointer) (pointer & 0x3fffffff)
#define MAKE_POINTER(type, value) ((type & 3) << 30) | (value & 0x3fffffff)
#define joyInt (0)
#define joyList (1)
#define joySymbol (2)
#define joyBool (3)


u32 empty_list = 0xffffffff;


u32
cons(u32 head, u32 tail)
{
	if (free >= HEAP_SIZE) {
		//panic
		return;
	}
	heads[free] = head;
	tails[free] = tail;
	u32 cell = MAKE_POINTER(joyList, free);
	++free;
	return cell;
}

u32
head(u32 list)
{
	return heads[VALUE_OF(list)];
}

u32
tail(u32 list)
{
	return tails[VALUE_OF(list)];
}



void
main()
{
	u32 stack = empty_list;

	stack = cons(23, stack);
	stack = cons(42, stack);
	stack = cons(273, stack);

	print_i64(head(stack));print_str(",");
	stack = tail(stack);
	print_i64(head(stack));print_str(",");
	stack = tail(stack);
	print_i64(head(stack));print_str(",");
	stack = tail(stack);
	print_i64(head(stack));print_str(",");
	print_endl();
}



