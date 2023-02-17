#include <gc.h>
#include "joy.h"


typedef struct vlist {
	struct vlist *base;
	int offset, size, last_used;
	JoyTypePtr *data;
} VList;

typedef struct {
	VList *base;
	int offset;
} VListNode;


VListNode *
vlist_cons(JoyTypePtr thing, VListNode *vlnode)
{
	VListNode *result = GC_malloc(sizeof(VListNode));
	if (!vlnode) {
		result->base = GC_malloc(sizeof(VList));
		result->offset = 0;
		result->base->base = NULL;
		result->base->offset = 0;
		result->base->size = 1;
		result->base->last_used = 1;
		result->base->data[0] = thing;
		return result;
	}
	if (vlnode->offset == vlnode->base->last_used - 1 && vlnode->base->last_used < vlnode->base->size) {
		result->base = vlnode->base;
		result->offset = vlnode->offset + 1;
		vlnode->base->data[result->offset] = thing;
		vlnode->base->last_used += 1;
		return result;
	}
	result->base = GC_malloc(sizeof(VList));
	result->offset = 0;
	result->base->base = vlnode->base;
	result->base->offset = 0;
	result->base->size = vlnode->base->size << 1;
	result->base->last_used = 1;
	result->base->data = GC_malloc(result->base->size * sizeof(JoyTypePtr));
	result->base->data[0] = thing;
	return result;
}


