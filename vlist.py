'''
An exploration of Phil Bagwell's VList.

A VList is modeled as a 5-tuple:
    base: VList
    offset: int (indexing base VList data)
    size: int
    last_used: to make a mutable int it's an int in a list
               It's a count here rather than an offset!
    data: a list of length length

a Pointer to a VList is a pair of (VList and offset).

'''

def new_VList(base=(), offset=0, size=1):
    assert size >= 1
    return (base, offset, size, [0], [None] * length)


# In Fig 2 a list has been created with the integers (8,7,6,5,4,3)

o1 = ((), 0, 1, [1], [3])
o2 = (o1, 0, 2, [2], [4, 5])
o3 = (o2, 1, 4, [3], [6, 7, 8, None])

p0 = (o3, 2)  # points to 8
p1 = (o3, 0)  # points to 6

# cons(9, p1) = o4

o4 = (o3, 0, 1, [1], [9])
p2 = (o4, 0)  # points to 9


def cons(thing, vlist_ptr):
    if not vlist_ptr:
        return ((), 0, 1, [1], [thing]), 0

    (base, offset, size, last_used_list, data), pointer_offset = vlist_ptr
    [last_used] = last_used_list

    '''

    During the consing of (9) the pointer offset
    is compared with the last used offset, LastUsed. If it is the same and less than
    the block size then it is simply incremented, the new entry made and LastUsed
    updated.

    '''
    if pointer_offset == last_used - 1 and last_used < size:
        pointer_offset += 1
        data[pointer_offset] = thing
        last_used_list[0] = last_used + 1
        return vlist_ptr[0], pointer_offset
    '''

    If on the other-hand the pointer offset is less than the LastUsed a cons is being applied
    to the tail of a longer list, as is the case with the (9). In this case a new list block
    must be allocated and its Base-Offset pointer set to the tail contained in the original
    list. The offset part being set to the point in tail that must be extended. The new
    entry can now be made and additional elements added.

    '''

    # Is this where we increase the size x 2?
    size <<= 1 ; l = [None] * size ; l[0] = thing
    return (vlist_ptr[0], pointer_offset, size, [1], l), 0

    #return (vlist_ptr[0], pointer_offset, 1, [1], [thing]), 0


#p3 = cons(10, p0)


def head(vlist_ptr):
    if not vlist_ptr:
        raise ValueError("Empty list has no head!")
    vlist, offset = vlist_ptr
    return vlist[-1][offset]


def tail(vlist_ptr):
    if not vlist_ptr:
        raise ValueError("Empty list has no tail!")
    vlist, offset = vlist_ptr
    offset -= 1
    return (vlist[:2] if vlist[0] else ()) if offset < 0 else (vlist, offset)


def iter_vlist(vlist_ptr):
    while vlist_ptr:
        yield head(vlist_ptr)
        vlist_ptr = tail(vlist_ptr)


##for i, p in enumerate((p0, p1, p2, p3)):
##    print(f'p{i}')
##    print(' '.join(map(str, iter_vlist(p))))
##    print()


p = ()
p = cons(3, p) ; print(p)
p = cons(4, p) ; print(p)
p = cons(5, p) ; print(p)
p = cons(6, p) ; print(p)
p = cons(7, p) ; print(p)
p = cons(8, p) ; print(p)
print(' '.join(map(str, iter_vlist(p))))
assert p[0] == o3


# There is no such thing as a vlist_ptr with a null () vlist.  That's an invariant.
