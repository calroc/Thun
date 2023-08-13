#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#    Copyright © 2022 Simon Forman
#
#    This file is part of Thun
#
#    Thun is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Thun is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Thun.  If not see <http://www.gnu.org/licenses/>.
#
'''
An exploration of Phil Bagwell's VList.

A VList is modeled as a 5-tuple:
    base: VList
    offset: int, indexing (from 0) the base VList data list.
    size: int, length of the data list.
    last_used: to make a mutable int it's an int in a list
               It's a count here rather than an offset!
               That is, it starts from 1, rather than 0.
    data: a list of length size.

A pointer to a VList is a two-tuple of (VList, offset) where
the offset is an index into the VList's data list.

'''


def cons(thing, vlist_ptr):
    if not vlist_ptr:
        return ((), 0, 1, [1], [thing]), 0

    vlist, pointer_offset = vlist_ptr
    base, _offset, size, last_used_list, data = vlist
    last_used = last_used_list[0]
    # Note that we want to keep that length-one list "last_used_list"
    # around so we can mutate the last_used value of "vlist".

    if pointer_offset == last_used - 1 and last_used < size:
        pointer_offset += 1
        data[pointer_offset] = thing
        last_used_list[0] += 1
        return vlist, pointer_offset

    # Is this where we increase the size x 2?
    size <<= 1
    offset = 0
    data = [None] * size
    data[offset] = thing
    return (vlist, pointer_offset, size, [1], data), offset


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


def repr_vlist(vlist_ptr):
    return ' '.join(map(str, iter_vlist(vlist_ptr)))


def pick(n, vlist_ptr):
    '''
    Consider starting with a list pointer in Fig 1 then to find the nth
    element subtract n from the pointer offset. If the result is positive
    then the element is in the first block of the list at the calculated
    offset from the base. If the result is negative then move to the next
    block using the Base-Offset pointer. Add the Previous pointer offset
    to the negative offset. While this remains negative keep moving onto
    the next block. When it finally becomes positive the position of the
    required element has been found
    '''
    assert n >= 0
    if not vlist_ptr:
        raise ValueError('Empty list!')
    vlist, pointer_offset = vlist_ptr
    n = pointer_offset - n
    while n < 0:
        if not vlist:
            raise ValueError('Pick index greater than length of list.')
        vlist, offset = vlist[:2]
        n += offset + 1  # Offsets in the paper count from one, not zero?
    return vlist[-1][n]


def length(vlist_ptr):
    if not vlist_ptr:
        return 0
    vlist, n = vlist_ptr
    while vlist:
        vlist, offset = vlist[:2]
        n += offset + 1
    return n


if __name__ == '__main__':
    p = ()
    for n in range(16):
        p = cons(n, p)
        #print(p)

    print(repr_vlist(p))

    for n in range(length(p)):
        print(pick(n, p), end=' ')
    print()



# There is no such thing as a vlist_ptr with a null () vlist.  That's an invariant.


##    # In Fig 2 a list has been created with the integers (8,7,6,5,4,3)
##
##    o1 = ((), 0, 1, [1], [3])
##    o2 = (o1, 0, 2, [2], [4, 5])
##    o3 = (o2, 1, 4, [3], [6, 7, 8, None])
##
##    p0 = (o3, 2)  # points to 8
##    p1 = (o3, 0)  # points to 6
##
##    # cons(9, p1) = o4
##
##    o4 = (o3, 0, 1, [1], [9])
##    p2 = (o4, 0)  # points to 9
##
##
##    p3 = cons(10, p0)



##for i, p in enumerate((p0, p1, p2, p3)):
##    print(f'p{i}')
##    print(' '.join(map(str, iter_vlist(p))))
##    print()


# https://pythontutor.com/visualize.html#code='''%0AAn%20exploration%20of%20Phil%20Bagwell's%20VList.%0A%0AA%20VList%20is%20modeled%20as%20a%205-tuple%3A%0A%20%20%20%20base%3A%20VList%0A%20%20%20%20offset%3A%20int,%20indexing%20%28from%200%29%20the%20base%20VList%20data%20list.%0A%20%20%20%20size%3A%20int,%20length%20of%20the%20data%20list.%0A%20%20%20%20last_used%3A%20to%20make%20a%20mutable%20int%20it's%20an%20int%20in%20a%20list%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20It's%20a%20count%20here%20rather%20than%20an%20offset!%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20That%20is,%20it%20starts%20from%201,%20rather%20than%200.%0A%20%20%20%20data%3A%20a%20list%20of%20length%20size.%0A%0AA%20pointer%20to%20a%20VList%20is%20a%20two-tuple%20of%20%28VList,%20offset%29%20where%0Athe%20offset%20is%20an%20index%20into%20the%20VList's%20data%20list.%0A%0A'''%0A%0A%0Adef%20cons%28thing,%20vlist_ptr%29%3A%0A%20%20%20%20if%20not%20vlist_ptr%3A%0A%20%20%20%20%20%20%20%20return%20%28%28%29,%200,%201,%20%5B1%5D,%20%5Bthing%5D%29,%200%0A%0A%20%20%20%20%28base,%20_offset,%20size,%20last_used_list,%20data%29,%20pointer_offset%20%3D%20vlist_ptr%0A%20%20%20%20%5Blast_used%5D%20%3D%20last_used_list%0A%0A%20%20%20%20'''%0A%20%20%20%20During%20the%20consing%20of%20%289%29%20the%20pointer%20offset%20is%20compared%20with%20the%0A%20%20%20%20last%20used%20offset,%20LastUsed.%20If%20it%20is%20the%20same%20and%20less%20than%20the%20block%0A%20%20%20%20size%20then%20it%20is%20simply%20incremented,%20the%20new%20entry%20made%20and%20LastUsed%0A%20%20%20%20updated.%0A%20%20%20%20'''%0A%0A%20%20%20%20if%20pointer_offset%20%3D%3D%20last_used%20-%201%20and%20last_used%20%3C%20size%3A%0A%20%20%20%20%20%20%20%20pointer_offset%20%2B%3D%201%0A%20%20%20%20%20%20%20%20data%5Bpointer_offset%5D%20%3D%20thing%0A%20%20%20%20%20%20%20%20last_used_list%5B0%5D%20%3D%20last_used%20%2B%201%0A%20%20%20%20%20%20%20%20return%20vlist_ptr%5B0%5D,%20pointer_offset%0A%0A%20%20%20%20'''%0A%20%20%20%20If%20on%20the%20other-hand%20the%20pointer%20offset%20is%20less%20than%20the%20LastUsed%20a%0A%20%20%20%20cons%20is%20being%20applied%20to%20the%20tail%20of%20a%20longer%20list,%20as%20is%20the%20case%0A%20%20%20%20with%20the%20%289%29.%20In%20this%20case%20a%20new%20list%20block%20must%20be%20allocated%20and%20its%0A%20%20%20%20Base-Offset%20pointer%20set%20to%20the%20tail%20contained%20in%20the%20original%20list.%0A%20%20%20%20The%20offset%20part%20being%20set%20to%20the%20point%20in%20tail%20that%20must%20be%20extended.%0A%20%20%20%20The%20new%20entry%20can%20now%20be%20made%20and%20additional%20elements%20added.%0A%20%20%20%20'''%0A%0A%20%20%20%20%23%20Is%20this%20where%20we%20increase%20the%20size%20x%202%3F%0A%20%20%20%20size%20%3C%3C%3D%201%0A%20%20%20%20data%20%3D%20%5BNone%5D%20*%20size%0A%20%20%20%20data%5B0%5D%20%3D%20thing%0A%20%20%20%20return%20%28vlist_ptr%5B0%5D,%20pointer_offset,%20size,%20%5B1%5D,%20data%29,%200%0A%0A%0A%0Ap%20%3D%20%28%29%0Ap%20%3D%20cons%283,%20p%29%23%20%3B%20print%28p%29%0Ap%20%3D%20cons%284,%20p%29%23%20%3B%20print%28p%29%0Ap%20%3D%20cons%285,%20p%29%23%20%3B%20print%28p%29%0Ap%20%3D%20cons%286,%20p%29%23%20%3B%20print%28p%29%0Ap%20%3D%20cons%287,%20p%29%23%20%3B%20print%28p%29%0Ap%20%3D%20cons%288,%20p%29%23%20%3B%20print%28p%29%0A&cumulative=false&heapPrimitives=nevernest&mode=edit&origin=opt-frontend.js&py=3&rawInputLstJSON=%5B%5D&textReferences=false
