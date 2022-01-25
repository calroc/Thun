# -*- coding: utf-8 -*-
#
#    Copyright © 2014, 2015, 2017 Simon Forman
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
When talking about Joy we use the terms "stack", "quote", "sequence",
"list", and others to mean the same thing: a simple linear datatype that
permits certain operations such as iterating and pushing and popping
values from (at least) one end.

There is no "Stack" Python class, instead we use the  `cons list`_, a 
venerable two-tuple recursive sequence datastructure, where the
empty tuple ``()`` is the empty stack and ``(head, rest)`` gives the
recursive form of a stack with one or more items on it::

    stack := () | (item, stack)

Putting some numbers onto a stack::

    ()
    (1, ())
    (2, (1, ()))
    (3, (2, (1, ())))
    ...

Python has very nice "tuple packing and unpacking" in its syntax which
means we can directly "unpack" the expected arguments to a Joy function.

For example::

    def dup((head, tail)):
        return head, (head, tail)

We replace the argument "stack" by the expected structure of the stack,
in this case "(head, tail)", and Python takes care of unpacking the
incoming tuple and assigning values to the names.  (Note that Python
syntax doesn't require parentheses around tuples used in expressions
where they would be redundant.)

Unfortunately, the Sphinx documentation generator, which is used to generate this
web page, doesn't handle tuples in the function parameters.  And in Python 3, this
syntax was removed entirely.  Instead you would have to write::

    def dup(stack):
        head, tail = stack
        return head, (head, tail)


We have two very simple functions, one to build up a stack from a Python
iterable and another to iterate through a stack and yield its items
one-by-one in order.  There are also two functions to generate string representations
of stacks.  They only differ in that one prints the terms in stack from left-to-right while the other prints from right-to-left.  In both functions *internal stacks* are
printed left-to-right.  These functions are written to support :doc:`../pretty`.

.. _cons list: https://en.wikipedia.org/wiki/Cons#Lists

'''
from .errors import NotAListError


def list_to_stack(el, stack=()):
    '''Convert a Python list (or other sequence) to a Joy stack::

    [1, 2, 3] -> (1, (2, (3, ())))

    :param list el: A Python list or other sequence (iterators and generators
             won't work because ``reverse()`` is called on ``el``.)
    :param stack stack: A stack, optional, defaults to the empty stack.
    :rtype: stack

    '''
    for item in reversed(el):
        stack = item, stack
    return stack


def iter_stack(stack):
    '''Iterate through the items on the stack.

    :param stack stack: A stack.
    :rtype: iterator
    '''
    while stack:
        item, stack = stack
        yield item


def stack_to_string(stack):
    '''
    Return a "pretty print" string for a stack.

    The items are written right-to-left::

        (top, (second, ...)) -> '... second top'

    :param stack stack: A stack.
    :rtype: str
    '''
    f = lambda stack: reversed(list(iter_stack(stack)))
    return _to_string(stack, f)


def expression_to_string(expression):
    '''
    Return a "pretty print" string for a expression.

    The items are written left-to-right::

        (top, (second, ...)) -> 'top second ...'

    :param stack expression: A stack.
    :rtype: str
    '''
    return _to_string(expression, iter_stack)


_JOY_BOOL_LITS = 'false', 'true'


def _joy_repr(thing):
        if isinstance(thing, bool):
                return _JOY_BOOL_LITS[thing]
        return repr(thing)


def _to_string(stack, f):
    if not isinstance(stack, tuple): return _joy_repr(stack)
    if not stack: return ''  # shortcut
    return ' '.join(map(_s, f(stack)))


_s = lambda s: (
    '[%s]' % expression_to_string(s)
        if isinstance(s, tuple)
    else _joy_repr(s)
    )


def concat(quote, expression):
    '''Concatinate quote onto expression.

    In joy [1 2] [3 4] would become [1 2 3 4].

    :param stack quote: A stack.
    :param stack expression: A stack.
    :raises RuntimeError: if quote is larger than sys.getrecursionlimit().
    :rtype: stack
    '''
    # This is the fastest implementation, but will trigger
    # RuntimeError: maximum recursion depth exceeded
    # on quotes longer than sys.getrecursionlimit().

##    return (quote[0], concat(quote[1], expression)) if quote else expression

    # Original implementation.

##  return list_to_stack(list(iter_stack(quote)), expression)

    # In-lining is slightly faster (and won't break the
    # recursion limit on long quotes.)

    temp = []
    while quote:
        if not isinstance(quote, tuple):
            raise NotAListError(repr(quote))
        item, quote = quote
        temp.append(item)
    for item in reversed(temp):
        expression = item, expression
    return expression



def dnd(stack, from_index, to_index):
    '''
    Given a stack and two indices return a rearranged stack.
    First remove the item at from_index and then insert it at to_index,
    the second index is relative to the stack after removal of the item
    at from_index.

    This function reuses all of the items and as much of the stack as it
    can.  It's meant to be used by remote clients to support drag-n-drop
    rearranging of the stack from e.g. the StackListbox.
    '''
    assert 0 <= from_index
    assert 0 <= to_index
    if from_index == to_index:
        return stack
    head, n = [], from_index
    while True:
        item, stack = stack
        n -= 1
        if n < 0:
            break
        head.append(item)
    assert len(head) == from_index
    # now we have two cases:
    diff = from_index - to_index
    if diff < 0:
        # from < to
        # so the destination index is still in the stack
        while diff:
            h, stack = stack
            head.append(h)
            diff += 1
    else:
        # from > to
        # so the destination is in the head list
        while diff:
            stack = head.pop(), stack
            diff -= 1
    stack = item, stack
    while head:
        stack = head.pop(), stack
    return stack


def pick(stack, n):
    '''
    Return the nth item on the stack.

    :param stack stack: A stack.
    :param int n: An index into the stack.
    :raises ValueError: if ``n`` is less than zero.
    :raises IndexError: if ``n`` is equal to or greater than the length of ``stack``.
    :rtype: whatever
    '''
    if n < 0:
        raise ValueError
    while True:
        try:
            item, stack = stack
        except ValueError:
            raise IndexError
        n -= 1
        if n < 0:
            break
    return item
