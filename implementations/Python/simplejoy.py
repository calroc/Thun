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
████████╗██╗  ██╗██╗   ██╗███╗   ██╗
╚══██╔══╝██║  ██║██║   ██║████╗  ██║
   ██║   ███████║██║   ██║██╔██╗ ██║
   ██║   ██╔══██║██║   ██║██║╚██╗██║
   ██║   ██║  ██║╚██████╔╝██║ ╚████║
   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

This script implements an interpreter for a dialect of Joy.

'''
from functools import wraps
from re import Scanner
from traceback import print_exc
import operator


JOY_BOOL_LITERALS = 'false', 'true'


class NotAListError(Exception):
    pass


class NotAnIntError(Exception):
    pass


class StackUnderflowError(Exception):
    pass


class UnknownSymbolError(KeyError):
    pass


'''
██╗███╗   ██╗████████╗███████╗██████╗ ██████╗ ██████╗ ███████╗████████╗███████╗██████╗
██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗
██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██████╔╝██████╔╝█████╗     ██║   █████╗  ██████╔╝
██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██╔═══╝ ██╔══██╗██╔══╝     ██║   ██╔══╝  ██╔══██╗
██║██║ ╚████║   ██║   ███████╗██║  ██║██║     ██║  ██║███████╗   ██║   ███████╗██║  ██║
╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
'''


def joy(stack, expr, dictionary):
    '''
    Evaluate a Joy expression on a stack.

    This function iterates through a sequence of terms.
    Literals are put onto the stack and Symbols are
    looked up in the dictionary and the functions they
    denote are executed.

    :param stack stack: The stack.
    :param stack expression: The expression to evaluate.
    :param dict dictionary: A ``dict`` mapping names to Joy functions.

    :rtype: (stack, (), dictionary)

    '''
    while expr:
        term, expr = expr
        if isinstance(term, Symbol):
            try:
                func = dictionary[term]
            except KeyError:
                raise UnknownSymbolError(term) from None
            stack, expr, dictionary = func(stack, expr, dictionary)
        else:
            stack = term, stack
    return stack, expr, dictionary


'''
██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝

There is a single function for converting text to joy expressions
as well as a Symbol class and an Exception type.  The Symbol
string class is used by the interpreter to recognize literals by
the fact that they are not Symbol objects.

A crude grammar::

    joy = term*
    term = integer | '[' joy ']' | symbol

A Joy expression is a sequence of zero or more terms.  A term is a
literal value (integer or quoted Joy expression) or a function symbol.
Function symbols are sequences of non-blanks and cannot contain square
brackets.   Terms must be separated by blanks, which can be omitted
around square brackets.
'''

BRACKETS = r'\[|\]'  # Left or right square bracket.
BLANKS = r'\s+'  # One-or-more blankspace.
WORDS = (
    '['  # Character class
    '^'  # not a
    '['  # left square bracket nor a
    '\]'  # right square bracket (escaped so it doesn't close the character class)
    '\s'  # nor blankspace
    ']+'  # end character class, one-or-more.
)


token_scanner = Scanner(
    [
        (BRACKETS, lambda _, token: token),
        (BLANKS, None),
        (WORDS, lambda _, token: token),
    ]
)


class Symbol(str):
    '''A string class that represents Joy function names.'''

    __repr__ = str.__str__


def text_to_expression(text):
    '''Convert a string to a Joy expression.

    When supplied with a string this function returns a Python datastructure
    that represents the Joy datastructure described by the text expression.
    Any unbalanced square brackets will raise a ParseError.

    :param str text: Text to convert.
    :rtype: stack
    :raises ParseError: if the parse fails.
    '''
    return _parse(_tokenize(text))


class ParseError(ValueError):
    '''Raised when there is a error while parsing text.'''


def _tokenize(text):
    '''Convert a text into a stream of tokens.

    Converts function names to Symbols.

    Raise ParseError (with some of the failing text) if the scan fails.
    '''
    tokens, rest = token_scanner.scan(text)
    if rest:
        raise ParseError(
            'Scan failed at position %i, %r'
            % (len(text) - len(rest), rest[:10])
        )
    return tokens


def _parse(tokens):
    '''
    Return a stack/list expression of the tokens.
    '''
    frame = []
    stack = []
    for tok in tokens:
        if tok == '[':
            stack.append(frame)
            frame = []
        elif tok == ']':
            v = frame
            try:
                frame = stack.pop()
            except IndexError:
                raise ParseError('Extra closing bracket.') from None
            frame.append(list_to_stack(v))
        elif tok == 'true':
            frame.append(True)
        elif tok == 'false':
            frame.append(False)
        else:
            try:
                thing = int(tok)
            except ValueError:
                thing = Symbol(tok)
            frame.append(thing)
    if stack:
        raise ParseError('Unclosed bracket.')
    return list_to_stack(frame)


'''
███████╗████████╗ █████╗  ██████╗██╗  ██╗
██╔════╝╚══██╔══╝██╔══██╗██╔════╝██║ ██╔╝
███████╗   ██║   ███████║██║     █████╔╝
╚════██║   ██║   ██╔══██║██║     ██╔═██╗
███████║   ██║   ██║  ██║╚██████╗██║  ██╗
╚══════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝

When talking about Joy we use the terms "stack", "quote", "sequence",
"list", and others to mean the same thing: a simple linear datatype that
permits certain operations such as iterating and pushing and popping
values from (at least) one end.

    In describing Joy I have used the term quotation to describe all of the
    above, because I needed a word to describe the arguments to combinators
    which fulfill the same role in Joy as lambda abstractions (with
    variables) fulfill in the more familiar functional languages. I use the
    term list for those quotations whose members are what I call literals:
    numbers, characters, truth values, sets, strings and other quotations.
    All these I call literals because their occurrence in code results in
    them being pushed onto the stack. But I also call [London Paris] a list.
    So, [dup \*] is a quotation but not a list.

`"A Conversation with Manfred von Thun" w/ Stevan Apter <http://archive.vector.org.uk/art10000350>`_

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
list and another to iterate through a stack and yield its items
one-by-one in order.  There are also two functions to generate string representations
of stacks.  They only differ in that one prints the terms in stack from left-to-right while the other prints from right-to-left.  In both functions *internal stacks* are
printed left-to-right.  These functions are written to support :doc:`../pretty`.

.. _cons list: https://en.wikipedia.org/wiki/Cons#Lists

'''


def list_to_stack(el, stack=()):
    '''Convert a Python list (or other sequence) to a Joy stack::

    [1, 2, 3] -> (1, (2, (3, ())))

    :param list el: A Python list or other sequence (iterators and generators
             won't work because ``reverse()`` is called on ``el``.)
    :param stack stack: A stack, optional, defaults to the empty stack. This
             allows for concatinating Python lists (or other sequence objects)
             onto an existing Joy stack.
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


def concat(quote, expression):
    '''Concatinate quote onto expression.

    In joy [1 2] [3 4] would become [1 2 3 4].

    :param stack quote: A stack.
    :param stack expression: A stack.
    :rtype: stack
    '''
    # This is the fastest implementation, but will trigger
    # RuntimeError: maximum recursion depth exceeded
    # on quotes longer than sys.getrecursionlimit().
    # :raises RuntimeError: if quote is larger than sys.getrecursionlimit().

    ##    return (quote[0], concat(quote[1], expression)) if quote else expression

    # Original implementation.

    ##  return list_to_stack(list(iter_stack(quote)), expression)

    # In-lining is slightly faster (and won't break the
    # recursion limit on long quotes.)

    if not isinstance(quote, tuple):
        raise NotAListError('Not a list.')
    temp = []
    while quote:
        item, quote = quote
        temp.append(item)
    for item in reversed(temp):
        expression = item, expression
    return expression


'''
██████╗ ██████╗ ██╗███╗   ██╗████████╗███████╗██████╗
██╔══██╗██╔══██╗██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗
██████╔╝██████╔╝██║██╔██╗ ██║   ██║   █████╗  ██████╔╝
██╔═══╝ ██╔══██╗██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗
██║     ██║  ██║██║██║ ╚████║   ██║   ███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
'''


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


def _joy_repr(thing):
    return (
        JOY_BOOL_LITERALS[thing]
        if isinstance(thing, bool)
        else repr(thing)
    )


def _to_string(stack, f):
    if not isinstance(stack, tuple):
        return _joy_repr(stack)
    if not stack:
        return ''  # shortcut
    return ' '.join(map(_s, f(stack)))


_s = lambda s: (
    '[%s]' % expression_to_string(s)
    if isinstance(s, tuple)
    else _joy_repr(s)
)


'''
██████╗ ███████╗██████╗ ██╗
██╔══██╗██╔════╝██╔══██╗██║
██████╔╝█████╗  ██████╔╝██║
██╔══██╗██╔══╝  ██╔═══╝ ██║
██║  ██║███████╗██║     ███████╗
╚═╝  ╚═╝╚══════╝╚═╝     ╚══════╝

Read-Evaluate-Print Loop

'''


def repl(stack=(), dictionary=None):
    '''
    Read-Evaluate-Print Loop

    Accept input and run it on the stack, loop.

    :param stack stack: The stack.
    :param dict dictionary: A ``dict`` mapping names to Joy functions.
    :rtype: stack

    '''
    if dictionary is None:
        dictionary = {}
    try:
        while True:
            print()
            print(stack_to_string(stack), '<-top')
            print()
            try:
                text = input('joy? ')
            except (EOFError, KeyboardInterrupt):
                break
            try:
                stack, _, dictionary = run(text, stack, dictionary)
            except:
                print_exc()
    except:
        print_exc()
    print()
    return stack


def run(text, stack, dictionary):
    '''
    Return the stack resulting from running the Joy code text on the stack.

    :param str text: Joy code.
    :param stack stack: The stack.
    :param dict dictionary: A ``dict`` mapping names to Joy functions.
    :rtype: (stack, (), dictionary)

    '''
    expr = text_to_expression(text)
    return joy(stack, expr, dictionary)


'''
██████╗ ██╗ ██████╗████████╗██╗ ██████╗ ███╗   ██╗ █████╗ ██████╗ ██╗   ██╗
██╔══██╗██║██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║██╔══██╗██╔══██╗╚██╗ ██╔╝
██║  ██║██║██║        ██║   ██║██║   ██║██╔██╗ ██║███████║██████╔╝ ╚████╔╝
██║  ██║██║██║        ██║   ██║██║   ██║██║╚██╗██║██╔══██║██╔══██╗  ╚██╔╝
██████╔╝██║╚██████╗   ██║   ██║╚██████╔╝██║ ╚████║██║  ██║██║  ██║   ██║
╚═════╝ ╚═╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝
'''


# This is the main dict we're building.
_dictionary = {}


def inscribe(function, d=_dictionary):
    '''A decorator to inscribe functions into the default dictionary.'''
    d[function.__name__.rstrip('_')] = function
    return function


def initialize():
    '''Return a dictionary of Joy functions for use with joy().'''
    return _dictionary.copy()


def SimpleFunctionWrapper(f):
    '''Wrap functions that take and return just a stack.'''

    @wraps(f)
    def inner(stack, expr, dictionary):
        return f(stack), expr, dictionary

    return inner


'''
 ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗███╗   ██╗ █████╗ ████████╗ ██████╗ ██████╗ ███████╗
██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║████╗  ██║██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝
██║     ██║   ██║██╔████╔██║██████╔╝██║██╔██╗ ██║███████║   ██║   ██║   ██║██████╔╝███████╗
██║     ██║   ██║██║╚██╔╝██║██╔══██╗██║██║╚██╗██║██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║
╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝██║██║ ╚████║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║
 ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝
'''


@inscribe
def branch(stack, expr, dictionary):
    (then, (else_, (flag, stack))) = stack
    do = then if flag else else_
    return stack, concat(do, expr), dictionary


@inscribe
def dip(stack, expr, dictionary):
    quote, (x, stack) = stack
    return stack, concat(quote, (x, expr)), dictionary


@inscribe
def i(stack, expr, dictionary):
    quote, stack = stack
    return stack, concat(quote, expr), dictionary


LOOP = Symbol('loop')


@inscribe
def loop(stack, expr, dictionary):
    quote, (flag, stack) = stack
    if flag:
        expr = concat(quote, (quote, (LOOP, expr)))
    return stack, expr, dictionary


'''
 ██████╗ ██████╗ ██████╗ ███████╗    ██╗    ██╗ ██████╗ ██████╗ ██████╗ ███████╗
██╔════╝██╔═══██╗██╔══██╗██╔════╝    ██║    ██║██╔═══██╗██╔══██╗██╔══██╗██╔════╝
██║     ██║   ██║██████╔╝█████╗      ██║ █╗ ██║██║   ██║██████╔╝██║  ██║███████╗
██║     ██║   ██║██╔══██╗██╔══╝      ██║███╗██║██║   ██║██╔══██╗██║  ██║╚════██║
╚██████╗╚██████╔╝██║  ██║███████╗    ╚███╔███╔╝╚██████╔╝██║  ██║██████╔╝███████║
 ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚══╝╚══╝  ╚═════╝ ╚═╝  ╚═╝╚═════╝ ╚══════╝
'''


@inscribe
@SimpleFunctionWrapper
def clear(stack):
    return ()


@inscribe
@SimpleFunctionWrapper
def concat_(stack):
    (tos, (second, stack)) = stack
    return concat(second, tos), stack


@inscribe
@SimpleFunctionWrapper
def cons(stack):
    s0, (a1, stack) = stack
    return ((a1, s0), stack)


@inscribe
@SimpleFunctionWrapper
def dup(stack):
    (a1, s23) = stack
    return (a1, (a1, s23))


@inscribe
@SimpleFunctionWrapper
def first(stack):
    ((a1, s1), s23) = stack
    return (a1, s23)


@inscribe
@SimpleFunctionWrapper
def pop(stack):
    (_, s23) = stack
    return s23


@inscribe
@SimpleFunctionWrapper
def rest(stack):
    (_, s1), stack = stack
    return (s1, stack)


@inscribe
@SimpleFunctionWrapper
def stack(stack):
    return stack, stack


@inscribe
@SimpleFunctionWrapper
def swaack(stack):
    (s1, s0) = stack
    return (s0, s1)


@inscribe
@SimpleFunctionWrapper
def swap(stack):
    (a2, (a1, s23)) = stack
    return (a1, (a2, s23))


def BinaryFunc(f):
    '''
    Wrap functions that take two arguments and return a single result.
    '''

    @wraps(f)
    def inner(stack, expression, dictionary):
        (a, (b, stack)) = stack
        result = f(b, a)
        return (result, stack), expression, dictionary

    return inner


def UnaryBuiltinWrapper(f):
    '''
    Wrap functions that take one argument and return a single result.
    '''

    @wraps(f)
    def inner(stack, expression, dictionary):
        (a, stack) = stack
        result = f(a)
        return (result, stack), expression, dictionary

    return inner


for F in (
    ## ██████╗ ██████╗ ███╗   ███╗██████╗  █████╗ ██████╗ ██╗███████╗██╗ ██████╗ ███╗   ██╗
    ##██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔══██╗██╔══██╗██║██╔════╝██║██╔═══██╗████╗  ██║
    ##██║     ██║   ██║██╔████╔██║██████╔╝███████║██████╔╝██║███████╗██║██║   ██║██╔██╗ ██║
    ##██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██╔══██║██╔══██╗██║╚════██║██║██║   ██║██║╚██╗██║
    ##╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║  ██║██║  ██║██║███████║██║╚██████╔╝██║ ╚████║
    ## ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝
    BinaryFunc(operator.eq),
    BinaryFunc(operator.ge),
    BinaryFunc(operator.gt),
    BinaryFunc(operator.le),
    BinaryFunc(operator.lt),
    BinaryFunc(operator.ne),
    ##██╗      ██████╗  ██████╗ ██╗ ██████╗
    ##██║     ██╔═══██╗██╔════╝ ██║██╔════╝
    ##██║     ██║   ██║██║  ███╗██║██║
    ##██║     ██║   ██║██║   ██║██║██║
    ##███████╗╚██████╔╝╚██████╔╝██║╚██████╗
    ##╚══════╝ ╚═════╝  ╚═════╝ ╚═╝ ╚═════╝
    BinaryFunc(operator.xor),
    BinaryFunc(operator.and_),
    BinaryFunc(operator.or_),
    UnaryBuiltinWrapper(operator.not_),
    ##███╗   ███╗ █████╗ ████████╗██╗  ██╗
    ##████╗ ████║██╔══██╗╚══██╔══╝██║  ██║
    ##██╔████╔██║███████║   ██║   ███████║
    ##██║╚██╔╝██║██╔══██║   ██║   ██╔══██║
    ##██║ ╚═╝ ██║██║  ██║   ██║   ██║  ██║
    ##╚═╝     ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝
    BinaryFunc(operator.lshift),
    BinaryFunc(operator.rshift),
    BinaryFunc(operator.add),
    BinaryFunc(operator.floordiv),
    BinaryFunc(operator.mod),
    BinaryFunc(operator.mul),
    BinaryFunc(operator.pow),
    BinaryFunc(operator.sub),
    UnaryBuiltinWrapper(abs),
    UnaryBuiltinWrapper(bool),
    UnaryBuiltinWrapper(operator.neg),
):
    inscribe(F)


'''
██████╗ ███████╗███████╗██╗███╗   ██╗██╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
██╔══██╗██╔════╝██╔════╝██║████╗  ██║██║╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
██║  ██║█████╗  █████╗  ██║██╔██╗ ██║██║   ██║   ██║██║   ██║██╔██╗ ██║███████╗
██║  ██║██╔══╝  ██╔══╝  ██║██║╚██╗██║██║   ██║   ██║██║   ██║██║╚██╗██║╚════██║
██████╔╝███████╗██║     ██║██║ ╚████║██║   ██║   ██║╚██████╔╝██║ ╚████║███████║
╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝
'''


class Def(object):
    def __init__(self, name, body):
        self.__name__ = name
        self.body = tuple(iter_stack(body))

    def __call__(self, stack, expr, dictionary):
        expr = list_to_stack(self.body, expr)
        return stack, expr, dictionary

    @classmethod
    def load_definitions(class_, stream, dictionary):
        for line in stream:
            if line.lstrip().startswith('#'):
                continue
            name, body = text_to_expression(line)
            if name not in dictionary:
                inscribe(class_(name, body), dictionary)


DEFS = '''\
-- 1 -
? dup bool
&& nulco [nullary [false]] dip branch
++ 1 +
|| nulco [nullary] dip [true] branch
!- 0 >=
<{} [] swap
<<{} [] rollup
abs dup 0 < [] [neg] branch
anamorphism [pop []] swap [dip swons] genrec
app1 grba infrst
app2 [grba swap grba swap] dip [infrst] cons ii
app3 3 appN
appN [grabN] codi map disenstacken
at drop first
average [sum] [size] cleave /
b [i] dip i
binary unary popd
ccccons ccons ccons
ccons cons cons
clear [] swaack pop
cleave fork popdd
clop cleave popdd
cmp [[>] swap] dipd [ifte] ccons [=] swons ifte
codi cons dip
codireco codi reco
dinfrirst dip infrst
dipd [dip] codi
disenstacken ? [uncons ?] loop pop
down_to_zero [0 >] [dup --] while
drop [rest] times
dupd [dup] dip
dupdd [dup] dipd
dupdip dupd dip
dupdipd dup dipd
enstacken stack [clear] dip
first uncons pop
flatten <{} [concat] step
fork [i] app2
fourth rest third
gcd true [tuck mod dup 0 >] loop pop
genrec [[genrec] ccccons] nullary swons concat ifte
grabN <{} [cons] times
grba [stack popd] dip
hypot [sqr] ii + sqrt
ifte [nullary] dipd swap branch
ii [dip] dupdip i
infra swons swaack [i] dip swaack
infrst infra first
make_generator [codireco] ccons
mod %
neg 0 swap -
not [true] [false] branch
nulco [nullary] cons
nullary [stack] dinfrirst
of swap at
pam [i] map
pm [+] [-] clop
popd [pop] dip
popdd [pop] dipd
popop pop pop
popopop pop popop
popopd [popop] dip
popopdd [popop] dipd
product 1 swap [*] step
quoted [unit] dip
range [0 <=] [1 - dup] anamorphism
range_to_zero unit [down_to_zero] infra
reco rest cons
rest uncons popd
reverse <{} shunt
roll> swap swapd
roll< swapd swap
rollup roll>
rolldown roll<
rrest rest rest
run <{} infra
second rest first
shift uncons [swons] dip
shunt [swons] step
size [pop ++] step_zero
spiral_next [[[abs] ii <=] [[<>] [pop !-] ||] &&] [[!-] [[++]] [[--]] ifte dip] [[pop !-] [--] [++] ifte] ifte
split_at [drop] [take] clop
split_list [take reverse] [drop] clop
sqr dup *
stackd [stack] dip
step_zero 0 roll> step
stuncons stack uncons
sum [+] step_zero
swapd [swap] dip
swons swap cons
swoncat swap concat
sqr dup mul
tailrec [i] genrec
take <<{} [shift] times pop
ternary binary popd
third rest second
tuck dup swapd
unary nullary popd
uncons [first] [rest] cleave
unit [] cons
unquoted [i] dip
unswons uncons swap
while swap nulco dupdipd concat loop
x dup i
step [_step0] x
_step0 _step1 [popopop] [_stept] branch
_step1 [?] dipd roll<
_stept [uncons] dipd [dupdipd] dip x
times [_times0] x
_times0 _times1 [popopop] [_timest] branch
_times1 [dup 0 >] dipd roll<
_timest [[--] dip dupdipd] dip x
map [_map0] cons [[] [_map?] [_mape]] dip tailrec
_map? pop bool not
_mape popd reverse
_map0 [_map1] dipd _map2
_map1 stackd shift
_map2 [infrst] cons dipd roll< swons
'''


if __name__ == '__main__':
    dictionary = initialize()
    Def.load_definitions(DEFS.splitlines(), dictionary)
    stack = repl(dictionary=dictionary)
