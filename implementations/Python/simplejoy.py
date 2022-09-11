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
████████╗██╗  ██╗██╗   ██╗███╗   ██╗
╚══██╔══╝██║  ██║██║   ██║████╗  ██║
   ██║   ███████║██║   ██║██╔██╗ ██║
   ██║   ██╔══██║██║   ██║██║╚██╗██║
   ██║   ██║  ██║╚██████╔╝██║ ╚████║
   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

This script implements an interpreter for a dialect of Joy.

Joy is a programming language created by Manfred von Thun that is easy to
use and understand and has many other nice properties. This Python
package implements an interpreter for a dialect of Joy that attempts to
stay very close to the spirit of Joy but does not precisely match the
behaviour of the original version(s) written in C. The main difference
between Thun and the originals, other than being written in Python, is
that it works by the “Continuation-Passing Style”.

Here is an example of Joy code:


    [   [[abs] ii <=]
        [
            [<>] [pop !-] ||
        ] &&
    ]
    [[    !-] [[++]] [[--]] ifte dip]
    [[pop !-]  [--]   [++]  ifte    ]
    ifte

This function accepts two integers on the stack and increments or
decrements one of them such that the new pair of numbers is the next
coordinate pair in a square spiral (like the kind used to construct an
Ulam Spiral).


██╗     ██╗██████╗ ██████╗  █████╗ ██████╗ ██╗   ██╗
██║     ██║██╔══██╗██╔══██╗██╔══██╗██╔══██╗╚██╗ ██╔╝
██║     ██║██████╔╝██████╔╝███████║██████╔╝ ╚████╔╝
██║     ██║██╔══██╗██╔══██╗██╔══██║██╔══██╗  ╚██╔╝
███████╗██║██████╔╝██║  ██║██║  ██║██║  ██║   ██║
╚══════╝╚═╝╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝


Start with increment and decrement:

    -- ≡ 1 -
    ++ ≡ 1 +

Common symbols for operations:

    = ≡ eq
    + ≡ add
    > ≡ gt
    < ≡ lt
    >= ≡ ge
    <= ≡ le
    != ≡ ne
    <> ≡ ne

    % ≡ mod
    + ≡ add
    - ≡ sub
    * ≡ mul
    / ≡ floordiv
    div ≡ floordiv

    & ≡ and
    | ≡ or
    ! ≡ not

    << ≡ lshift
    >> ≡ rshift



? ≡ dup bool


&& ≡ nulco [nullary [false]] dip branch
|| ≡ nulco [nullary] dip [true] branch


!- ≡ 0 >=

<{} ≡ [] swap
<<{} ≡ [] rollup

abs ≡ dup 0 < [] [neg] branch

anamorphism ≡ [pop []] swap [dip swons] genrec

app1 ≡ grba infrst
app2 ≡ [grba swap grba swap] dip [infrst] cons ii
app3 ≡ 3 appN
appN ≡ [grabN] codi map disenstacken

at ≡ drop first

b ≡ [i] dip i
dipd ≡ [dip] codi
genrec ≡ [[genrec] ccccons] nullary swons concat ifte
tailrec ≡ [i] genrec
ifte ≡ [nullary] dipd swap branch
ii ≡ [dip] dupdip i
infra ≡ swons swaack [i] dip swaack
x ≡ dup i
pam ≡ [i] map


nullary ≡ [stack] dinfrirst
unary ≡ nullary popd
binary ≡ unary popd
ternary ≡ binary popd

ccccons ≡ ccons ccons
ccons ≡ cons cons

clear ≡ [] swaack pop

cleave ≡ fork popdd
clop ≡ cleave popdd
fork ≡ [i] app2

cmp ≡ [[>] swap] dipd [ifte] ccons [=] swons ifte

codi ≡ cons dip
codireco ≡ codi reco

dinfrirst ≡ dip infrst


disenstacken ≡ ? [uncons ?] loop pop
enstacken ≡ stack [clear] dip

down_to_zero ≡ [0 >] [dup --] while

drop ≡ [rest] times

dupd ≡ [dup] dip
dupdd ≡ [dup] dipd
dupdip ≡ dupd dip
dupdipd ≡ dup dipd


      rest ≡ uncons popd
     first ≡ uncons pop
    second ≡ rest first
     third ≡ rest second
    fourth ≡ rest third


flatten ≡ <{} [concat] step

gcd ≡ true [tuck mod dup 0 >] loop pop


grabN ≡ <{} [cons] times
grba ≡ [stack popd] dip

hypot [sqr] ii + sqrt

infrst ≡ infra first

make_generator ≡ [codireco] ccons

manual ≡ [] words [help] step pop

neg ≡ 0 swap -
not ≡ [true] [false] branch

nulco ≡ [nullary] cons
of ≡ swap at

over ≡ [dup] dip swap

pm ≡ [+] [-] clop

popd ≡ [pop] dip
popdd ≡ [pop] dipd
popop ≡ pop pop
popopop ≡ pop popop
popopd ≡ [popop] dip
popopdd ≡ [popop] dipd

product ≡ 1 swap [*] step

quoted ≡ [unit] dip

range ≡ [0 <=] [1 - dup] anamorphism
range_to_zero ≡ unit [down_to_zero] infra

reco ≡ rest cons


reverse ≡ <{} shunt

roll> ≡ swap swapd
roll< ≡ swapd swap
rollup ≡ roll>
rolldown roll<

rrest ≡ rest rest

run ≡ <{} infra
shift ≡ uncons [swons] dip
shunt ≡ [swons] step
size ≡ [pop ++] step_zero
spiral_next ≡ [[[abs] ii <=] [[<>] [pop !-] ||] &&] [[!-] [[++]] [[--]] ifte dip] [[pop !-] [--] [++] ifte] ifte
split_at ≡ [drop] [take] clop
split_list ≡ [take reverse] [drop] clop
sqr ≡ dup *
stackd ≡ [stack] dip
step_zero ≡ 0 roll> step
stuncons ≡ stack uncons
sum ≡ [+] step_zero
swapd ≡ [swap] dip
swons ≡ swap cons
swoncat ≡ swap concat
sqr ≡ dup mul
take ≡ <<{} [shift] times pop
tuck ≡ dup swapd
uncons ≡ [first] [rest] cleave
unit ≡ [] cons
unquoted ≡ [i] dip
unswons ≡ uncons swap
while ≡ swap nulco dupdipd concat loop

step ≡ [_step0] x
_step0 ≡ _step1 [popopop] [_stept] branch
_step1 ≡ [?] dipd roll<
_stept ≡ [uncons] dipd [dupdipd] dip x

times ≡ [_times0] x
_times0 ≡ _times1 [popopop] [_timest] branch
_times1 ≡ [dup 0 >] dipd roll<
_timest ≡ [[--] dip dupdipd] dip x

map ≡ [_map0] cons [[] [_map?] [_mape]] dip tailrec
_map? ≡ pop bool not
_mape ≡ popd reverse
_map0 ≡ [_map1] dipd _map2
_map1 ≡ stackd shift
_map2 ≡ [infrst] cons dipd roll< swons


'''
from functools import wraps
from inspect import getdoc
from traceback import print_exc
import operator


'''
██╗███╗   ██╗████████╗███████╗██████╗ ██████╗ ██████╗ ███████╗████████╗███████╗██████╗
██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗
██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██████╔╝██████╔╝█████╗     ██║   █████╗  ██████╔╝
██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██╔═══╝ ██╔══██╗██╔══╝     ██║   ██╔══╝  ██╔══██╗
██║██║ ╚████║   ██║   ███████╗██║  ██║██║     ██║  ██║███████╗   ██║   ███████╗██║  ██║
╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝

The joy() interpreter function is extrememly simple. It accepts a stack,
an expression, and a dictionary, and it iterates through the expression
putting values onto the stack and delegating execution to functions which
it looks up in the dictionary.

'''


def joy(stack, expression, dictionary):
    '''
    Evaluate a Joy expression on a stack.

    This function iterates through a sequence of terms.
    Literals are put onto the stack and Symbols are
    looked up in the dictionary and the functions they
    denote are executed.

    :param stack stack: The stack.
    :param stack expression: The expression to evaluate.
    :param dict dictionary: A ``dict`` mapping names to Joy functions.

    :rtype: (stack, dictionary)

    '''
    expr = push_quote(expression)  # We keep a stack-of-stacks, see below.
    while expr:
        term, expr = next_term(expr)
        if isinstance(term, Symbol):
            try:
                func = dictionary[term]
            except KeyError:
                raise UnknownSymbolError(term) from None
            stack, expr, dictionary = func(stack, expr, dictionary)
        else:
            stack = term, stack
    return stack, dictionary


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
    So, [dup *] is a quotation but not a list.

`"A Conversation with Manfred von Thun" w/ Stevan Apter <http://archive.vector.org.uk/art10000350>`_

There is no "Stack" Python class, instead we use the  `cons list`_, a
venerable two-tuple recursive sequence datastructure, where the empty
tuple ``()`` is the empty stack and ``(head, rest)`` gives the recursive
form of a stack with one or more items on it::

    stack := () | (item, stack)

Putting some numbers onto a stack::

    Joy       Python
    []        ()
    [1]       (1, ())
    [2 1]     (2, (1, ()))
    [3 2 1]   (3, (2, (1, ())))
    ...

Python has very nice "tuple packing and unpacking" in its syntax which
means we can directly "unpack" the expected arguments to a Joy function.
We assign the argument stack to the expected structure of the stack and
Python takes care of unpacking the incoming tuple and assigning values to
the names.  (Note that Python syntax doesn't require parentheses around
tuples used in expressions where they would be redundant.)

    def dup(stack):
        head, tail = stack
        return head, (head, tail)


.. _cons list: https://en.wikipedia.org/wiki/Cons#Lists

'''


class StackUnderflowError(Exception):
    pass


def list_to_stack(el, stack=()):
    '''
    Convert a Python list (or other sequence) to a Joy stack::

    [1, 2, 3] -> (1, (2, (3, ())))

    :param list el: A Python list or other sequence (iterators and generators
             won't work because ``reversed()`` is called on ``el``.)
    :param stack stack: A stack, optional, defaults to the empty stack. This
             allows for concatinating Python lists (or other sequence objects)
             onto an existing Joy stack.
    :rtype: stack

    '''
    for item in reversed(el):
        stack = item, stack
    return stack


def iter_stack(stack):
    '''
    Iterate through the items on the stack.

    :param stack stack: A stack.
    :rtype: iterator
    '''
    while stack:
        item, stack = stack
        yield item


def concat(quote, expression):
    '''
    Concatinate quote onto expression.

    In joy [1 2] [3 4] would become [1 2 3 4].

    :param stack quote: A stack.
    :param stack expression: A stack.
    :rtype: stack
    '''
    isnt_stack(quote)
    isnt_stack(expression)
    return list_to_stack(list(iter_stack(quote)), expression)

    ## return (quote[0], concat(quote[1], expression)) if quote else expression
    # :raises RuntimeError: if quote is larger than sys.getrecursionlimit().
    # This is faster implementation but it would trigger
    # RuntimeError: maximum recursion depth exceeded
    # on quotes longer than sys.getrecursionlimit().


def get_n_items(n, stack):
    '''
    Return n items and remainder of stack.
    Raise StackUnderflowError if there are fewer than n items on the stack.
    '''
    assert n > 0, repr(n)
    temp = []
    while n > 0:
        n -= 1
        try:
            item, stack = stack
        except ValueError:
            raise StackUnderflowError(
                'Not enough values on stack.'
            ) from None
        temp.append(item)
    temp.append(stack)
    return tuple(temp)


def reversed_stack(stack):
    '''
    Return list_reverseiterator object for a stack.
    '''
    return reversed(list(iter_stack(stack)))


'''
███████╗██╗  ██╗██████╗ ██████╗ ███████╗███████╗███████╗██╗ ██████╗ ███╗   ██╗
██╔════╝╚██╗██╔╝██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝██║██╔═══██╗████╗  ██║
█████╗   ╚███╔╝ ██████╔╝██████╔╝█████╗  ███████╗███████╗██║██║   ██║██╔██╗ ██║
██╔══╝   ██╔██╗ ██╔═══╝ ██╔══██╗██╔══╝  ╚════██║╚════██║██║██║   ██║██║╚██╗██║
███████╗██╔╝ ██╗██║     ██║  ██║███████╗███████║███████║██║╚██████╔╝██║ ╚████║
╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝

As elegant as it is to model the expression as a stack, it's not very
efficient, as concatenating definitions and other quoted programs to
the expression is a common and expensive operation.

Instead, let's keep a stack of sub-expressions, reading from them
one-by-one, and prepending new sub-expressions to the stack rather than
concatenating them.
'''


def push_quote(quote, expression=()):
    '''
    Put the quoted program onto the stack-of-stacks.
    '''
    return (quote, expression) if quote else expression


def next_term(expression):
    '''
    Return the next term from the expression and the new expression.
    Raises ValueError if called on an empty expression.
    '''
    (item, quote), expression = expression
    return item, push_quote(quote, expression)


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

    joy := <term>*
    term := <integer> | 'true' | 'false' | '[' <joy> ']' | <symbol>

A Joy expression is a sequence of zero or more terms.  A term is a
literal value (integer, Boolean, or quoted Joy expression) or a function symbol.
Function symbols are sequences of non-blanks and cannot contain square
brackets.   Terms must be separated by blanks, which can be omitted
around square brackets.
'''


JOY_BOOL_LITERALS = _F, _T = 'false', 'true'


class ParseError(ValueError):
    '''
    Raised when there is a error while parsing text.
    '''


class Symbol(str):
    '''
    A string class that represents Joy function names.
    '''

    __repr__ = str.__str__


def text_to_expression(text):
    '''
    Convert a string to a Joy expression.

    When supplied with a string this function returns a Python datastructure
    that represents the Joy datastructure described by the text expression.
    Any unbalanced square brackets will raise a ParseError.

    :param str text: Text to convert.
    :rtype: stack
    :raises ParseError: if the parse fails.
    '''
    frame = []
    stack = []
    for tok in text.replace('[', ' [ ').replace(']', ' ] ').split():
        if tok == '[':
            stack.append(frame)
            frame = []
            continue

        if tok == ']':
            thing = list_to_stack(frame)
            try:
                frame = stack.pop()
            except IndexError:
                raise ParseError('Extra closing bracket.') from None
        elif tok == _T:
            thing = True
        elif tok == _F:
            thing = False
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
    return _stack_to_string(stack, reversed_stack)


def expression_to_string(expression):
    '''
    Return a "pretty print" string for a expression.
    (For historical reasons this function works on a single quote
    not a stack-of-stacks.)

    The items are written left-to-right::

        (top, (second, ...)) -> 'top second ...'

    :param stack expression: A stack.
    :rtype: str
    '''
    return _stack_to_string(expression, iter_stack)


def _stack_to_string(stack, iterator):
    isnt_stack(stack)
    if not stack:  # shortcut
        return ''
    return ' '.join(map(_s, iterator(stack)))


def _s(thing):
    return (
        '[%s]' % expression_to_string(thing)
        if isinstance(thing, tuple)
        else JOY_BOOL_LITERALS[thing]
        if isinstance(thing, bool)
        else repr(thing)
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
                stack, dictionary = run(text, stack, dictionary)
            except SystemExit as e:
                raise SystemExit from e
            except:
                print_exc()
    except SystemExit as e:
        raise SystemExit from e
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
    return joy(stack, text_to_expression(text), dictionary)


def interp(stack=(), dictionary=None):
    '''
    Simple REPL with no extra output, suitable for use in scripts.
    '''
    if dictionary is None:
        dictionary = {}
    try:
        while True:
            try:
                text = input()
            except (EOFError, KeyboardInterrupt):
                break
            try:
                stack, dictionary = run(text, stack, dictionary)
            except UnknownSymbolError as sym:
                print('Unknown:', sym)
            except (
                StackUnderflowError,
                NotABoolError,
                NotAListError,
                NotAnIntError,
            ) as e:
                print(e)
            except SystemExit as e:
                raise SystemExit from e
            except:
                print_exc()
            print(stack_to_string(stack))
    except SystemExit as e:
        raise SystemExit from e
    except:
        print_exc()
    return stack


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
    '''
    A decorator to inscribe functions into the default dictionary.
    '''
    d[function.__name__.rstrip('_')] = function
    return function


def initialize():
    '''
    Return a dictionary of Joy functions for use with joy().
    '''
    return _dictionary.copy()


def SimpleFunctionWrapper(f):
    '''
    Wrap functions that take and return just a stack.
    '''

    @wraps(f)
    def SimpleFunctionWrapper_inner(stack, expr, dictionary):
        return f(stack), expr, dictionary

    return SimpleFunctionWrapper_inner


@inscribe
def words(stack, expression, dictionary):
    '''
    Put a list of all the words in alphabetical order onto the stack.
    '''
    w = ()
    for name in reversed(sorted(dictionary)):
        if name.startswith('_'):
            continue
        w = (Symbol(name), ()), w
    return (w, stack), expression, dictionary


HELP_TEMPLATE = '''\

==== Help on %s ====

%s

---- end ( %s )
'''


@inscribe
def help_(stack, expression, dictionary):
    '''
    Accepts a quoted symbol on the top of the stack and prints its docs.
    '''
    ((symbol, _), stack) = stack
    word = dictionary[symbol]
    print(HELP_TEMPLATE % (symbol, getdoc(word), symbol))
    return stack, expression, dictionary


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
    '''
    Use a Boolean value to select one of two quoted programs to run.

    ::

        branch == roll< choice i

    ::

           False [F] [T] branch
        --------------------------
              F

           True [F] [T] branch
        -------------------------
                 T

    '''
    then, else_, flag, stack = get_n_items(3, stack)
    isnt_bool(flag)
    isnt_stack(else_)
    isnt_stack(then)
    expr = push_quote((then if flag else else_), expr)
    return stack, expr, dictionary


@inscribe
def dip(stack, expr, dictionary):
    '''
    The dip combinator expects a quoted program on the stack and below it
    some item, it hoists the item into the expression and runs the program
    on the rest of the stack.
    ::

           ... x [Q] dip
        -------------------
             ... Q x

    '''
    quote, x, stack = get_n_items(2, stack)
    isnt_stack(quote)
    expr = push_quote((x, ()), expr)
    expr = push_quote(quote, expr)
    return stack, expr, dictionary


@inscribe
def i(stack, expr, dictionary):
    '''
    The i combinator expects a quoted program on the stack and unpacks it
    onto the pending expression for evaluation.
    ::

           [Q] i
        -----------
            Q

    '''
    quote, stack = get_n_items(1, stack)
    isnt_stack(quote)
    return stack, push_quote(quote, expr), dictionary


LOOP = Symbol('loop')


@inscribe
def loop(stack, expr, dictionary):
    '''
    Basic loop combinator.
    ::

           ... True [Q] loop
        -----------------------
              ... Q [Q] loop

           ... False [Q] loop
        ------------------------
              ...

    '''
    quote, flag, stack = get_n_items(2, stack)
    isnt_bool(flag)
    isnt_stack(quote)
    if flag:
        expr = push_quote((quote, (LOOP, ())), expr)
        expr = push_quote(quote, expr)
    return stack, expr, dictionary


@inscribe
def quit(stack, expr, dictionary):
    '''
    Stop the interpreter.
    '''
    raise SystemExit


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
    '''
    Clear everything from the stack.
    ::

        clear == stack [pop stack] loop

           ... clear
        ---------------

    '''
    return ()


@inscribe
@SimpleFunctionWrapper
def concat_(stack):
    '''
    Concatinate the two lists on the top of the stack.
    ::

           [a b c] [d e f] concat
        ----------------------------
               [a b c d e f]

    '''
    tos, second, stack = get_n_items(2, stack)
    return concat(second, tos), stack


@inscribe
@SimpleFunctionWrapper
def cons(stack):
    '''
    Given an item and a list, append the item to the list to make a new list.
    ::

           a [...] cons
        ------------------
             [a ...]

    Cons is a venerable old function from Lisp
    ( https://en.wikipedia.org/wiki/Cons#Lists ).
    Its inverse operation is uncons.
    '''
    s0, a1, stack = get_n_items(2, stack)
    isnt_stack(s0)
    return ((a1, s0), stack)


@inscribe
@SimpleFunctionWrapper
def dup(stack):
    '''
    "Dup"licate the top item on the stack.
    ::

           a dup
        -----------
            a a

    '''
    a1, stack = get_n_items(1, stack)
    return a1, (a1, stack)


@inscribe
@SimpleFunctionWrapper
def first(stack):
    '''
    Replace a list with its first item.

           [a ...]
        --------------
              a

    '''
    s0, stack = get_n_items(1, stack)
    isnt_stack(s0)
    a1, _ = get_n_items(1, s0)
    return a1, stack


@inscribe
@SimpleFunctionWrapper
def pop(stack):
    '''
    Pop the top item from the stack and discard it.

           a pop
        -----------

    '''
    try:
        _, stack = stack
    except ValueError:
        raise StackUnderflowError('Cannot pop empty stack.') from None
    return stack


@inscribe
@SimpleFunctionWrapper
def rest(stack):
    '''
    Replace a list with its tail.

           [a b c] rest
        ------------------
              [b c]

    '''
    s0, stack = get_n_items(1, stack)
    isnt_stack(s0)
    try:
        _, s1 = s0
    except ValueError:
        raise StackUnderflowError(
            'Cannot take rest of empty list.'
        ) from None
    return s1, stack


@inscribe
@SimpleFunctionWrapper
def stack(stack):
    '''
    Put the stack onto the stack.

              ... c b a stack
        ---------------------------
           ... c b a [a b c ...]

    '''
    return stack, stack


@inscribe
@SimpleFunctionWrapper
def swaack(stack):
    '''
    Swap stack.  Take a list from the top of the stack, replace the stack
    with the list, and put the old stack onto it.

           1 2 3 [4 5 6] swaack
        --------------------------
              6 5 4 [3 2 1]

    '''
    s1, s0 = get_n_items(1, stack)
    isnt_stack(s1)
    return s0, s1


@inscribe
@SimpleFunctionWrapper
def swap(stack):
    '''
    Swap the top two items on the stack.

           a b swap
        --------------
             b a

    '''
    a2, a1, stack = get_n_items(2, stack)
    return (a1, (a2, stack))


def BinaryLogicWrapper(f):
    '''
    Wrap functions that take two numbers and return a single result.
    '''

    @wraps(f)
    def BinaryLogicWrapper_inner(stack, expression, dictionary):
        a, b, stack = get_n_items(2, stack)
        isnt_bool(a)
        isnt_bool(b)
        result = f(b, a)
        return (result, stack), expression, dictionary

    return BinaryLogicWrapper_inner


def BinaryMathWrapper(func):
    '''
    Wrap functions that take two numbers and return a single result.
    '''

    @wraps(func)
    def BinaryMathWrapper_inner(stack, expression, dictionary):
        a, b, stack = get_n_items(2, stack)
        isnt_int(a)
        isnt_int(b)
        result = func(b, a)
        return (result, stack), expression, dictionary

    return BinaryMathWrapper_inner


def UnaryLogicWrapper(f):
    '''
    Wrap functions that take one argument and return a single result.
    '''

    @wraps(f)
    def UnaryLogicWrapper_inner(stack, expression, dictionary):
        a, stack = get_n_items(1, stack)
        isnt_bool(a)
        result = f(a)
        return (result, stack), expression, dictionary

    return UnaryLogicWrapper_inner


def UnaryMathWrapper(f):
    '''
    Wrap functions that take one argument and return a single result.
    '''

    @wraps(f)
    def UnaryMathWrapper_inner(stack, expression, dictionary):
        a, stack = get_n_items(1, stack)
        isnt_int(a)
        result = f(a)
        return (result, stack), expression, dictionary

    return UnaryMathWrapper_inner


def UnaryWrapper(f):
    '''
    Wrap functions that take one argument and return a single result.
    '''

    @wraps(f)
    def UnaryWrapper_inner(stack, expression, dictionary):
        a, stack = get_n_items(1, stack)
        result = f(a)
        return (result, stack), expression, dictionary

    return UnaryWrapper_inner


for F in (
    ## ██████╗ ██████╗ ███╗   ███╗██████╗  █████╗ ██████╗ ██╗███████╗██╗ ██████╗ ███╗   ██╗
    ##██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔══██╗██╔══██╗██║██╔════╝██║██╔═══██╗████╗  ██║
    ##██║     ██║   ██║██╔████╔██║██████╔╝███████║██████╔╝██║███████╗██║██║   ██║██╔██╗ ██║
    ##██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██╔══██║██╔══██╗██║╚════██║██║██║   ██║██║╚██╗██║
    ##╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ██║  ██║██║  ██║██║███████║██║╚██████╔╝██║ ╚████║
    ## ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝
    BinaryMathWrapper(operator.eq),
    BinaryMathWrapper(operator.ge),
    BinaryMathWrapper(operator.gt),
    BinaryMathWrapper(operator.le),
    BinaryMathWrapper(operator.lt),
    BinaryMathWrapper(operator.ne),
    ##██╗      ██████╗  ██████╗ ██╗ ██████╗
    ##██║     ██╔═══██╗██╔════╝ ██║██╔════╝
    ##██║     ██║   ██║██║  ███╗██║██║
    ##██║     ██║   ██║██║   ██║██║██║
    ##███████╗╚██████╔╝╚██████╔╝██║╚██████╗
    ##╚══════╝ ╚═════╝  ╚═════╝ ╚═╝ ╚═════╝
    UnaryWrapper(bool),  # Convert any value to Boolean.
    # (The only polymorphic function.)
    BinaryLogicWrapper(operator.xor),
    BinaryLogicWrapper(operator.and_),
    BinaryLogicWrapper(operator.or_),
    UnaryLogicWrapper(operator.not_),
    ##███╗   ███╗ █████╗ ████████╗██╗  ██╗
    ##████╗ ████║██╔══██╗╚══██╔══╝██║  ██║
    ##██╔████╔██║███████║   ██║   ███████║
    ##██║╚██╔╝██║██╔══██║   ██║   ██╔══██║
    ##██║ ╚═╝ ██║██║  ██║   ██║   ██║  ██║
    ##╚═╝     ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝
    BinaryMathWrapper(operator.lshift),
    BinaryMathWrapper(operator.rshift),
    BinaryMathWrapper(operator.add),
    BinaryMathWrapper(operator.floordiv),
    BinaryMathWrapper(operator.mod),
    BinaryMathWrapper(operator.mul),
    BinaryMathWrapper(operator.pow),
    BinaryMathWrapper(operator.sub),
    UnaryMathWrapper(abs),
    UnaryMathWrapper(operator.neg),
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
    '''
    Definitions are given by equations:

        name ≡ foo bar baz ...

    When a definition symbol is evaluated its body expression is put onto
    the pending expression.
    '''

    # tribar = '\u2261'  # '≡'

    def __init__(self, name, body):
        self.__doc__ = f'{name} ≡ {expression_to_string(body)}'
        self.__name__ = name
        self.body = body

    def __call__(self, stack, expr, dictionary):
        return stack, push_quote(self.body, expr), dictionary

    @classmethod
    def load_definitions(class_, stream, dictionary):
        '''
        Given an iterable of lines (strings) and a dictionary put any
        definitions (lines with '≡' in them) into the dictionary.
        '''
        for line in stream:
            if '≡' not in line:
                continue
            name, body = text_to_expression(line.replace('≡', ''))
            if name not in dictionary:
                inscribe(class_(name, body), dictionary)


'''
████████╗██╗   ██╗██████╗ ███████╗     ██████╗██╗  ██╗███████╗ ██████╗██╗  ██╗███████╗
╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝    ██╔════╝██║  ██║██╔════╝██╔════╝██║ ██╔╝██╔════╝
   ██║    ╚████╔╝ ██████╔╝█████╗      ██║     ███████║█████╗  ██║     █████╔╝ ███████╗
   ██║     ╚██╔╝  ██╔═══╝ ██╔══╝      ██║     ██╔══██║██╔══╝  ██║     ██╔═██╗ ╚════██║
   ██║      ██║   ██║     ███████╗    ╚██████╗██║  ██║███████╗╚██████╗██║  ██╗███████║
   ╚═╝      ╚═╝   ╚═╝     ╚══════╝     ╚═════╝╚═╝  ╚═╝╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝
'''


class NotAListError(Exception):
    '''
    Raised when a stack is expected but not received.
    '''


class NotAnIntError(Exception):
    pass


class NotABoolError(Exception):
    pass


class UnknownSymbolError(KeyError):
    pass


def isnt_int(i):
    '''
    Raise NotAnIntError if i isn't an integer.
    (Booleans are not integers in Joy.)
    '''
    if not isinstance(i, int) or isinstance(i, bool):
        raise NotAnIntError(f'Not an integer: {_s(i)}')
    return i


def isnt_bool(b):
    '''
    Raise NotABoolError if b isn't a Boolean.
    '''
    if not isinstance(b, bool):
        raise NotABoolError(f'Not a Boolean value: {_s(b)}')
    return b


def isnt_stack(el):
    '''
    Raise NotAListError if el isn't a stack/quote/list.
    '''
    if not isinstance(el, tuple):
        raise NotAListError(f'Not a list: {_s(el)}')
    return el


# Put these into the dictionary so users can, uh, use them.
# Not as decorators because we want to use the unwrapped
# functions in our python code.
inscribe(UnaryWrapper(isnt_int))
inscribe(UnaryWrapper(isnt_bool))
inscribe(UnaryWrapper(isnt_stack))


if __name__ == '__main__':
    import sys

    J = interp if '-q' in sys.argv else repl
    dictionary = initialize()
    Def.load_definitions(__doc__.splitlines(), dictionary)
    try:
        stack = J(dictionary=dictionary)
    except SystemExit:
        pass
