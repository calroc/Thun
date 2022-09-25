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


• ≡


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
appN ≡ [grabN] codi map reverse disenstacken

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


    nullary ≡ [stack] dip infra first
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
        #print(
        #    f'{stack_to_string(stack)} • {expr_to_string(expr)}'
        #    )
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


class UnknownSymbolError(KeyError):
    pass


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


def expr_to_string(expr):
    '''
    Return a "pretty print" string for a stack-of-stacks expression.
    '''
    return ' '.join(map(expression_to_string, iter_stack(expr)))


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
    isnt_stack(then)
    isnt_stack(else_)
    isnt_bool(flag)
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


S_loop = Symbol('loop')


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
        expr = push_quote((quote, (S_loop, ())), expr)
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


'''
███████╗██╗  ██╗████████╗██████╗  █████╗
██╔════╝╚██╗██╔╝╚══██╔══╝██╔══██╗██╔══██╗
█████╗   ╚███╔╝    ██║   ██████╔╝███████║
██╔══╝   ██╔██╗    ██║   ██╔══██╗██╔══██║
███████╗██╔╝ ██╗   ██║   ██║  ██║██║  ██║
╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝
'''


@inscribe
def trace(stack, expr, dictionary):
    '''Evaluate a Joy expression on a stack and print a trace.

    This function is just like the `i` combinator but it also prints a
    trace of the evaluation

    :param stack stack: The stack.
    :param stack expression: The expression to evaluate.
    :param dict dictionary: A ``dict`` mapping names to Joy functions.
    :rtype: (stack, (), dictionary)

    '''
    quote, stack = get_n_items(1, stack)
    isnt_stack(quote)
    history = []
    append = history.append
    local_expr = push_quote(quote)
    while local_expr:
        append((stack, local_expr))
        term, local_expr = next_term(local_expr)
        if isinstance(term, Symbol):
            try:
                func = dictionary[term]
            except KeyError:
                print(trace_to_string(history))
                raise UnknownSymbolError(term) from None
            stack, local_expr, dictionary = func(stack, local_expr, dictionary)
        else:
            stack = term, stack
    append((stack, local_expr))
    print(trace_to_string(history))
    return stack, expr, dictionary


def trace_to_string(history):
    max_stack_length = 0
    lines = []
    for stack, expression in history:
        stack = stack_to_string(stack)
        expression = expr_to_string(expression)
        length = len(stack)
        max_stack_length = max(max_stack_length, length)
        lines.append((length, stack, expression))
    return '\n'.join(
        # Prefix spaces to line up '•'s.
        (' ' * (max_stack_length - length) + f'{stack} • {expression}')
        for i, (length, stack, expression) in enumerate(lines)
        )


S_swaack = Symbol('swaack')
S_genrec = Symbol('genrec')
S_ifte = Symbol('ifte')
S_infra = Symbol('infra')
S_first = Symbol('first')
S_primrec = Symbol('primrec')
S_choice = Symbol('choice')
S_i = Symbol('i')
S_cond = Symbol('cond')
S_step = Symbol('step')
S_times = Symbol('times')

_ifte_ = (S_infra, (S_first, (S_choice, (S_i, ()))))


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


@inscribe
def inscribe_(stack, expression, dictionary):
    '''
    Create a new Joy function definition in the Joy dictionary.  A
    definition is given as a quote with a name followed by a Joy
    expression. for example:

        [sqr dup mul] inscribe

    '''
    (name, body), stack = stack
    inscribe(Def(name, body), dictionary)
    return stack, expression, dictionary


@inscribe
@SimpleFunctionWrapper
def getitem(stack):
    '''
    ::

        getitem == drop first

    Expects an integer and a quote on the stack and returns the item at the
    nth position in the quote counting from 0.
    ::

           [a b c d] 0 getitem
        -------------------------
            a

    '''
    n, (Q, stack) = stack
    return pick(Q, n), stack


@inscribe
@SimpleFunctionWrapper
def drop(stack):
    '''
    ::

        drop == [rest] times

    Expects an integer and a quote on the stack and returns the quote with
    n items removed off the top.
    ::

           [a b c d] 2 drop
        ----------------------
               [c d]

    '''
    n, (Q, stack) = stack
    while n > 0:
        try:
            _, Q = Q
        except ValueError:
            raise StackUnderflowError
        n -= 1
    return Q, stack


@inscribe
@SimpleFunctionWrapper
def take(stack):
    '''
    Expects an integer and a quote on the stack and returns the quote with
    just the top n items in reverse order (because that's easier and you can
    use reverse if needed.)
    ::

           [a b c d] 2 take
        ----------------------
               [b a]

    '''
    n, (Q, stack) = stack
    x = ()
    while n > 0:
        try:
            item, Q = Q
        except ValueError:
            raise StackUnderflowError
        x = item, x
        n -= 1
    return x, stack


@inscribe
def gcd2(stack, expression, dictionary):
    '''Compiled GCD function.'''
    (v1, (v2, stack)) = stack
    tos = True
    while tos:
        v3 = v2 % v1
        tos = v3 > 0
        (v1, (v2, stack)) = (v3, (v1, stack))
    return (v2, stack), expression, dictionary


@inscribe
@SimpleFunctionWrapper
def choice(stack):
    '''
    Use a Boolean value to select one of two items.
    ::

           A B false choice
        ----------------------
           A


           A B true choice
        ---------------------
             B

    '''
    (if_, (then, (else_, stack))) = stack
    assert isinstance(if_, bool), repr(if_)
    return then if if_ else else_, stack


@inscribe
@SimpleFunctionWrapper
def select(stack):
    '''
    Use a Boolean value to select one of two items from a sequence.
    ::

           [A B] false select
        ------------------------
            A


           [A B] true select
        -----------------------
              B

    The sequence can contain more than two items but not fewer.
    Currently Python semantics are used to evaluate the "truthiness" of the
    Boolean value (so empty string, zero, etc. are counted as false, etc.)
    '''
    (flag, (choices, stack)) = stack
    (else_, (then, _)) = choices
    return then if flag else else_, stack


@inscribe
@SimpleFunctionWrapper
def max_(S):
    '''Given a list find the maximum.'''
    tos, stack = S
    return max(iter_stack(tos)), stack


@inscribe
@SimpleFunctionWrapper
def min_(S):
    '''Given a list find the minimum.'''
    tos, stack = S
    return min(iter_stack(tos)), stack


@inscribe
@SimpleFunctionWrapper
def sum_(S):
    '''
    Given a quoted sequence of numbers return the sum.
    ::

        sum == 0 swap [+] step

    '''
    tos, stack = S
    return sum(iter_stack(tos)), stack


@inscribe
@SimpleFunctionWrapper
def remove(S):
    '''
    Expects an item on the stack and a quote under it and removes that item
    from the the quote.  The item is only removed once.  If the list is
    empty or the item isn't in the list then the list is unchanged.
    ::

           [1 2 3 1] 1 remove
        ------------------------
             [2 3 1]

    '''
    (item, (quote, stack)) = S
    return _remove(item, quote), stack


def _remove(item, quote):
    try: head, tail = quote
    except ValueError: return quote
    return tail if head == item else (head, _remove(item, tail))


@inscribe
@SimpleFunctionWrapper
def unique(S):
    '''Given a list remove duplicate items.'''
    tos, stack = S
    I = list(iter_stack(tos))
    return list_to_stack(sorted(set(I), key=I.index)), stack


@inscribe
@SimpleFunctionWrapper
def sort_(S):
    '''Given a list return it sorted.'''
    tos, stack = S
    return list_to_stack(sorted(iter_stack(tos))), stack


@inscribe
@SimpleFunctionWrapper
def disenstacken(stack):
    '''
    The disenstacken operator expects a list on top of the stack and makes that
    the stack discarding the rest of the stack.
    '''
    return stack[0]


@inscribe
@SimpleFunctionWrapper
def reverse(S):
    '''
    Reverse the list on the top of the stack.
    ::

        reverse == [] swap shunt
    '''
    (tos, stack) = S
    res = ()
    for term in iter_stack(tos):
        res = term, res
    return res, stack


@inscribe
@SimpleFunctionWrapper
def shunt(stack):
    '''
    Like concat but reverses the top list into the second.
    ::

        shunt == [swons] step == reverse swap concat

           [a b c] [d e f] shunt
        ---------------------------
               [f e d a b c]

    '''
    (tos, (second, stack)) = stack
    while tos:
        term, tos = tos
        second = term, second
    return second, stack


@inscribe
@SimpleFunctionWrapper
def zip_(S):
    '''
    Replace the two lists on the top of the stack with a list of the pairs
    from each list.  The smallest list sets the length of the result list.
    '''
    (tos, (second, stack)) = S
    accumulator = [
        (a, (b, ()))
        for a, b in zip(iter_stack(tos), iter_stack(second))
        ]
    return list_to_stack(accumulator), stack


@inscribe
@SimpleFunctionWrapper
def succ(S):
    '''Increment TOS.'''
    (tos, stack) = S
    return tos + 1, stack


@inscribe
@SimpleFunctionWrapper
def pred(S):
    '''Decrement TOS.'''
    (tos, stack) = S
    return tos - 1, stack


@inscribe
@SimpleFunctionWrapper
def pm(stack):
    '''
    Plus or minus
    ::

           a b pm
        -------------
           a+b a-b

    '''
    a, (b, stack) = stack
    p, m, = b + a, b - a
    return m, (p, stack)


@inscribe
@SimpleFunctionWrapper
def divmod_(S):
    '''
    Similarly to pm ("Plus or minus") this function computes
    both the
    ::

              a b divmod
        ---------------------
           a b div a b mod
        ---------------------
                 q r

    Where: q * b + r == a

    '''
    y, (x, stack) = S
    q, r = divmod(x, y)
    return r, (q, stack)


@inscribe
def sharing(stack, expression, dictionary):
    '''Print redistribution information.'''
    print("You may convey verbatim copies of the Program's source code as"
    ' you receive it, in any medium, provided that you conspicuously'
    ' and appropriately publish on each copy an appropriate copyright'
    ' notice; keep intact all notices stating that this License and'
    ' any non-permissive terms added in accord with section 7 apply'
    ' to the code; keep intact all notices of the absence of any'
    ' warranty; and give all recipients a copy of this License along'
    ' with the Program.'
    ' You should have received a copy of the GNU General Public License'
    ' along with Thun.  If not see <http://www.gnu.org/licenses/>.')
    return stack, expression, dictionary


@inscribe
def warranty(stack, expression, dictionary):
    '''Print warranty information.'''
    print('THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY'
    ' APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE'
    ' COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM'
    ' "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR'
    ' IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES'
    ' OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE'
    ' ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS'
    ' WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE'
    ' COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.')
    return stack, expression, dictionary


@inscribe
def x(stack, expr, dictionary):
    '''
    ::

        x == dup i

        ... [Q] x = ... [Q] dup i
        ... [Q] x = ... [Q] [Q] i
        ... [Q] x = ... [Q]  Q

    '''
    quote, _ = stack
    isnt_stack(quote)
    return stack, push_quote(quote, expr), dictionary


@inscribe
def b(stack, expr, dictionary):
    '''
    ::

        b == [i] dip i

        ... [P] [Q] b == ... [P] i [Q] i
        ... [P] [Q] b == ... P Q

    '''
    q, (p, (stack)) = stack
    isnt_stack(q)
    isnt_stack(p)
    expr = push_quote(q, expr)
    expr = push_quote(p, expr)
    return stack, expr, dictionary


@inscribe
def ii(stack, expr, dictionary):
    '''
    ::

           ... a [Q] ii
        ------------------
            ... Q a Q

    '''
    quote, (a, stack) = stack
    isnt_stack(quote)
    expr = push_quote((a, quote), expr)
    expr = push_quote(quote, expr)
    return stack, expr, dictionary


@inscribe
def dupdip(stack, expr, dictionary):
    '''
    ::

        [F] dupdip == dup [F] dip

        ... a [F] dupdip
        ... a dup [F] dip
        ... a a   [F] dip
        ... a F a

    '''
    quote, stack = stack
    isnt_stack(quote)
    a = stack[0]
    expr = push_quote((a, ()), expr)
    expr = push_quote(quote, expr)
    return stack, expr, dictionary


@inscribe
def infra(stack, expr, dictionary):
    '''
    Accept a quoted program and a list on the stack and run the program
    with the list as its stack.  Does not affect the rest of the stack.
    ::

           ... [a b c] [Q] . infra
        -----------------------------
            c b a . Q [...] swaack

    '''
    quote, aggregate, stack = get_n_items(2, stack)
    isnt_stack(quote)
    isnt_stack(aggregate)
    expr = push_quote((stack, (S_swaack, ())), expr)
    expr = push_quote(quote, expr)
    return aggregate, expr, dictionary


@inscribe
def genrec(stack, expr, dictionary):
    '''
    General Recursion Combinator.
    ::

                  [if] [then] [rec1] [rec2] genrec
        ---------------------------------------------------------------------
           [if] [then] [rec1 [[if] [then] [rec1] [rec2] genrec] rec2] ifte

    From "Recursion Theory and Joy" (j05cmp.html) by Manfred von Thun:
    "The genrec combinator takes four program parameters in addition to
    whatever data parameters it needs. Fourth from the top is an if-part,
    followed by a then-part. If the if-part yields true, then the then-part
    is executed and the combinator terminates. The other two parameters are
    the rec1-part and the rec2-part. If the if-part yields false, the
    rec1-part is executed. Following that the four program parameters and
    the combinator are again pushed onto the stack bundled up in a quoted
    form. Then the rec2-part is executed, where it will find the bundled
    form. Typically it will then execute the bundled form, either with i or
    with app2, or some other combinator."

    The way to design one of these is to fix your base case [then] and the
    test [if], and then treat rec1 and rec2 as an else-part "sandwiching"
    a quotation of the whole function.

    For example, given a (general recursive) function 'F':
    ::

        F == [I] [T] [R1] [R2] genrec

    If the [I] if-part fails you must derive R1 and R2 from:
    ::

        ... R1 [F] R2

    Just set the stack arguments in front, and figure out what R1 and R2
    have to do to apply the quoted [F] in the proper way.  In effect, the
    genrec combinator turns into an ifte combinator with a quoted copy of
    the original definition in the else-part:
    ::

        F == [I] [T] [R1]   [R2] genrec
          == [I] [T] [R1 [F] R2] ifte

    Primitive recursive functions are those where R2 == i.
    ::

        P == [I] [T] [R] tailrec
          == [I] [T] [R [P] i] ifte
          == [I] [T] [R P] ifte

    '''
    rec2, rec1, then, if_, stack = get_n_items(4, stack)
    isnt_stack(if_)
    isnt_stack(then)
    isnt_stack(rec1)
    isnt_stack(rec2)
    F = (if_, (then, (rec1, (rec2, (S_genrec, ())))))
    else_ = concat(rec1, (F, rec2))
    stack = (else_, (then, (if_, stack)))
    expr = push_quote((S_ifte, ()), expr)
    return stack, expr, dictionary


@inscribe
def map_(stack, expr, dictionary):
    '''
    Run the quoted program on TOS on the items in the list under it, push a
    new list with the results in place of the program and original list.
    '''
    quote, aggregate, stack = get_n_items(2, stack)
    isnt_stack(quote)
    isnt_stack(aggregate)
    if not aggregate:
        return (aggregate, stack), expr, dictionary
    batch = ()
    for term in iter_stack(aggregate):
        s = term, stack
        batch = (s, (quote, (S_infra, (S_first, batch))))
    stack = (batch, ((), stack))
    expr = push_quote((S_infra, ()), expr)
    return stack, expr, dictionary


@inscribe
def primrec(stack, expr, dictionary):
    '''
    From the "Overview of the language JOY":

    > The primrec combinator expects two quoted programs in addition to a
    data parameter. For an integer data parameter it works like this: If
    the data parameter is zero, then the first quotation has to produce
    the value to be returned. If the data parameter is positive then the
    second has to combine the data parameter with the result of applying
    the function to its predecessor.::

        5  [1]  [*]  primrec

    > Then primrec tests whether the top element on the stack (initially
    the 5) is equal to zero. If it is, it pops it off and executes one of
    the quotations, the [1] which leaves 1 on the stack as the result.
    Otherwise it pushes a decremented copy of the top element and
    recurses. On the way back from the recursion it uses the other
    quotation, [*], to multiply what is now a factorial on top of the
    stack by the second element on the stack.::

        n [Base] [Recur] primrec

           0 [Base] [Recur] primrec
        ------------------------------
              Base

             n [Base] [Recur] primrec
        ------------------------------------------ n > 0
           n (n-1) [Base] [Recur] primrec Recur

    '''
    recur, base, n, stack = get_n_items(3, stack)
    isnt_stack(recur)
    isnt_stack(base)
    if n <= 0:
        expr = push_quote(base, expr)
    else:
        expr = push_quote(recur, expr)
        expr = push_quote((S_primrec, ()), expr)
        stack = recur, (base, (n - 1, (n, stack)))
    return stack, expr, dictionary


@inscribe
def ifte(stack, expr, dictionary):
    '''
    If-Then-Else Combinator


                ... [if] [then] [else] ifte
       -------------------------------------------------------
          ... [else] [then] [...] [if] infra first choice i


    Has the effect of grabbing a copy of the stack on which to run the
    if-part using infra.
    '''
    else_, then, if_, stack = get_n_items(3, stack)
    expr = push_quote(_ifte_, expr)
    stack = (if_, (stack, (then, (else_, stack))))
    return stack, expr, dictionary


@inscribe
def cond(stack, expr, dictionary):
    '''
    This combinator works like a case statement.  It expects a single quote
    on the stack that must contain zero or more condition quotes and a
    default quote.  Each condition clause should contain a quoted predicate
    followed by the function expression to run if that predicate returns
    true.  If no predicates return true the default function runs.

    It works by rewriting into a chain of nested `ifte` expressions, e.g.::

           [[D]] cond
        ----------------
             D

           [[[IF] THEN] [D]] cond
        ---------------------------- (with single condition, same as ifte)
            [IF] [THEN] [D] ifte


              [[[IF] THEN] ...] cond
        ----------------------------------- (multiple conditions)
           [IF] [THEN] [[...] cond] ifte


    The middle case isn't actually implemented.  It's implied by the
    base case and the "multiple conditions" case.
    '''
    conditions, stack = get_n_items(1, stack)
    isnt_stack(conditions)
    if not conditions:
        raise StackUnderflowError('cond without default clause')

    condition_clause, conditions = conditions
    isnt_stack(condition_clause)

    if not conditions: # This is the default clause, run it.
        expr = push_quote(condition_clause, expr)
    else:
        if_, then = get_n_items(1, condition_clause)
        isnt_stack(if_)
        else_ = (conditions, (S_cond, ()))
        stack = (else_, (then, (if_, stack)))
        expr = push_quote((S_ifte, ()), expr)

    return stack, expr, dictionary


@inscribe
def dipd(stack, expr, dictionary):
    '''
    The dipd combinator is like dip but expects two items.

           ... y x [Q] dipd
        ----------------------
             ... Q y x

    '''
    quote, x, y, stack = get_n_items(3, stack)
    isnt_stack(quote)
    expr = push_quote((y, (x, ())), expr)
    expr = push_quote(quote, expr)
    return stack, expr, dictionary


@inscribe
def dipdd(stack, expr, dictionary):
    '''
    The dipdd combinator is like dip but expects three items.

           ... y x z [Q] dipdd
        -------------------------
             ... Q y x z

    '''
    quote, x, y, z, stack = get_n_items(3, stack)
    isnt_stack(quote)
    expr = push_quote((z, (y, (x, ()))), expr)
    expr = push_quote(quote, expr)
    return stack, expr, dictionary


@inscribe
def cmp_(stack, expr, dictionary):
    '''
    cmp takes two values and three quoted programs on the stack and runs
    one of the three depending on the results of comparing the two values:
    ::

           a b [G] [E] [L] cmp
        ------------------------- a > b
            G

           a b [G] [E] [L] cmp
        ------------------------- a = b
                E

           a b [G] [E] [L] cmp
        ------------------------- a < b
                L
    '''
    L, E, G, b, a, stack = get_n_items(5, stack)
    isnt_stack(L)
    isnt_stack(E)
    isnt_stack(G)
    isnt_int(b)
    isnt_int(a)
    expr = push_quote(G if a > b else L if a < b else E, expr)
    return stack, expr, dictionary


@inscribe
def step(stack, expr, dictionary):
    '''
    Run a quoted program on each item in a sequence.
    ::

           ... [] [Q] step
        ---------------------
                 ...


           ... [a] [Q] step
        ----------------------
               ... a Q


            ... [a b c] [Q] step
        ----------------------------
           ... a Q [b c] [Q] step

    The step combinator executes the quotation on each member of the list
    on top of the stack.
    '''
    quote, aggregate, stack = get_n_items(2, stack)
    isnt_stack(quote)
    isnt_stack(aggregate)
    if not aggregate:
        return stack, expr, dictionary

    head, tail = aggregate
    stack = head, stack
    if tail:
        expr = push_quote((tail, (quote, (S_step, ()))), expr)
    expr = push_quote(quote, expr)
    return stack, expr, dictionary


@inscribe
def times(stack, expr, dictionary):
    '''
    times == [-- dip] cons [swap] infra [0 >] swap while pop
    ::

           ... n [Q] times
        ---------------------  w/ n <= 0
                 ...


           ... 1 [Q] times
        ---------------------
                ... Q


              ... n [Q] times
        ---------------------------  w/ n > 1
           ... Q (n-1) [Q] times

    '''
    # times == [-- dip] cons [swap] infra [0 >] swap while pop
    quote, n, stack = get_n_items(2, stack)
    isnt_stack(quote)
    isnt_int(n)
    if n <= 0:
        return stack, expr, dictionary
    n -= 1
    if n:
        expr = push_quote((n, (quote, (S_times, ()))), expr)
    expr = push_quote(quote, expr)
    return stack, expr, dictionary


@inscribe
@SimpleFunctionWrapper
def _Tree_add_Ee(stack):
  """
  ::

    ([a4 a5 ...1] a3 a2 a1 -- [a2 a3 ...1])

  """
  (a1, (a2, (a3, ((a4, (a5, s1)), s2)))) = stack
  return ((a2, (a3, s1)), s2)


@inscribe
@SimpleFunctionWrapper
def _Tree_delete_R0(stack):
  """
  ::

    ([a2 ...1] a1 -- [a2 ...1] a2 a1 a1)

  """
  (a1, ((a2, s1), s2)) = stack
  return (a1, (a1, (a2, ((a2, s1), s2))))


@inscribe
@SimpleFunctionWrapper
def _Tree_delete_clear_stuff(stack):
  """
  ::

    (a3 a2 [a1 ...1] -- [...1])

  """
  ((a1, s1), (a2, (a3, s2))) = stack
  return (s1, s2)


@inscribe
@SimpleFunctionWrapper
def _Tree_get_E(stack):
  """
  ::

    ([a3 a4 ...1] a2 a1 -- a4)

  """
  (a1, (a2, ((a3, (a4, s1)), s2))) = stack
  return (a4, s2)


@inscribe
@SimpleFunctionWrapper
def ccons(stack):
  """
  ::

    (a2 a1 [...1] -- [a2 a1 ...1])

  """
  (s1, (a1, (a2, s2))) = stack
  return ((a2, (a1, s1)), s2)


##def cons(stack):
##  """
##  ::
##
##    (a1 [...0] -- [a1 ...0])
##
##  """
##  try: s0, stack = stack
##  except ValueError: raise StackUnderflowError('Not enough values on stack.')
##  if not isinstance(s0, tuple): raise NotAListError('Not a list.')
##  try: a1, s23 = stack
##  except ValueError: raise StackUnderflowError('Not enough values on stack.')
##  return ((a1, s0), s23)


##def dup(stack):
##  """
##  ::
##
##    (a1 -- a1 a1)
##
##  """
##  (a1, s23) = stack
##  return (a1, (a1, s23))


@inscribe
@SimpleFunctionWrapper
def dupd(stack):
  """
  ::

    (a2 a1 -- a2 a2 a1)

  """
  (a1, (a2, s23)) = stack
  return (a1, (a2, (a2, s23)))


@inscribe
@SimpleFunctionWrapper
def dupdd(stack):
  """
  ::

    (a3 a2 a1 -- a3 a3 a2 a1)

  """
  (a1, (a2, (a3, s23))) = stack
  return (a1, (a2, (a3, (a3, s23))))


##def first(stack):
##  """
##  ::
##
##    ([a1 ...1] -- a1)
##
##  """
##  ((a1, s1), s23) = stack
##  return (a1, s23)


@inscribe
@SimpleFunctionWrapper
def first_two(stack):
  """
  ::

    ([a1 a2 ...1] -- a1 a2)

  """
  ((a1, (a2, s1)), s2) = stack
  return (a2, (a1, s2))


@inscribe
@SimpleFunctionWrapper
def fourth(stack):
  """
  ::

    ([a1 a2 a3 a4 ...1] -- a4)

  """
  ((a1, (a2, (a3, (a4, s1)))), s2) = stack
  return (a4, s2)


@inscribe
@SimpleFunctionWrapper
def over(stack):
  """
  ::

    (a2 a1 -- a2 a1 a2)

  """
  (a1, (a2, s23)) = stack
  return (a2, (a1, (a2, s23)))


##def pop(stack):
##  """
##  ::
##
##    (a1 --)
##
##  """
##  try:
##    (a1, s23) = stack
##  except ValueError:
##    raise StackUnderflowError('Cannot pop empty stack.')
##  return s23


@inscribe
@SimpleFunctionWrapper
def popd(stack):
  """
  ::

    (a2 a1 -- a1)

  """
  (a1, (a2, s23)) = stack
  return (a1, s23)


@inscribe
@SimpleFunctionWrapper
def popdd(stack):
  """
  ::

    (a3 a2 a1 -- a2 a1)

  """
  (a1, (a2, (a3, s23))) = stack
  return (a1, (a2, s23))


@inscribe
@SimpleFunctionWrapper
def popop(stack):
  """
  ::

    (a2 a1 --)

  """
  (a1, (a2, s23)) = stack
  return s23


@inscribe
@SimpleFunctionWrapper
def popopd(stack):
  """
  ::

    (a3 a2 a1 -- a1)

  """
  (a1, (a2, (a3, s23))) = stack
  return (a1, s23)


@inscribe
@SimpleFunctionWrapper
def popopdd(stack):
  """
  ::

    (a4 a3 a2 a1 -- a2 a1)

  """
  (a1, (a2, (a3, (a4, s23)))) = stack
  return (a1, (a2, s23))


##def rest(stack):
##  """
##  ::
##
##    ([a1 ...0] -- [...0])
##
##  """
##  try:
##    s0, stack = stack
##  except ValueError:
##    raise StackUnderflowError
##  if not isinstance(s0, tuple):
##    raise NotAListError('Not a list.')
##  try:
##    _, s1 = s0
##  except ValueError:
##    raise StackUnderflowError('Cannot take rest of empty list.')
##  return (s1, stack)


@inscribe
@SimpleFunctionWrapper
def rolldown(stack):
  """
  ::

    (a1 a2 a3 -- a2 a3 a1)

  """
  (a3, (a2, (a1, s23))) = stack
  return (a1, (a3, (a2, s23)))


@inscribe
@SimpleFunctionWrapper
def rollup(stack):
  """
  ::

    (a1 a2 a3 -- a3 a1 a2)

  """
  (a3, (a2, (a1, s23))) = stack
  return (a2, (a1, (a3, s23)))


@inscribe
@SimpleFunctionWrapper
def rrest(stack):
  """
  ::

    ([a1 a2 ...1] -- [...1])

  """
  ((a1, (a2, s1)), s2) = stack
  return (s1, s2)


@inscribe
@SimpleFunctionWrapper
def second(stack):
  """
  ::

    ([a1 a2 ...1] -- a2)

  """
  ((a1, (a2, s1)), s2) = stack
  return (a2, s2)


@inscribe
@SimpleFunctionWrapper
def stack(stack):
  """
  ::

    (... -- ... [...])

  """
  s0 = stack
  return (s0, s0)


@inscribe
@SimpleFunctionWrapper
def stuncons(stack):
  """
  ::

    (... a1 -- ... a1 a1 [...])

  """
  (a1, s1) = stack
  return (s1, (a1, (a1, s1)))


@inscribe
@SimpleFunctionWrapper
def stununcons(stack):
  """
  ::

    (... a2 a1 -- ... a2 a1 a1 a2 [...])

  """
  (a1, (a2, s1)) = stack
  return (s1, (a2, (a1, (a1, (a2, s1)))))


##def swaack(stack):
##  """
##  ::
##
##    ([...1] -- [...0])
##
##  """
##  try:
##    (s1, s0) = stack
##  except ValueError:
##    raise StackUnderflowError('Not enough values on stack.')
##  if not isinstance(s1, tuple):
##    raise NotAListError('Not a list.')
##  return (s0, s1)


##def swap(stack):
##  """
##  ::
##
##    (a1 a2 -- a2 a1)
##
##  """
##  try:
##    (a2, (a1, s23)) = stack
##  except ValueError:
##    raise StackUnderflowError('Not enough values on stack.')
##  return (a1, (a2, s23))


@inscribe
@SimpleFunctionWrapper
def swons(stack):
  """
  ::

    ([...1] a1 -- [a1 ...1])

  """
  (a1, (s1, s2)) = stack
  return ((a1, s1), s2)


@inscribe
@SimpleFunctionWrapper
def third(stack):
  """
  ::

    ([a1 a2 a3 ...1] -- a3)

  """
  ((a1, (a2, (a3, s1))), s2) = stack
  return (a3, s2)


@inscribe
@SimpleFunctionWrapper
def tuck(stack):
  """
  ::

    (a2 a1 -- a1 a2 a1)

  """
  (a1, (a2, s23)) = stack
  return (a1, (a2, (a1, s23)))


@inscribe
@SimpleFunctionWrapper
def uncons(stack):
  """
  ::

    ([a1 ...0] -- a1 [...0])

  """
  ((a1, s0), s23) = stack
  return (s0, (a1, s23))


@inscribe
@SimpleFunctionWrapper
def unit(stack):
  """
  ::

    (a1 -- [a1 ])

  """
  (a1, s23) = stack
  return ((a1, ()), s23)


@inscribe
@SimpleFunctionWrapper
def unswons(stack):
  """
  ::

    ([a1 ...1] -- [...1] a1)

  """
  ((a1, s1), s2) = stack
  return (a1, (s1, s2))


def default_defs(dictionary):
    Def.load_definitions(__doc__.splitlines(), dictionary)


if __name__ == '__main__':
    import sys

    J = interp if '-q' in sys.argv else repl
    dictionary = initialize()
    default_defs(dictionary)
    try:
        stack = J(dictionary=dictionary)
    except SystemExit:
        pass
##    jcode = "5 10 [>][++][*]ifte"
##    jcode = '1 2 [[+]] cond'
##    jcode = '1 2 [[[>] -] [[<] +] [*]] cond'
##    jcode = '2 1 [[[>] -] [[<] +] [*]] cond'
##    jcode = '3 3 [[[>] -] [[<] +] [*]] cond'
##    jcode = '3 dup [dup mul] times'
##    jcode = '0 [1 2 3] [add] step'
##    stack, _ = run(jcode, (), dictionary)
    print(stack_to_string(stack), '•')
