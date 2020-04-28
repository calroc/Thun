# -*- coding: utf-8 -*-
#
#    Copyright © 2014, 2015, 2017, 2018 Simon Forman
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
This module contains the Joy function infrastructure and a library of
functions.  Its main export is a Python function initialize() that
returns a dictionary of Joy functions suitable for use with the joy()
function.
'''
from __future__ import print_function
from builtins import map, object, range, zip
from logging import getLogger

_log = getLogger(__name__)
_log.info('Loading library.')

from inspect import getdoc
from functools import wraps
from itertools import count
from inspect import getmembers, isfunction
import operator, math

from .parser import text_to_expression, Symbol
from .utils.stack import expression_to_string, list_to_stack, iter_stack, pick, concat
import sys
if sys.version_info.major < 3:
	from .utils.brutal_hackery import rename_code_object
else:
	rename_code_object = lambda _: lambda f: f

from .utils import generated_library as genlib
from .utils.types import (
	compose,
	ef,
	stack_effect,
	AnyJoyType,
	AnyStarJoyType,
	BooleanJoyType,
	NumberJoyType,
	NumberStarJoyType,
	StackJoyType,
	StackStarJoyType,
	FloatJoyType,
	IntJoyType,
	SymbolJoyType,
	CombinatorJoyType,
	TextJoyType,
	_functions,
	FUNCTIONS,
	infer,
	infer_expression,
	JoyTypeError,
	combinator_effect,
	poly_combinator_effect,
	doc_from_stack_effect,
	)


HELP_TEMPLATE = '''\

==== Help on %s ====

%s

---- end (%s)
'''


_SYM_NUMS = lambda c=count(): next(c)
_COMB_NUMS = lambda c=count(): next(c)


_R = list(range(10))
A = a0, a1, a2, a3, a4, a5, a6, a7, a8, a9 = list(map(AnyJoyType, _R))
B = b0, b1, b2, b3, b4, b5, b6, b7, b8, b9 = list(map(BooleanJoyType, _R))
N = n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 = list(map(NumberJoyType, _R))
S = s0, s1, s2, s3, s4, s5, s6, s7, s8, s9 = list(map(StackJoyType, _R))
F = f0, f1, f2, f3, f4, f5, f6, f7, f8, f9 = list(map(FloatJoyType, _R))
I = i0, i1, i2, i3, i4, i5, i6, i7, i8, i9 = list(map(IntJoyType, _R))
T = t0, t1, t2, t3, t4, t5, t6, t7, t8, t9 = list(map(TextJoyType, _R))


_R = list(range(1, 11))
As = list(map(AnyStarJoyType, _R))
Ns = list(map(NumberStarJoyType, _R))
Ss = list(map(StackStarJoyType, _R))


sec0 = stack_effect(t1)()
sec1 = stack_effect(s0, i1)(s1)
sec2 = stack_effect(s0, i1)(a1)
sec_binary_cmp = stack_effect(n1, n2)(b1)
sec_binary_ints = stack_effect(i1, i2)(i3)
sec_binary_logic = stack_effect(b1, b2)(b3)
sec_binary_math = stack_effect(n1, n2)(n3)
sec_unary_logic = stack_effect(a1)(b1)
sec_unary_math = stack_effect(n1)(n2)
sec_Ns_math = stack_effect((Ns[1], s1),)(n0)

_dictionary = {}


def inscribe(function):
	'''A decorator to inscribe functions into the default dictionary.'''
	_dictionary[function.name] = function
	return function


def initialize():
	'''Return a dictionary of Joy functions for use with joy().'''
	return _dictionary.copy()


ALIASES = (
	('add', ['+']),
	('and', ['&']),
	('bool', ['truthy']),
	('mul', ['*']),
	('floordiv', ['/floor', '//']),
	('floor', ['round']),
	('truediv', ['/', 'div']),
	('mod', ['%', 'rem', 'remainder', 'modulus']),
	('eq', ['=']),
	('ge', ['>=']),
	('getitem', ['pick', 'at']),
	('gt', ['>']),
	('le', ['<=']),
	('lshift', ['<<']),
	('lt', ['<']),
	('ne', ['<>', '!=']),
	('rshift', ['>>']),
	('sub', ['-']),
	('xor', ['^']),
	('succ', ['++']),
	('pred', ['--']),
	('rolldown', ['roll<']),
	('rollup', ['roll>']),
	('eh', ['?']),
	('id', [u'•']),
	)


def add_aliases(D, A):
	'''
	Given a dict and a iterable of (name, [alias, ...]) pairs, create
	additional entries in the dict mapping each alias to the named function
	if it's in the dict.  Aliases for functions not in the dict are ignored.
	'''
	for name, aliases in A:
		try:
			F = D[name]
		except KeyError:
			continue
		for alias in aliases:
			D[alias] = F


def yin_functions():
	'''
	Return a dict of named stack effects.

	"Yin" functions are those that only rearrange items in stacks and
	can be defined completely by their stack effects.  This means they
	can be auto-compiled.
	'''
	# pylint: disable=unused-variable
	cons = ef(a1, s0)((a1, s0))
	ccons = compose(cons, cons)
	dup = ef(a1)(a1, a1)
	dupd = ef(a2, a1)(a2, a2, a1)
	dupdd = ef(a3, a2, a1)(a3, a3, a2, a1)
	first = ef((a1, s1),)(a1,)
	over = ef(a2, a1)(a2, a1, a2)
	pop = ef(a1)()
	popd = ef(a2, a1,)(a1)
	popdd = ef(a3, a2, a1,)(a2, a1,)
	popop = ef(a2, a1,)()
	popopd = ef(a3, a2, a1,)(a1)
	popopdd = ef(a4, a3, a2, a1,)(a2, a1)
	rest = ef((a1, s0),)(s0,)
	rolldown = ef(a1, a2, a3)(a2, a3, a1)
	rollup = ef(a1, a2, a3)(a3, a1, a2)
	rrest = compose(rest, rest)
	second = compose(rest, first)
	stack = s0, (s0, s0)
	swaack = (s1, s0), (s0, s1)
	swap = ef(a1, a2)(a2, a1)
	swons = compose(swap, cons)
	third = compose(rest, second)
	tuck = ef(a2, a1)(a1, a2, a1)
	uncons = ef((a1, s0),)(a1, s0)
	unswons = compose(uncons, swap)
	stuncons = compose(stack, uncons)
	stununcons = compose(stack, uncons, uncons)
	unit = ef(a1)((a1, ()))

	first_two = compose(uncons, uncons, pop)
	fourth = compose(rest, third)

	_Tree_add_Ee = compose(pop, swap, rolldown, rrest, ccons)
	_Tree_get_E = compose(popop, second)
	_Tree_delete_clear_stuff = compose(rollup, popop, rest)
	_Tree_delete_R0 = compose(over, first, swap, dup)

	return locals()


definitions = ('''\
? == dup truthy
*fraction == [uncons] dip uncons [swap] dip concat [*] infra [*] dip cons
*fraction0 == concat [[swap] dip * [*] dip] infra
anamorphism == [pop []] swap [dip swons] genrec
average == [sum 1.0 *] [size] cleave /
binary == nullary [popop] dip
cleave == fork [popd] dip
codireco == cons dip rest cons
dinfrirst == dip infra first
unstack == ? [uncons ?] loop pop
down_to_zero == [0 >] [dup --] while
dupdipd == dup dipd
enstacken == stack [clear] dip
flatten == [] swap [concat] step
fork == [i] app2
gcd == 1 [tuck modulus dup 0 >] loop pop
ifte == [nullary not] dipd branch
ii == [dip] dupdip i
least_fraction == dup [gcd] infra [div] concat map
make_generator == [codireco] ccons
nullary == [stack] dinfrirst
of == swap at
pam == [i] map
tailrec == [i] genrec
product == 1 swap [*] step
quoted == [unit] dip
range == [0 <=] [1 - dup] anamorphism
range_to_zero == unit [down_to_zero] infra
run == [] swap infra
size == 0 swap [pop ++] step
sqr == dup mul
step_zero == 0 roll> step
swoncat == swap concat
ternary == unary [popop] dip
unary == nullary popd
unquoted == [i] dip
while == swap [nullary] cons dup dipd concat loop
'''
#
#
# ifte == [nullary] dipd swap branch
# genrec == [[genrec] cons cons cons cons] nullary swons concat ifte

# Another definition for while. FWIW
# while == over [[i] dip nullary] ccons [nullary] dip loop

##ccons == cons cons
##unit == [] cons
##second == rest first
##third == rest rest first
##swons == swap cons

##Zipper
##z-down == [] swap uncons swap
##z-up == swons swap shunt
##z-right == [swons] cons dip uncons swap
##z-left == swons [uncons swap] dip swap

##Quadratic Formula
##divisor == popop 2 *
##minusb == pop neg
##radical == swap dup * rollup * 4 * - sqrt
##root1 == + swap /
##root2 == - swap /
##q0 == [[divisor] [minusb] [radical]] pam
##q1 == [[root1] [root2]] pam
##quadratic == [q0] ternary i [q1] ternary

# Project Euler
##'''\
##PE1.1 == + dup [+] dip
##PE1.2 == dup [3 & PE1.1] dip 2 >>
##PE1.3 == 14811 swap [PE1.2] times pop
##PE1 == 0 0 66 [7 PE1.3] times 4 PE1.3 pop
##'''
#PE1.2 == [PE1.1] step
#PE1 == 0 0 66 [[3 2 1 3 1 2 3] PE1.2] times [3 2 1 3] PE1.2 pop
)


def FunctionWrapper(f):
	'''Set name attribute.'''
	if not f.__doc__:
		raise ValueError('Function %s must have doc string.' % f.__name__)
	f.name = f.__name__.rstrip('_')  # Don't shadow builtins.
	return f


def SimpleFunctionWrapper(f):
	'''
	Wrap functions that take and return just a stack.
	'''
	@FunctionWrapper
	@wraps(f)
	@rename_code_object(f.__name__)
	def inner(stack, expression, dictionary):
		return f(stack), expression, dictionary
	return inner


def BinaryBuiltinWrapper(f):
	'''
	Wrap functions that take two arguments and return a single result.
	'''
	@FunctionWrapper
	@wraps(f)
	@rename_code_object(f.__name__)
	def inner(stack, expression, dictionary):
		(a, (b, stack)) = stack
		result = f(b, a)
		return (result, stack), expression, dictionary
	return inner


def UnaryBuiltinWrapper(f):
	'''
	Wrap functions that take one argument and return a single result.
	'''
	@FunctionWrapper
	@wraps(f)
	@rename_code_object(f.__name__)
	def inner(stack, expression, dictionary):
		(a, stack) = stack
		result = f(a)
		return (result, stack), expression, dictionary
	return inner


class DefinitionWrapper(object):
	'''
	Provide implementation of defined functions, and some helper methods.
	'''

	def __init__(self, name, body_text, doc=None):
		self.name = self.__name__ = name
		self.body = text_to_expression(body_text)
		self._body = tuple(iter_stack(self.body))
		self.__doc__ = doc or body_text
		self._compiled = None

	def __call__(self, stack, expression, dictionary):
		if self._compiled:
			return self._compiled(stack, expression, dictionary)  # pylint: disable=E1102
		expression = list_to_stack(self._body, expression)
		return stack, expression, dictionary

	@classmethod
	def parse_definition(class_, defi):
		'''
		Given some text describing a Joy function definition parse it and
		return a DefinitionWrapper.
		'''
		name, proper, body_text = (n.strip() for n in defi.partition('=='))
		if not proper:
			raise ValueError('Definition %r failed' % (defi,))
		return class_(name, body_text)

	@classmethod
	def add_definitions(class_, defs, dictionary):
		'''
		Scan multi-line string defs for definitions and add them to the
		dictionary.
		'''
		for definition in _text_to_defs(defs):
			class_.add_def(definition, dictionary)

	@classmethod
	def add_def(class_, definition, dictionary, fail_fails=False):
		'''
		Add the definition to the dictionary.
		'''
		F = class_.parse_definition(definition)
		_log.info('Adding definition %s := %s', F.name, expression_to_string(F.body))
		dictionary[F.name] = F

	@classmethod
	def load_definitions(class_, filename, dictionary):
		with open(filename) as f:
			lines = [line for line in f if '==' in line]
		for line in lines:
			class_.add_def(line, dictionary)


def _text_to_defs(text):
	return (line.strip() for line in text.splitlines() if '==' in line)


#
# Functions
#


@inscribe
@sec0
@FunctionWrapper
def inscribe_(stack, expression, dictionary):
	'''
	Create a new Joy function definition in the Joy dictionary.  A
	definition is given as a string with a name followed by a double
	equal sign then one or more Joy functions, the body. for example:

			sqr == dup mul

	If you want the definition to persist over restarts, enter it into
	the definitions.txt resource.
	'''
	definition, stack = stack
	DefinitionWrapper.add_def(definition, dictionary, fail_fails=True)
	return stack, expression, dictionary


@inscribe
@SimpleFunctionWrapper
def parse(stack):
	'''Parse the string on the stack to a Joy expression.'''
	text, stack = stack
	expression = text_to_expression(text)
	return expression, stack


@inscribe
@SimpleFunctionWrapper
def infer_(stack):
	'''Attempt to infer the stack effect of a Joy expression.'''
	E, stack = stack
	effects = infer_expression(E)
	e = list_to_stack([(fi, (fo, ())) for fi, fo in effects])
	return e, stack


@inscribe
@sec2
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
@sec1
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
			raise IndexError
		n -= 1
	return Q, stack


@inscribe
@sec1
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
			raise IndexError
		x = item, x
		n -= 1
	return x, stack


@inscribe
@SimpleFunctionWrapper
def choice(stack):
	'''
	Use a Boolean value to select one of two items.
	::

				A B False choice
		 ----------------------
							 A


				A B True choice
		 ---------------------
							 B

	Currently Python semantics are used to evaluate the "truthiness" of the
	Boolean value (so empty string, zero, etc. are counted as false, etc.)
	'''
	(if_, (then, (else_, stack))) = stack
	return then if if_ else else_, stack


@inscribe
@SimpleFunctionWrapper
def select(stack):
	'''
	Use a Boolean value to select one of two items from a sequence.
	::

				[A B] False select
		 ------------------------
								A


				[A B] True select
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
@sec_Ns_math
@SimpleFunctionWrapper
def max_(S):
	'''Given a list find the maximum.'''
	tos, stack = S
	return max(iter_stack(tos)), stack


@inscribe
@sec_Ns_math
@SimpleFunctionWrapper
def min_(S):
	'''Given a list find the minimum.'''
	tos, stack = S
	return min(iter_stack(tos)), stack


@inscribe
@sec_Ns_math
@SimpleFunctionWrapper
def sum_(S):
	'''Given a quoted sequence of numbers return the sum.

	sum == 0 swap [+] step
	'''
	tos, stack = S
	return sum(iter_stack(tos)), stack


@inscribe
@SimpleFunctionWrapper
def remove(S):
	'''
	Expects an item on the stack and a quote under it and removes that item
	from the the quote.  The item is only removed once.
	::

			 [1 2 3 1] 1 remove
		------------------------
						[2 3 1]

	'''
	(tos, (second, stack)) = S
	l = list(iter_stack(second))
	l.remove(tos)
	return list_to_stack(l), stack


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


_functions['clear'] = s0, s1
@inscribe
@SimpleFunctionWrapper
def clear(stack):
	'''Clear everything from the stack.
	::

		clear == stack [pop stack] loop

			 ... clear
		---------------

	'''
	return ()


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
	'''Reverse the list on the top of the stack.
	::

		reverse == [] swap shunt
	'''
	(tos, stack) = S
	res = ()
	for term in iter_stack(tos):
		res = term, res
	return res, stack


@inscribe
@combinator_effect(_COMB_NUMS(), s7, s6)
@SimpleFunctionWrapper
def concat_(S):
	'''Concatinate the two lists on the top of the stack.
	::

			 [a b c] [d e f] concat
		----------------------------
					 [a b c d e f]

'''
	(tos, (second, stack)) = S
	return concat(second, tos), stack


@inscribe
@SimpleFunctionWrapper
def shunt(stack):
	'''Like concat but reverses the top list into the second.
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
@sec_unary_math
@SimpleFunctionWrapper
def succ(S):
	'''Increment TOS.'''
	(tos, stack) = S
	return tos + 1, stack


@inscribe
@sec_unary_math
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


def floor(n):
	return int(math.floor(n))

floor.__doc__ = math.floor.__doc__


@inscribe
@SimpleFunctionWrapper
def divmod_(S):
	'''
	divmod(x, y) -> (quotient, remainder)

	Return the tuple (x//y, x%y).  Invariant: div*y + mod == x.
	'''
	a, (b, stack) = S
	d, m = divmod(a, b)
	return d, (m, stack)


def sqrt(a):
	'''
	Return the square root of the number a.
	Negative numbers return complex roots.
	'''
	try:
		r = math.sqrt(a)
	except ValueError:
		assert a < 0, repr(a)
		r = math.sqrt(-a) * 1j
	return r


#def execute(S):
#  (text, stack) = S
#  if isinstance(text, str):
#    return run(text, stack)
#  return stack


@inscribe
@SimpleFunctionWrapper
def id_(stack):
	'''The identity function.'''
	return stack


@inscribe
@SimpleFunctionWrapper
def void(stack):
	'''True if the form on TOS is void otherwise False.'''
	form, stack = stack
	return _void(form), stack


def _void(form):
	return any(not _void(i) for i in iter_stack(form))



##  transpose
##  sign
##  take


@inscribe
@FunctionWrapper
def words(stack, expression, dictionary):
	'''Print all the words in alphabetical order.'''
	print(' '.join(sorted(dictionary)))
	return stack, expression, dictionary


@inscribe
@FunctionWrapper
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
@FunctionWrapper
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


# def simple_manual(stack):
#   '''
#   Print words and help for each word.
#   '''
#   for name, f in sorted(FUNCTIONS.items()):
#     d = getdoc(f)
#     boxline = '+%s+' % ('-' * (len(name) + 2))
#     print('\n'.join((
#       boxline,
#       '| %s |' % (name,),
#       boxline,
#       d if d else '   ...',
#       '',
#       '--' * 40,
#       '',
#       )))
#   return stack


@inscribe
@FunctionWrapper
def help_(S, expression, dictionary):
	'''Accepts a quoted symbol on the top of the stack and prints its docs.'''
	((symbol, _), stack) = S
	word = dictionary[symbol]
	print(HELP_TEMPLATE % (symbol, getdoc(word), symbol))
	return stack, expression, dictionary


#
# § Combinators
#


# Several combinators depend on other words in their definitions,
# we use symbols to prevent hard-coding these, so in theory, you
# could change the word in the dictionary to use different semantics.
S_choice = Symbol('choice')
S_first = Symbol('first')
S_getitem = Symbol('getitem')
S_genrec = Symbol('genrec')
S_loop = Symbol('loop')
S_i = Symbol('i')
S_ifte = Symbol('ifte')
S_infra = Symbol('infra')
S_pop = Symbol('pop')
S_step = Symbol('step')
S_times = Symbol('times')
S_swaack = Symbol('swaack')


@inscribe
@combinator_effect(_COMB_NUMS(), s1)
@FunctionWrapper
def i(stack, expression, dictionary):
	'''
	The i combinator expects a quoted program on the stack and unpacks it
	onto the pending expression for evaluation.
	::

			 [Q] i
		-----------
				Q

	'''
	quote, stack = stack
	return stack, concat(quote, expression), dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), s1)
@FunctionWrapper
def x(stack, expression, dictionary):
	'''
	::

		x == dup i

		... [Q] x = ... [Q] dup i
		... [Q] x = ... [Q] [Q] i
		... [Q] x = ... [Q]  Q

	'''
	quote, _ = stack
	return stack, concat(quote, expression), dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), s7, s6)
@FunctionWrapper
def b(stack, expression, dictionary):
	'''
	::

		b == [i] dip i

		... [P] [Q] b == ... [P] i [Q] i
		... [P] [Q] b == ... P Q

	'''
	q, (p, (stack)) = stack
	return stack, concat(p, concat(q, expression)), dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), a1, s1)
@FunctionWrapper
def dupdip(stack, expression, dictionary):
	'''
	::

		[F] dupdip == dup [F] dip

		... a [F] dupdip
		... a dup [F] dip
		... a a   [F] dip
		... a F a

	'''
	F, stack = stack
	a = stack[0]
	return stack, concat(F, (a,  expression)), dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), s7, s6)
@FunctionWrapper
def infra(stack, expression, dictionary):
	'''
	Accept a quoted program and a list on the stack and run the program
	with the list as its stack.  Does not affect the rest of the stack.
	::

			 ... [a b c] [Q] . infra
		-----------------------------
			 c b a . Q [...] swaack

	'''
	(quote, (aggregate, stack)) = stack
	return aggregate, concat(quote, (stack, (S_swaack, expression))), dictionary


@inscribe
#@combinator_effect(_COMB_NUMS(), s7, s6, s5, s4)
@FunctionWrapper
def genrec(stack, expression, dictionary):
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
	(rec2, (rec1, stack)) = stack
	(then, (if_, _)) = stack
	F = (if_, (then, (rec1, (rec2, (S_genrec, ())))))
	else_ = concat(rec1, (F, rec2))
	return (else_, stack), (S_ifte, expression), dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), s7, s6)
@FunctionWrapper
def map_(S, expression, dictionary):
	'''
	Run the quoted program on TOS on the items in the list under it, push a
	new list with the results in place of the program and original list.
	'''
#  (quote, (aggregate, stack)) = S
#  results = list_to_stack([
#    joy((term, stack), quote, dictionary)[0][0]
#    for term in iter_stack(aggregate)
#    ])
#  return (results, stack), expression, dictionary
	(quote, (aggregate, stack)) = S
	if not aggregate:
		return (aggregate, stack), expression, dictionary
	batch = ()
	for term in iter_stack(aggregate):
		s = term, stack
		batch = (s, (quote, (S_infra, (S_first, batch))))
	stack = (batch, ((), stack))
	return stack, (S_infra, expression), dictionary


#def cleave(S, expression, dictionary):
#  '''
#  The cleave combinator expects two quotations, and below that an item X.
#  It first executes [P], with X on top, and saves the top result element.
#  Then it executes [Q], again with X, and saves the top result.
#  Finally it restores the stack to what it was below X and pushes the two
#  results P(X) and Q(X).
#  '''
#  (Q, (P, (x, stack))) = S
#  p = joy((x, stack), P, dictionary)[0][0]
#  q = joy((x, stack), Q, dictionary)[0][0]
#  return (q, (p, stack)), expression, dictionary


def branch_true(stack, expression, dictionary):
	# pylint: disable=unused-variable
	(then, (else_, (flag, stack))) = stack
	return stack, concat(then, expression), dictionary


def branch_false(stack, expression, dictionary):
	# pylint: disable=unused-variable
	(then, (else_, (flag, stack))) = stack
	return stack, concat(else_, expression), dictionary


@inscribe
@poly_combinator_effect(_COMB_NUMS(), [branch_true, branch_false], b1, s7, s6)
@FunctionWrapper
def branch(stack, expression, dictionary):
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
	(then, (else_, (flag, stack))) = stack
	return stack, concat(then if flag else else_, expression), dictionary


#FUNCTIONS['branch'] = CombinatorJoyType('branch', [branch_true, branch_false], 100)


##@inscribe
##@FunctionWrapper
##def ifte(stack, expression, dictionary):
##  '''
##  If-Then-Else Combinator
##  ::
##
##                  ... [if] [then] [else] ifte
##       ---------------------------------------------------
##          ... [[else] [then]] [...] [if] infra select i
##
##
##
##
##                ... [if] [then] [else] ifte
##       -------------------------------------------------------
##          ... [else] [then] [...] [if] infra first choice i
##
##
##  Has the effect of grabbing a copy of the stack on which to run the
##  if-part using infra.
##  '''
##  (else_, (then, (if_, stack))) = stack
##  expression = (S_infra, (S_first, (S_choice, (S_i, expression))))
##  stack = (if_, (stack, (then, (else_, stack))))
##  return stack, expression, dictionary


@inscribe
@FunctionWrapper
def cond(stack, expression, dictionary):
	'''
	This combinator works like a case statement.  It expects a single quote
	on the stack that must contain zero or more condition quotes and a 
	default quote.  Each condition clause should contain a quoted predicate
	followed by the function expression to run if that predicate returns
	true.  If no predicates return true the default function runs.

	It works by rewriting into a chain of nested `ifte` expressions, e.g.::

						[[[B0] T0] [[B1] T1] [D]] cond
			-----------------------------------------
				 [B0] [T0] [[B1] [T1] [D] ifte] ifte

	'''
	conditions, stack = stack
	if conditions:
		expression = _cond(conditions, expression)
		try:
			# Attempt to preload the args to first ifte.
			(P, (T, (E, expression))) = expression
		except ValueError:
			# If, for any reason, the argument to cond should happen to contain
			# only the default clause then this optimization will fail.
			pass
		else:
			stack = (E, (T, (P, stack)))
	return stack, expression, dictionary


def _cond(conditions, expression):
	(clause, rest) = conditions
	if not rest:  # clause is [D]
		return clause
	P, T = clause
	return (P, (T, (_cond(rest, ()), (S_ifte, expression))))


@inscribe
@combinator_effect(_COMB_NUMS(), a1, s1)
@FunctionWrapper
def dip(stack, expression, dictionary):
	'''
	The dip combinator expects a quoted program on the stack and below it
	some item, it hoists the item into the expression and runs the program
	on the rest of the stack.
	::

			 ... x [Q] dip
		-------------------
				 ... Q x

	'''
	(quote, (x, stack)) = stack
	expression = (x, expression)
	return stack, concat(quote, expression), dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), a2, a1, s1)
@FunctionWrapper
def dipd(S, expression, dictionary):
	'''
	Like dip but expects two items.
	::

			 ... y x [Q] dip
		---------------------
				 ... Q y x

	'''
	(quote, (x, (y, stack))) = S
	expression = (y, (x, expression))
	return stack, concat(quote, expression), dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), a3, a2, a1, s1)
@FunctionWrapper
def dipdd(S, expression, dictionary):
	'''
	Like dip but expects three items.
	::

			 ... z y x [Q] dip
		-----------------------
				 ... Q z y x

	'''
	(quote, (x, (y, (z, stack)))) = S
	expression = (z, (y, (x, expression)))
	return stack, concat(quote, expression), dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), a1, s1)
@FunctionWrapper
def app1(S, expression, dictionary):
	'''
	Given a quoted program on TOS and anything as the second stack item run
	the program and replace the two args with the first result of the
	program.
	::

							... x [Q] . app1
		 -----------------------------------
				... [x ...] [Q] . infra first
	'''
	(quote, (x, stack)) = S
	stack = (quote, ((x, stack), stack))
	expression = (S_infra, (S_first, expression))
	return stack, expression, dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), a2, a1, s1)
@FunctionWrapper
def app2(S, expression, dictionary):
	'''Like app1 with two items.
	::

						... y x [Q] . app2
		 -----------------------------------
				... [y ...] [Q] . infra first
						[x ...] [Q]   infra first

	'''
	(quote, (x, (y, stack))) = S
	expression = (S_infra, (S_first,
		((x, stack), (quote, (S_infra, (S_first,
			expression))))))
	stack = (quote, ((y, stack), stack))
	return stack, expression, dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), a3, a2, a1, s1)
@FunctionWrapper
def app3(S, expression, dictionary):
	'''Like app1 with three items.
	::

						... z y x [Q] . app3
		 -----------------------------------
				... [z ...] [Q] . infra first
						[y ...] [Q]   infra first
						[x ...] [Q]   infra first

	'''
	(quote, (x, (y, (z, stack)))) = S
	expression = (S_infra, (S_first,
		((y, stack), (quote, (S_infra, (S_first,
		((x, stack), (quote, (S_infra, (S_first,
			expression))))))))))
	stack = (quote, ((z, stack), stack))
	return stack, expression, dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), s7, s6)
@FunctionWrapper
def step(S, expression, dictionary):
	'''
	Run a quoted program on each item in a sequence.
	::

					... [] [Q] . step
			 -----------------------
								 ... .


				 ... [a] [Q] . step
			------------------------
							 ... a . Q


			 ... [a b c] [Q] . step
		----------------------------------------
								 ... a . Q [b c] [Q] step

	The step combinator executes the quotation on each member of the list
	on top of the stack.
	'''
	(quote, (aggregate, stack)) = S
	if not aggregate:
		return stack, expression, dictionary
	head, tail = aggregate
	stack = quote, (head, stack)
	if tail:
		expression = tail, (quote, (S_step, expression))
	expression = S_i, expression
	return stack, expression, dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), i1, s6)
@FunctionWrapper
def times(stack, expression, dictionary):
	'''
	times == [-- dip] cons [swap] infra [0 >] swap while pop
	::

			 ... n [Q] . times
		---------------------  w/ n <= 0
						 ... .


			 ... 1 [Q] . times
		---------------------------------
						 ... . Q


			 ... n [Q] . times
		---------------------------------  w/ n > 1
						 ... . Q (n - 1) [Q] times

	'''
	# times == [-- dip] cons [swap] infra [0 >] swap while pop
	(quote, (n, stack)) = stack
	if n <= 0:
		return stack, expression, dictionary
	n -= 1
	if n:
		expression = n, (quote, (S_times, expression))
	expression = concat(quote, expression)
	return stack, expression, dictionary


# The current definition above works like this:

#             [P] [Q] while
# --------------------------------------
#    [P] nullary [Q [P] nullary] loop

#   while == [pop i not] [popop] [dudipd] tailrec

#def while_(S, expression, dictionary):
#  '''[if] [body] while'''
#  (body, (if_, stack)) = S
#  while joy(stack, if_, dictionary)[0][0]:
#    stack = joy(stack, body, dictionary)[0]
#  return stack, expression, dictionary


def loop_true(stack, expression, dictionary):
		quote, (flag, stack) = stack  # pylint: disable=unused-variable
		return stack, concat(quote, (S_pop, expression)), dictionary

def loop_two_true(stack, expression, dictionary):
		quote, (flag, stack) = stack  # pylint: disable=unused-variable
		return stack, concat(quote, (S_pop, concat(quote, (S_pop, expression)))), dictionary

def loop_false(stack, expression, dictionary):
		quote, (flag, stack) = stack  # pylint: disable=unused-variable
		return stack, expression, dictionary


@inscribe
@poly_combinator_effect(_COMB_NUMS(), [loop_two_true, loop_true, loop_false], b1, s6)
@FunctionWrapper
def loop(stack, expression, dictionary):
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
	quote, (flag, stack) = stack
	if flag:
		expression = concat(quote, (quote, (S_loop, expression)))
	return stack, expression, dictionary


@inscribe
@combinator_effect(_COMB_NUMS(), a1, a2, s6, s7, s8)
@FunctionWrapper
def cmp_(stack, expression, dictionary):
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
	L, (E, (G, (b, (a, stack)))) = stack
	expression = concat(G if a > b else L if a < b else E, expression)
	return stack, expression, dictionary


#  FunctionWrapper(cleave),
#  FunctionWrapper(while_),


for F in (

	#divmod_ = pm = __(n2, n1), __(n4, n3)

	sec_binary_cmp(BinaryBuiltinWrapper(operator.eq)),
	sec_binary_cmp(BinaryBuiltinWrapper(operator.ge)),
	sec_binary_cmp(BinaryBuiltinWrapper(operator.gt)),
	sec_binary_cmp(BinaryBuiltinWrapper(operator.le)),
	sec_binary_cmp(BinaryBuiltinWrapper(operator.lt)),
	sec_binary_cmp(BinaryBuiltinWrapper(operator.ne)),

	sec_binary_ints(BinaryBuiltinWrapper(operator.xor)),
	sec_binary_ints(BinaryBuiltinWrapper(operator.lshift)),
	sec_binary_ints(BinaryBuiltinWrapper(operator.rshift)),

	sec_binary_logic(BinaryBuiltinWrapper(operator.and_)),
	sec_binary_logic(BinaryBuiltinWrapper(operator.or_)),

	sec_binary_math(BinaryBuiltinWrapper(operator.add)),
	sec_binary_math(BinaryBuiltinWrapper(operator.floordiv)),
	sec_binary_math(BinaryBuiltinWrapper(operator.mod)),
	sec_binary_math(BinaryBuiltinWrapper(operator.mul)),
	sec_binary_math(BinaryBuiltinWrapper(operator.pow)),
	sec_binary_math(BinaryBuiltinWrapper(operator.sub)),
	sec_binary_math(BinaryBuiltinWrapper(operator.truediv)),

	sec_unary_logic(UnaryBuiltinWrapper(bool)),
	sec_unary_logic(UnaryBuiltinWrapper(operator.not_)),

	sec_unary_math(UnaryBuiltinWrapper(abs)),
	sec_unary_math(UnaryBuiltinWrapper(operator.neg)),
	sec_unary_math(UnaryBuiltinWrapper(sqrt)),

	stack_effect(n1)(i1)(UnaryBuiltinWrapper(floor)),
	):
	inscribe(F)
del F  # Otherwise Sphinx autodoc will pick it up.


YIN_STACK_EFFECTS = yin_functions()
add_aliases(YIN_STACK_EFFECTS, ALIASES)

# Load the auto-generated primitives into the dictionary.
_functions.update(YIN_STACK_EFFECTS)
# exec '''

# eh = compose(dup, bool)
# sqr = compose(dup, mul)
# of = compose(swap, at)

# ''' in dict(compose=compose), _functions
for name in sorted(_functions):
	sec = _functions[name]
	F = FUNCTIONS[name] = SymbolJoyType(name, [sec], _SYM_NUMS())
	if name in YIN_STACK_EFFECTS:
		_log.info('Setting stack effect for Yin function %s := %s', F.name, doc_from_stack_effect(*sec))

for name, primitive in getmembers(genlib, isfunction):
	inscribe(SimpleFunctionWrapper(primitive))


add_aliases(_dictionary, ALIASES)
add_aliases(_functions, ALIASES)
add_aliases(FUNCTIONS, ALIASES)


DefinitionWrapper.add_definitions(definitions, _dictionary)


EXPECTATIONS = dict(
	ifte=(s7, (s6, (s5, s4))),
	nullary=(s7, s6),
	run=(s7, s6),
)
EXPECTATIONS['while'] = (s7, (s6, s5))


for name in '''
	dinfrirst
	nullary
	ifte
	run
	dupdipd codireco
	while
	'''.split():
	C = _dictionary[name]
	expect = EXPECTATIONS.get(name)
	if expect:
		sec = doc_from_stack_effect(expect)
		_log.info('Setting stack EXPECT for combinator %s := %s', C.name, sec)
	else:
		_log.info('combinator %s', C.name)
	FUNCTIONS[name] = CombinatorJoyType(name, [C], _COMB_NUMS(), expect)


for name in ('''
	of quoted enstacken ?
	unary binary ternary
	sqr unquoted
	'''.split()):
	of_ = _dictionary[name]
	secs = infer_expression(of_.body)
	assert len(secs) == 1, repr(secs)
	_log.info(
		'Setting stack effect for definition %s := %s',
		name,
		doc_from_stack_effect(*secs[0]),
		)
	FUNCTIONS[name] = SymbolJoyType(name, infer_expression(of_.body), _SYM_NUMS())


#sec_Ns_math(_dictionary['product'])

##  product == 1 swap [*] step
##  flatten == [] swap [concat] step
##  pam == [i] map
##  size == 0 swap [pop ++] step
##  fork == [i] app2
##  cleave == fork [popd] dip
##  average == [sum 1.0 *] [size] cleave /
##  gcd == 1 [tuck modulus dup 0 >] loop pop
##  least_fraction == dup [gcd] infra [div] concat map
##  *fraction == [uncons] dip uncons [swap] dip concat [*] infra [*] dip cons
##  *fraction0 == concat [[swap] dip * [*] dip] infra
##  down_to_zero == [0 >] [dup --] while
##  range_to_zero == unit [down_to_zero] infra
##  anamorphism == [pop []] swap [dip swons] genrec
##  range == [0 <=] [1 - dup] anamorphism
##  while == swap [nullary] cons dup dipd concat loop
##  dupdipd == dup dipd
##  tailrec == [i] genrec
##  step_zero == 0 roll> step
##  codireco == cons dip rest cons
##  make_generator == [codireco] ccons
##  ifte == [nullary not] dipd branch
