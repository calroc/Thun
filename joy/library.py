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

from inspect import getdoc
from functools import wraps
from itertools import count
from inspect import getmembers, isfunction
import operator, math

from .parser import text_to_expression, Symbol
from .utils.stack import expression_to_string, list_to_stack, iter_stack, pick, concat
from .utils import generated_library as genlib


HELP_TEMPLATE = '''\

==== Help on %s ====

%s

---- end (%s)
'''


# This is the main dict we're building.
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


definitions = ('''\
? dup truthy
*fraction [uncons] dip uncons [swap] dip concat [*] infra [*] dip cons
*fraction0 concat [[swap] dip * [*] dip] infra
anamorphism [pop []] swap [dip swons] genrec
average [sum 1.0 *] [size] cleave /
binary nullary [popop] dip
cleave fork [popd] dip
codireco cons dip rest cons
dinfrirst dip infra first
unstack ? [uncons ?] loop pop
down_to_zero [0 >] [dup --] while
dupdipd dup dipd
enstacken stack [clear] dip
flatten [] swap [concat] step
fork [i] app2
gcd 1 [tuck modulus dup 0 >] loop pop
ifte [nullary not] dipd branch
ii [dip] dupdip i
least_fraction dup [gcd] infra [div] concat map
make_generator [codireco] ccons
nullary [stack] dinfrirst
of swap at
pam [i] map
tailrec [i] genrec
product 1 swap [*] step
quoted [unit] dip
range [0 <=] [1 - dup] anamorphism
range_to_zero unit [down_to_zero] infra
run [] swap infra
size 0 swap [pop ++] step
sqr dup mul
step_zero 0 roll> step
swoncat swap concat
ternary unary [popop] dip
unary nullary popd
unquoted [i] dip
while swap [nullary] cons dup dipd concat loop
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
	def inner(stack, expression, dictionary):
		return f(stack), expression, dictionary
	return inner


def BinaryBuiltinWrapper(f):
	'''
	Wrap functions that take two arguments and return a single result.
	'''
	@FunctionWrapper
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
	@FunctionWrapper
	@wraps(f)
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
		return class_(*(n.strip() for n in defi.split(None, 1)))

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
		dictionary[F.name] = F

	@classmethod
	def load_definitions(class_, filename, dictionary):
		with open(filename) as f:
			lines = [line for line in f if '==' in line]
		for line in lines:
			class_.add_def(line, dictionary)


def _text_to_defs(text):
	return (
		line.strip()
		for line in text.splitlines()
		if not line.startswith('#')
		)


#
# Functions
#


@inscribe
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


# @inscribe
# @SimpleFunctionWrapper
# def infer_(stack):
# 	'''Attempt to infer the stack effect of a Joy expression.'''
# 	E, stack = stack
# 	effects = infer_expression(E)
# 	e = list_to_stack([(fi, (fo, ())) for fi, fo in effects])
# 	return e, stack


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
			raise IndexError
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
S_genrec = Symbol('genrec')
S_getitem = Symbol('getitem')
S_i = Symbol('i')
S_ifte = Symbol('ifte')
S_infra = Symbol('infra')
S_loop = Symbol('loop')
S_pop = Symbol('pop')
S_primrec = Symbol('primrec')
S_step = Symbol('step')
S_swaack = Symbol('swaack')
S_times = Symbol('times')


@inscribe
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


@inscribe
@FunctionWrapper
def primrec(stack, expression, dictionary):
	'''
	From the "Overview of the language JOY":

	> The primrec combinator expects two quoted programs in addition to a
	data parameter. For an integer data parameter it works like this: If
	the data parameter is zero, then the first quotation has to produce
	the value to be returned. If the data parameter is positive then the
	second has to combine the data parameter with the result of applying
	the function to its predecessor.

	5  [1]  [*]  primrec

	> Then primrec tests whether the top element on the stack (initially
	the 5) is equal to zero. If it is, it pops it off and executes one of
	the quotations, the [1] which leaves 1 on the stack as the result.
	Otherwise it pushes a decremented copy of the top element and
	recurses. On the way back from the recursion it uses the other
	quotation, [*], to multiply what is now a factorial on top of the
	stack by the second element on the stack.

		n [Base] [Recur] primrec

	   0 [Base] [Recur] primrec
	------------------------------
	      Base

	   n [Base] [Recur] primrec
	------------------------------------------ n > 0
	   n (n-1) [Base] [Recur] primrec Recur

	'''
	recur, (base, (n, stack)) = stack
	if n <= 0:
		expression = concat(base, expression)
	else:
		expression = S_primrec, concat(recur, expression)
		stack = recur, (base, (n - 1, (n, stack)))
	return stack, expression, dictionary


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


@inscribe
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

	BinaryBuiltinWrapper(operator.eq),
	BinaryBuiltinWrapper(operator.ge),
	BinaryBuiltinWrapper(operator.gt),
	BinaryBuiltinWrapper(operator.le),
	BinaryBuiltinWrapper(operator.lt),
	BinaryBuiltinWrapper(operator.ne),

	BinaryBuiltinWrapper(operator.xor),
	BinaryBuiltinWrapper(operator.lshift),
	BinaryBuiltinWrapper(operator.rshift),

	BinaryBuiltinWrapper(operator.and_),
	BinaryBuiltinWrapper(operator.or_),

	BinaryBuiltinWrapper(operator.add),
	BinaryBuiltinWrapper(operator.floordiv),
	BinaryBuiltinWrapper(operator.mod),
	BinaryBuiltinWrapper(operator.mul),
	BinaryBuiltinWrapper(operator.pow),
	BinaryBuiltinWrapper(operator.sub),
	BinaryBuiltinWrapper(operator.truediv),

	UnaryBuiltinWrapper(bool),
	UnaryBuiltinWrapper(operator.not_),

	UnaryBuiltinWrapper(abs),
	UnaryBuiltinWrapper(operator.neg),
	UnaryBuiltinWrapper(sqrt),

	UnaryBuiltinWrapper(floor),
	):
	inscribe(F)
del F  # Otherwise Sphinx autodoc will pick it up.


for name, primitive in getmembers(genlib, isfunction):
	inscribe(SimpleFunctionWrapper(primitive))


add_aliases(_dictionary, ALIASES)


DefinitionWrapper.add_definitions(definitions, _dictionary)


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
