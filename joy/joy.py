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
This module implements an interpreter for a dialect of Joy that
attempts to stay very close to the spirit of Joy but does not precisely
match the behaviour of the original version(s) written in C.

'''
from __future__ import print_function
try:
  input = raw_input
except NameError:
  pass
from traceback import print_exc, format_exc
from .parser import text_to_expression, ParseError, Symbol
from .utils.stack import stack_to_string
from .utils.pretty_print import TracePrinter


def joy(stack, expression, dictionary, viewer=None):
  '''Evaluate a Joy expression on a stack.

  This function iterates through a sequence of terms which are either
  literals (strings, numbers, sequences of terms) or function symbols.
  Literals are put onto the stack and functions are looked up in the
  disctionary and executed.

  The viewer is a function that is called with the stack and expression
  on every iteration, its return value is ignored.

  :param stack stack: The stack.
  :param stack expression: The expression to evaluate.
  :param dict dictionary: A ``dict`` mapping names to Joy functions.
  :param function viewer: Optional viewer function.
  :rtype: (stack, (), dictionary)

  '''
  while expression:

    if viewer: viewer(stack, expression)

    term, expression = expression
    if isinstance(term, Symbol):
      term = dictionary[term]
      stack, expression, dictionary = term(stack, expression, dictionary)
    else:
      stack = term, stack

  if viewer: viewer(stack, expression)
  return stack, expression, dictionary


def run(text, stack, dictionary, viewer=None):
  '''
  Return the stack resulting from running the Joy code text on the stack.

  :param str text: Joy code.
  :param stack stack: The stack.
  :param dict dictionary: A ``dict`` mapping names to Joy functions.
  :param function viewer: Optional viewer function.
  :rtype: (stack, (), dictionary)

  '''
  expression = text_to_expression(text)
  return joy(stack, expression, dictionary, viewer)


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
      viewer = TracePrinter()
      try:
        stack, _, dictionary = run(text, stack, dictionary, viewer.viewer)
      except:
        exc = format_exc() # Capture the exception.
        viewer.print_() # Print the Joy trace.
        print('-' * 73)
        print(exc) # Print the original exception.
      else:
        viewer.print_()
  except:
    print_exc()
  print()
  return stack
