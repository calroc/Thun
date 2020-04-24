# -*- coding: utf-8 -*-
#
#    Copyright © 2016 Simon Forman
#
#    This file is part of Thun.
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
Pretty printing support, e.g.::

    Joy? 23 18 * 99 +
           . 23 18 mul 99 add
        23 . 18 mul 99 add
     23 18 . mul 99 add
       414 . 99 add
    414 99 . add
       513 . 

    513 <-top

    joy? 

On each line the stack is printed with the top to the right, then a ``.`` to
represent the current locus of processing, then the pending expression to the
left.

'''
# (Kinda clunky and hacky.  This should be swapped out in favor of much
# smarter stuff.)
from __future__ import print_function
from builtins import object
from traceback import print_exc
from .stack import expression_to_string, stack_to_string


class TracePrinter(object):
  '''
  This is what does the formatting.  You instantiate it and pass the ``viewer()``
  method to the :py:func:`joy.joy.joy` function, then print it to see the
  trace.
  '''

  def __init__(self):
    self.history = []

  def viewer(self, stack, expression):
    '''
    Record the current stack and expression in the TracePrinter's history.
    Pass this method as the ``viewer`` argument to the :py:func:`joy.joy.joy` function.

    :param stack quote: A stack.
    :param stack expression: A stack.
    '''
    self.history.append((stack, expression))

  def __str__(self):
    return '\n'.join(self.go())

  def go(self):
    '''
    Return a list of strings, one for each entry in the history, prefixed
    with enough spaces to align all the interpreter dots.

    This method is called internally by the ``__str__()`` method.

    :rtype: list(str)
    '''
    max_stack_length = 0
    lines = []
    for stack, expression in self.history:
      stack = stack_to_string(stack)
      expression = expression_to_string(expression)
      n = len(stack)
      if n > max_stack_length:
        max_stack_length = n
      lines.append((n, '%s . %s' % (stack, expression)))
    return [  # Prefix spaces to line up '.'s.
      (' ' * (max_stack_length - length) + line)
      for length, line in lines
      ]

  def print_(self):
    try:
      print(self)
    except:
      print_exc()
      print('Exception while printing viewer.')
