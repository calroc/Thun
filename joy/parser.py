# -*- coding: utf-8 -*-
#
#    Copyright © 2014, 2015, 2016, 2017 Simon Forman
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
This module exports a single function for converting text to a joy
expression as well as a single Symbol class and a single Exception type.

The Symbol string class is used by the interpreter to recognize literals
by the fact that they are not Symbol objects.

A crude grammar::

  joy = term*
  term = int | float | string | '[' joy ']' | function

A Joy expression is a sequence of zero or more terms


'''
#TODO: explain the details of float lits and strings.
from re import Scanner
from .utils.stack import list_to_stack


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
  tokens, rest = _scanner.scan(text)
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
      stack[-1].append(frame)
    elif tok == ']':
      try:
        frame = stack.pop()
      except IndexError:
        raise ParseError('Extra closing bracket.')
      frame[-1] = list_to_stack(frame[-1])
    else:
      frame.append(tok)
  if stack:
    raise ParseError('Unclosed bracket.')
  return list_to_stack(frame)


_scanner = Scanner([
  (r'-?\d+\.\d*', lambda _, token: float(token)),
  (r'-?\d+', lambda _, token: int(token)),
  (r'[•\w!@$%^&*()_+<>?|\/;:`~,.=-]+', lambda _, token: Symbol(token)),
  (r'\[|\]', lambda _, token: token),
  (r'"(?:[^"\\]|\\.)*"', lambda _, token: token[1:-1].replace('\\"', '"')),
  (r"'(?:[^'\\]|\\.)*'", lambda _, token: token[1:-1].replace("\\'", "'")),
  (r'\s+', None),
  ])
