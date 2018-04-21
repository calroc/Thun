# -*- coding: utf-8 -*-
#
#    Copyright © 2014, 2015, 2016, 2017 Simon Forman
#
#    This file is part of Joypy.
#
#    Joypy is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Joypy is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Joypy.  If not see <http://www.gnu.org/licenses/>.
#
'''


§ Converting text to a joy expression.

This module exports a single function:

  text_to_expression(text)

As well as a single Symbol class and a single Exception type:

  ParseError

When supplied with a string this function returns a Python datastructure
that represents the Joy datastructure described by the text expression.
Any unbalanced square brackets will raise a ParseError.
'''
from re import Scanner
from .utils.stack import list_to_stack


class Symbol(str):
  __repr__ = str.__str__


def text_to_expression(text):
  '''
  Convert a text to a Joy expression.
  '''
  return _parse(_tokenize(text))


class ParseError(ValueError): pass


def _tokenize(text):
  '''
  Convert a text into a stream of tokens, converting symbols using
  symbol(token).  Raise ValueError (with some of the failing text)
  if the scan fails.
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
