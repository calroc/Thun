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
    term = int | float | string | '[' joy ']' | symbol

A Joy expression is a sequence of zero or more terms.  A term is a
literal value (integer, float, string, or Joy expression) or a function
symbol.  Function symbols are unquoted strings and cannot contain square
brackets.   Terms must be separated by blanks, which can be omitted
around square brackets.

'''
from re import Scanner
from .utils.stack import list_to_stack


BRACKETS = r'\[|\]'
BLANKS = r'\s+'
WORDS = r'[^[\]\s]+'


token_scanner = Scanner([
    (BRACKETS, lambda _, token: token),
    (BLANKS, None),
    (WORDS, lambda _, token: token),
    ])


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
            try: frame = stack.pop()
            except IndexError:
                raise ParseError('Extra closing bracket.')
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
