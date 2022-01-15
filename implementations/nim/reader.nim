#[

    Copyright © 2021 Simon Forman

    This file is part of Bliss

    Bliss is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Bliss is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Bliss.  If not see <http://www.gnu.org/licenses/>.


]#
import pegs, bigints, fp, types

type

  Token = string

  Reader = tuple
    tokens : seq[Token]
    position: int
    eof: bool

let token_pattern = peg"""

  Token <- Bracket / Symbol

  Bracket <- '[' / ']'
  Symbol <- (!Bracket \S)+

  """

# TODO: Maybe use PEG eventParser?


proc peek(reader: Reader): Token =
  reader.tokens[reader.position]


proc next(reader: var Reader): Token =
  if reader.eof:
    raise newException(ValueError, "EOF")
  result = peek(reader)
  inc reader.position
  reader.eof = reader.position >= reader.tokens.len


proc read_atom(reader: var Reader): JoyType =
  let tok = next(reader)
  if tok =~ peg"^('+' / '-' )? \d+$":
    JoyType(kind: joyInt, intVal: tok.initBigInt)
  elif tok == "true": j_true
  elif tok == "false": j_false
  else: JoyType(kind: joyAtom, atomVal: tok)


proc read_form(reader: var Reader): JoyType


proc read_list(reader: var Reader): JoyType =
  var items : seq[JoyType] = @[]
  discard next(reader)  # Skip the '['.
  while true:
    if reader.eof:
      return JoyType(kind: joyParseError, errorMessage: "EOF while scanning list.")
    if peek(reader) == "]":
      discard next(reader)  # Skip the ']'.
      break
    items.add(read_form(reader))
  JoyType(kind: joyList, listVal: items.asList)


proc read_form(reader: var Reader): JoyType =
  if reader.eof:
    # Blank or empty input, not really an error.
    JoyType(kind: joyParseError, errorMessage: "")
  elif peek(reader) == "[":
    read_list(reader)
  else:
    read_atom(reader)


proc tokens_to_reader(tokens: seq[Token]): Reader =
  var reader: Reader = (
    tokens: tokens,
    position: 0,
    eof: tokens.len == 0
    )
  reader


proc read_str*(str: string): JoyListType =
  var items : seq[JoyType] = @[]
  var reader = tokens_to_reader(findAll(str, token_pattern))
  while not reader.eof:
    items.add(read_form(reader))
  items.asList

