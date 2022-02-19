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
import bigints, fp

type

  JoyListType* = List[JoyType]
  JoyMapType* = Map[string, JoyListType]

  JoyState* = tuple
    stack: JoyListType
    expression: JoyListType

  JoyTypeType* = enum
    joyAtom,
    joyFalse,
    joyInt,
    joyList,
    joyParseError,
    joyTrue

  JoyType* = ref object
    case kind*: JoyTypeType
    of joyAtom: atomVal*: string
    of joyFalse, joyTrue: nil
    of joyInt: intVal*: BigInt
    of joyList: listVal*: JoyListType
    of joyParseError: errorMessage*: string


# Singleton values for Boolean type.

let j_true* = JoyType(kind: joyTrue)
let j_false* = JoyType(kind: joyFalse)

# Singleton values for Symbols.

let j_loop* = JoyType(kind: joyAtom, atomVal: "loop")

