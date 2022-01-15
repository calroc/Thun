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
import strutils, bigints, fp, types

proc pr_str(thing: JoyType): string

proc joystr(s: JoyListType): string =
  s.map(pr_str).asSeq.join(" ")

proc pr_str(thing: JoyType): string =
  case thing.kind
  of joyAtom: thing.atomVal
  of joyInt: thing.intVal.toString
  of joyList: "[" & joystr(thing.listVal) & "]"
  of joyParseError: thing.errorMessage
  of joyTrue: "true"
  of joyFalse: "false"

proc print_expression*(stack: JoyListType): string =
  joystr(stack)

proc print_stack*(stack: JoyListType): string =
  joystr(stack.reverse)

