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
import bigints, fp, types


proc pop_any*(state: var JoyState): JoyType =
  # TODO: detect and report nils.
  if state.stack.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  let a = state.stack.head
  state.stack = state.stack.tail
  return a

proc pop_bool*(state: var JoyState): bool =
  let a = pop_any(state)
  case a.kind:
  of joyTrue: result = true
  of joyFalse: result = false
  else:
    raise newException(ValueError, "Not a Boolean value.")

proc pop_int*(state: var JoyState): BigInt =
  let a = pop_any(state)
  case a.kind:
  of joyInt:
    return a.intVal
  else:
    raise newException(ValueError, "Not an integer.")

proc pop_list*(state: var JoyState): JoyListType =
  let a = pop_any(state)
  case a.kind:
  of joyList:
    return a.listVal
  else:
    raise newException(ValueError, "Not a list.")


proc push_int*(a: BigInt, state: var JoyState) =
  state.stack = JoyType(kind: joyInt, intVal: a) ^^ state.stack

proc push_list*(a: JoyListType, state: var JoyState) =
  state.stack = JoyType(kind: joyList, listVal: a) ^^ state.stack

proc push_bool*(a: bool, state: var JoyState) =
  if a:
    state.stack = j_true ^^ state.stack
  else:
    state.stack = j_false ^^ state.stack

