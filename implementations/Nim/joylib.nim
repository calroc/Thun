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
import bigints, fp, types, utils

proc branch*(state: var JoyState) =
  let true_body = pop_list(state)
  let false_body = pop_list(state)
  if pop_bool(state):
    state.expression = true_body ++ state.expression
  else:
    state.expression = false_body ++ state.expression

proc clear*(state: var JoyState) =
  state.stack = Nil[JoyType]()

proc concat*(state: var JoyState) =
  let tos = pop_list(state)
  let second = pop_list(state)
  push_list((second ++ tos), state)

proc cons*(state: var JoyState) =
  let tos = pop_list(state)
  let second = pop_any(state)
  push_list((second ^^ tos), state)

proc dip*(state: var JoyState) =
  let body = pop_list(state)
  let tos = pop_any(state)
  state.expression = body ++ tos ^^ state.expression

proc dup*(state: var JoyState) =
  if state.stack.isEmpty:
    raise newException(ValueError, "Cannot dup empty stack.")
  state.stack = state.stack.head ^^ state.stack

proc first*(state: var JoyState) =
  let tos = pop_list(state)
  if tos.isEmpty:
    raise newException(ValueError, "Cannot take first of empty list.")
  state.stack = tos.head ^^ state.stack

proc i*(state: var JoyState) =
  let body = pop_list(state)
  state.expression = body ++ state.expression

proc loop*(state: var JoyState) =
  let tos = pop_any(state)
  case tos.kind:
  of joyList:
    if pop_bool(state):
      state.expression = tos.listVal ++ tos ^^ j_loop ^^ state.expression
  else:
    raise newException(ValueError, "Loop body not a list.")

proc pop*(state: var JoyState) =
  if state.stack.isEmpty:
    raise newException(ValueError, "Cannot pop empty stack.")
  state.stack = state.stack.tail

proc rest*(state: var JoyState) =
  let tos = pop_list(state)
  if tos.isEmpty:
    raise newException(ValueError, "Cannot take rest of empty list.")
  push_list(tos.tail, state)

proc stack*(state: var JoyState) =
  push_list(state.stack, state)

proc swaack*(state: var JoyState) =
  let tos = pop_list(state)
  let stack = state.stack
  state.stack = tos
  push_list(stack, state)

proc swap*(state: var JoyState) =
  let tos = pop_any(state)
  let second = pop_any(state)
  state.stack = second ^^ tos ^^ state.stack

proc truthy*(state: var JoyState) =
  let tos = pop_any(state)
  case tos.kind:
  of joyTrue, joyFalse: 
    state.stack = tos ^^ state.stack
  of joyInt:
    push_bool(tos.intVal != zero, state)
  of joyList:
    push_bool(not tos.listVal.isEmpty, state)
  else:
    raise newException(ValueError, "Cannot Boolify.")

