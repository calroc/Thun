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
import rdstdin, bigints, fp, printer, reader, types, joylib, utils, defs


# Handle Ctrl-C by raising an IOError to break out of the mainloop
# without waiting for the user to press enter.
proc ctrlc() {.noconv.} =
  raise newException(IOError, "Got Ctrl-C, bye!")

setControlCHook(ctrlc)


proc joy_eval(sym: string, state: var JoyState): JoyState =
  case sym

  # Integer Math

  of "+": push_int(pop_int(state) + pop_int(state), state)
  of "*": push_int(pop_int(state) * pop_int(state), state)
  of "-":
    let tos = pop_int(state)
    push_int(pop_int(state) - tos, state)
  of "/":
    let tos = pop_int(state)
    push_int(pop_int(state) div tos, state)
  of "%":
    let tos = pop_int(state)
    push_int(pop_int(state) mod tos, state)

  # Comparision

  of "<":
    let tos = pop_int(state)
    let second = pop_int(state)
    push_bool(second < tos, state)
  of ">":
    let tos = pop_int(state)
    let second = pop_int(state)
    push_bool(second > tos, state)
  of "<=":
    let tos = pop_int(state)
    let second = pop_int(state)
    push_bool(second <= tos, state)
  of ">=":
    let tos = pop_int(state)
    let second = pop_int(state)
    push_bool(second >= tos, state)
  of "=":
    let tos = pop_int(state)
    let second = pop_int(state)
    push_bool(second == tos, state)
  of "<>":
    let tos = pop_int(state)
    let second = pop_int(state)
    push_bool(second != tos, state)

  # Boolean logic

  of "and":  # Have to pop, Nim `and` short-circuits.
    let tos = pop_bool(state)
    let second = pop_bool(state)
    push_bool(tos and second, state)
  of "or":  # Have to pop, Nim `or` short-circuits.
    let tos = pop_bool(state)
    let second = pop_bool(state)
    push_bool(tos or second, state)

  # Built-in Functions and Combinators

  of "bool": truthy(state)
  of "branch": branch(state)
  of "clear": clear(state)
  of "concat": concat(state)
  of "cons": cons(state)
  of "dip": dip(state)
  of "dup": dup(state)
  of "first": first(state)
  of "i": i(state)
  of "loop": loop(state)
  of "pop": pop(state)
  of "rest": rest(state)
  of "stack": stack(state)
  of "swaack": swaack(state)
  of "swap": swap(state)

  else:
    let def = dictionary.get(sym)
    if def.isEmpty:
      raise newException(ValueError, "Unknown: " & sym)
    state.expression = def.get() ++ state.expression
  state


proc joy(state: var JoyState) =
  while not state.expression.isEmpty:
    # echo print_stack(state.stack), " . ", print_expression(state.expression)
    let term = state.expression.head
    state.expression = state.expression.tail
    case term.kind
    of joyInt, joyList, joyTrue, joyFalse:
      state.stack = term ^^ state.stack
    of joyAtom:
      state = joy_eval(term.atomVal, state)
    of joyParseError:
      echo term.errorMessage
      break



var state0: JoyState = (stack: Nil[JoyType](), expression: Nil[JoyType]())
var state: JoyState
while true:
  try:
    let e = read_str(readLineFromStdin("joy? "))
    state = (stack: state0.stack, expression: e)
  except IOError:
    break
  try:
    joy(state)
  except:
    echo getCurrentExceptionMsg()
    echo print_stack(state0.stack)
    continue
  echo print_stack(state.stack)
  state0 = state
