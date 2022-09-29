import rdstdin, strutils
import bigints, fp


#[
███████╗████████╗ █████╗  ██████╗██╗  ██╗
██╔════╝╚══██╔══╝██╔══██╗██╔════╝██║ ██╔╝
███████╗   ██║   ███████║██║     █████╔╝
╚════██║   ██║   ██╔══██║██║     ██╔═██╗
███████║   ██║   ██║  ██║╚██████╗██║  ██╗
╚══════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝

When talking about Joy we use the terms "stack", "quote", "sequence",
"list", and others to mean the same thing: a simple linear datatype that
permits certain operations such as iterating and pushing and popping
values from (at least) one end.

    In describing Joy I have used the term quotation to describe all of the
    above, because I needed a word to describe the arguments to combinators
    which fulfill the same role in Joy as lambda abstractions (with
    variables) fulfill in the more familiar functional languages. I use the
    term list for those quotations whose members are what I call literals:
    numbers, characters, truth values, sets, strings and other quotations.
    All these I call literals because their occurrence in code results in
    them being pushed onto the stack. But I also call [London Paris] a list.
    So, [dup *] is a quotation but not a list.

`"A Conversation with Manfred von Thun" w/ Stevan Apter <http://archive.vector.org.uk/art10000350>`_

In Nim we use the cons list provided by nimfp: https://github.com/vegansk/nimfp

Cons list: https://en.wikipedia.org/wiki/Cons#Lists

The nodes in the list must be of one type (JoyType) of four kinds (JoyTypeType) corresponding to
the four kinds of values in Joy: symbols, Booleans, integers, and lists.  Note that true and false each are
given their own JoyTypeType and singleton constant values (so technically there are five kinds.)

It will be important to keep clear the distinction between a list (instance of JoyListType) vs. a
list node (instance of JoyType) containing a list.

]#

type

  JoyListType* = List[JoyType]
  JoyMapType* = Map[string, JoyListType]

  JoyTypeType* = enum
    joySymbol,
    joyTrue,
    joyFalse,
    joyInt,
    joyList

  JoyType* = ref object
    case kind*: JoyTypeType
    of joySymbol: symVal*: string
    of joyFalse, joyTrue: nil
    of joyInt: intVal*: BigInt
    of joyList: listVal*: JoyListType


  ParseError* = object of ValueError
  UnknownWordError* = object of ValueError

# Singleton values for Boolean type.

let j_true* = JoyType(kind: joyTrue)
let j_false* = JoyType(kind: joyFalse)

# Singleton values for Symbols.

let j_loop* = JoyType(kind: joySymbol, symVal: "loop")

# Singleton value for the empty list node.  The
# singleton value for the empty list itself is empty_list.listVal

let empty_list = JoyType(kind: joyList, listVal: Nil[JoyType]())

#[
██╗   ██╗████████╗██╗██╗     ███████╗
██║   ██║╚══██╔══╝██║██║     ██╔════╝
██║   ██║   ██║   ██║██║     ███████╗
██║   ██║   ██║   ██║██║     ╚════██║
╚██████╔╝   ██║   ██║███████╗███████║
 ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝

Convert from nodes to values or raise errors.

]#

proc as_list(thing: JoyType): JoyListType =
  # You've got a node, you want the list or it's an error.
  case thing.kind
  of joyList:
    return thing.listVal
  else:
    raise newException(ValueError, "Not a list.")


proc as_int(i: JoyType): BigInt =
  # You've got a node, you want the int or it's an error.
  case i.kind
  of joyInt:
    return i.intVal
  else:
    raise newException(ValueError, "Not an integer.")


#[
Push values onto stacks wrapping them in the correct node kinds.
]#

proc push_int(n: BigInt, stack: JoyListType): JoyListType =
  JoyType(kind: joyInt, intVal: n) ^^ stack

proc push_list(el: JoyListType, stack: JoyListType): JoyListType =
  JoyType(kind: joyList, listVal: el) ^^ stack

proc push_bool(a: bool, stack: JoyListType): JoyListType =
  if a:
    return j_true ^^ stack
  return j_false ^^ stack

# Whence push_symbol()?  We do not push symbols onto stacks.


#[
Pop values from stacks.
]#

proc pop_any(stack: JoyListType): (JoyType, JoyListType) =
  if stack.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  return (stack.head, stack.tail)


proc pop_int(stack: JoyListType): (BigInt, JoyListType) =
  if stack.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  return (as_int(stack.head), stack.tail)


proc pop_list_node(stack: JoyListType): (JoyType, JoyListType) =
  # For quotes that will be added to the expression we want to
  # keep the node wrapper.
  if stack.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  case stack.head.kind:
  of joyList:
    return (stack.head, stack.tail)
  else:
    raise newException(ValueError, "Not a list.")


proc pop_list(stack: JoyListType): (JoyListType, JoyListType) =
  # For stacks that will be used (e.g. concat'd or something)
  # we want the List.
  if stack.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  return (as_list(stack.head), stack.tail)


proc pop_bool(stack: JoyListType): (bool, JoyListType) =
  if stack.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  case stack.head.kind:
  of joyTrue:
    return (true, stack.tail)
  of joyFalse:
    return (false, stack.tail)
  else:
    raise newException(ValueError, "Not a Boolean value.")

# We might not need to reify to bool?  Just "if foo == j_true" everywhere?


#[
███████╗██╗  ██╗██████╗ ██████╗ ███████╗███████╗███████╗██╗ ██████╗ ███╗   ██╗
██╔════╝╚██╗██╔╝██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝██║██╔═══██╗████╗  ██║
█████╗   ╚███╔╝ ██████╔╝██████╔╝█████╗  ███████╗███████╗██║██║   ██║██╔██╗ ██║
██╔══╝   ██╔██╗ ██╔═══╝ ██╔══██╗██╔══╝  ╚════██║╚════██║██║██║   ██║██║╚██╗██║
███████╗██╔╝ ██╗██║     ██║  ██║███████╗███████║███████║██║╚██████╔╝██║ ╚████║
╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝

As elegant as it is to model the expression as a stack, it's not very
efficient, as concatenating definitions and other quoted programs to
the expression is a common and expensive operation.

Instead, let's keep a stack of sub-expressions, reading from them
one-by-one, and prepending new sub-expressions to the stack rather than
concatenating them.
]#


proc push_quote(quote: JoyType, expression: JoyListType): JoyListType =
  #[
  Put the quoted program NODE onto the stack-of-stacks.
  ]#
  if as_list(quote).isEmpty:
    return expression
  return quote ^^ expression


proc push_quote_list(quote: JoyListType, expression: JoyListType): JoyListType =
  #[
  Put the quoted program LIST onto the stack-of-stacks.
  ]#
  return push_quote(JoyType(kind: joyList, listVal: quote), expression)


proc next_term(expression: JoyListType): (JoyType, JoyListType) =
  #[
  Return the next term from the expression and the new expression.
  Raises ValueError if called on an empty expression.

    (item, quote), expression = expression
    return item, push_quote(quote, expression)

  ]#
  let (eheadl, etail) = pop_list(expression)
  let item = eheadl.head ## JoyType
  let quote = eheadl.tail ## JoyListType
  if quote.isEmpty:
    return (item, etail)
  else:
    return (item, push_list(quote, etail))


#[
██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
]#


proc text_to_expression(text: string): JoyListType =
  #[
    Convert a string to a Joy expression.

    When supplied with a string this function returns a Python datastructure
    that represents the Joy datastructure described by the text expression.
    Any unbalanced square brackets will raise a ParseError.

    :param str text: Text to convert.
    :rtype: stack
    :raises ParseError: if the parse fails.
    ]#
  var frame: seq[JoyType] = @[]
  var stack: seq[seq[JoyType]] = @[]
  var thing: JoyType

  for tok in text.replace("[", " [ ").replace("]", " ] ").splitWhitespace():

    if tok == "[":
      stack.add(frame)
      frame = newSeq[JoyType](0)
      continue

    if tok == "]":
      thing = JoyType(kind: joyList, listVal: frame.asList)
      try:
        frame = stack.pop()
      except IndexDefect:
        raise newException(ParseError, "Extra closing bracket.")
    elif tok == "true":
      thing = j_true
    elif tok == "false":
      thing = j_false
    elif tok == "-":  # see https://github.com/nim-lang/bigints/issues/116
      thing = JoyType(kind: joySymbol, symVal: tok)
    else:
      try:
        thing = JoyType(kind: joyInt, intVal: tok.initBigInt)
      except ValueError:
        thing = JoyType(kind: joySymbol, symVal: tok)

    frame.add(thing)

  if stack.len() != 0:
    raise newException(ParseError, "Unclosed bracket.")

  frame.asList


#██████╗ ██████╗ ██╗███╗   ██╗████████╗███████╗██████╗
#██╔══██╗██╔══██╗██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗
#██████╔╝██████╔╝██║██╔██╗ ██║   ██║   █████╗  ██████╔╝
#██╔═══╝ ██╔══██╗██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗
#██║     ██║  ██║██║██║ ╚████║   ██║   ███████╗██║  ██║
#╚═╝     ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝


proc pr_str(thing: JoyType): string

proc print_expression(s: JoyListType): string =
  s.map(pr_str).asSeq.join(" ")

proc pr_str(thing: JoyType): string =
  case thing.kind
  of joySymbol: thing.symVal
  of joyInt: thing.intVal.toString
  of joyList: "[" & print_expression(thing.listVal) & "]"
  of joyTrue: "true"
  of joyFalse: "false"

proc print_stack*(stack: JoyListType): string =
  print_expression(stack.reverse)


# ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗███╗   ██╗ █████╗ ████████╗ ██████╗ ██████╗ ███████╗
#██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║████╗  ██║██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝
#██║     ██║   ██║██╔████╔██║██████╔╝██║██╔██╗ ██║███████║   ██║   ██║   ██║██████╔╝███████╗
#██║     ██║   ██║██║╚██╔╝██║██╔══██╗██║██║╚██╗██║██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║
#╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝██║██║ ╚████║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║
# ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝


proc branch(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (true_body_node, s0) = pop_list_node(stack)
  let (false_body_node, s1) = pop_list_node(s0)
  let (flag, s2) = pop_bool(s1)
  if flag:
    return (s2, push_quote(true_body_node, expression), dictionary)
  return (s2, push_quote(false_body_node, expression), dictionary)


proc dip(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (body_node, s0) = pop_list_node(stack)
  if s0.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  let tos_as_list_of_one = s0.head ^^ empty_list.listVal
  return (
    s0.tail,
    push_quote(body_node, push_quote_list(tos_as_list_of_one, expression)),
    dictionary
    )

proc i(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (body_node, s0) = pop_list_node(stack)
  return (
    s0,
    push_quote(body_node, expression),
    dictionary
    )

proc loop(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (body_node, s0) = pop_list_node(stack)
  let (flag, s1) = pop_bool(s0)
  if flag:
    let l1 = body_node ^^ j_loop ^^ empty_list.listVal
    let e0 = push_quote_list(l1, expression)
    let e1 = push_quote(body_node, e0)
    return (s1, e1, dictionary)
  return (s1, expression, dictionary)


#[
 ██████╗ ██████╗ ██████╗ ███████╗    ██╗    ██╗ ██████╗ ██████╗ ██████╗ ███████╗
██╔════╝██╔═══██╗██╔══██╗██╔════╝    ██║    ██║██╔═══██╗██╔══██╗██╔══██╗██╔════╝
██║     ██║   ██║██████╔╝█████╗      ██║ █╗ ██║██║   ██║██████╔╝██║  ██║███████╗
██║     ██║   ██║██╔══██╗██╔══╝      ██║███╗██║██║   ██║██╔══██╗██║  ██║╚════██║
╚██████╗╚██████╔╝██║  ██║███████╗    ╚███╔███╔╝╚██████╔╝██║  ██║██████╔╝███████║
 ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚══╝╚══╝  ╚═════╝ ╚═╝  ╚═╝╚═════╝ ╚══════╝
]#


proc clear(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  return (empty_list.listVal, expression, dictionary)


proc concat(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (tos, s0) = pop_list(stack)
  let (second, s1) = pop_list(s0)
  return (push_list((second ++ tos), s1), expression, dictionary)


proc cons(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (tos, s0) = pop_list(stack)
  if s0.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  return (push_list((s0.head ^^ tos), s0.tail), expression, dictionary)


proc dup(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  if stack.isEmpty:
    raise newException(ValueError, "Cannot dup empty stack.")
  return (stack.head ^^ stack, expression, dictionary)


proc first(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (tos, s0) = pop_list(stack)
  if tos.isEmpty:
    raise newException(ValueError, "Cannot take first of empty list.")
  return (tos.head ^^ s0, expression, dictionary)


proc pop(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  if stack.isEmpty:
    raise newException(ValueError, "Cannot pop empty stack.")
  return (stack.tail, expression, dictionary)


proc rest(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (tos, s0) = pop_list(stack)
  if tos.isEmpty:
    raise newException(ValueError, "Cannot take rest of empty list.")
  return (push_list(tos.tail, s0), expression, dictionary)


proc stack(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  return (push_list(stack, stack), expression, dictionary)


proc swaack(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (tos, s0) = pop_list(stack)
  return (push_list(s0, tos), expression, dictionary)


proc swap(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (tos, s0) = pop_any(stack)
  let (second, s1) = pop_any(s0)
  return ((second ^^ tos ^^ s1), expression, dictionary)


proc truthy(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  let (tos, s0) = pop_any(stack)
  case tos.kind:
  of joyTrue, joyFalse:
    return (stack, expression, dictionary)
  of joyInt:
    return (push_bool(tos.intVal != zero, s0), expression, dictionary)
  of joyList:
    return (push_bool(not tos.listVal.isEmpty, s0), expression, dictionary)
  else:
    raise newException(ValueError, "Cannot Boolify.")


#[
██╗███╗   ██╗████████╗███████╗██████╗ ██████╗ ██████╗ ███████╗████████╗███████╗██████╗
██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗
██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██████╔╝██████╔╝█████╗     ██║   █████╗  ██████╔╝
██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██╔═══╝ ██╔══██╗██╔══╝     ██║   ██╔══╝  ██╔══██╗
██║██║ ╚████║   ██║   ███████╗██║  ██║██║     ██║  ██║███████╗   ██║   ███████╗██║  ██║
╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝

The joy() interpreter function is extrememly simple. It accepts a stack,
an expression, and a dictionary, and it iterates through the expression
putting values onto the stack and delegating execution to functions which
it looks up in the dictionary.
]#


proc joy_eval(sym: string, stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyListType, JoyMapType) =
  case sym

  of "+":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(a + b, s1), expression, dictionary)

  of "-":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(b - a, s1), expression, dictionary)

  of "*":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(a * b, s1), expression, dictionary)

  of "/":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(b div a, s1), expression, dictionary)

  of "%":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(b mod a, s1), expression, dictionary)

  of ">":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b > a, s1), expression, dictionary)

  of "<":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b < a, s1), expression, dictionary)

  of ">=":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b >= a, s1), expression, dictionary)

  of "<=":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b <= a, s1), expression, dictionary)

  of "!=", "<>":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b != a, s1), expression, dictionary)

  of "=":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b == a, s1), expression, dictionary)

  of "branch":
    return branch(stack, expression, dictionary)
  of "clear":
    return clear(stack, expression, dictionary)
  of "concat":
    return concat(stack, expression, dictionary)
  of "cons":
    return cons(stack, expression, dictionary)
  of "dip":
    return dip(stack, expression, dictionary)
  of "dup":
    return dup(stack, expression, dictionary)
  of "first":
    return first(stack, expression, dictionary)
  of "i":
    return i(stack, expression, dictionary)
  of "loop":
    return loop(stack, expression, dictionary)
  of "pop":
    return pop(stack, expression, dictionary)
  of "rest":
    return rest(stack, expression, dictionary)
  of "stack":
    return stack(stack, expression, dictionary)
  of "swaack":
    return swaack(stack, expression, dictionary)
  of "swap":
    return swap(stack, expression, dictionary)
  of "bool": # bool is a reserved word in Nim.
    return truthy(stack, expression, dictionary)

  else:
    let def = dictionary.get(sym)
    if def.isEmpty:
      raise newException(UnknownWordError, "Unknown: " & sym)
    return (stack, push_quote_list(def.get(), expression), dictionary)


proc joy(stack: JoyListType, expression: JoyListType, dictionary: JoyMapType): (JoyListType, JoyMapType) =
  var s = stack
  var d = dictionary
  var e = push_quote_list(expression, empty_list.listVal)
  var term: JoyType

  while not e.isEmpty:
    (term, e) = next_term(e)
    #echo pr_str(term)
    case term.kind
    of joySymbol:
      (s, e, d) = joy_eval(term.symVal, s, e, d)
    else:
      s = term ^^ s

  return (s, d)


proc add_def(def: string, dictionary: var JoyMapType) =
  let d = text_to_expression(def)
  let sym = d.head
  case sym.kind:
  of joySymbol:
    dictionary = dictionary + (sym.symVal, d.tail)
  else:
    raise newException(ValueError, def)


#proc defs_file2dict(defs_filename: string = "defs.txt"): JoyMapType =
#  var strm = newFileStream(defs_filename, fmRead)
#  var dictionary = newMap[string, JoyListType]()
#  var line = ""
#  if not isNil(strm):
#    while strm.readLine(line):
#      if line.isEmptyOrWhitespace:
#        continue
#      add_def(line, dictionary)
#    strm.close()
#  return dictionary


#let dictionary = defs_file2dict()
#var d = dictionary


const defs_text = staticRead"defs.txt"
var d = newMap[string, JoyListType]()
for line in defs_text.splitLines:
  if line.isEmptyOrWhitespace:
    continue
  add_def(line, d)

var s = empty_list.listVal
var exp: JoyListType
while true:
  try:
    exp = text_to_expression(readLineFromStdin("joy? "))
  except IOError:
    break
  try:
    (s, d) = joy(s, exp, d)
  except:
    echo getCurrentExceptionMsg()
  echo print_stack(s)
