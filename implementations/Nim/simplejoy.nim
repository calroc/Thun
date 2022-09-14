import rdstdin, strutils
import bigints, fp


type

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
]#


#[

Convert from nodes to values (or raise errors!)

]#

proc as_list(thing: JoyType): JoyListType =
  # You've got a node, you want the list or it's an error.
  case thing.kind
  of joyList:
    return thing.listVal
  else:
    raise newException(ValueError, "Only lists!")





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

proc pop_int(stack: JoyListType): (BigInt, JoyListType) =
  if stack.isEmpty:
    raise newException(ValueError, "Not enough values on stack.")
  case stack.head.kind:
  of joyInt:
    return (stack.head.intVal, stack.tail)
  else:
    raise newException(ValueError, "Not an integer.")


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
    raise newException(ValueError, "Not a list.")

# We might not need to reify to bool?  Just "if foo == j_true" everywhere?







# ███████╗██╗  ██╗██████╗ ██████╗ ███████╗███████╗███████╗██╗ ██████╗ ███╗   ██╗
# ██╔════╝╚██╗██╔╝██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝██║██╔═══██╗████╗  ██║
# █████╗   ╚███╔╝ ██████╔╝██████╔╝█████╗  ███████╗███████╗██║██║   ██║██╔██╗ ██║
# ██╔══╝   ██╔██╗ ██╔═══╝ ██╔══██╗██╔══╝  ╚════██║╚════██║██║██║   ██║██║╚██╗██║
# ███████╗██╔╝ ██╗██║     ██║  ██║███████╗███████║███████║██║╚██████╔╝██║ ╚████║
# ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝
#
# As elegant as it is to model the expression as a stack, it's not very
# efficient, as concatenating definitions and other quoted programs to
# the expression is a common and expensive operation.
#
# Instead, let's keep a stack of sub-expressions, reading from them
# one-by-one, and prepending new sub-expressions to the stack rather than
# concatenating them.


proc push_quote(quote: JoyType, expression: JoyType): JoyType =
  #[
  Put the quoted program onto the stack-of-stacks.
  ]#
  let ql = as_list(quote)
  if ql.isEmpty:
    return expression
  let el = as_list(expression)
  return JoyType(kind: joyList, listVal: (quote ^^ el))


proc push_quote_list(quote: JoyListType, expression: JoyType): JoyType =
  #[
  Put the quoted program onto the stack-of-stacks.
  ]#
  return push_quote(JoyType(kind: joyList, listVal: quote), expression)


proc next_term(expression: JoyType): (JoyType, JoyType) =
  #[
  Return the next term from the expression and the new expression.
  Raises ValueError if called on an empty expression.

    (item, quote), expression = expression
    return item, push_quote(quote, expression)

  ]#
  let el = as_list(expression) ## JoyListType
  let ehead = el.head ## JoyType
  let eheadl = as_list(ehead) ## JoyListType
  let item = eheadl.head ## JoyType
  let quote = eheadl.tail ## JoyListType
  if quote.isEmpty:
    let t = JoyType(kind: joyList, listVal: el.tail)
    return (item, t)
  else:
    let q = JoyType(kind: joyList, listVal: quote)
    let t = JoyType(kind: joyList, listVal: (q ^^ el.tail))
    return (item, t)


#██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
#██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
#██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
#██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
#██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
#╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝


proc text_to_expression(text: string): JoyType =
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
    else:
      try:
        thing = JoyType(kind: joyInt, intVal: tok.initBigInt)
      except ValueError:
        thing = JoyType(kind: joySymbol, symVal: tok)

    frame.add(thing)

  if stack.len() != 0:
    raise newException(ParseError, "Unclosed bracket.")

  JoyType(kind: joyList, listVal: frame.asList)


#██████╗ ██████╗ ██╗███╗   ██╗████████╗███████╗██████╗
#██╔══██╗██╔══██╗██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗
#██████╔╝██████╔╝██║██╔██╗ ██║   ██║   █████╗  ██████╔╝
#██╔═══╝ ██╔══██╗██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗
#██║     ██║  ██║██║██║ ╚████║   ██║   ███████╗██║  ██║
#╚═╝     ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝


proc pr_str(thing: JoyType): string

proc joystr(s: JoyListType): string =
  s.map(pr_str).asSeq.join(" ")

proc pr_str(thing: JoyType): string =
  case thing.kind
  of joySymbol: thing.symVal
  of joyInt: thing.intVal.toString
  of joyList: "[" & joystr(thing.listVal) & "]"
  of joyTrue: "true"
  of joyFalse: "false"

proc print_expression*(stack: JoyListType): string =
  joystr(stack)

proc print_stack*(stack: JoyListType): string =
  joystr(stack.reverse)


# ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗███╗   ██╗ █████╗ ████████╗ ██████╗ ██████╗ ███████╗
#██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║████╗  ██║██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝
#██║     ██║   ██║██╔████╔██║██████╔╝██║██╔██╗ ██║███████║   ██║   ██║   ██║██████╔╝███████╗
#██║     ██║   ██║██║╚██╔╝██║██╔══██╗██║██║╚██╗██║██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║
#╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝██║██║ ╚████║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║
# ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝


proc branch(stack: JoyListType, expression: JoyType, dictionary: JoyMapType): (
    JoyListType, JoyType, JoyMapType) =
  let (true_body_node, s0) = pop_list_node(stack)
  let (false_body_node, s1) = pop_list_node(s0)
  let (flag, s2) = pop_bool(s1)
  if flag:
    return (s2, push_quote(true_body_node, expression), dictionary)
  return (s2, push_quote(false_body_node, expression), dictionary)


proc clear(stack: JoyListType, expression: JoyType, dictionary: JoyMapType): (
    JoyListType, JoyType, JoyMapType) =
  return (empty_list.listVal, expression, dictionary)


proc concat(stack: JoyListType, expression: JoyType, dictionary: JoyMapType): (
    JoyListType, JoyType, JoyMapType) =
  let (tos, s0) = pop_list(stack)
  let (second, s1) = pop_list(s0)
  return (push_list((second ++ tos), s1), expression, dictionary)


# ██╗███╗   ██╗████████╗███████╗██████╗ ██████╗ ██████╗ ███████╗████████╗███████╗██████╗
# ██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗
# ██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██████╔╝██████╔╝█████╗     ██║   █████╗  ██████╔╝
# ██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██╔═══╝ ██╔══██╗██╔══╝     ██║   ██╔══╝  ██╔══██╗
# ██║██║ ╚████║   ██║   ███████╗██║  ██║██║     ██║  ██║███████╗   ██║   ███████╗██║  ██║
# ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
#
# The joy() interpreter function is extrememly simple. It accepts a stack,
# an expression, and a dictionary, and it iterates through the expression
# putting values onto the stack and delegating execution to functions which
# it looks up in the dictionary.


proc joy_eval(sym: string, stack: JoyListType, expression: JoyType,
    dictionary: JoyMapType): (JoyListType, JoyType, JoyMapType) =
  case sym

  of "add":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(a + b, s1), expression, dictionary)

  of "mul":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(a * b, s1), expression, dictionary)

  of "div":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(b div a, s1), expression, dictionary)

  of "mod":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_int(b mod a, s1), expression, dictionary)

  of "gt":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b > a, s1), expression, dictionary)

  of "lt":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b < a, s1), expression, dictionary)

  of "ge":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b >= a, s1), expression, dictionary)

  of "le":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b <= a, s1), expression, dictionary)

  of "ne":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b != a, s1), expression, dictionary)

  of "eq":
    let (a, s0) = pop_int(stack)
    let (b, s1) = pop_int(s0)
    return (push_bool(b != a, s1), expression, dictionary)

  of "branch":
    return branch(stack, expression, dictionary)
  of "clear":
    return clear(stack, expression, dictionary)
  of "concat":
    return concat(stack, expression, dictionary)

  else:
    raise newException(UnknownWordError, "Unknown: " & sym)

  return (stack, expression, dictionary)



#    = ≡ eq
#    != ≡ ne
#    <> ≡ ne


proc joy(stack: JoyListType, expression: JoyType, dictionary: JoyMapType): (
    JoyListType, JoyMapType) =
  var s = stack
  var d = dictionary
  var e = push_quote(expression, empty_list)
  var term: JoyType

  while not e.listVal.isEmpty:
    (term, e) = next_term(e)
    #echo pr_str(term)
    case term.kind
    of joySymbol:
      (s, e, d) = joy_eval(term.symVal, s, e, d)
    else:
      s = term ^^ s

  return (s, d)


#




let stack = empty_list.listVal
let dict = newMap[string, JoyListType]()
 #let exp = text_to_expression("2 3 add 23 mul 45 gt")
 #let exp = text_to_expression("2 3 false [add] [mul] branch")
 #let exp = text_to_expression("2 3 true [add] [mul] branch")
 #let exp = text_to_expression("[add] [mul] concat")
 #let (s,d) = joy(stack, exp, dict)

 #echo print_stack(s)

var s = stack
var d = dict
var exp: JoyType
while true:
  try:
    exp = text_to_expression(readLineFromStdin("joy? "))
  except IOError:
    break
  try:
    (s, d) = joy(s, exp, dict)
  except:
    echo getCurrentExceptionMsg()
    echo print_stack(s)
    continue
  echo print_stack(s)



#echo pr_str(text_to_expression("""
#    [   [[abs] ii <=]
#        [
#            [<>] [pop !-] ||
#        ] &&
#    ]
#    [[    !-] [[++]] [[--]] ifte dip]
#    [[pop !-]  [--]   [++]  ifte    ]
#    ifte
#    true false 23
#"""))

# we could start with an empty list and add two expressions
# but instead let's preload a few "commands":
#let e = text_to_expression("[55 true][42]")
#let t = text_to_expression("23")
#
#let f = push_quote(t, e)

#echo pr_str(t)
#echo pr_str(e)
#echo pr_str(f)
#
#var (a, b) = next_term(f)
#echo pr_str(a)
#(a, b) = next_term(b)
#echo pr_str(a)
#(a, b) = next_term(b)
#echo pr_str(a)
#(a, b) = next_term(b)
#echo pr_str(a)


#text_to_expression("""[] [[]]""")
