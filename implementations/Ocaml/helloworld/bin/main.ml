type joy_type =
  | JoySymbol of string
  | JoyTrue
  | JoyFalse
  | JoyInt of int
  | JoyList of joy_type list

type joy_list = joy_type list

let joy_true = JoyTrue
let joy_false = JoyFalse

(*
let j_loop = JoySymbol "loop"
let zero = JoyInt 0
let dummy = JoyList [ joy_true; joy_false; j_loop; zero ]
let joy_nil = JoyList []


██████╗ ██╗ ██████╗████████╗██╗ ██████╗ ███╗   ██╗ █████╗ ██████╗ ██╗   ██╗
██╔══██╗██║██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║██╔══██╗██╔══██╗╚██╗ ██╔╝
██║  ██║██║██║        ██║   ██║██║   ██║██╔██╗ ██║███████║██████╔╝ ╚████╔╝ 
██║  ██║██║██║        ██║   ██║██║   ██║██║╚██╗██║██╔══██║██╔══██╗  ╚██╔╝  
██████╔╝██║╚██████╗   ██║   ██║╚██████╔╝██║ ╚████║██║  ██║██║  ██║   ██║   
╚═════╝ ╚═╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   

https://stackoverflow.com/questions/13708701/how-to-implement-a-dictionary-as-a-function-in-ocaml

Note that when you call dict_add you omit the lookup parameter!
It becomes the input parameter to a curried function (closure)
that performs the key lookup at a later time when you call it,

> Remember that our definition of a dictionary here is function
> from key to values so this closure is a dictionary.

Just to really spell it out, the call:

    dict_add dictionary key value

returns a function of signature:

    string -> joy_list

That either return its own value (if the string arg equals its stored key)
or delegates to the (stored) dictionary function (itself string -> joy_list).

This is really cute, but a little too magical for my taste.  Is this how you FP?
*)

exception UnknownWordError of string

let empty_dict (key : string) : joy_list = raise (UnknownWordError key)

let dict_add dictionary key value lookup =
  if key = lookup then value else dictionary lookup

type joy_dict = string -> joy_list

(*
██╗   ██╗████████╗██╗██╗     ███████╗
██║   ██║╚══██╔══╝██║██║     ██╔════╝
██║   ██║   ██║   ██║██║     ███████╗
██║   ██║   ██║   ██║██║     ╚════██║
╚██████╔╝   ██║   ██║███████╗███████║
 ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝
*)

exception StackUnderflow of string
exception ValueError of string

let pop_int : joy_list -> int * joy_list =
 fun stack ->
  match stack with
  | [] -> raise (StackUnderflow "Not enough values on stack.")
  | head :: tail -> (
      match head with
      | JoyInt i -> (i, tail)
      | _ -> raise (ValueError "Not an integer."))

(*
██████╗ ██████╗ ██╗███╗   ██╗████████╗███████╗██████╗
██╔══██╗██╔══██╗██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗
██████╔╝██████╔╝██║██╔██╗ ██║   ██║   █████╗  ██████╔╝
██╔═══╝ ██╔══██╗██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗
██║     ██║  ██║██║██║ ╚████║   ██║   ███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
*)

let rec joy_to_string jt =
  match jt with
  | JoySymbol sym -> sym
  | JoyTrue -> "true"
  | JoyFalse -> "false"
  | JoyInt i -> string_of_int i
  | JoyList el -> "[" ^ expression_to_string el ^ "]"

and expression_to_string el = String.concat " " (List.map joy_to_string el)

(*
██╗     ███████╗██╗  ██╗███████╗██████╗
██║     ██╔════╝╚██╗██╔╝██╔════╝██╔══██╗
██║     █████╗   ╚███╔╝ █████╗  ██████╔╝
██║     ██╔══╝   ██╔██╗ ██╔══╝  ██╔══██╗
███████╗███████╗██╔╝ ██╗███████╗██║  ██║
╚══════╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
*)

type token = Left_bracket | Right_bracket | Token of string

let delimiter str i =
  i >= String.length str || String.contains "[] " (String.get str i)

let make_token str index i = (Token (String.sub str index (i - index)), i)

(* string -> int -> int -> token * int *)
let rec tokenize1 str index i =
  if delimiter str i then make_token str index i else tokenize1 str index (i + 1)

let rec tokenize0 str index acc =
  if index >= String.length str then acc
  else
    match String.get str index with
    | '[' -> Left_bracket :: tokenize0 str (index + 1) acc
    | ']' -> Right_bracket :: tokenize0 str (index + 1) acc
    | ' ' -> tokenize0 str (index + 1) acc
    | _ ->
        let token, n = tokenize1 str index (index + 1) in
        token :: tokenize0 str n acc

let tokenize str = tokenize0 str 0 []

(*
let token_to_string token =
  match token with
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Token str -> str

let s = String.concat " " (List.map token_to_string (tokenize "1 Pat [2]3"))

let () = print_endline s

let s = String.concat "" (List.map token_to_string (text_to_expression "1 [2]3" ))
*)

(*
██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
*)

exception ParseError of string

let tokenator tok =
  if String.equal tok "true" then joy_true
  else if String.equal tok "false" then joy_false
  else
    match int_of_string_opt tok with
    | Some i -> JoyInt i
    | None -> JoySymbol tok

(* Get the prefix of the list as JoyList and return rest of list.
   token list -> joy_list -> joy_list * token list *)
let rec expect_right_bracket tokens acc =
  match tokens with
  | [] -> raise (ParseError "Missing closing bracket.")
  | head :: tail -> expect_right_bracket_one_token_lookahead head tail acc

and expect_right_bracket_one_token_lookahead token tokens acc =
  match token with
  | Right_bracket -> (acc, tokens)
  | Left_bracket ->
      (* extract the sub-list *)
      let sub_list, rest = expect_right_bracket tokens [] in
      (* continue looking for the expected "]" *)
      let el, rrest = expect_right_bracket rest acc in
      (JoyList sub_list :: el, rrest)
  | Token tok ->
      let el, rest = expect_right_bracket tokens acc in
      let jt = tokenator tok in
      (jt :: el, rest)

(* token -> token list -> joy_type * token list *)
let one_token_lookahead token tokens =
  match token with
  | Right_bracket -> raise (ParseError "Extra closing bracket.")
  | Left_bracket ->
      let el, rest = expect_right_bracket tokens [] in
      (JoyList el, rest)
  | Token tok ->
      let jt = tokenator tok in
      (jt, tokens)

(* token list -> joy_type list -> joy_type list *)
let rec parse0 tokens acc =
  match tokens with
  | [] -> acc
  | head :: tail ->
      let item, rest = one_token_lookahead head tail in
      item :: parse0 rest acc

let parse tokens = parse0 tokens []
let text_to_expression text = parse (tokenize text)

(*
 ██████╗ ██████╗ ██████╗ ███████╗    ██╗    ██╗ ██████╗ ██████╗ ██████╗ ███████╗
██╔════╝██╔═══██╗██╔══██╗██╔════╝    ██║    ██║██╔═══██╗██╔══██╗██╔══██╗██╔════╝
██║     ██║   ██║██████╔╝█████╗      ██║ █╗ ██║██║   ██║██████╔╝██║  ██║███████╗
██║     ██║   ██║██╔══██╗██╔══╝      ██║███╗██║██║   ██║██╔══██╗██║  ██║╚════██║
╚██████╗╚██████╔╝██║  ██║███████╗    ╚███╔███╔╝╚██████╔╝██║  ██║██████╔╝███████║
 ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚══╝╚══╝  ╚═════╝ ╚═╝  ╚═╝╚═════╝ ╚══════╝

let clear s e d = (Joy_nil, e, d)

*)

let concat s e d =
  match s with
  | JoyList tos :: JoyList second :: s0 -> (JoyList (second @ tos) :: s0, e, d)
  | _ -> raise (ValueError "some damn thing.")

(*
██╗███╗   ██╗████████╗███████╗██████╗ ██████╗ ██████╗ ███████╗████████╗███████╗██████╗
██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗
██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██████╔╝██████╔╝█████╗     ██║   █████╗  ██████╔╝
██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██╔═══╝ ██╔══██╗██╔══╝     ██║   ██╔══╝  ██╔══██╗
██║██║ ╚████║   ██║   ███████╗██║  ██║██║     ██║  ██║███████╗   ██║   ███████╗██║  ██║
╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
*)

let joy_eval sym stack expression dictionary =
  match sym with
  | "+" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (JoyInt (a + b) :: s1, expression, dictionary)
  | "-" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (JoyInt (b - a) :: s1, expression, dictionary)
  | "clear" -> ([], expression, dictionary)
  | "concat" -> concat stack expression dictionary
  | _ ->
      let func = dictionary sym in
      (stack, func @ expression, dictionary)

let rec joy : joy_list -> joy_list -> joy_dict -> joy_list * joy_dict =
 fun stack expression dictionary ->
  match expression with
  | [] -> (stack, dictionary)
  | head :: tail -> (
      match head with
      | JoySymbol sym ->
          let s, e, d = joy_eval sym stack tail dictionary in
          joy s e d
      | _ -> joy (head :: stack) tail dictionary)

(* Of course this could be a fold over a list of strings or something... *)
let d0 = dict_add empty_dict "++" (text_to_expression "1 +")
let d = dict_add d0 "sqr" (text_to_expression "dup mul")

(*
let expr = text_to_expression "1 2 + 3 4 + 5 6 + 7 8 + 9 10 + 11 + + + + + - "
let expr = text_to_expression "1 2 3 4 clear 5"
*)
let expr = text_to_expression "clear [23] [18] concat 32 ++"
let s = text_to_expression "23 [18 99] "
let stack, _ = joy s expr d
let () = print_endline (expression_to_string stack)
(* print_endline
        (expression_to_string
           (text_to_expression "1 2 3[4 5 6[7 8]9 10]11[][][[]]"));
      print_endline (expression_to_string (text_to_expression "true [ false]true"));
   print_endline (joy_to_string dummy)
*)
