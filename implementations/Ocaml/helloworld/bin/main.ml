type joy_type =
  | JoySymbol of string
  | JoyTrue
  | JoyFalse
  | JoyInt of int
  | JoyList of joy_type list

type joy_list = joy_type list

let joy_true = JoyTrue
let joy_false = JoyFalse
let j_loop = JoySymbol "loop"

(*
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

let pop_item : joy_list -> joy_type * joy_list =
 fun stack ->
  match stack with
  | [] -> raise (StackUnderflow "Not enough values on stack.")
  | head :: tail -> (head, tail)

let is_int : joy_type -> int =
 fun jt ->
  match jt with JoyInt i -> i | _ -> raise (ValueError "Not an integer.")

let is_list : joy_type -> joy_list =
 fun jt ->
  match jt with JoyList el -> el | _ -> raise (ValueError "Not a list.")

let is_bool : joy_type -> bool =
 fun jt ->
  match jt with
  | JoyTrue -> true
  | JoyFalse -> false
  | _ -> raise (ValueError "Not a Boolean value.")

let pop_thing func stack =
  let jt, stack = pop_item stack in
  (func jt, stack)

let pop_int : joy_list -> int * joy_list = pop_thing is_int
let pop_list : joy_list -> joy_list * joy_list = pop_thing is_list
let pop_bool : joy_list -> bool * joy_list = pop_thing is_bool
let push_bool b stack = if b then JoyTrue :: stack else JoyFalse :: stack

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

let stack_to_string stack = expression_to_string (List.rev stack)

(*
██╗     ███████╗██╗  ██╗███████╗██████╗
██║     ██╔════╝╚██╗██╔╝██╔════╝██╔══██╗
██║     █████╗   ╚███╔╝ █████╗  ██████╔╝
██║     ██╔══╝   ██╔██╗ ██╔══╝  ██╔══██╗
███████╗███████╗██╔╝ ██╗███████╗██║  ██║
╚══════╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
*)

type token = Left_bracket | Right_bracket | Token of string

let delimiter : char -> bool = String.contains "[] "
let delimits str i = i >= String.length str || delimiter (String.get str i)
let make_token str index i = (Token (String.sub str index (i - index)), i)

(* string -> int -> int -> token * int *)
let rec tokenize1 str index i =
  if delimits str i then make_token str index i else tokenize1 str index (i + 1)

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
      (tokenator tok :: el, rest)

(* token -> token list -> joy_type * token list *)
let one_token_lookahead token tokens =
  match token with
  | Right_bracket -> raise (ParseError "Extra closing bracket.")
  | Left_bracket ->
      let el, rest = expect_right_bracket tokens [] in
      (JoyList el, rest)
  | Token tok -> (tokenator tok, tokens)

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
 ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗███╗   ██╗ █████╗ ████████╗ ██████╗ ██████╗ ███████╗
██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║████╗  ██║██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝
██║     ██║   ██║██╔████╔██║██████╔╝██║██╔██╗ ██║███████║   ██║   ██║   ██║██████╔╝███████╗
██║     ██║   ██║██║╚██╔╝██║██╔══██╗██║██║╚██╗██║██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║
╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝██║██║ ╚████║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║
 ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝
*)

let branch s e d =
  (* check_stack 3 or something to ensure there are enough items *)
  let true_body, s0 = pop_list s in
  let false_body, s1 = pop_list s0 in
  let flag, s2 = pop_bool s1 in
  if flag then (s2, true_body @ e, d) else (s2, false_body @ e, d)

let dip s e d =
  let body, s0 = pop_list s in
  match s0 with
  | item :: s1 -> (s1, body @ (item :: e), d)
  | [] -> raise (StackUnderflow "Not enough values on stack.")

let i s e d =
  let body, s0 = pop_list s in
  (s0, body @ e, d)

let loop s e d =
  let body, s0 = pop_list s in
  let flag, s1 = pop_bool s0 in
  if flag then (s1, body @ (JoyList body :: j_loop :: e), d) else (s1, e, d)

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

let cons s e d =
  let body, s0 = pop_list s in
  match s0 with
  | item :: s1 -> (JoyList (item :: body) :: s1, e, d)
  | [] -> raise (StackUnderflow "Not enough values on stack.")

let swap s e d =
  match s with
  | tos :: second :: s0 -> (second :: tos :: s0, e, d)
  | _ :: [] | [] -> raise (StackUnderflow "Not enough values on stack.")

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
  | "+" | "add" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (JoyInt (a + b) :: s1, expression, dictionary)
  | "-" | "sub" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (JoyInt (b - a) :: s1, expression, dictionary)
  | "<" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (push_bool (b < a) s1, expression, dictionary)
  | ">" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (push_bool (b > a) s1, expression, dictionary)
  | "<=" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (push_bool (b <= a) s1, expression, dictionary)
  | ">=" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (push_bool (b >= a) s1, expression, dictionary)
  | "!=" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (push_bool (b != a) s1, expression, dictionary)
  | "=" ->
      let a, s0 = pop_int stack in
      let b, s1 = pop_int s0 in
      (push_bool (b = a) s1, expression, dictionary)
  | "branch" -> branch stack expression dictionary
  | "i" -> i stack expression dictionary
  | "loop" -> loop stack expression dictionary
  | "dip" -> dip stack expression dictionary
  | "clear" -> ([], expression, dictionary)
  | "concat" -> concat stack expression dictionary
  | "cons" -> cons stack expression dictionary
  | "swap" -> swap stack expression dictionary
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
https://riptutorial.com/ocaml/example/9450/read-from-standard-input-and-print-to-standard-output
*)

let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

let rec main_loop stack dictionary =
  match maybe_read_line () with
  | Some line ->
      let expr = text_to_expression line in
      let stack0, dictionary0 = joy stack expr dictionary in
      let () = print_endline (stack_to_string stack0) in
      main_loop stack0 dictionary0
  | None -> exit 0

let () = main_loop [] d
