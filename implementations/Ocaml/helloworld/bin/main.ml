type joy_type =
  | JoySymbol of string
  | JoyTrue
  | JoyFalse
  | JoyInt of int
  | JoyList of joy_type list

(* type joy_list = joy_type list *)

let joy_true = JoyTrue
let joy_false = JoyFalse
let j_loop = JoySymbol "loop"
let zero = JoyInt 0
let dummy = JoyList [ joy_true; joy_false; j_loop; zero ]

let rec joy_to_string jt =
  match jt with
  | JoySymbol sym -> sym
  | JoyTrue -> "true"
  | JoyFalse -> "false"
  | JoyInt i -> string_of_int i
  | JoyList el -> "[" ^ expression_to_string el ^ "]"

and expression_to_string el = String.concat " " (List.map joy_to_string el)

type token = Left_bracket | Right_bracket | Token of string

let delimiter str last =
  last >= String.length str || String.contains "[] " (String.get str last)

(* string -> int -> int -> token * int *)
let rec tokenize1 str start last =
  if delimiter str last then (Token (String.sub str start (last - start)), last)
  else tokenize1 str start (last + 1)

let rec tokenize0 str start acc =
  if start >= String.length str then acc
  else
    let ch = String.get str start in
    match ch with
    | '[' -> Left_bracket :: tokenize0 str (start + 1) acc
    | ']' -> Right_bracket :: tokenize0 str (start + 1) acc
    | ' ' -> tokenize0 str (start + 1) acc
    | _ ->
        let token, n = tokenize1 str start (start + 1) in
        token :: tokenize0 str n acc

let tokenize str = tokenize0 str 0 []

let token_to_string token =
  match token with
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Token str -> str

(*
let char_tok ch acc =
  match ch with
  | '[' -> Left_bracket :: acc
  | ']' -> Right_bracket :: acc
  | ' ' -> acc
  | x -> (Token x) :: acc

let tokenize str = 
  String.fold_right char_tok str []

let text_to_expression str =
  let tokens = tokenize str in
    tokens

let token_to_string token = 
  match token with
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Token x -> Char.escaped x
    
let s = String.concat "" (List.map token_to_string (text_to_expression "1 [2]3" ))
*)

(* let () = print_endline (joy_to_string dummy) *)

let s = String.concat " " (List.map token_to_string (tokenize "1 [2]3"))

let () =
  print_endline s;
  print_endline (joy_to_string dummy)
