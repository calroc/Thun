type joy_type =
  | JoySymbol of string
  | JoyTrue
  | JoyFalse
  | JoyInt of int
  | JoyList of joy_type list

(*
type joy_list = joy_type list
*)

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
    match String.get str start with
    | '[' -> Left_bracket :: tokenize0 str (start + 1) acc
    | ']' -> Right_bracket :: tokenize0 str (start + 1) acc
    | ' ' -> tokenize0 str (start + 1) acc
    | _ ->
        let token, n = tokenize1 str start (start + 1) in
        token :: tokenize0 str n acc

let tokenize str = tokenize0 str 0 []

(*
let token_to_string token =
  match token with
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Token str -> str

let s = String.concat "" (List.map token_to_string (text_to_expression "1 [2]3" ))
let s = String.concat " " (List.map token_to_string (tokenize "1 Pat [2]3"))
*)

exception ParseError of string

(*
let rec parse : token list -> joy_list = fun tokens ->
  match tokens with
  | [] -> []
  | head :: tail ->
    match head with
    | Left_bracket ->  let (el, rest) = parse_list tail in el :: parse rest
    | Right_bracket -> raise  (ParseError "Extra closing bracket.")
    | Token tok ->
      match tok with
      | "true" -> joy_true :: parse tail
      | "false"-> joy_false :: parse tail
      | _ -> JoySymbol tok :: parse tail

*)

(* Get the prefix of the list as joy type and return rest of list.

    token list -> joy_type list -> joy_type list * token list
*)
let rec expect_right_bracket tokens acc =
  match tokens with
  | [] -> raise (ParseError "Missing closing bracket.")
  | head :: tail -> (
      match head with
      | Right_bracket -> (acc, tail)
      | Left_bracket ->
          (* extract the sub-list *)
          let sub_list, rest = expect_right_bracket tail [] in
          (* continue looking for the expected "]" *)
          let el, rrest = expect_right_bracket rest acc in
          (JoyList sub_list :: el, rrest)
      | Token tok -> (
          let el, rest = expect_right_bracket tail acc in
          match tok with
          | "true" -> (joy_true :: el, rest)
          | "false" -> (joy_false :: el, rest)
          | _ -> (JoySymbol tok :: el, rest)))

(* token -> token list -> joy_type * token list *)
let one_token_lookahead token tokens =
  match token with
  | Right_bracket -> raise (ParseError "Extra closing bracket.")
  | Left_bracket ->
      let el, rest = expect_right_bracket tokens [] in
      (JoyList el, rest)
  | Token tok -> (
      match tok with
      | "true" -> (joy_true, tokens)
      | "false" -> (joy_false, tokens)
      | _ -> (JoySymbol tok, tokens))

(* token list -> joy_type list -> joy_type list *)
let rec parse0 tokens acc =
  match tokens with
  | [] -> acc
  | head :: tail ->
      let item, rest = one_token_lookahead head tail in
      item :: parse0 rest acc

let parse tokens = parse0 tokens []
let text_to_expression text = parse (tokenize text)

let () =
  print_endline
    (expression_to_string
       (text_to_expression "1 2 3[4 5 6[7 8]9 10]11[][][[]]"));
  print_endline (expression_to_string (text_to_expression "true [ false]true"));
  print_endline (joy_to_string dummy)
