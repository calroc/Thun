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
  | JoyList el -> "[" ^ expression_to_joy el ^ "]"

and expression_to_joy el = String.concat " " (List.map joy_to_string el)

let () = print_endline (joy_to_string dummy)
