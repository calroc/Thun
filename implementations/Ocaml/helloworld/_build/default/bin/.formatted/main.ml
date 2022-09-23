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
let empty_list = JoyList []
let zero = JoyInt 0
let dummy = [ joy_true; joy_false; j_loop; zero ]
let list_get jt = match jt with JoyList el -> el | _ -> []
let () = print_endline "Hello, World!"
