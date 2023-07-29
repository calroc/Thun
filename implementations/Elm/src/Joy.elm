module Joy exposing (doit)

import Bitwise
import Result exposing (andThen)
import String exposing (replace, words)


type JoyType
    = JoySymbol String
    | JoyInt Int
    | JoyList (List JoyType)
    | JoyTrue
    | JoyFalse

type alias JList = List JoyType



joy : JList -> JList -> Result String JList
joy stack expression =
        case expression of
                [] ->
                        Ok stack
                term :: rest_of_expression ->
                        case term of
                                JoySymbol symbol ->
                                        case joy_eval symbol stack rest_of_expression of
                                            Err msg -> Err msg
                                            Ok (s, e) -> joy s e
                                _ ->
                                        joy (term :: stack) rest_of_expression


joy_eval : String -> JList -> JList -> Result String (JList, JList)
joy_eval symbol stack expression =
    case symbol of
        "+" -> joy_binary_math_op (+) stack expression
        "-" -> joy_binary_math_op (-) stack expression
        "*" -> joy_binary_math_op (*) stack expression
        "/" -> joy_binary_math_op (//) stack expression
        "%" -> joy_binary_math_op (modBy) stack expression
        "and" -> joy_binary_math_op (Bitwise.and) stack expression
        "or" -> joy_binary_math_op (Bitwise.or) stack expression
        "xor" -> joy_binary_math_op (Bitwise.xor) stack expression
        "clear" -> Ok ([], expression)
        "concat" -> joy_concat stack expression
        "cons" -> joy_cons stack expression
        _ -> Err ("Unknown word: " ++ symbol)


joy_binary_math_op : (Int -> Int -> Int) -> JList -> JList -> Result String (JList, JList)
joy_binary_math_op op stack expression =
    case pop_int(stack) of
        Ok (a, s0) ->
            case pop_int(s0) of
                Ok (b, s1) ->
                    Ok ((push_int (op b a) s1), expression)
                Err msg -> Err msg
        Err msg -> Err msg

joy_concat : JList -> JList -> Result String (JList, JList)
joy_concat stack expression =
    case pop_list(stack) of
        Ok (a, s0) ->
            case pop_list(s0) of
                Ok (b, s1) ->
                    Ok ((push_list (b ++ a) s1), expression)
                Err msg -> Err msg
        Err msg -> Err msg


joy_cons : JList -> JList -> Result String (JList, JList)
joy_cons stack expression =
    case pop_list(stack) of
        Ok (a, s0) ->
            case pop_any(s0) of
                Ok (b, s1) ->
                    Ok ((push_list (b :: a) s1), expression)
                Err msg -> Err msg
        Err msg -> Err msg



push_int : Int -> JList -> JList
push_int i stack = (JoyInt i) :: stack


push_list : JList -> JList -> JList
push_list el stack = (JoyList el) :: stack


pop_int : JList -> Result String (Int, JList)
pop_int stack = pop_any stack |> andThen isnt_int


pop_list : JList -> Result String (JList, JList)
pop_list stack = pop_any stack |> andThen isnt_list


pop_any : JList -> Result String (JoyType,  JList)
pop_any stack =
        case stack of
                [] ->
                        Err "Not enough values on Stack"
                item :: rest ->
                        Ok (item, rest)


isnt_int : (JoyType,  JList) -> Result String (Int, JList)
isnt_int (item, stack) =
        case item of
                JoyInt i ->
                        Ok (i, stack)
                _ ->
                        Err "Not an integer."



isnt_list : (JoyType,  JList) -> Result String (JList, JList)
isnt_list (item, stack) =
        case item of
                JoyList el ->
                        Ok (el, stack)
                _ ->
                        Err "Not a list."



-- Printer

joyTermToString : JoyType -> String
joyTermToString term =
    case term of
        JoySymbol name -> name
        JoyInt n -> String.fromInt n
        JoyTrue -> "true"
        JoyFalse -> "false"
        JoyList list ->
            "[" ++ (joyExpressionToString list) ++ "]"

joyExpressionToString expr = String.join " " (List.map joyTermToString expr)





-- Use the old S-expression lexing trick.

tokenize : String -> (List String)
tokenize text = words (replace "[" " [ " (replace "]" " ] " text))


tokenator : String -> JoyType
tokenator tok =
    case tok of
        "true" -> JoyTrue
        "false" -> JoyFalse
        _ -> case String.toInt tok of
            Just i -> JoyInt i
            Nothing -> JoySymbol tok

-- I don't like this because it won't reject "[" and "]"
-- instead turning them into symbols!



expect_right_bracket : (List String) -> JList -> Result String (JList, List String)
expect_right_bracket tokens acc =
    case tokens of
    [] -> Err "Missing closing bracket."
    h :: t -> expect_right_bracket_one_token_lookahead h t acc


expect_right_bracket_one_token_lookahead : String -> (List String) -> JList -> Result String (JList, List String)
expect_right_bracket_one_token_lookahead token tokens acc =
    case token of
    "]" -> Ok (acc, tokens)
    "[" ->
        -- (* extract the sub-list *)
        case expect_right_bracket tokens [] of
            Err msg -> Err msg
            Ok (sub_list, rest) ->
                -- (* continue looking for the expected "]" *)
                case expect_right_bracket rest acc of
                    Err msg -> Err msg
                    Ok (el, rrest) ->
                        Ok ((JoyList sub_list) :: el, rrest)
    _ ->
        case expect_right_bracket tokens acc of
            Err msg -> Err msg
            Ok (el, rest) ->
                Ok ((tokenator token) :: el, rest)


---(* token -> token list -> joy_type * token list *)
one_token_lookahead : String -> (List String) -> Result String (JoyType, List String)
one_token_lookahead token tokens =
    case token of
        "]" -> Err "Extra closing bracket."
        "[" -> case expect_right_bracket tokens [] of
            Err msg -> Err msg
            Ok (list_term, rest_of_tokens) -> Ok (JoyList list_term, rest_of_tokens)
        _ -> Ok (tokenator token, tokens)


parse0 : (List String) -> JList -> Result String JList
parse0 tokens acc =
    case tokens of
        [] -> Ok acc
        token :: tokens_tail ->
            case one_token_lookahead token tokens_tail of
                Err msg -> Err msg
                Ok (term, rest_of_tokens) ->
                    case parse0 rest_of_tokens acc of
                        Err msg -> Err msg
                        Ok terms -> Ok (term :: terms)


parse tokens = parse0 tokens []

text_to_expression text = parse (tokenize text)



doit text =
    case text_to_expression text of
        Ok ast ->
            case joy [] ast of
                Ok expr -> Ok (joyExpressionToString expr)
                Err msg -> Err msg
        Err msg -> Err msg

