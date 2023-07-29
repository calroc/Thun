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

        "branch" -> joy_branch stack expression
        "i" -> joy_i stack expression
        "dip" -> joy_dip stack expression
        "loop" -> joy_loop stack expression

        "+" -> joy_binary_math_op (+) stack expression
        "-" -> joy_binary_math_op (-) stack expression
        "*" -> joy_binary_math_op (*) stack expression
        "/" -> joy_binary_math_op (//) stack expression
        "%" -> joy_binary_math_op (modBy) stack expression

        "<" -> joy_comparison_op (<) stack expression
        ">" -> joy_comparison_op (>) stack expression
        "<=" -> joy_comparison_op (<=) stack expression
        ">=" -> joy_comparison_op (>=) stack expression
        "<>" -> joy_comparison_op (/=) stack expression
        "!=" -> joy_comparison_op (/=) stack expression
        "=" -> joy_comparison_op (==) stack expression

        "and" -> joy_binary_math_op (Bitwise.and) stack expression
        "or" -> joy_binary_math_op (Bitwise.or) stack expression
        "xor" -> joy_binary_math_op (Bitwise.xor) stack expression

        "clear" -> Ok ([], expression)
        "concat" -> joy_concat stack expression
        "cons" -> joy_cons stack expression
        "dup" -> joy_dup stack expression
        "first" -> joy_first stack expression
        "pop" -> joy_pop stack expression
        "rest" -> joy_rest stack expression
        "stack" -> joy_stack stack expression
        "swaack" -> joy_swaack stack expression
        "swap" -> joy_swap stack expression
        "truthy" -> joy_truthy stack expression

        _ -> Err ("Unknown word: " ++ symbol)


joy_branch : JList -> JList -> Result String (JList, JList)
joy_branch stack expression =
    case pop_list(stack) of
        Ok (true_body, s0) ->
            case pop_list(s0) of
                Ok (false_body, s1) ->
                    case pop_bool(s1) of
                        Ok (flag, s2) ->
                            if flag then
                                Ok (s2, true_body ++ expression)
                            else
                                Ok (s2, false_body ++ expression)
                        Err msg -> Err msg
                Err msg -> Err msg
        Err msg -> Err msg


joy_i : JList -> JList -> Result String (JList, JList)
joy_i stack expression =
    case pop_list(stack) of
        Ok (a, s0) -> Ok (s0, a ++ expression)
        Err msg -> Err msg

joy_dip : JList -> JList -> Result String (JList, JList)
joy_dip stack expression =
    case pop_list(stack) of
        Ok (quoted_expression, s0) ->
            case pop_any(s0) of
                Ok (x, s1) -> Ok (s1, quoted_expression ++ (x :: expression))
                Err msg -> Err msg
        Err msg -> Err msg


joy_loop : JList -> JList -> Result String (JList, JList)
joy_loop stack expression =
    case pop_list(stack) of
        Ok (loop_body, s0) ->
            case pop_bool(s0) of
                Ok (flag, s1) ->
                    if flag then
                        Ok (s1, loop_body ++ ((JoyList loop_body) :: (JoySymbol "loop") :: expression))
                    else
                        Ok (s1, expression)
                Err msg -> Err msg
        Err msg -> Err msg


joy_binary_math_op : (Int -> Int -> Int) -> JList -> JList -> Result String (JList, JList)
joy_binary_math_op op stack expression =
    case pop_int(stack) of
        Ok (a, s0) ->
            case pop_int(s0) of
                Ok (b, s1) ->
                    Ok ((push_int (op b a) s1), expression)
                Err msg -> Err msg
        Err msg -> Err msg


joy_comparison_op : (Int -> Int -> Bool) -> JList -> JList -> Result String (JList, JList)
joy_comparison_op op stack expression =
    case pop_int(stack) of
        Ok (a, s0) ->
            case pop_int(s0) of
                Ok (b, s1) ->
                    Ok ((push_bool (op b a) s1), expression)
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


joy_dup : JList -> JList -> Result String (JList, JList)
joy_dup stack expression =
    case pop_any(stack) of
        Ok (a, s0) -> Ok ((a :: stack), expression)
        Err msg -> Err msg


joy_first : JList -> JList -> Result String (JList, JList)
joy_first stack expression =
    case pop_list(stack) of
        Ok (a, s0) ->
            case pop_any(a) of
                Ok (b, _) -> Ok ((push_any b s0), expression)
                Err _ -> Err "Cannot take first of empty list."
        Err msg -> Err msg


joy_pop : JList -> JList -> Result String (JList, JList)
joy_pop stack expression =
    case pop_any(stack) of
        Ok (_, s0) -> Ok (s0, expression)
        Err msg -> Err msg


joy_rest : JList -> JList -> Result String (JList, JList)
joy_rest stack expression =
    case pop_list(stack) of
        Ok (a, s0) ->
            case pop_any(a) of
                Ok (_, el) -> Ok ((push_list el s0), expression)
                Err _ -> Err "Cannot take rest of empty list."
        Err msg -> Err msg


joy_stack : JList -> JList -> Result String (JList, JList)
joy_stack stack expression =
    Ok ((push_list stack stack), expression)


joy_swaack : JList -> JList -> Result String (JList, JList)
joy_swaack stack expression =
    case pop_list(stack) of
        Ok (s, s0) -> Ok ((push_list s0 s), expression)
        Err msg -> Err msg


joy_swap : JList -> JList -> Result String (JList, JList)
joy_swap stack expression =
    case pop_any(stack) of
        Ok (a, s0) ->
            case pop_any(s0) of
                Ok (b, s1) -> Ok ((b :: a :: s1), expression)
                Err msg -> Err msg
        Err msg -> Err msg


joy_truthy : JList -> JList -> Result String (JList, JList)
joy_truthy stack expression =
    case pop_any(stack) of
        Ok (a, s0) ->
            case a of
                JoyTrue -> Ok (stack, expression)
                JoyFalse -> Ok (stack, expression)
                JoyInt i ->
                    if 0 == i then
                        Ok (JoyFalse :: s0, expression)
                    else
                        Ok (JoyTrue :: s0, expression)
                JoyList el ->
                    if [] == el then
                        Ok (JoyFalse :: s0, expression)
                    else
                        Ok (JoyTrue :: s0, expression)
                JoySymbol _ ->
                    Err "Cannot Boolify."
        Err msg -> Err msg


push_bool : Bool -> JList -> JList
push_bool flag stack =
    if flag then
        JoyTrue :: stack
    else
        JoyFalse :: stack


push_int : Int -> JList -> JList
push_int i stack = (JoyInt i) :: stack


push_list : JList -> JList -> JList
push_list el stack = (JoyList el) :: stack


push_any : JoyType -> JList -> JList
push_any j stack = j :: stack


pop_int : JList -> Result String (Int, JList)
pop_int stack = pop_any stack |> andThen isnt_int


pop_list : JList -> Result String (JList, JList)
pop_list stack = pop_any stack |> andThen isnt_list


pop_bool : JList -> Result String (Bool, JList)
pop_bool stack = pop_any stack |> andThen isnt_bool


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


isnt_bool : (JoyType,  JList) -> Result String (Bool, JList)
isnt_bool (item, stack) =
        case item of
                JoyTrue -> Ok (True, stack)
                JoyFalse -> Ok (False, stack)
                _ -> Err "Not a Boolean value."



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

