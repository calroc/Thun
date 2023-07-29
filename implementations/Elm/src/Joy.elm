module Joy exposing (doit)

import String exposing (replace, words)


type JoyType
    = JoySymbol String
    | JoyInt Int
    | JoyList (List JoyType)
    | JoyTrue
    | JoyFalse

type alias JoyList = List JoyType



joy : (List JoyType) -> (List JoyType) -> Result String (List JoyType)
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


joy_eval : String -> (List JoyType) -> (List JoyType) -> Result String (List JoyType, List JoyType)
joy_eval symbol stack expression =
    case symbol of
        "+" -> joy_add stack expression
        _ -> Err ("Unknown word: " ++ symbol)


joy_add : (List JoyType) -> (List JoyType) -> Result String (List JoyType, List JoyType)
joy_add stack expression =
    case pop_int(stack) of
        Err msg -> Err msg
        Ok (a, s0) ->
            case pop_int(s0) of
                Err msg -> Err msg
                Ok (b, s1) ->
                    let c = a + b in
                    Ok ((push_int c s1), expression)


push_int : Int -> (List JoyType) -> (List JoyType)
push_int i stack = (JoyInt i) :: stack


pop_int : (List JoyType) -> Result String (Int, List JoyType)
pop_int stack =
    case stack of
        [] -> Err "Not enough values on Stack"
        h :: t ->
            case h of
                JoyInt i ->
                    Ok (i, t)
                _ ->
                    Err "Not an integer."

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



expect_right_bracket : (List String) -> (List JoyType) -> Result String (List JoyType, List String)
expect_right_bracket tokens acc =
    case tokens of
    [] -> Err "Missing closing bracket."
    h :: t -> expect_right_bracket_one_token_lookahead h t acc


expect_right_bracket_one_token_lookahead : String -> (List String) -> (List JoyType) -> Result String (List JoyType, List String)
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


parse0 : (List String) -> (List JoyType) -> Result String (List JoyType)
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
        Err msg -> Err msg
        Ok ast ->
            case joy [] ast of
                Err msg -> Err msg
                Ok expr -> Ok (joyExpressionToString expr)

