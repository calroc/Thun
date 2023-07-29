module Joy exposing (doit)

import String exposing (replace, words)


type JoyType
    = JoySymbol String
    | JoyInt Int
    | JoyList (List JoyType)
    | JoyTrue
    | JoyFalse

type alias JoyList = List JoyType




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
        Ok ast -> Ok (joyExpressionToString ast)

