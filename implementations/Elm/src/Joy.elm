module Joy exposing (joyTermToString)


type JoyType
    = Symbol String
    | Integer Int
    | JoyList (List JoyType)
    | JoyTrue
    | JoyFalse


joyTermToString : JoyType -> String
joyTermToString term =
    case term of
        Symbol name ->
            name
        Integer n ->
            String.fromInt n
        JoyTrue ->
            "true"
        JoyFalse ->
            "false"
        JoyList list ->
            "[" ++ (String.join " " (List.map joyTermToString list)) ++ "]"

