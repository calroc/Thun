module Joy exposing (doit, JoyDict, initialize)

import Bitwise
import Dict exposing (Dict, get, insert)
import Result exposing (andThen)
import String exposing (replace, words, lines)


type JoyType
    = JoySymbol String
    | JoyInt Int
    | JoyList (List JoyType)
    | JoyTrue
    | JoyFalse

type alias JList = List JoyType

type alias JoyDict = Dict String JList


joy : JList -> JList -> JoyDict -> Result String (JList, JoyDict)
joy stack expression dict =
        case expression of
                [] ->
                        Ok (stack, dict)
                term :: rest_of_expression ->
                        case term of
                                JoySymbol symbol ->
                                        case joy_eval symbol stack rest_of_expression dict of
                                            Err msg -> Err msg
                                            Ok (s, e, dict0) -> joy s e dict0
                                _ ->
                                        joy (term :: stack) rest_of_expression dict


joy_eval : String -> JList -> JList -> JoyDict -> Result String (JList, JList, JoyDict)
joy_eval symbol stack expression dict =
    if symbol == "" then
        Ok (stack, expression, dict)
    else if symbol == "inscribe" then
        joy_inscribe stack expression dict
    else
        case joy_function_eval symbol stack expression of
            Err msg ->
                if "Unknown word." == msg then
                    -- Look up word in dictionary.
                    case get symbol dict of
                        Just definition ->
                            Ok (stack, definition ++ expression, dict)
                        Nothing ->
                            Err ("Unknown word: " ++ symbol)
                else
                    Err msg
            Ok (stack0, expression0) -> Ok (stack0, expression0, dict)


joy_function_eval symbol stack expression =
    case symbol of

        "branch" -> joy_branch stack expression
        "i" -> joy_i stack expression
        "dip" -> joy_dip stack expression
        "loop" -> joy_loop stack expression

        "+" -> joy_binary_math_op (+) stack expression
        "-" -> joy_binary_math_op (-) stack expression
        "*" -> joy_binary_math_op (*) stack expression
        "/" -> joy_binary_math_op (//) stack expression
        "%" -> joy_binary_math_op (swap_args remainderBy) stack expression

        "add" -> joy_binary_math_op (+) stack expression
        "sub" -> joy_binary_math_op (-) stack expression
        "mul" -> joy_binary_math_op (*) stack expression
        "div" -> joy_binary_math_op (//) stack expression
        "mod" -> joy_binary_math_op (swap_args remainderBy) stack expression

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
        "lshift" -> joy_binary_math_op (swap_args Bitwise.shiftLeftBy) stack expression
        "<<" -> joy_binary_math_op (swap_args Bitwise.shiftLeftBy) stack expression
        "rshift" -> joy_binary_math_op (swap_args Bitwise.shiftRightBy) stack expression
        ">>" -> joy_binary_math_op (swap_args Bitwise.shiftRightBy) stack expression

        "/\\" -> joy_logical_op (&&) stack expression
        "\\/" -> joy_logical_op (||) stack expression
        "_\\/_" -> joy_logical_op (xor) stack expression

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
        "bool" -> joy_truthy stack expression

        _ -> Err ("Unknown word.")


joy_inscribe : JList -> JList -> JoyDict -> Result String (JList, JList, JoyDict)
joy_inscribe stack expression dict =
    case pop_list(stack) of
        Ok (def, s0) ->

            case def of
                [] -> Err "Empty definition."
                sym :: body ->
                    -- check that name is a symbol
                    case sym of
                        JoySymbol name ->
                            Ok (s0, expression, (insert name body dict))
                        _ ->
                            Err "Def name isn't symbol."
        Err msg -> Err msg


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


swap_args : (Int -> Int -> Int) -> (Int -> Int -> Int)
swap_args op = (\a b -> op b a)


joy_comparison_op : (Int -> Int -> Bool) -> JList -> JList -> Result String (JList, JList)
joy_comparison_op op stack expression =
    case pop_int(stack) of
        Ok (a, s0) ->
            case pop_int(s0) of
                Ok (b, s1) ->
                    Ok ((push_bool (op b a) s1), expression)
                Err msg -> Err msg
        Err msg -> Err msg


joy_logical_op : (Bool -> Bool -> Bool) -> JList -> JList -> Result String (JList, JList)
joy_logical_op op stack expression =
    case pop_bool(stack) of
        Ok (a, s0) ->
            case pop_bool(s0) of
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


doit : String -> JoyDict -> Result String (String, JoyDict)
doit text dict =
    case text_to_expression text of
        Ok ast ->
            case joy [] ast dict of
                Ok (expr, dict0) -> Ok (joyExpressionToString expr, dict0)
                Err msg -> Err msg
        Err msg -> Err msg


add_def : String -> JoyDict -> JoyDict
add_def def dict =
    case text_to_expression def of
        Err msg -> dict
        Ok expr ->
            case expr of
                [] -> dict
                sym :: body ->
                    -- check that name is a symbol
                    case sym of
                        JoySymbol name -> (insert name body dict)
                        _ -> dict


initialize : JoyDict -> JoyDict
initialize dict = List.foldl (add_def) dict (lines """eq [false] [true] [false] cmp
gt [true] [false] [false] cmp
lt [false] [false] [true] cmp
neq [true] [false] [true] cmp
le [false] [true] [true] cmp
ge [true] [true] [false] cmp
-- 1 -
? dup bool
and nulco [nullary [false]] dip branch
++ 1 +
or nulco [nullary] dip [true] branch
!- 0 >=
<{} [] swap
<<{} [] rollup
abs dup 0 < [] [neg] branch
anamorphism [pop []] swap [dip swons] genrec
app1 grba infrst
app2 [grba swap grba swap] dip [infrst] cons ii
app3 3 appN
appN [grabN] codi map reverse disenstacken
at drop first
average [sum] [size] cleave /
b [i] dip i
binary unary popd
ccccons ccons ccons
ccons cons cons
clear [] swaack pop
cleave fork popdd
clop cleave popdd
cmp [[>] swap] dipd [ifte] ccons [=] swons ifte
codi cons dip
codireco codi reco
dinfrirst dip infrst
dipd [dip] codi
disenstacken swaack pop
divmod [/] [%] clop
down_to_zero [0 >] [dup --] while
drop [rest] times
dupd [dup] dip
dupdd [dup] dipd
dupdip dupd dip
dupdipd dup dipd
enstacken stack [clear] dip
first uncons pop
flatten <{} [concat] step
fork [i] app2
fourth rest third
gcd true [tuck mod dup 0 >] loop pop
genrec [[genrec] ccccons] nullary swons concat ifte
grabN <{} [cons] times
grba [stack popd] dip
hypot [sqr] ii + sqrt
ifte [nullary] dipd swap branch
ii [dip] dupdip i
infra swons swaack [i] dip swaack
infrst infra first
make_generator [codireco] ccons
mod %
neg 0 swap -
not [true] [false] branch
nulco [nullary] cons
null [] concat bool not
nullary [stack] dinfrirst
of swap at
pam [i] map
pm [+] [-] clop
popd [pop] dip
popdd [pop] dipd
popop pop pop
popopop pop popop
popopd [popop] dip
popopdd [popop] dipd
product 1 swap [*] step
quoted [unit] dip
range [0 <=] [-- dup] anamorphism
range_to_zero unit [down_to_zero] infra
reco rest cons
rest uncons popd
reverse <{} shunt
roll> swap swapd
roll< swapd swap
rollup roll>
rolldown roll<
rrest rest rest
run <{} infra
second rest first
shift uncons [swons] dip
shunt [swons] step
size [pop ++] step_zero
small dup null [rest null] [pop true] branch
spiral_next [[[abs] ii <=] [[<>] [pop !-] or] and] [[!-] [[++]] [[--]] ifte dip] [[pop !-] [--] [++] ifte] ifte
split_at [drop] [take] clop
split_list [take reverse] [drop] clop
sqr dup mul
stackd [stack] dip
step_zero 0 roll> step
stuncons stack uncons
sum [+] step_zero
swapd [swap] dip
swons swap cons
swoncat swap concat
tailrec [i] genrec
take <<{} [shift] times pop
ternary binary popd
third rest second
tuck dup swapd
unary nullary popd
uncons [first] [rest] cleave
unit [] cons
unquoted [i] dip
unstack [[] swaack] dip swoncat swaack pop
unswons uncons swap
while swap nulco dupdipd concat loop
x dup i
step [_step0] x
_step0 _step1 [popopop] [_stept] branch
_step1 [?] dipd roll<
_stept [uncons] dipd [dupdipd] dip x
times [_times0] x
_times0 _times1 [popopop] [_timest] branch
_times1 [dup 0 >] dipd roll<
_timest [[--] dip dupdipd] dip x
map [_map0] cons [[] [_map?] [_mape]] dip tailrec
_map? pop bool not
_mape popd reverse
_map0 [_map1] dipd _map2
_map1 stackd shift
_map2 [infrst] cons dipd roll< swons
_\\/_ [not not] [not] branch
/\\ [not not] ii [pop false] [] branch
\\/ [not not] ii [] [pop true] branch""")

