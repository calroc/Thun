module Joy exposing (JoyDict, doit, initialize)

import Bitwise
import Dict exposing (Dict, get, insert)
import Result exposing (andThen)
import String exposing (lines, replace, words)
import Integer exposing (Integer)


type JoyType
    = JoySymbol String
    | JoyInt Integer
    | JoyList (List JoyType)
    | JoyTrue
    | JoyFalse


type alias JList =
    List JoyType


type alias JoyDict =
    Dict String JList


type JoyErr
    = UnknownWord
    | EmptyDefinition
    | DefinitionNameMustBeSymbol
    | DivisionByZero
    | CannotTakeFirstOfEmptyList
    | CannotTakeRestOfEmptyList
    | CannotBoolify
    | NotEnoughValuesOnStack
    | NotAnInteger
    | NotAList
    | NotABooleanValue
    | MissingClosingBracket
    | ExtraClosingBracket



-- Joy functions take a stack and expression and return a stack and
-- expression, but something might go wrong, so they really return a
-- Result value.


type alias JoyFunction =
    JList -> JList -> Result JoyErr ( JList, JList )


joy_err : JoyErr -> String
joy_err err =
    case err of
        UnknownWord ->
            "Unknown word."

        EmptyDefinition ->
            "Empty definition."

        DefinitionNameMustBeSymbol ->
            "Def name isn't symbol."

        DivisionByZero ->
            "Integer division or modulo by zero."

        CannotTakeFirstOfEmptyList ->
            "Cannot take first of empty list."

        CannotTakeRestOfEmptyList ->
            "Cannot take rest of empty list."

        CannotBoolify ->
            "Cannot Boolify."

        NotEnoughValuesOnStack ->
            "Not enough values on Stack"

        NotAnInteger ->
            "Not an integer."

        NotAList ->
            "Not a list."

        NotABooleanValue ->
            "Not a Boolean value."

        MissingClosingBracket ->
            "Missing closing bracket."

        ExtraClosingBracket ->
            "Extra closing bracket."


joy : JList -> JList -> JoyDict -> Result JoyErr ( JList, JoyDict )
joy stack expression dict =
    case expression of
        [] ->
            Ok ( stack, dict )

        term :: rest_of_expression ->
            case term of
                JoySymbol symbol ->
                    case joy_eval symbol stack rest_of_expression dict of
                        Err msg ->
                            Err msg

                        Ok ( s, e, dict0 ) ->
                            joy s e dict0

                _ ->
                    joy (term :: stack) rest_of_expression dict


joy_eval : String -> JList -> JList -> JoyDict -> Result JoyErr ( JList, JList, JoyDict )
joy_eval symbol stack expression dict =
    if symbol == "" then
        Ok ( stack, expression, dict )

    else if symbol == "inscribe" then
        joy_inscribe stack expression dict

    else
        case joy_function_eval symbol stack expression of
            Err err ->
                case err of
                    UnknownWord ->
                        -- Look up word in dictionary.
                        case get symbol dict of
                            Just definition ->
                                Ok ( stack, definition ++ expression, dict )

                            Nothing ->
                                Err UnknownWord

                    -- ("Unknown word: " ++ symbol)
                    _ ->
                        Err err

            Ok ( stack0, expression0 ) ->
                Ok ( stack0, expression0, dict )


joy_function_eval symbol stack expression =
    case symbol of
        "branch" ->
            joy_branch stack expression

        "i" ->
            joy_i stack expression

        "dip" ->
            joy_dip stack expression

        "loop" ->
            joy_loop stack expression

        "+" ->
            joy_binary_math_op (Integer.add) stack expression

        "-" ->
            joy_binary_math_op (Integer.sub) stack expression

        "*" ->
            joy_binary_math_op (Integer.mul) stack expression

        "/" ->
            joy_binary_div_op Integer.div stack expression

        "%" ->
            joy_binary_div_op Integer.remainderBy stack expression

        "add" ->
            joy_binary_math_op (Integer.add) stack expression

        "sub" ->
            joy_binary_math_op (Integer.sub) stack expression

        "mul" ->
            joy_binary_math_op (Integer.mul) stack expression

        "div" ->
            joy_binary_div_op Integer.div stack expression

        "mod" ->
            joy_binary_div_op Integer.remainderBy stack expression

        "<" ->
            joy_comparison_op (Integer.lt) stack expression

        ">" ->
            joy_comparison_op (Integer.gt) stack expression

        "<=" ->
            joy_comparison_op (Integer.lt) stack expression

        ">=" ->
            joy_comparison_op (Integer.gte) stack expression

        "<>" ->
            joy_comparison_op (/=) stack expression

        "!=" ->
            joy_comparison_op (/=) stack expression

        "=" ->
            joy_comparison_op (Integer.eq) stack expression

        "/\\" ->
            joy_logical_op (&&) stack expression

        "\\/" ->
            joy_logical_op (||) stack expression

        "_\\/_" ->
            joy_logical_op xor stack expression

        "clear" ->
            Ok ( [], expression )

        "concat" ->
            joy_concat stack expression

        "cons" ->
            joy_cons stack expression

        "dup" ->
            joy_dup stack expression

        "first" ->
            joy_first stack expression

        "pop" ->
            joy_pop stack expression

        "rest" ->
            joy_rest stack expression

        "stack" ->
            joy_stack stack expression

        "swaack" ->
            joy_swaack stack expression

        "swap" ->
            joy_swap stack expression

        "truthy" ->
            joy_truthy stack expression

        "bool" ->
            joy_truthy stack expression

        _ ->
            Err UnknownWord


joy_inscribe : JList -> JList -> JoyDict -> Result JoyErr ( JList, JList, JoyDict )
joy_inscribe stack expression dict =
    case pop_list stack of
        Ok ( def, s0 ) ->
            case def of
                [] ->
                    Err EmptyDefinition

                sym :: body ->
                    -- check that name is a symbol
                    case sym of
                        JoySymbol name ->
                            Ok ( s0, expression, insert name body dict )

                        _ ->
                            Err DefinitionNameMustBeSymbol

        Err msg ->
            Err msg


joy_branch : JoyFunction
joy_branch stack expression =
    case pop_list stack of
        Ok ( true_body, s0 ) ->
            case pop_list s0 of
                Ok ( false_body, s1 ) ->
                    case pop_bool s1 of
                        Ok ( flag, s2 ) ->
                            if flag then
                                Ok ( s2, true_body ++ expression )

                            else
                                Ok ( s2, false_body ++ expression )

                        Err msg ->
                            Err msg

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_i : JoyFunction
joy_i stack expression =
    case pop_list stack of
        Ok ( a, s0 ) ->
            Ok ( s0, a ++ expression )

        Err msg ->
            Err msg


joy_dip : JoyFunction
joy_dip stack expression =
    case pop_list stack of
        Ok ( quoted_expression, s0 ) ->
            case pop_any s0 of
                Ok ( x, s1 ) ->
                    Ok ( s1, quoted_expression ++ (x :: expression) )

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_loop : JoyFunction
joy_loop stack expression =
    case pop_list stack of
        Ok ( loop_body, s0 ) ->
            case pop_bool s0 of
                Ok ( flag, s1 ) ->
                    if flag then
                        Ok ( s1, loop_body ++ (JoyList loop_body :: JoySymbol "loop" :: expression) )

                    else
                        Ok ( s1, expression )

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_binary_math_op : (Integer -> Integer -> Integer) -> JoyFunction
joy_binary_math_op op stack expression =
    case pop_int stack of
        Ok ( a, s0 ) ->
            case pop_int s0 of
                Ok ( b, s1 ) ->
                    Ok ( push_int (op b a) s1, expression )

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_binary_div_op : (Integer -> Integer -> Maybe Integer) -> JoyFunction
joy_binary_div_op op stack expression =
    case pop_int stack of
        Ok ( a, s0 ) ->
            case pop_int s0 of
                Ok ( b, s1 ) ->
                    case op b a of
                        Just n ->
                            Ok ( push_int n s1, expression )
                        Nothing ->
                            Err DivisionByZero
                Err msg ->
                    Err msg

        Err msg ->
            Err msg


swap_args : (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
swap_args op =
    \a b -> op b a


joy_comparison_op : (Integer -> Integer -> Bool) -> JoyFunction
joy_comparison_op op stack expression =
    case pop_int stack of
        Ok ( a, s0 ) ->
            case pop_int s0 of
                Ok ( b, s1 ) ->
                    Ok ( push_bool (op b a) s1, expression )

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_logical_op : (Bool -> Bool -> Bool) -> JoyFunction
joy_logical_op op stack expression =
    case pop_bool stack of
        Ok ( a, s0 ) ->
            case pop_bool s0 of
                Ok ( b, s1 ) ->
                    Ok ( push_bool (op b a) s1, expression )

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_concat : JoyFunction
joy_concat stack expression =
    case pop_list stack of
        Ok ( a, s0 ) ->
            case pop_list s0 of
                Ok ( b, s1 ) ->
                    Ok ( push_list (b ++ a) s1, expression )

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_cons : JoyFunction
joy_cons stack expression =
    case pop_list stack of
        Ok ( a, s0 ) ->
            case pop_any s0 of
                Ok ( b, s1 ) ->
                    Ok ( push_list (b :: a) s1, expression )

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_dup : JoyFunction
joy_dup stack expression =
    case pop_any stack of
        Ok ( a, s0 ) ->
            Ok ( a :: stack, expression )

        Err msg ->
            Err msg


joy_first : JoyFunction
joy_first stack expression =
    case pop_list stack of
        Ok ( a, s0 ) ->
            case pop_any a of
                Ok ( b, _ ) ->
                    Ok ( push_any b s0, expression )

                Err _ ->
                    Err CannotTakeFirstOfEmptyList

        Err msg ->
            Err msg


joy_pop : JoyFunction
joy_pop stack expression =
    case pop_any stack of
        Ok ( _, s0 ) ->
            Ok ( s0, expression )

        Err msg ->
            Err msg


joy_rest : JoyFunction
joy_rest stack expression =
    case pop_list stack of
        Ok ( a, s0 ) ->
            case pop_any a of
                Ok ( _, el ) ->
                    Ok ( push_list el s0, expression )

                Err _ ->
                    Err CannotTakeRestOfEmptyList

        Err msg ->
            Err msg


joy_stack : JoyFunction
joy_stack stack expression =
    Ok ( push_list stack stack, expression )


joy_swaack : JoyFunction
joy_swaack stack expression =
    case pop_list stack of
        Ok ( s, s0 ) ->
            Ok ( push_list s0 s, expression )

        Err msg ->
            Err msg


joy_swap : JoyFunction
joy_swap stack expression =
    case pop_any stack of
        Ok ( a, s0 ) ->
            case pop_any s0 of
                Ok ( b, s1 ) ->
                    Ok ( b :: a :: s1, expression )

                Err msg ->
                    Err msg

        Err msg ->
            Err msg


joy_truthy : JoyFunction
joy_truthy stack expression =
    case pop_any stack of
        Ok ( a, s0 ) ->
            case a of
                JoyTrue ->
                    Ok ( stack, expression )

                JoyFalse ->
                    Ok ( stack, expression )

                JoyInt i ->
                    if Integer.eq Integer.zero i then
                        Ok ( JoyFalse :: s0, expression )

                    else
                        Ok ( JoyTrue :: s0, expression )

                JoyList el ->
                    if [] == el then
                        Ok ( JoyFalse :: s0, expression )

                    else
                        Ok ( JoyTrue :: s0, expression )

                JoySymbol _ ->
                    Err CannotBoolify

        Err msg ->
            Err msg


push_bool : Bool -> JList -> JList
push_bool flag stack =
    if flag then
        JoyTrue :: stack

    else
        JoyFalse :: stack


push_int : Integer -> JList -> JList
push_int i stack =
    JoyInt i :: stack


push_list : JList -> JList -> JList
push_list el stack =
    JoyList el :: stack


push_any : JoyType -> JList -> JList
push_any j stack =
    j :: stack


pop_int : JList -> Result JoyErr ( Integer, JList )
pop_int stack =
    pop_any stack |> andThen isnt_int


pop_list : JList -> Result JoyErr ( JList, JList )
pop_list stack =
    pop_any stack |> andThen isnt_list


pop_bool : JList -> Result JoyErr ( Bool, JList )
pop_bool stack =
    pop_any stack |> andThen isnt_bool


pop_any : JList -> Result JoyErr ( JoyType, JList )
pop_any stack =
    case stack of
        [] ->
            Err NotEnoughValuesOnStack

        item :: rest ->
            Ok ( item, rest )


isnt_int : ( JoyType, JList ) -> Result JoyErr ( Integer, JList )
isnt_int ( item, stack ) =
    case item of
        JoyInt i ->
            Ok ( i, stack )

        _ ->
            Err NotAnInteger


isnt_list : ( JoyType, JList ) -> Result JoyErr ( JList, JList )
isnt_list ( item, stack ) =
    case item of
        JoyList el ->
            Ok ( el, stack )

        _ ->
            Err NotAList


isnt_bool : ( JoyType, JList ) -> Result JoyErr ( Bool, JList )
isnt_bool ( item, stack ) =
    case item of
        JoyTrue ->
            Ok ( True, stack )

        JoyFalse ->
            Ok ( False, stack )

        _ ->
            Err NotABooleanValue



-- Printer


joyTermToString : JoyType -> String
joyTermToString term =
    case term of
        JoySymbol name ->
            name

        JoyInt n ->
            Integer.toString n

        JoyTrue ->
            "true"

        JoyFalse ->
            "false"

        JoyList list ->
            "[" ++ joyExpressionToString list ++ "]"


joyExpressionToString expr =
    String.join " " (List.map joyTermToString expr)



-- Use the old S-expression lexing trick.


tokenize : String -> List String
tokenize text =
    words (replace "[" " [ " (replace "]" " ] " text))


tokenator : String -> JoyType
tokenator tok =
    case tok of
        "true" ->
            JoyTrue

        "false" ->
            JoyFalse

        _ ->
            case Integer.fromString tok of
                Just i ->
                    JoyInt i

                Nothing ->
                    JoySymbol tok



-- I don't like this because it won't reject "[" and "]"
-- instead turning them into symbols!


expect_right_bracket : List String -> JList -> Result JoyErr ( JList, List String )
expect_right_bracket tokens acc =
    case tokens of
        [] ->
            Err MissingClosingBracket

        h :: t ->
            expect_right_bracket_one_token_lookahead h t acc


expect_right_bracket_one_token_lookahead : String -> List String -> JList -> Result JoyErr ( JList, List String )
expect_right_bracket_one_token_lookahead token tokens acc =
    case token of
        "]" ->
            Ok ( acc, tokens )

        "[" ->
            -- (* extract the sub-list *)
            case expect_right_bracket tokens [] of
                Err msg ->
                    Err msg

                Ok ( sub_list, rest ) ->
                    -- (* continue looking for the expected "]" *)
                    case expect_right_bracket rest acc of
                        Err msg ->
                            Err msg

                        Ok ( el, rrest ) ->
                            Ok ( JoyList sub_list :: el, rrest )

        _ ->
            case expect_right_bracket tokens acc of
                Err msg ->
                    Err msg

                Ok ( el, rest ) ->
                    Ok ( tokenator token :: el, rest )



---(* token -> token list -> joy_type * token list *)


one_token_lookahead : String -> List String -> Result JoyErr ( JoyType, List String )
one_token_lookahead token tokens =
    case token of
        "]" ->
            Err ExtraClosingBracket

        "[" ->
            case expect_right_bracket tokens [] of
                Err msg ->
                    Err msg

                Ok ( list_term, rest_of_tokens ) ->
                    Ok ( JoyList list_term, rest_of_tokens )

        _ ->
            Ok ( tokenator token, tokens )


parse0 : List String -> JList -> Result JoyErr JList
parse0 tokens acc =
    case tokens of
        [] ->
            Ok acc

        token :: tokens_tail ->
            case one_token_lookahead token tokens_tail of
                Err msg ->
                    Err msg

                Ok ( term, rest_of_tokens ) ->
                    case parse0 rest_of_tokens acc of
                        Err msg ->
                            Err msg

                        Ok terms ->
                            Ok (term :: terms)


parse tokens =
    parse0 tokens []


text_to_expression text =
    parse (tokenize text)


doit : String -> JoyDict -> Result String ( String, JoyDict )
doit text dict =
    case text_to_expression text of
        Ok ast ->
            case joy [] ast dict of
                Ok ( expr, dict0 ) ->
                    Ok ( joyExpressionToString expr, dict0 )

                Err err ->
                    Err (joy_err err)

        Err err ->
            Err (joy_err err)


add_def : String -> JoyDict -> JoyDict
add_def def dict =
    case text_to_expression def of
        Err msg ->
            dict

        Ok expr ->
            case expr of
                [] ->
                    dict

                sym :: body ->
                    -- check that name is a symbol
                    case sym of
                        JoySymbol name ->
                            insert name body dict

                        _ ->
                            dict


initialize : JoyDict -> JoyDict
initialize dict =
    List.foldl add_def dict (lines """eq [false] [true] [false] cmp
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
