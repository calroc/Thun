module Main exposing (..)

import Browser
import Dict
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Joy exposing (JoyDict, doit, initialize)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    , evaluated : String
    , dictionary : JoyDict
    }


init : Model
init =
    { content = ""
    , evaluated = ""
    , dictionary = initialize Dict.empty
    }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            case doit newContent model.dictionary of
                Err err ->
                    { model | content = newContent, evaluated = err }

                Ok ( output, dict ) ->
                    { content = newContent
                    , evaluated = output
                    , dictionary = dict
                    }



-- VIEW


view : Model -> Html Msg
view model =
    div [ attribute "id" "joy_interpreter" ]
        [ input
            [ placeholder "Thun expression to evaluate"
            , value model.content
            , onInput Change
            ]
            []
        , div [] [ text model.evaluated ]
        ]
