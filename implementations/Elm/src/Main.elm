module Main exposing (..)

import Dict

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Joy exposing (doit, JoyDict)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  , dictionary : JoyDict
  }


init : Model
init =
  { content = "", dictionary = Dict.empty }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
  case doit model.content of
    Err msg ->
      div []
        [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
        , div [] [ text msg ]
        ]
    Ok message ->
      div []
        [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
        , div [] [ text message ]
        ]
