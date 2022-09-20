module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (a, b, li, text, ul, Html)
import Html.Attributes exposing (href)
import Url
import Url.Parser exposing (Parser, parse, string, s, (</>))

-- MAIN
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL
type alias Model =
  { key : Nav.Key
  , url : Url.Url
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  -- ignore flags arg
  ( Model key url, Cmd.none )

-- UPDATE
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          -- Don't clutter browser history if the user clicks links to
          -- the current URL.
          if url == model.url then
            ( model, Cmd.none )
          else
            ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
  case parse docFunct model.url of
    Nothing ->
      viewGeneric (Url.toString model.url)
    Just functor_name ->
      viewFunctorDocs functor_name


viewGeneric : String -> Browser.Document Msg
viewGeneric current =
  { title = "URL Interceptor: " ++ current
  , body =
      [ text "The current URL is: "
      , b [] [ text current ]
      , ul []
          [ viewLink "Home" "/home"
          , viewLink "Profile" "/profile"
          , viewLink "Cent" "/reviews/the-century-of-the-self"
          , viewLink "Pub" "/reviews/public-opinion"
          , viewLink "cons" "/doc/functors/cons"
          ]
      ]
  }


viewFunctorDocs : String -> Browser.Document Msg
viewFunctorDocs functor_name =
  { title = "Reference: " ++ functor_name
  , body =
      [ text "Reference documentation for "
      , b [] [ text functor_name ]
      , ul []
          [ viewLink "Home" "/home"
          , viewLink "cons" "/doc/functors/cons"
          ]
      ]
  }


viewLink : String -> String -> Html msg
viewLink link_text path =
  li [] [ a [ href path ] [ text link_text ] ]


docFunct : Parser (String -> a) a
docFunct =
  s "doc" </> s "functors" </> string
