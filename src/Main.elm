module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    Model "" "" "" ""



-- MODEL


type alias Model =
    { text : String
    , input : String
    , parsed : String
    , tag : String
    }



-- UPDATE


type Msg
    = Text String
    | Tag String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Text txt ->
            { model | text = txt, input = txt, parsed = parser model.tag txt }

        Tag tag ->
            { model | tag = tag, parsed = parser tag model.input }


parser : String -> String -> String
parser tag s =
    String.join "\n" (List.map (\str -> tag ++ "\t" ++ str) (List.map .match (Regex.find sentence s)))


sentence : Regex.Regex
sentence =
    Maybe.withDefault Regex.never <| Regex.fromString "[^ \n][^?.。！]+[.?！。]"



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Tag "
        , viewInput "Tag" "Tag" model.tag Tag
        , br [] []
        , text "Text "
        , textarea [ value model.input, onInput Text ] []
        , br [] []
        , pre [] [ text model.parsed ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []
