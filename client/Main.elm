module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { someText : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "nyaan"
    , Cmd.none
    )


type Msg
    = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.someText ]
        ]
