module App exposing (..)

import Html exposing (Html, text, div)
import Svg exposing (Svg)
import Svg.Attributes exposing (class, width, height, x, y)


---- MODEL ----


type alias Model =
    { message : String
    , logo : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { message = "Your Elm App is working!", logo = path }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        rows =
            List.map (row resolution) <| List.range 0 (resolution - 1)
    in
        div []
            [ Svg.svg
                [ class "pixel-grid" ]
                rows
            ]


resolution : Int
resolution =
    16


pixelSize : Int
pixelSize =
    20


marginSize : Int
marginSize =
    1


row : Int -> Int -> Svg msg
row n j =
    let
        rect i =
            Svg.rect
                [ width <| toString pixelSize
                , height <| toString pixelSize
                , x << toString <| i * (pixelSize + marginSize)
                , y << toString <| j * (pixelSize + marginSize)
                ]
                []

        rects =
            List.map rect <| List.range 0 (n - 1)
    in
        Svg.g [] rects



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
