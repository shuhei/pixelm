module App exposing (..)

import Html exposing (Html, text, div)
import Svg exposing (Svg)
import Svg.Attributes exposing (class, width, height, x, y, fill)
import Svg.Events exposing (onClick)
import Array exposing (Array)
import Hex


---- MODEL ----


type alias Color =
    { red : Int
    , green : Int
    , blue : Int
    }


black : Color
black =
    Color 0 0 0


resolution : Int
resolution =
    16


type alias Pixel =
    { x : Int
    , y : Int
    , color : Maybe Color
    }


type alias Grid =
    Array (Array Pixel)


type alias Model =
    { brushColor : Color
    , grid : Grid
    }


makeGrid : Maybe Color -> Int -> Int -> Grid
makeGrid color rows cols =
    let
        makeRow row =
            Array.initialize cols (\col -> Pixel col row color)
    in
        Array.initialize rows makeRow


init : String -> ( Model, Cmd Msg )
init path =
    ( { brushColor = black
      , grid = makeGrid Nothing resolution resolution
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Paint Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Paint x y ->
            ( { model | grid = updateGrid model.grid x y <| Just model.brushColor }
            , Cmd.none
            )


updateGrid : Grid -> Int -> Int -> Maybe Color -> Grid
updateGrid grid x y color =
    let
        updatePixel col row px =
            if col == x && row == y then
                { px | color = color }
            else
                px

        updateRow row pixels =
            Array.indexedMap (\col px -> updatePixel col row px) pixels
    in
        Array.indexedMap (\row pxs -> updateRow row pxs) grid



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg [ class "pixel-grid" ] <| viewGrid model.grid
        ]


viewGrid : Grid -> List (Svg Msg)
viewGrid grid =
    List.concat <| Array.toList <| Array.map viewRow grid


viewRow : Array Pixel -> List (Svg Msg)
viewRow pixels =
    let
        makeRect pixel =
            Svg.rect
                [ width <| toString pixelSize
                , height <| toString pixelSize
                , x << toString <| pixel.x * (pixelSize + marginSize)
                , y << toString <| pixel.y * (pixelSize + marginSize)
                , onClick <| Paint pixel.x pixel.y
                , fill <|
                    Maybe.withDefault "#ccc" <|
                        Maybe.map toHexColor pixel.color
                ]
                []
    in
        Array.toList <| Array.map makeRect pixels


toHexColor : Color -> String
toHexColor { red, green, blue } =
    "#" ++ toHex red ++ toHex green ++ toHex blue


toHex : Int -> String
toHex n =
    let
        str =
            Hex.toString n
    in
        if String.length str == 1 then
            "0" ++ str
        else
            str


pixelSize : Int
pixelSize =
    20


marginSize : Int
marginSize =
    1



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
