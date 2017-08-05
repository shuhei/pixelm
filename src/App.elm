module App exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes as HA
import Html.Events as HE
import Svg exposing (Svg)
import Svg.Attributes exposing (class, width, height, x, y, fill)
import Svg.Events as SE
import Array exposing (Array)
import Hex
import Array2 exposing (Array2)


---- CONSTANTS ----


resolution : Int
resolution =
    16


pixelSize : Int
pixelSize =
    20


marginSize : Int
marginSize =
    1



---- MODEL ----


type alias Color =
    { red : Int
    , green : Int
    , blue : Int
    }



-- TODO: Remove x and y from Pixel!


type alias Pixel =
    { x : Int
    , y : Int
    , color : Maybe Color
    }


type alias Grid =
    Array2 Pixel


type alias Model =
    { brushColor : Color
    , brushes : List Color
    , grid : Grid
    }


makeGrid : Int -> Int -> Maybe Color -> Grid
makeGrid cols rows color =
    Array2.initialize cols rows (\x y -> Pixel x y color)


init : String -> ( Model, Cmd Msg )
init path =
    let
        black =
            Color 0 0 0

        brushes =
            [ black
            , Color 255 0 0
            , Color 0 255 0
            , Color 0 0 255
            ]

        model =
            { brushColor = black
            , brushes = brushes
            , grid = makeGrid resolution resolution Nothing
            }
    in
        ( model, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Paint Int Int
    | SelectBrush Color


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Paint x y ->
            ( { model | grid = updateGrid x y (Just model.brushColor) model.grid }
            , Cmd.none
            )

        SelectBrush color ->
            ( { model | brushColor = color }
            , Cmd.none
            )


updateGrid : Int -> Int -> Maybe Color -> Grid -> Grid
updateGrid col row color =
    let
        updatePixel c r px =
            if col == c && row == r then
                { px | color = color }
            else
                px
    in
        Array2.indexedMap updatePixel



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg [ class "pixel-grid" ] <| viewGrid model.grid
        , viewBrushSelector model.brushColor model.brushes
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
                , SE.onMouseDown <| Paint pixel.x pixel.y
                , fill <|
                    Maybe.withDefault "#f9f9f9" <|
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


viewBrushSelector : Color -> List Color -> Html Msg
viewBrushSelector selected brushes =
    let
        viewBrush brush =
            div
                [ HA.classList
                    [ ( "brush-selector__brush", True )
                    , ( "brush-selector__brush--selected", brush == selected )
                    ]
                , HA.style [ ( "background-color", toHexColor brush ) ]
                , HE.onClick <| SelectBrush brush
                ]
                []
    in
        div [ HA.class "brush-selector" ] <|
            List.map viewBrush brushes



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
