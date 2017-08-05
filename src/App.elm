module App exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes as HA
import Html.Events as HE
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Array2 exposing (Array2)
import Color exposing (Color)
import ColorUtil


---- CONSTANTS ----


resolution : Int
resolution =
    16


pixelSize : Int
pixelSize =
    20


canvasSize : Int
canvasSize =
    pixelSize * resolution



---- MODEL ----


type alias Grid =
    Array2 Color


type alias Model =
    { brushColor : Color
    , brushes : List Color
    , grid : Grid
    }


makeGrid : Int -> Int -> Color -> Grid
makeGrid cols rows color =
    Array2.initialize cols rows (\x y -> color)


init : String -> ( Model, Cmd Msg )
init path =
    let
        brushes =
            [ Color.black
            , Color.red
            , Color.green
            , Color.blue
            ]

        model =
            { brushColor = Color.black
            , brushes = brushes
            , grid = makeGrid resolution resolution <| Color.rgba 0 0 0 0
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
            ( { model | grid = Array2.set x y model.brushColor model.grid }
            , Cmd.none
            )

        SelectBrush color ->
            ( { model | brushColor = color }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg
            [ SA.class "pixel-grid"
            , SA.width <| toString canvasSize
            , SA.height <| toString canvasSize
            ]
            [ viewGrid model.grid
            , viewBorders
            ]
        , viewBrushSelector model.brushColor model.brushes
        ]


viewBorders : Svg msg
viewBorders =
    let
        ns =
            List.range 0 (resolution - 1)

        pos n =
            toFloat (pixelSize * n) + 0.5

        vertical n =
            Svg.line
                [ SA.x1 <| toString <| pos n
                , SA.y1 <| toString 0
                , SA.x2 <| toString <| pos n
                , SA.y2 <| toString canvasSize
                ]
                []

        horizontal n =
            Svg.line
                [ SA.x1 <| toString 0
                , SA.y1 <| toString <| pos n
                , SA.x2 <| toString canvasSize
                , SA.y2 <| toString <| pos n
                ]
                []
    in
        Svg.g
            [ SA.class "grid-borders"
            , SA.strokeWidth "1"
            , SA.stroke "white"
            ]
            (List.map vertical ns ++ List.map horizontal ns)


viewGrid : Grid -> Svg Msg
viewGrid grid =
    let
        makeRect col row pixel =
            Svg.rect
                [ SA.width <| toString pixelSize
                , SA.height <| toString pixelSize
                , SA.x <| toString <| col * pixelSize
                , SA.y <| toString <| row * pixelSize
                , SA.fill <| ColorUtil.toColorString pixel
                , SE.onMouseDown <| Paint col row
                ]
                []

        rects =
            Array2.toList <| Array2.indexedMap makeRect grid
    in
        Svg.g [] rects


viewBrushSelector : Color -> List Color -> Html Msg
viewBrushSelector selected brushes =
    let
        viewBrush brush =
            div
                [ HA.classList
                    [ ( "brush-selector__brush", True )
                    , ( "brush-selector__brush--selected", brush == selected )
                    ]
                , HA.style [ ( "background-color", ColorUtil.toColorString brush ) ]
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
