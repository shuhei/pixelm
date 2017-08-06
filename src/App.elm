port module App exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Array2 exposing (Array2)
import Color exposing (Color)
import ColorUtil exposing (RGBA)


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


type Mode
    = Paint
    | Eraser


type alias Grid =
    Array2 Color


type alias Model =
    { mode : Mode
    , brushColor : Color
    , brushes : List Color
    , grid : Grid
    }


makeGrid : Int -> Int -> Color -> Grid
makeGrid cols rows color =
    Array2.initialize cols rows (\x y -> color)


brushes : List Color
brushes =
    [ Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.lightRed
    , Color.lightOrange
    , Color.lightYellow
    , Color.lightGreen
    , Color.lightBlue
    , Color.lightPurple
    , Color.darkRed
    , Color.darkOrange
    , Color.darkYellow
    , Color.darkGreen
    , Color.darkBlue
    , Color.darkPurple
    , Color.white
    , Color.lightGray
    , Color.gray
    , Color.darkGray
    , Color.lightCharcoal
    , Color.charcoal
    , Color.darkCharcoal
    , Color.black
    ]


init : String -> ( Model, Cmd Msg )
init path =
    let
        model =
            { mode = Paint
            , brushColor = Color.black
            , brushes = brushes
            , grid = makeGrid resolution resolution ColorUtil.transparent
            }
    in
        ( model, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | ClickPixel Int Int
    | SelectBrush Color
    | SelectMode Mode
    | Download


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectMode mode ->
            ( { model | mode = mode }, Cmd.none )

        ClickPixel x y ->
            case model.mode of
                Paint ->
                    ( { model | grid = Array2.set x y model.brushColor model.grid }
                    , Cmd.none
                    )

                Eraser ->
                    ( { model | grid = Array2.set x y ColorUtil.transparent model.grid }
                    , Cmd.none
                    )

        SelectBrush color ->
            ( { model | brushColor = color, mode = Paint }
            , Cmd.none
            )

        Download ->
            ( model
            , download <| Array2.map Color.toRgb model.grid
            )


port download : Array2 RGBA -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div []
        [ Svg.svg
            [ SA.class "pixel-grid"
            , SA.width <| toString canvasSize
            , SA.height <| toString canvasSize
            ]
            [ viewGrid model.grid
            , viewBorders
            ]
        , viewModes model.mode
        , viewBrushSelector model.brushColor model.brushes
        , viewDownload
        ]


viewModes : Mode -> Html Msg
viewModes selectedMode =
    let
        menu mode label =
            Html.div []
                [ Html.a
                    [ HA.classList
                        [ ( "mode", True )
                        , ( "mode--selected", mode == selectedMode )
                        ]
                    , HA.href "#"
                    , HE.onClick <| SelectMode mode
                    ]
                    [ Html.text label ]
                ]
    in
        Html.div []
            [ menu Paint "Paint"
            , menu Eraser "Eraser"
            ]


viewDownload : Html Msg
viewDownload =
    Html.div []
        [ Html.a
            [ HA.href "#"
            , HE.onClick Download
            ]
            [ Html.text "Download" ]
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
                , SE.onMouseDown <| ClickPixel col row
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
            Html.div
                [ HA.class "brush-selector__brush"
                , HA.style [ ( "background-color", ColorUtil.toColorString brush ) ]
                , HE.onClick <| SelectBrush brush
                ]
                []

        viewBrushes =
            List.map viewBrush brushes

        viewSelected =
            viewBrush selected

        separator =
            Html.div [ HA.class "brush-selector__separator" ] []

        views =
            viewSelected :: separator :: viewBrushes
    in
        Html.div [ HA.class "brush-selector" ] views



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
