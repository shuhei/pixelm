port module App exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes as SA
import Array2 exposing (Array2)
import Color exposing (Color)
import ColorUtil exposing (RGBA)
import Events
import History exposing (History)


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
    | Bucket
    | Move


type alias Grid =
    Array2 Color


type alias ImagePaths =
    { pencil : String
    , eraser : String
    , bucket : String
    }


type alias Model =
    { mode : Mode
    , isMouseDown : Bool
    , previousMouseDown : Maybe ( Int, Int )
    , foregroundColor : Color
    , colors : List Color
    , history : History Grid
    , grid : Grid
    , images : ImagePaths
    }


makeGrid : Int -> Int -> Color -> Grid
makeGrid cols rows color =
    Array2.initialize cols rows (\x y -> color)


emptyGrid : Grid
emptyGrid =
    makeGrid resolution resolution ColorUtil.transparent


colors : List Color
colors =
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


init : ImagePaths -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { mode = Paint
            , isMouseDown = False
            , previousMouseDown = Nothing
            , foregroundColor = Color.black
            , colors = colors
            , history = History.initialize 20
            , grid = emptyGrid
            , images = flags
            }
    in
        ( model, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | SelectColor Color
    | SelectMode Mode
    | ClearCanvas
    | Undo
    | Download
    | MouseDownOnCanvas ( Int, Int )
    | MouseMoveOnCanvas ( Int, Int )
    | MouseUpOnCanvas ( Int, Int )
    | MouseDownOnContainer
    | MouseUpOnContainer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        SelectMode mode ->
            ( { model | mode = mode }, Cmd.none )

        SelectColor color ->
            ( { model
                | foregroundColor = color
                , mode =
                    if model.mode == Bucket then
                        Bucket
                    else
                        Paint
              }
            , Cmd.none
            )

        ClearCanvas ->
            ( { model
                | history = History.push model.grid model.history
                , grid = emptyGrid
              }
            , Cmd.none
            )

        Undo ->
            let
                ( grid, history ) =
                    History.pop model.history
            in
                ( { model
                    | history = history
                    , grid = Maybe.withDefault model.grid grid
                  }
                , Cmd.none
                )

        MouseDownOnCanvas pos ->
            let
                pixelPos =
                    getPixelPos pos
            in
                ( { model
                    | history = History.push model.grid model.history
                    , grid = updateGrid pixelPos model
                    , isMouseDown = True
                    , previousMouseDown = Just pixelPos
                  }
                , Cmd.none
                )

        MouseMoveOnCanvas pos ->
            let
                pixelPos =
                    getPixelPos pos
            in
                if model.isMouseDown then
                    ( { model
                        | grid = updateGrid pixelPos model
                        , previousMouseDown = Just pixelPos
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        MouseUpOnCanvas pos ->
            let
                pixelPos =
                    getPixelPos pos
            in
                ( { model
                    | grid = updateGrid pixelPos model
                    , isMouseDown = False
                    , previousMouseDown = Nothing
                  }
                , Cmd.none
                )

        MouseDownOnContainer ->
            ( { model
                | history = History.push model.grid model.history
                , isMouseDown = True
              }
            , Cmd.none
            )

        MouseUpOnContainer ->
            ( { model | isMouseDown = False, previousMouseDown = Nothing }
            , Cmd.none
            )

        Download ->
            ( model
            , download <| Array2.map Color.toRgb model.grid
            )


getPixelPos : ( Int, Int ) -> ( Int, Int )
getPixelPos ( x, y ) =
    ( x // pixelSize, y // pixelSize )


updateGrid : ( Int, Int ) -> Model -> Grid
updateGrid ( col, row ) model =
    case model.mode of
        Paint ->
            Array2.set col row model.foregroundColor model.grid

        Eraser ->
            Array2.set col row ColorUtil.transparent model.grid

        Bucket ->
            Array2.fill col row model.foregroundColor model.grid

        Move ->
            case model.previousMouseDown of
                Nothing ->
                    model.grid

                Just ( prevCol, prevRow ) ->
                    Array2.move
                        (col - prevCol)
                        (row - prevRow)
                        ColorUtil.transparent
                        model.grid


port download : Array2 RGBA -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div
        [ HE.onMouseDown <| MouseDownOnContainer
        , HE.onMouseUp <| MouseUpOnContainer
        ]
        [ viewGrid model.grid
        , viewMenus model.mode model.images
        , viewColorSelector model.foregroundColor model.colors
        , viewPalette model.grid
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    Html.div
        [ HA.class "pixel-grid-container"
        , Events.onWithStopAndPrevent "mousedown" <| Events.decodeMouseEvent MouseDownOnCanvas
        , Events.onWithStopAndPrevent "mousemove" <| Events.decodeMouseEvent MouseMoveOnCanvas
        , Events.onWithStopAndPrevent "mouseup" <| Events.decodeMouseEvent MouseUpOnCanvas
        , Events.onWithStopAndPrevent "touchstart" <| Events.decodeTouchEvent MouseDownOnCanvas
        , Events.onWithStopAndPrevent "touchmove" <| Events.decodeTouchEvent MouseMoveOnCanvas
        , Events.onWithStopAndPrevent "touchend" <| Events.decodeTouchEvent MouseUpOnCanvas
        ]
        [ Svg.svg
            [ SA.class "pixel-grid"
            , SA.width <| toString canvasSize
            , SA.height <| toString canvasSize
            ]
            [ viewRects grid
            , viewBorders
            ]
        ]


viewMenus : Mode -> ImagePaths -> Html Msg
viewMenus selectedMode images =
    let
        menu selected msg label content =
            Html.a
                [ HA.classList
                    [ ( "mode", True )
                    , ( "mode--selected", selected )
                    ]
                , HA.href "#"
                , HA.title label
                , HE.onClick msg
                , Events.onWithStopAndPrevent "mousedown" <| Json.succeed NoOp
                ]
                [ content ]

        modeMenu mode content =
            menu (mode == selectedMode) (SelectMode mode) content
    in
        Html.div [ HA.class "menu" ]
            [ modeMenu Paint "Paint" <| svgIcon images.pencil
            , modeMenu Eraser "Eraser" <| svgIcon images.eraser
            , modeMenu Bucket "Bucket" <| svgIcon images.bucket
            , modeMenu Move "Move" <| faIcon "arrows"
            , menu False ClearCanvas "Clear" <| faIcon "trash"
            , menu False Undo "Undo" <| faIcon "undo"
            , menu False Download "Download" <| faIcon "download"
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


viewRects : Grid -> Svg Msg
viewRects grid =
    let
        makeRect col row pixel =
            Svg.rect
                [ SA.width <| toString pixelSize
                , SA.height <| toString pixelSize
                , SA.x <| toString <| col * pixelSize
                , SA.y <| toString <| row * pixelSize
                , SA.fill <| ColorUtil.toColorString pixel
                ]
                []

        rects =
            Array2.toList <| Array2.indexedMap makeRect grid
    in
        Svg.g [] rects


viewColor : Color -> Html Msg
viewColor color =
    Html.div
        [ HA.class "color-selector__color"
        , HA.style [ ( "background-color", ColorUtil.toColorString color ) ]
        , HE.onClick <| SelectColor color
        ]
        []


viewColorSelector : Color -> List Color -> Html Msg
viewColorSelector selected colors =
    let
        separator =
            Html.div [ HA.class "color-selector__separator" ] []

        views =
            viewColor selected :: separator :: List.map viewColor colors
    in
        Html.div [ HA.class "color-selector" ] views


usedColors : Grid -> List Color
usedColors grid =
    let
        putColor c used =
            Dict.insert (ColorUtil.toColorString c) c used
    in
        Array2.foldr putColor Dict.empty grid
            |> Dict.values
            |> List.filter (\x -> x /= ColorUtil.transparent)


viewPalette : Grid -> Html Msg
viewPalette grid =
    usedColors grid
        |> List.map viewColor
        |> Html.div [ HA.class "color-selector" ]


faIcon : String -> Html msg
faIcon name =
    Html.i
        [ (HA.class <| "fa fa-" ++ name) ]
        []


svgIcon : String -> Html msg
svgIcon path =
    Html.img
        [ HA.src path
        , HA.width 32
        ]
        []



---- PROGRAM ----


main : Program ImagePaths Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
