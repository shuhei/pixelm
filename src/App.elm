port module App exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Dict
import Svg exposing (Svg)
import Svg.Attributes as SA
import Array2 exposing (Array2)
import Color exposing (Color)
import ColorUtil exposing (RGBA)
import Events


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


type alias Model =
    { mode : Mode
    , isMouseDown : Bool
    , previousMouseDown : Maybe ( Int, Int )
    , foregroundColor : Color
    , colors : List Color
    , grid : Grid
    }


makeGrid : Int -> Int -> Color -> Grid
makeGrid cols rows color =
    Array2.initialize cols rows (\x y -> color)


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


emptyGrid : Grid
emptyGrid =
    makeGrid resolution resolution ColorUtil.transparent


init : String -> ( Model, Cmd Msg )
init path =
    let
        model =
            { mode = Paint
            , isMouseDown = False
            , previousMouseDown = Nothing
            , foregroundColor = Color.black
            , colors = colors
            , grid = makeGrid resolution resolution ColorUtil.transparent
            }
    in
        ( model, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | SelectColor Color
    | SelectMode Mode
    | ClearCanvas
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

        Download ->
            ( model
            , download <| Array2.map Color.toRgb model.grid
            )

        ClearCanvas ->
            ( { model | grid = emptyGrid }
            , Cmd.none
            )

        MouseDownOnCanvas pos ->
            let
                pixelPos =
                    getPixelPos pos
            in
                ( { model
                    | grid = updateGrid pixelPos model
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
            ( { model | isMouseDown = True }
            , Cmd.none
            )

        MouseUpOnContainer ->
            ( { model | isMouseDown = False, previousMouseDown = Nothing }
            , Cmd.none
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
        , viewMenus model.mode
        , viewColorSelector model.foregroundColor model.colors
        , viewPalette model.grid
        , viewDownload
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    Html.div
        [ HA.class "pixel-grid-container"
        , HE.on "mousedown" <| Events.decodeMouseEvent MouseDownOnCanvas
        , HE.on "mousemove" <| Events.decodeMouseEvent MouseMoveOnCanvas
        , HE.on "mouseup" <| Events.decodeMouseEvent MouseUpOnCanvas
        , HE.on "touchstart" <| Events.decodeTouchEvent MouseDownOnCanvas
        , HE.on "touchmove" <| Events.decodeTouchEvent MouseMoveOnCanvas
        , HE.on "touchend" <| Events.decodeTouchEvent MouseUpOnCanvas
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


viewMenus : Mode -> Html Msg
viewMenus selectedMode =
    let
        menu selected msg label iconName =
            Html.a
                [ HA.classList
                    [ ( "mode", True )
                    , ( "mode--selected", selected )
                    ]
                , HA.href "#"
                , HA.title label
                , HE.onClick msg
                ]
                [ icon iconName [] ]

        modeMenu mode =
            menu (mode == selectedMode) (SelectMode mode)
    in
        Html.div []
            [ modeMenu Paint "Paint" "paint-brush"
            , modeMenu Eraser "Eraser" "eraser"
            , modeMenu Bucket "Bucket" "shopping-basket"
            , modeMenu Move "Move" "arrows"
            , menu False ClearCanvas "Clear" "trash"
            ]


viewDownload : Html Msg
viewDownload =
    Html.div []
        [ Html.a
            [ HA.href "#"
            , HA.title "Download"
            , HE.onClick Download
            ]
            [ icon "download" [] ]
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


icon : String -> List (Html.Attribute msg) -> Html msg
icon name attrs =
    Html.i
        ((HA.class <| "fa fa-" ++ name) :: attrs)
        []



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
