port module App exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Dict
import Set
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
    | Bucket


type alias Grid =
    Array2 Color


type alias Model =
    { mode : Mode
    , isMouseDown : Bool
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


init : String -> ( Model, Cmd Msg )
init path =
    let
        model =
            { mode = Paint
            , isMouseDown = False
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
    | Download
    | MouseDownOnPixel Int Int
    | MouseOverOnPixel Int Int
    | MouseUpOnPixel Int Int
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
            ( { model | foregroundColor = color }
            , Cmd.none
            )

        Download ->
            ( model
            , download <| Array2.map Color.toRgb model.grid
            )

        MouseDownOnPixel x y ->
            ( setMouseDown True { model | grid = updateGrid x y model }
            , Cmd.none
            )

        MouseOverOnPixel x y ->
            if model.isMouseDown then
                ( { model | grid = updateGrid x y model }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        MouseUpOnPixel x y ->
            ( setMouseDown False { model | grid = updateGrid x y model }
            , Cmd.none
            )

        MouseDownOnContainer ->
            ( setMouseDown True model, Cmd.none )

        MouseUpOnContainer ->
            ( setMouseDown False model, Cmd.none )


updateGrid : Int -> Int -> Model -> Grid
updateGrid x y model =
    case model.mode of
        Paint ->
            Array2.set x y model.foregroundColor model.grid

        Eraser ->
            Array2.set x y ColorUtil.transparent model.grid

        Bucket ->
            case Array2.get x y model.grid of
                Nothing ->
                    model.grid

                Just color ->
                    fillColor color model.foregroundColor x y model.grid


fillColor : Color -> Color -> Int -> Int -> Grid -> Grid
fillColor fromColor toColor x y grid =
    let
        fill ( x, y ) ( visited, grid ) =
            case Array2.get x y grid of
                Nothing ->
                    ( visited, grid )

                Just c ->
                    if Set.member ( x, y ) visited then
                        ( visited, grid )
                    else if c == fromColor then
                        let
                            state =
                                ( Set.insert ( x, y ) visited, Array2.set x y toColor grid )

                            neighbors =
                                [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
                        in
                            List.foldl fill state neighbors
                    else
                        ( Set.insert ( x, y ) visited, grid )

        ( _, nextGrid ) =
            fill ( x, y ) ( Set.empty, grid )
    in
        nextGrid


setMouseDown : Bool -> Model -> Model
setMouseDown mouseDown model =
    { model | isMouseDown = mouseDown }


port download : Array2 RGBA -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div
        [ HE.onMouseDown <| MouseDownOnContainer
        , HE.onMouseUp <| MouseUpOnContainer
        ]
        [ Svg.svg
            [ SA.class "pixel-grid"
            , SA.width <| toString canvasSize
            , SA.height <| toString canvasSize
            ]
            [ viewGrid model.grid
            , viewBorders
            ]
        , viewModes model.mode
        , viewColorSelector model.foregroundColor model.colors
        , viewPalette model.grid
        , viewDownload
        ]


viewModes : Mode -> Html Msg
viewModes selectedMode =
    let
        menu mode label iconName =
            Html.a
                [ HA.classList
                    [ ( "mode", True )
                    , ( "mode--selected", mode == selectedMode )
                    ]
                , HA.href "#"
                , HA.title label
                , HE.onClick <| SelectMode mode
                ]
                [ icon iconName [] ]
    in
        Html.div []
            [ menu Paint "Paint" "paint-brush"
            , menu Eraser "Eraser" "eraser"
            , menu Bucket "Bucket" "shopping-basket"
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
                , SE.onMouseDown <| MouseDownOnPixel col row
                , SE.onMouseOver <| MouseOverOnPixel col row
                , SE.onMouseUp <| MouseUpOnPixel col row
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


viewPalette : Grid -> Html Msg
viewPalette grid =
    let
        putColor c used =
            Dict.insert (ColorUtil.toColorString c) c used

        usedColors =
            Array2.foldr putColor Dict.empty grid
                |> Dict.values
                |> List.filter (\x -> x /= ColorUtil.transparent)

        views =
            List.map viewColor usedColors
    in
        Html.div [ HA.class "color-selector" ] views


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
