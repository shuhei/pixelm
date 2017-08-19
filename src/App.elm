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


type alias Frames =
    { previous : List Grid
    , current : Grid
    , next : List Grid
    }


type alias ImagePaths =
    { pencil : String
    , eraser : String
    , bucket : String
    , move : String
    , trash : String
    , undo : String
    , download : String
    }


type alias Model =
    { mode : Mode
    , isMouseDown : Bool
    , previousMouseDown : Maybe ( Int, Int )
    , foregroundColor : Color
    , colors : List Color
    , history : History Frames
    , frames : Frames
    , images : ImagePaths
    }


makeGrid : Int -> Int -> Color -> Grid
makeGrid cols rows color =
    Array2.initialize cols rows (\x y -> color)


emptyGrid : Grid
emptyGrid =
    makeGrid resolution resolution ColorUtil.transparent


initFrames : Frames
initFrames =
    { previous = []
    , current = emptyGrid
    , next = []
    }


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
            , frames = initFrames
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
                | history = History.push model.frames model.history
                , frames = clearCurrentFrame model.frames
              }
            , Cmd.none
            )

        Undo ->
            let
                ( frames, history ) =
                    History.pop model.history
            in
                ( { model
                    | history = history
                    , frames = Maybe.withDefault model.frames frames
                  }
                , Cmd.none
                )

        MouseDownOnCanvas pos ->
            let
                pixelPos =
                    getPixelPos pos
            in
                ( { model
                    | history = History.push model.frames model.history
                    , frames = updateCurrentFrame pixelPos model
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
                        | frames = updateCurrentFrame pixelPos model
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
                    | frames = updateCurrentFrame pixelPos model
                    , isMouseDown = False
                    , previousMouseDown = Nothing
                  }
                , Cmd.none
                )

        MouseDownOnContainer ->
            ( { model
                | history = History.push model.frames model.history
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
            , download <| Array2.map Color.toRgb model.frames.current
            )


getPixelPos : ( Int, Int ) -> ( Int, Int )
getPixelPos ( x, y ) =
    ( x // pixelSize, y // pixelSize )


clearCurrentFrame : Frames -> Frames
clearCurrentFrame frames =
    { frames | current = emptyGrid }


updateCurrentFrame : ( Int, Int ) -> Model -> Frames
updateCurrentFrame ( col, row ) model =
    let
        frames =
            model.frames

        current =
            frames.current

        updated =
            case model.mode of
                Paint ->
                    Array2.set col row model.foregroundColor current

                Eraser ->
                    Array2.set col row ColorUtil.transparent current

                Bucket ->
                    Array2.fill col row model.foregroundColor current

                Move ->
                    case model.previousMouseDown of
                        Nothing ->
                            current

                        Just ( prevCol, prevRow ) ->
                            Array2.move
                                (col - prevCol)
                                (row - prevRow)
                                ColorUtil.transparent
                                current
    in
        { frames | current = updated }


port download : Array2 RGBA -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div
        [ HE.onMouseDown <| MouseDownOnContainer
        , HE.onMouseUp <| MouseUpOnContainer
        ]
        [ viewGrid model.frames.current
        , viewMenus model.mode model.images
        , viewColorSelector model.foregroundColor model.colors <| usedColors model.frames.current
        , viewFrames model.frames
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    Html.div
        [ HA.class "pixel-grid-container"
        , HA.style
            [ ( "width", toString (resolution * pixelSize) ++ "px" )
            , ( "height", toString (resolution * pixelSize) ++ "px" )
            ]
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
            [ viewRects pixelSize grid
            , viewBorders
            ]
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


viewRects : Int -> Grid -> Svg Msg
viewRects size grid =
    let
        makeRect col row pixel =
            Svg.rect
                [ SA.width <| toString size
                , SA.height <| toString size
                , SA.x <| toString <| col * size
                , SA.y <| toString <| row * size
                , SA.fill <| ColorUtil.toColorString pixel
                ]
                []

        rects =
            Array2.toList <| Array2.indexedMap makeRect grid
    in
        Svg.g [] rects


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
            , modeMenu Move "Move" <| svgIcon images.move
            , menu False ClearCanvas "Clear" <| svgIcon images.trash
            , menu False Undo "Undo" <| svgIcon images.undo
            , menu False Download "Download" <| svgIcon images.download
            ]


viewColorSelector : Color -> List Color -> List Color -> Html Msg
viewColorSelector selected colors usedColors =
    Html.div
        [ HA.class "color-selector" ]
        [ Html.div [] <|
            viewColor [ HA.class "color-selector__color--foreground" ] selected
                :: List.map (viewColor []) usedColors
        , Html.div [] <|
            List.map (viewColor []) colors
        ]


viewColor : List (Html.Attribute Msg) -> Color -> Html Msg
viewColor attrs color =
    let
        attributes =
            List.append
                [ HA.class "color-selector__color"
                , HA.style [ ( "background-color", ColorUtil.toColorString color ) ]
                , HE.onClick <| SelectColor color
                ]
                attrs
    in
        Html.div attributes []


usedColors : Grid -> List Color
usedColors grid =
    let
        putColor c used =
            Dict.insert (ColorUtil.toColorString c) c used
    in
        Array2.foldr putColor Dict.empty grid
            |> Dict.values
            |> List.filter (\x -> x /= ColorUtil.transparent)


viewFrames : Frames -> Html Msg
viewFrames frames =
    Html.div
        [ HA.class "frame-list" ]
        [ viewFrame frames.current ]


viewFrame : Grid -> Html Msg
viewFrame grid =
    let
        size =
            4

        canvasSize =
            resolution * size
    in
        Html.div
            []
            [ Svg.svg
                [ SA.class "frame"
                , SA.width <| toString canvasSize
                , SA.height <| toString canvasSize
                ]
                [ viewRects size grid
                ]
            ]


faIcon : String -> Html msg
faIcon name =
    Html.i
        [ HA.class <| "fa fa-" ++ name ]
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
