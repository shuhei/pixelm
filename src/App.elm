port module App exposing (..)

import Array exposing (Array)
import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy as HL
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Time)
import Array2 exposing (Array2)
import Color exposing (Color)
import ColorUtil exposing (RGBA)
import Events
import SelectionList exposing (SelectionList)
import History exposing (History)


---- CONSTANTS ----


resolution : Int
resolution =
    16


pixelSize : Int
pixelSize =
    20


framePixelSize : Int
framePixelSize =
    4


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
    SelectionList Grid


type alias ImagePaths =
    { pencil : String
    , eraser : String
    , bucket : String
    , move : String
    , trash : String
    , plus : String
    , undo : String
    , download : String
    }


type ModalConfig
    = NoModal
    | FrameModal Grid


type alias Model =
    { mode : Mode
    , isMouseDown : Bool
    , previousMouseDown : Maybe ( Int, Int )
    , foregroundColor : Color
    , colors : List Color
    , history : History Frames
    , frames : Frames
    , fps : Int
    , frameIndex : Int
    , images : ImagePaths
    , modalConfig : ModalConfig
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
            , frames = SelectionList.init emptyGrid
            , fps = 10
            , frameIndex = 0
            , images = flags
            , modalConfig = NoModal
            }
    in
        ( model, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | SelectColor Color
    | SelectMode Mode
    | ClearCanvas
    | AddFrame
    | Undo
    | Download
    | SelectFrame Grid
    | DeleteFrame Grid
    | ShowFrameModal Grid
    | MouseDownOnCanvas ( Int, Int )
    | MouseMoveOnCanvas ( Int, Int )
    | MouseUpOnCanvas ( Int, Int )
    | MouseDownOnContainer
    | MouseUpOnContainer
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                , frames = SelectionList.updateCurrent emptyGrid model.frames
              }
            , Cmd.none
            )

        AddFrame ->
            ( { model
                | frames = SelectionList.append emptyGrid model.frames
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

        Download ->
            ( model
            , download <| Array.map (Array2.map Color.toRgb) <| SelectionList.toArray model.frames
            )

        SelectFrame frame ->
            ( { model | frames = SelectionList.selectCurrent frame model.frames }
            , Cmd.none
            )

        DeleteFrame grid ->
            ( { model
                | history = History.push model.frames model.history
                , frames = SelectionList.deleteCurrent <| SelectionList.selectCurrent grid model.frames
                , modalConfig = NoModal
              }
            , Cmd.none
            )

        ShowFrameModal grid ->
            ( { model | modalConfig = FrameModal grid }
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

        Tick time ->
            ( { model
                | frameIndex = (floor <| Time.inMilliseconds time) // model.fps
              }
            , Cmd.none
            )


getPixelPos : ( Int, Int ) -> ( Int, Int )
getPixelPos ( x, y ) =
    ( x // pixelSize, y // pixelSize )


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


port download : Array (Array2 RGBA) -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div
        [ HE.onMouseDown <| MouseDownOnContainer
        , HE.onMouseUp <| MouseUpOnContainer
        ]
        [ viewGrid model.frames.current
        , viewMenus model.mode model.images
        , viewColorSelector model.foregroundColor model.colors <|
            usedColors (Array.toList <| SelectionList.toArray model.frames)
        , viewFrames model.images model.frameIndex model.frames
        , viewModal model.modalConfig
        ]


viewModal : ModalConfig -> Html Msg
viewModal config =
    let
        content =
            case config of
                NoModal ->
                    []

                FrameModal frame ->
                    [ Html.p
                        []
                        [ Html.button
                            [ HA.class "modal-button"
                            , HE.onClick <| DeleteFrame frame
                            ]
                            [ Html.text "Delete Frame" ]
                        ]
                    ]
    in
        Html.div
            [ HA.classList
                [ ( "modal", True )
                , ( "modal--shown", config /= NoModal )
                ]
            ]
            [ Html.div
                [ HA.class "modal-content" ]
                content
            ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    Html.div
        [ HA.class "pixel-grid-container"
        , sizeStyle (resolution * pixelSize)
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


usedColors : List Grid -> List Color
usedColors grids =
    let
        putColor c used =
            Dict.insert (ColorUtil.toColorString c) c used
    in
        List.foldr (\grid used -> Array2.foldr putColor used grid) Dict.empty grids
            |> Dict.values
            |> List.filter (\x -> x /= ColorUtil.transparent)


type FrameType
    = FrameNormal
    | FrameSelected
    | FramePreview


viewFrames : ImagePaths -> Int -> Frames -> Html Msg
viewFrames images index frames =
    Html.div
        [ HA.class "frame-list" ]
    <|
        List.concat
            [ if SelectionList.isSingle frames then
                []
              else
                [ viewFrame FramePreview <| SelectionList.get index frames ]
            , List.map (viewFrame FrameNormal) <| Array.toList frames.previous
            , [ viewFrame FrameSelected frames.current ]
            , List.map (viewFrame FrameNormal) <| Array.toList frames.next
            , [ viewAddFrame images ]
            ]


viewFrame : FrameType -> Grid -> Html Msg
viewFrame frameType grid =
    let
        canvasSize =
            resolution * framePixelSize

        attrs =
            List.concat
                [ [ HA.classList
                        [ ( "frame", True )
                        , ( "frame--normal", frameType == FrameNormal )
                        , ( "frame--selected", frameType == FrameSelected )
                        , ( "frame--preview", frameType == FramePreview )
                        ]
                  , sizeStyle canvasSize
                  ]
                , if frameType == FramePreview then
                    []
                  else
                    [ HE.onClick <| SelectFrame grid
                    , HE.onDoubleClick <| ShowFrameModal grid
                    ]
                ]
    in
        Html.div
            attrs
            [ Svg.svg
                [ SA.width <| toString canvasSize
                , SA.height <| toString canvasSize
                ]
                [ viewRects framePixelSize grid
                ]
            ]


viewAddFrame : ImagePaths -> Html Msg
viewAddFrame images =
    Html.div
        [ HA.class "frame frame--plus"
        , sizeStyle (resolution * framePixelSize)
        , HE.onClick AddFrame
        ]
        [ svgIcon images.plus ]


sizeStyle : Int -> Html.Attribute msg
sizeStyle size =
    HA.style
        [ ( "width", toString size ++ "px" )
        , ( "height", toString size ++ "px" )
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


tick : Model -> Sub Msg
tick model =
    Time.every (1000 / (60 / toFloat model.fps) * Time.millisecond) Tick


main : Program ImagePaths Model Msg
main =
    Html.programWithFlags
        { view = HL.lazy view
        , init = init
        , update = update
        , subscriptions = tick
        }
