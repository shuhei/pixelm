port module App exposing (..)

import Array.Hamt as Array exposing (Array)
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


type alias Frame =
    { id : Int
    , grid : Grid
    }


type alias Frames =
    SelectionList Frame


type alias ImagePaths =
    { pencil : String
    , eraser : String
    , bucket : String
    , move : String
    , trash : String
    , plus : String
    , undo : String
    , redo : String
    , download : String
    }


type ModalConfig
    = NoModal
    | FrameModal Frame


type alias Model =
    { mode : Mode
    , isMouseDown : Bool
    , previousMouseDown : Maybe ( Int, Int )
    , foregroundColor : Color
    , colors : List Color
    , history : History Frames
    , frames : Frames
    , frameSequence : Int
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


initialFrameSequence : Int
initialFrameSequence =
    0


init : ImagePaths -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { mode = Paint
            , isMouseDown = False
            , previousMouseDown = Nothing
            , foregroundColor = Color.black
            , colors = colors
            , history = History.initialize 50
            , frames = SelectionList.init <| Frame initialFrameSequence emptyGrid
            , frameSequence = initialFrameSequence + 1
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
    | Redo
    | Download
    | SelectFrame Frame
    | DeleteFrame Frame
    | DuplicateFrame Frame
    | ShowFrameModal Frame
    | CloseModal
    | MouseDownOnCanvas ( Int, Int )
    | MouseMoveOnCanvas ( Int, Int )
    | MouseUpOnCanvas
    | MouseDownOnContainer
    | MouseUpOnContainer
    | DropOnFrame Frame
    | Tick


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
                , frames =
                    SelectionList.updateCurrent
                        (\frame -> { frame | grid = emptyGrid })
                        model.frames
              }
            , Cmd.none
            )

        AddFrame ->
            ( { model
                | history = History.push model.frames model.history
                , frames =
                    SelectionList.append
                        { id = model.frameSequence, grid = emptyGrid }
                        model.frames
                , frameSequence = model.frameSequence + 1
              }
            , Cmd.none
            )

        Undo ->
            let
                ( frames, history ) =
                    History.undo model.frames model.history
            in
                ( { model
                    | history = history
                    , frames = Maybe.withDefault model.frames frames
                  }
                , Cmd.none
                )

        Redo ->
            let
                ( frames, history ) =
                    History.redo model.frames model.history
            in
                ( { model
                    | history = history
                    , frames = Maybe.withDefault model.frames frames
                  }
                , Cmd.none
                )

        Download ->
            let
                data =
                    List.map (Array2.toList2 << Array2.map Color.toRgb << .grid) <|
                        SelectionList.toList model.frames
            in
                ( model, download data )

        SelectFrame frame ->
            ( { model | frames = SelectionList.select frame model.frames }
            , Cmd.none
            )

        DeleteFrame frame ->
            ( { model
                | history = History.push model.frames model.history
                , frames = SelectionList.deleteCurrent <| SelectionList.select frame model.frames
                , modalConfig = NoModal
              }
            , Cmd.none
            )

        DuplicateFrame frame ->
            let
                copy =
                    { frame | id = model.frameSequence }
            in
                ( { model
                    | history = History.push model.frames model.history
                    , frames =
                        SelectionList.insertAfterCurrent copy <| SelectionList.select frame model.frames
                    , frameSequence = model.frameSequence + 1
                    , modalConfig = NoModal
                  }
                , Cmd.none
                )

        ShowFrameModal frame ->
            ( { model | modalConfig = FrameModal frame }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalConfig = NoModal }
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

        MouseUpOnCanvas ->
            ( { model
                | isMouseDown = False
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

        DropOnFrame frame ->
            ( { model
                | history = History.push model.frames model.history
                , frames = SelectionList.swapCurrent frame model.frames
              }
            , Cmd.none
            )

        Tick ->
            ( { model
                | frameIndex = (model.frameIndex + 1) % SelectionList.length model.frames
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

        update frame =
            case model.mode of
                Paint ->
                    { frame | grid = Array2.set col row model.foregroundColor frame.grid }

                Eraser ->
                    { frame | grid = Array2.set col row ColorUtil.transparent frame.grid }

                Bucket ->
                    { frame | grid = Array2.fill col row model.foregroundColor frame.grid }

                Move ->
                    case model.previousMouseDown of
                        Nothing ->
                            frame

                        Just ( prevCol, prevRow ) ->
                            { frame
                                | grid =
                                    Array2.move
                                        (col - prevCol)
                                        (row - prevRow)
                                        ColorUtil.transparent
                                        frame.grid
                            }
    in
        { frames | current = update frames.current }


port download : List (List (List RGBA)) -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div
        [ HE.onMouseDown <| MouseDownOnContainer
        , HE.onMouseUp <| MouseUpOnContainer
        ]
        [ viewGrid model.frames.current.grid
        , viewMenus model.mode model.images
        , viewColorSelector model.foregroundColor model.colors <|
            usedColors (Array.toList <| SelectionList.toArray model.frames)
        , viewFrames model.images model.frameIndex model.frames
        , viewModal model.modalConfig <| SelectionList.isSingle model.frames
        ]


viewModal : ModalConfig -> Bool -> Html Msg
viewModal config isSingleFrame =
    let
        deleteButton frame =
            Html.button
                [ HA.class "modal-button modal-button--primary"
                , Events.onWithStopAndPrevent "click" <| Json.succeed (DeleteFrame frame)
                ]
                [ Html.text "Delete Frame" ]

        duplicateButton frame =
            Html.button
                [ HA.class "modal-button modal-button--primary"
                , Events.onWithStopAndPrevent "click" <| Json.succeed (DuplicateFrame frame)
                ]
                [ Html.text "Duplicate Frame" ]

        closeButton =
            Html.button
                [ HA.class "modal-button modal-button--default"
                , Events.onWithStopAndPrevent "click" <| Json.succeed CloseModal
                ]
                [ Html.text "Close" ]

        buttons frame =
            if isSingleFrame then
                [ duplicateButton frame, closeButton ]
            else
                [ duplicateButton frame, deleteButton frame, closeButton ]

        content =
            case config of
                NoModal ->
                    []

                FrameModal frame ->
                    [ Html.p [] <| buttons frame ]
    in
        Html.div
            [ HA.classList
                [ ( "modal", True )
                , ( "modal--shown", config /= NoModal )
                ]
            , HE.onClick CloseModal
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
        , Events.onWithStopAndPrevent "mouseup" <| Json.succeed MouseUpOnCanvas
        , Events.onWithStopAndPrevent "touchstart" <| Events.decodeTouchEvent MouseDownOnCanvas
        , Events.onWithStopAndPrevent "touchmove" <| Events.decodeTouchEvent MouseMoveOnCanvas
        , Events.onWithStopAndPrevent "touchend" <| Json.succeed MouseUpOnCanvas
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
            , menu False Redo "Redo" <| svgIcon images.redo
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


usedColors : List Frame -> List Color
usedColors frames =
    let
        putColor c used =
            Dict.insert (ColorUtil.toColorString c) c used
    in
        List.foldr (\frame used -> Array2.foldr putColor used frame.grid) Dict.empty frames
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


viewFrame : FrameType -> Frame -> Html Msg
viewFrame frameType frame =
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
                  , HA.draggable <|
                        if frameType == FrameNormal || frameType == FrameSelected then
                            "true"
                        else
                            "false"
                  , Events.allowDrop
                  , Events.onDragStart <| SelectFrame frame
                  , Events.onDrop <| DropOnFrame frame
                  , sizeStyle canvasSize
                  ]
                , if frameType == FramePreview then
                    []
                  else
                    [ HE.onClick <| SelectFrame frame
                    , HE.onDoubleClick <| ShowFrameModal frame
                    ]
                ]
    in
        Html.div
            attrs
            [ Svg.svg
                [ SA.width <| toString canvasSize
                , SA.height <| toString canvasSize
                ]
                [ viewRects framePixelSize frame.grid
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
    Time.every (1000 / (60 / toFloat model.fps) * Time.millisecond) (\_ -> Tick)


main : Program ImagePaths Model Msg
main =
    Html.programWithFlags
        { view = HL.lazy view
        , init = init
        , update = update
        , subscriptions = tick
        }
