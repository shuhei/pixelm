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


framePixelSize : Float
framePixelSize =
    4



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
    | DownloadModal
    | ColorModal Float


type alias Model =
    { mode : Mode
    , isMouseDown : Bool
    , previousMouseDown : Maybe ( Int, Int )
    , foregroundColor : Color
    , history : History Frames
    , frames : Frames
    , frameSequence : Int
    , fps : Int
    , frameIndex : Int
    , images : ImagePaths
    , modalConfig : ModalConfig
    , resolution : Int
    , pixelSize : Float
    , zoom : Float
    , offset : ( Float, Float )
    }


makeGrid : Int -> Int -> Color -> Grid
makeGrid cols rows color =
    Array2.initialize cols rows (\x y -> color)


emptyGrid : Int -> Grid
emptyGrid resolution =
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
            , history = History.initialize 50
            , frames =
                SelectionList.init <|
                    Frame initialFrameSequence (emptyGrid 16)
            , frameSequence = initialFrameSequence + 1
            , fps = 10
            , frameIndex = 0
            , images = flags
            , modalConfig = NoModal
            , resolution = 16
            , pixelSize = 20
            , zoom = 1
            , offset = ( 0, 0 )
            }
    in
        ( model, Cmd.none )


type DownloadFormat
    = SVGFormat
    | GIFFormat
    | AnimatedGIFFormat



---- UPDATE ----


type Msg
    = NoOp
    | SelectColor Color
    | SelectMode Mode
    | ClearCanvas
    | AddFrame
    | Undo
    | Redo
    | Download DownloadFormat
    | SelectFrame Frame
    | DeleteFrame Frame
    | DuplicateFrame Frame
    | ShowFrameModal Frame
    | ShowDownloadModal
    | ShowColorModal
    | SelectHue Float
    | CloseModal
    | MouseDownOnCanvas ( Float, Float )
    | MouseMoveOnCanvas ( Float, Float )
    | MouseUpOnCanvas
    | MouseWheelOnCanvas ( Float, Float ) ( Float, Float )
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
                        (\frame -> { frame | grid = emptyGrid model.resolution })
                        model.frames
              }
            , Cmd.none
            )

        AddFrame ->
            ( { model
                | history = History.push model.frames model.history
                , frames =
                    SelectionList.append
                        { id = model.frameSequence, grid = emptyGrid model.resolution }
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

        Download format ->
            let
                data =
                    { grids =
                        List.map (Array2.toList2 << Array2.map Color.toRgb << .grid) <|
                            SelectionList.toList model.frames
                    , format =
                        case format of
                            SVGFormat ->
                                "svg"

                            GIFFormat ->
                                "gif"

                            AnimatedGIFFormat ->
                                "animated-gif"
                    }
            in
                ( { model | modalConfig = NoModal }
                , download data
                )

        SelectFrame frame ->
            ( { model
                | frames = SelectionList.select frame model.frames
              }
            , Cmd.none
            )

        DeleteFrame frame ->
            ( { model
                | history = History.push model.frames model.history
                , frames =
                    SelectionList.deleteCurrent <|
                        SelectionList.select frame model.frames
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
                        SelectionList.insertAfterCurrent copy <|
                            SelectionList.select frame model.frames
                    , frameSequence = model.frameSequence + 1
                    , modalConfig = NoModal
                  }
                , Cmd.none
                )

        ShowFrameModal frame ->
            ( { model | modalConfig = FrameModal frame }
            , Cmd.none
            )

        ShowDownloadModal ->
            ( { model | modalConfig = DownloadModal }
            , Cmd.none
            )

        ShowColorModal ->
            ( { model
                | modalConfig = ColorModal <| ColorUtil.hue model.foregroundColor
              }
            , Cmd.none
            )

        SelectHue hue ->
            ( { model | modalConfig = ColorModal hue }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalConfig = NoModal }
            , Cmd.none
            )

        MouseDownOnCanvas pos ->
            let
                pixelPos =
                    getPixelPos model.pixelSize model.zoom model.offset pos
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
                    getPixelPos model.pixelSize model.zoom model.offset pos
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

        MouseWheelOnCanvas ( _, deltaY ) pos ->
            let
                zoomBy =
                    if deltaY > 0 then
                        1.1
                    else
                        1 / 1.1

                zoom =
                    model.zoom * zoomBy

                original =
                    originalPos model.zoom model.offset pos

                offset =
                    interpolate original model.offset (1 / zoomBy)
            in
                if zoom <= 1 then
                    ( { model
                        | zoom = 1
                        , offset = ( 0, 0 )
                      }
                    , Cmd.none
                    )
                else if zoom > 1 && zoom < 10 then
                    ( { model
                        | zoom = zoom
                        , offset = offset
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        MouseDownOnContainer ->
            ( { model
                | history = History.push model.frames model.history
                , isMouseDown = True
              }
            , Cmd.none
            )

        MouseUpOnContainer ->
            ( { model
                | isMouseDown = False
                , previousMouseDown = Nothing
              }
            , Cmd.none
            )

        DropOnFrame frame ->
            ( { model
                | history = History.push model.frames model.history
                , frames = SelectionList.swapCurrent frame model.frames
                , isMouseDown = False
                , previousMouseDown = Nothing
              }
            , Cmd.none
            )

        Tick ->
            ( { model
                | frameIndex = (model.frameIndex + 1) % SelectionList.length model.frames
              }
            , Cmd.none
            )


interpolate : ( Float, Float ) -> ( Float, Float ) -> Float -> ( Float, Float )
interpolate ( x1, y1 ) ( x2, y2 ) ratio =
    ( x1 + ratio * (x2 - x1)
    , y1 + ratio * (y2 - y1)
    )


{-| Convert screen position into original position
-}
originalPos : Float -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
originalPos zoom ( offsetX, offsetY ) ( x, y ) =
    ( offsetX + x / zoom
    , offsetY + y / zoom
    )


getPixelPos : Float -> Float -> ( Float, Float ) -> ( Float, Float ) -> ( Int, Int )
getPixelPos pixelSize zoom offset pos =
    let
        ( ox, oy ) =
            originalPos zoom offset pos
    in
        ( floor (ox / pixelSize), floor (oy / pixelSize) )


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


type alias DownloadData =
    { grids : List (List (List RGBA))
    , format : String
    }


port download : DownloadData -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div
        [ HE.onMouseDown <| MouseDownOnContainer
        , HE.onMouseUp <| MouseUpOnContainer
        ]
        [ viewGrid model.resolution model.pixelSize model.zoom model.offset model.mode model.frames.current.grid
        , viewMenus model.mode model.images
        , viewCurrentColor model.foregroundColor <|
            usedColors (Array.toList <| SelectionList.toArray model.frames)
        , viewFrames model.resolution model.images model.frameIndex model.frames
        , viewModal model.modalConfig (SelectionList.isSingle model.frames) model.foregroundColor
        ]


viewModal : ModalConfig -> Bool -> Color -> Html Msg
viewModal config isSingleFrame foregroundColor =
    let
        viewButton style text msg =
            Html.button
                [ HA.class <| "modal-button modal-button--" ++ style
                , Events.onWithStopAndPrevent "click" <| Json.succeed msg
                ]
                [ Html.text text ]

        deleteButton frame =
            viewButton "primary" "Delete Frame" <| DeleteFrame frame

        duplicateButton frame =
            viewButton "primary" "Duplicate Frame" <| DuplicateFrame frame

        closeButton =
            viewButton "default" "Close" CloseModal

        content =
            case config of
                NoModal ->
                    []

                DownloadModal ->
                    if isSingleFrame then
                        [ viewButton "primary" "SVG" <| Download SVGFormat
                        , viewButton "primary" "GIF" <| Download GIFFormat
                        , closeButton
                        ]
                    else
                        [ viewButton "primary" "Animated GIF" <| Download AnimatedGIFFormat
                        , closeButton
                        ]

                FrameModal frame ->
                    if isSingleFrame then
                        [ duplicateButton frame, closeButton ]
                    else
                        [ duplicateButton frame, deleteButton frame, closeButton ]

                ColorModal hue ->
                    viewColorModal hue foregroundColor
    in
        Html.div
            [ HA.classList
                [ ( "modal", True )
                , ( "modal--shown", config /= NoModal )
                ]
            , HE.onClick CloseModal
            ]
            [ Html.div
                [ HA.class "modal-content"
                , Events.stopPropagation "onclick"
                ]
                content
            ]


viewColorModal : Float -> Color -> List (Html Msg)
viewColorModal selectedHue selectedColor =
    let
        zeroToOne count =
            List.range 0 (count - 1)
                |> List.map (\i -> toFloat i / toFloat (count - 1))

        hues count =
            zeroToOne (count + 1)
                |> List.take count
                |> List.map (\hue -> ColorUtil.hsv (degrees 360 * hue) 1 1)

        hsvs hue count =
            zeroToOne count
                |> List.reverse
                |> List.map
                    (\value ->
                        zeroToOne count
                            |> List.map (\saturation -> ColorUtil.hsv hue saturation value)
                    )

        svPickers =
            hsvs selectedHue 12
                |> List.map
                    (\colors ->
                        Html.div [ HA.class "color-picker__row" ] <|
                            List.map (\c -> viewColor [] SelectColor (c == selectedColor && Color.black /= c) c) colors
                    )

        selectHue color =
            SelectHue <| ColorUtil.hue color

        closeHue hue =
            hue - 0.001 <= selectedHue && selectedHue <= hue + 0.001

        huePickers =
            hues 12
                |> List.map (\h -> viewColor [] selectHue (closeHue <| ColorUtil.hue h) h)
    in
        [ Html.div
            [ HA.class "color-selector__row" ]
            (List.map (\c -> viewColor [] SelectColor (c == selectedColor) c) colors)
        , Html.div
            [ HA.class "color-picker" ]
            [ Html.div
                [ HA.class "color-picker__colors" ]
                svPickers
            , Html.div
                [ HA.class "color-picker__hues" ]
                huePickers
            ]
        ]


viewBox : Float -> Float -> Float -> Float -> String
viewBox minX minY width height =
    [ minX, minY, width, height ]
        |> List.map toString
        |> String.join " "


viewGrid : Int -> Float -> Float -> ( Float, Float ) -> Mode -> Grid -> Html Msg
viewGrid resolution pixelSize zoom ( offsetX, offsetY ) mode grid =
    let
        viewSize =
            (toFloat resolution * pixelSize) / zoom
    in
        Html.div
            [ HA.class "pixel-grid-container"
            , sizeStyle (toFloat resolution * pixelSize)
            , HA.style
                [ ( "cursor"
                  , if mode == Move then
                        "move"
                    else
                        "pointer"
                  )
                ]
            , Events.onWithStopAndPrevent "mousedown" <| Events.decodeMouseEvent MouseDownOnCanvas
            , Events.onWithStopAndPrevent "mousemove" <| Events.decodeMouseEvent MouseMoveOnCanvas
            , Events.onWithStopAndPrevent "mouseup" <| Json.succeed MouseUpOnCanvas
            , Events.onWithStopAndPrevent "touchstart" <| Events.decodeTouchEvent MouseDownOnCanvas
            , Events.onWithStopAndPrevent "touchmove" <| Events.decodeTouchEvent MouseMoveOnCanvas
            , Events.onWithStopAndPrevent "touchend" <| Json.succeed MouseUpOnCanvas
            , Events.onWithStopAndPrevent "mousewheel" <| Events.decodeWheelEvent MouseWheelOnCanvas
            ]
            [ Svg.svg
                [ SA.class "pixel-grid"
                , SA.width <| toString (toFloat resolution * pixelSize)
                , SA.height <| toString (toFloat resolution * pixelSize)
                , SA.viewBox <| viewBox offsetX offsetY viewSize viewSize
                ]
                [ viewRects pixelSize grid
                , viewBorders resolution pixelSize
                ]
            ]


drawLine : Float -> Float -> Float -> Float -> Svg msg
drawLine x1 y1 x2 y2 =
    Svg.line
        [ SA.x1 <| toString x1
        , SA.y1 <| toString y1
        , SA.x2 <| toString x2
        , SA.y2 <| toString y2
        ]
        []


viewBorders : Int -> Float -> Svg msg
viewBorders resolution pixelSize =
    let
        ns =
            List.range 0 (resolution - 1)

        pos n =
            pixelSize * toFloat n + 0.5

        vertical n =
            drawLine (pos n) 0 (pos n) (toFloat resolution * pixelSize)

        horizontal n =
            drawLine 0 (pos n) (toFloat resolution * pixelSize) (pos n)
    in
        Svg.g
            [ SA.class "grid-borders"
            , SA.strokeWidth "1"
            , SA.stroke "white"
            ]
            (List.map vertical ns ++ List.map horizontal ns)


viewRects : Float -> Grid -> Svg Msg
viewRects size grid =
    let
        makeRect col row pixel =
            Svg.rect
                [ SA.width <| toString size
                , SA.height <| toString size
                , SA.x <| toString <| toFloat col * size
                , SA.y <| toString <| toFloat row * size
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
            , menu False ShowDownloadModal "Download" <| svgIcon images.download
            ]


viewCurrentColor : Color -> List Color -> Html Msg
viewCurrentColor selected usedColors =
    let
        foreground =
            viewColor
                [ HA.class "color-selector__color--foreground" ]
                (\_ -> ShowColorModal)
                (selected == Color.white)
                selected
    in
        Html.div
            [ HA.class "color-selector" ]
            (foreground :: List.map (viewColor [] SelectColor False) usedColors)


viewColor : List (Html.Attribute msg) -> (Color -> msg) -> Bool -> Color -> Html msg
viewColor attrs tagger selected color =
    let
        borderColor =
            if selected then
                Color.lightGray
            else
                color

        attributes =
            List.append
                [ HA.class "color-selector__color"
                , HA.style
                    [ ( "background-color", ColorUtil.toColorString color )
                    , ( "border-color", ColorUtil.toColorString borderColor )
                    ]
                , HE.onClick <| tagger color
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


viewFrames : Int -> ImagePaths -> Int -> Frames -> Html Msg
viewFrames resolution images index frames =
    Html.div
        [ HA.class "frame-list" ]
    <|
        List.concat
            [ if SelectionList.isSingle frames then
                []
              else
                [ viewFrame resolution FramePreview <| SelectionList.get index frames ]
            , List.map (viewFrame resolution FrameNormal) <| Array.toList frames.previous
            , [ viewFrame resolution FrameSelected frames.current ]
            , List.map (viewFrame resolution FrameNormal) <| Array.toList frames.next
            , [ viewAddFrame resolution images ]
            ]


viewFrame : Int -> FrameType -> Frame -> Html Msg
viewFrame resolution frameType frame =
    let
        canvasSize =
            toFloat resolution * framePixelSize

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
                    [ Events.onSingleOrDoubleClick (SelectFrame frame) (ShowFrameModal frame)
                    , Events.prepareDoubleClick
                    , HA.draggable "true"
                    , Events.preventDefault "ondragenter"
                    , Events.preventDefault "ondragover"
                    , Events.preventDefault "ontouchmove"
                    , Events.onDragStart <| SelectFrame frame
                    , Events.onDrop <| DropOnFrame frame
                    , Events.setDummyDragData
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


viewAddFrame : Int -> ImagePaths -> Html Msg
viewAddFrame resolution images =
    Html.div
        [ HA.class "frame frame--plus"
        , sizeStyle <| (toFloat resolution * framePixelSize)
        , HE.onClick AddFrame
        ]
        [ svgIcon images.plus ]


sizeStyle : Float -> Html.Attribute msg
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
