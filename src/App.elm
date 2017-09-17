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
import Array2 exposing (Array2)
import Color exposing (Color)
import ColorUtil exposing (RGBA)
import Events
import SelectionArray exposing (SelectionArray)
import History exposing (History)


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
    SelectionArray Frame


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
    , settings : String
    }


type ModalConfig
    = NoModal
    | FrameModal Frame
    | DownloadModal
    | ColorModal Float
    | SettingsModal Int


type alias HistoryItem =
    { frames : SelectionArray Frame
    , fps : Int
    , resolution : Int
    }


type alias Model =
    { mode : Mode
    , isMouseDown : Bool
    , previousMouseDown : Maybe ( Int, Int )
    , previousDistance : Maybe Float
    , foregroundColor : Color
    , history : History HistoryItem
    , frames : Frames
    , frameSequence : Int
    , fps : Int
    , images : ImagePaths
    , modalConfig : ModalConfig
    , resolution : Int
    , canvasSize : Float
    , frameSize : Float
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


init : ImagePaths -> ( Model, Cmd Msg )
init flags =
    let
        initialFrameSequence =
            0

        initialResolution =
            16

        canvasSize =
            320

        model =
            { mode = Paint
            , isMouseDown = False
            , previousMouseDown = Nothing
            , previousDistance = Nothing
            , foregroundColor = Color.black
            , history = History.initialize 50
            , frames =
                SelectionArray.init <|
                    Frame initialFrameSequence (emptyGrid initialResolution)
            , frameSequence = initialFrameSequence + 1
            , fps = 5
            , images = flags
            , modalConfig = NoModal
            , resolution = initialResolution
            , canvasSize = canvasSize
            , frameSize = 64
            , zoom = canvasSize / initialResolution
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
    | UpdateHue Float
    | UpdateSV Float Float
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
    | ShowSettingsModal
    | UpdateFps Int
    | UpdateSettings Int
    | CloseModal
    | MouseDownOnCanvas ( Float, Float )
    | MouseMoveOnCanvas ( Float, Float )
    | TouchStartOnCanvas (List ( Float, Float ))
    | TouchMoveOnCanvas (List ( Float, Float ))
    | MouseUpOnCanvas
    | MouseWheelOnCanvas ( Float, Float ) ( Float, Float )
    | MouseDownOnContainer
    | MouseUpOnContainer
    | DropOnFrame Frame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectMode mode ->
            ( { model | mode = mode }, Cmd.none )

        SelectColor color ->
            let
                mode =
                    if model.mode == Bucket then
                        Bucket
                    else
                        Paint
            in
                ( updateColorModal
                    (ColorUtil.hue color)
                    { model | foregroundColor = color, mode = mode }
                , Cmd.none
                )

        UpdateHue newHue ->
            let
                { hue, saturation, value, alpha } =
                    ColorUtil.toHsv model.foregroundColor

                newColor =
                    ColorUtil.hsv newHue saturation value alpha
            in
                ( updateColorModal newHue { model | foregroundColor = newColor }
                , Cmd.none
                )

        UpdateSV newSaturatioin newValue ->
            let
                { hue, saturation, value, alpha } =
                    ColorUtil.toHsv model.foregroundColor

                newHue =
                    case model.modalConfig of
                        ColorModal selectedHue ->
                            selectedHue

                        _ ->
                            hue

                newColor =
                    ColorUtil.hsv newHue newSaturatioin newValue alpha
            in
                ( updateColorModal newHue { model | foregroundColor = newColor }
                , Cmd.none
                )

        ClearCanvas ->
            ( { model
                | history = pushHistory model
                , frames =
                    SelectionArray.updateCurrent
                        (\frame -> { frame | grid = emptyGrid model.resolution })
                        model.frames
              }
            , Cmd.none
            )

        AddFrame ->
            ( { model
                | history = pushHistory model
                , frames =
                    SelectionArray.append
                        { id = model.frameSequence, grid = emptyGrid model.resolution }
                        model.frames
                , frameSequence = model.frameSequence + 1
              }
            , Cmd.none
            )

        Undo ->
            ( updateHistory History.undo model, Cmd.none )

        Redo ->
            ( updateHistory History.redo model, Cmd.none )

        Download format ->
            let
                data =
                    { grids =
                        List.map (Array2.toList2 << Array2.map Color.toRgb << .grid) <|
                            SelectionArray.toList model.frames
                    , format =
                        case format of
                            SVGFormat ->
                                "svg"

                            GIFFormat ->
                                "gif"

                            AnimatedGIFFormat ->
                                "animated-gif"
                    , options =
                        { pixelSize = 20
                        , resolution = model.resolution
                        , fps = model.fps
                        }
                    }
            in
                ( { model | modalConfig = NoModal }
                , download data
                )

        SelectFrame frame ->
            ( { model
                | frames = SelectionArray.select frame model.frames
              }
            , Cmd.none
            )

        DeleteFrame frame ->
            ( { model
                | history = pushHistory model
                , frames =
                    SelectionArray.deleteCurrent <|
                        SelectionArray.select frame model.frames
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
                    | history = pushHistory model
                    , frames =
                        SelectionArray.insertAfterCurrent copy <|
                            SelectionArray.select frame model.frames
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

        ShowSettingsModal ->
            ( { model | modalConfig = SettingsModal model.fps }
            , Cmd.none
            )

        UpdateFps fps ->
            ( updateSettingsModal fps model
            , Cmd.none
            )

        UpdateSettings fps ->
            ( { model | fps = fps, modalConfig = NoModal }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalConfig = NoModal }
            , Cmd.none
            )

        MouseDownOnCanvas pos ->
            ( handleSinglePointerDown model pos
            , Cmd.none
            )

        MouseMoveOnCanvas pos ->
            ( handleSinglePointerMove model pos
            , Cmd.none
            )

        MouseUpOnCanvas ->
            ( { model
                | isMouseDown = False
                , previousMouseDown = Nothing
                , previousDistance = Nothing
              }
            , Cmd.none
            )

        TouchStartOnCanvas touches ->
            let
                nextModel =
                    case touches of
                        t1 :: (t2 :: ts) ->
                            { model
                                | previousDistance = Just <| euclidDistance t1 t2
                                , isMouseDown = True
                            }

                        t :: ts ->
                            handleSinglePointerDown model t

                        [] ->
                            model
            in
                ( nextModel, Cmd.none )

        TouchMoveOnCanvas touches ->
            let
                nextModel =
                    case touches of
                        t1 :: (t2 :: ts) ->
                            handleDoublePointerEvent model t1 t2

                        t :: ts ->
                            handleSinglePointerMove model t

                        [] ->
                            model
            in
                ( nextModel, Cmd.none )

        MouseWheelOnCanvas ( _, deltaY ) pos ->
            ( handleZoom model deltaY pos
            , Cmd.none
            )

        MouseDownOnContainer ->
            ( { model
                | history = pushHistory model
                , isMouseDown = True
              }
            , Cmd.none
            )

        MouseUpOnContainer ->
            ( { model
                | isMouseDown = False
                , previousMouseDown = Nothing
                , previousDistance = Nothing
              }
            , Cmd.none
            )

        DropOnFrame frame ->
            ( { model
                | history = pushHistory model
                , frames = SelectionArray.swapCurrent frame model.frames
                , isMouseDown = False
                , previousMouseDown = Nothing
                , previousDistance = Nothing
              }
            , Cmd.none
            )


pushHistory : Model -> History HistoryItem
pushHistory model =
    History.push { frames = model.frames, fps = model.fps, resolution = model.resolution } model.history


updateHistory : (HistoryItem -> History HistoryItem -> Maybe ( HistoryItem, History HistoryItem )) -> Model -> Model
updateHistory move model =
    case move { frames = model.frames, fps = model.fps, resolution = model.resolution } model.history of
        Nothing ->
            model

        Just ( { frames, fps, resolution }, history ) ->
            { model
                | history = history
                , frames = frames
                , fps = fps
                , resolution = resolution
            }


updateColorModal : Float -> Model -> Model
updateColorModal hue model =
    case model.modalConfig of
        ColorModal _ ->
            { model | modalConfig = ColorModal hue }

        _ ->
            model


updateSettingsModal : Int -> Model -> Model
updateSettingsModal fps model =
    case model.modalConfig of
        SettingsModal _ ->
            { model | modalConfig = SettingsModal fps }

        _ ->
            model


euclidDistance : ( Float, Float ) -> ( Float, Float ) -> Float
euclidDistance ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            x2 - x1

        dy =
            y2 - y1
    in
        sqrt <| dx * dx + dy * dy


zoomRate : Float
zoomRate =
    1.1


handleZoom : Model -> Float -> ( Float, Float ) -> Model
handleZoom model deltaY pos =
    let
        minZoom =
            model.canvasSize / toFloat model.resolution

        maxZoom =
            model.canvasSize / 4

        zoomBy =
            if deltaY > 0 then
                zoomRate
            else
                1 / zoomRate

        zoom =
            model.zoom * zoomBy

        offset =
            if deltaY > 0 then
                interpolate (originalPos model.zoom model.offset pos) model.offset (1 / zoomBy)
            else
                interpolate
                    model.offset
                    ( 0, 0 )
                    ((1 / zoom - 1 / model.zoom) / (1 / minZoom - 1 / model.zoom))
    in
        if zoom <= minZoom then
            { model | zoom = minZoom, offset = ( 0, 0 ) }
        else if zoom > minZoom && zoom < maxZoom then
            { model | zoom = zoom, offset = offset }
        else
            model


handleDoublePointerEvent : Model -> ( Float, Float ) -> ( Float, Float ) -> Model
handleDoublePointerEvent model t1 t2 =
    let
        distance =
            euclidDistance t1 t2
    in
        case model.previousDistance of
            Nothing ->
                { model
                    | previousDistance = Just distance
                    , previousMouseDown = Nothing
                }

            Just previousDistance ->
                handleZoom
                    { model
                        | previousDistance = Just distance
                        , previousMouseDown = Nothing
                    }
                    (distance - previousDistance)
                    (interpolate t1 t2 0.5)


handleSinglePointerDown : Model -> ( Float, Float ) -> Model
handleSinglePointerDown model pos =
    let
        pixelPos =
            getPixelPos model.zoom model.offset pos
    in
        { model
            | history = pushHistory model
            , frames = updateCurrentFrame pixelPos model
            , isMouseDown = True
            , previousMouseDown = Just pixelPos
        }


handleSinglePointerMove : Model -> ( Float, Float ) -> Model
handleSinglePointerMove model pos =
    let
        pixelPos =
            getPixelPos model.zoom model.offset pos
    in
        if model.isMouseDown then
            { model
                | frames = updateCurrentFrame pixelPos model
                , previousMouseDown = Just pixelPos
                , previousDistance = Nothing
            }
        else
            model


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


getPixelPos : Float -> ( Float, Float ) -> ( Float, Float ) -> ( Int, Int )
getPixelPos zoom offset pos =
    let
        ( ox, oy ) =
            originalPos zoom offset pos
    in
        ( floor ox, floor oy )


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
    , options :
        { pixelSize : Float
        , resolution : Int
        , fps : Int
        }
    }


port download : DownloadData -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div
        [ HE.onMouseDown <| MouseDownOnContainer
        , HE.onMouseUp <| MouseUpOnContainer
        ]
        [ viewGrid model.resolution model.canvasSize model.zoom model.offset model.mode model.frames.current.grid
        , viewMenus model.mode model.images (History.hasPrevious model.history) (History.hasNext model.history)
        , viewCurrentColor model.foregroundColor <|
            usedColors (Array.toList <| SelectionArray.toArray model.frames)
        , viewFrames model.resolution model.frameSize model.images model.fps model.frames
        , viewModal model.modalConfig (SelectionArray.isSingle model.frames) model.foregroundColor
        ]


viewModalButton : String -> String -> msg -> Html msg
viewModalButton style text msg =
    Html.button
        [ HA.class <| "modal-button modal-button--" ++ style
        , Events.onWithStopAndPrevent "click" <| Json.succeed msg
        ]
        [ Html.text text ]


viewModal : ModalConfig -> Bool -> Color -> Html Msg
viewModal config isSingleFrame foregroundColor =
    let
        deleteButton frame =
            viewModalButton "primary" "Delete Frame" <| DeleteFrame frame

        duplicateButton frame =
            viewModalButton "primary" "Duplicate Frame" <| DuplicateFrame frame

        closeButton =
            viewModalButton "default" "Close" CloseModal

        radioButton name checked label msg =
            Html.label
                [ HA.class "settings-radio"
                , HE.onClick msg
                ]
                [ Html.input
                    [ HA.type_ "radio"
                    , HA.name name
                    , HA.checked checked
                    ]
                    []
                , Html.text label
                ]

        content =
            case config of
                NoModal ->
                    []

                DownloadModal ->
                    if isSingleFrame then
                        [ viewModalButton "primary" "SVG" <| Download SVGFormat
                        , viewModalButton "primary" "GIF" <| Download GIFFormat
                        , closeButton
                        ]
                    else
                        [ viewModalButton "primary" "Animated GIF" <| Download AnimatedGIFFormat
                        , closeButton
                        ]

                FrameModal frame ->
                    if isSingleFrame then
                        [ duplicateButton frame, closeButton ]
                    else
                        [ duplicateButton frame, deleteButton frame, closeButton ]

                ColorModal hue ->
                    List.append (viewColorModal hue foregroundColor) [ closeButton ]

                SettingsModal fps ->
                    [ Html.div
                        [ HA.class "settings-radios" ]
                        [ radioButton "fps" (fps == 5) "5 FPS" (UpdateFps 5)
                        , radioButton "fps" (fps == 10) "10 FPS" (UpdateFps 10)
                        ]
                    , viewModalButton "primary" "Save" <| UpdateSettings fps
                    , closeButton
                    ]
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


{-| Keep hue in order to change hue even when a grayscale color is selected
-}
viewColorModal : Float -> Color -> List (Html Msg)
viewColorModal selectedHue selectedColor =
    let
        color =
            ColorUtil.hsv selectedHue 1 1 1
                |> ColorUtil.toColorString

        { hue, saturation, value, alpha } =
            ColorUtil.toHsv selectedColor

        pickHue ( _, y ) =
            UpdateHue <| 2 * pi * y / 240

        pickSV ( x, y ) =
            UpdateSV (x / 240) (1 - y / 240)

        toPx num =
            toString num ++ "px"
    in
        [ Html.div
            [ HA.class "color-selector__row" ]
            (List.map (\c -> viewColor [] SelectColor (c == selectedColor) c) colors)
        , Html.div
            [ HA.class "color-picker" ]
            [ Html.div
                [ HA.class "color-picker__color"
                , HA.style [ ( "background-color", color ) ]
                , HE.on "click" <| Events.decodeMouseEvent pickSV
                ]
                [ Html.div
                    [ HA.class "color-picker__saturation" ]
                    [ Html.div
                        [ HA.class "color-picker__value" ]
                        [ Html.div
                            [ HA.class "color-picker__sv-pointer"
                            , HA.style
                                [ ( "left", toPx <| saturation * 240 - 5 )
                                , ( "top", toPx <| (1 - value) * 240 - 5 )
                                ]
                            ]
                            []
                        ]
                    ]
                ]
            , Html.div
                [ HA.class "color-picker__hue"
                , HE.on "click" <| Events.decodeMouseEvent pickHue
                ]
                [ Html.div
                    [ HA.class "color-picker__hue-pointer"
                    , HA.style
                        [ ( "top", toPx <| 240 * selectedHue / (2 * pi) - 3 ) ]
                    ]
                    []
                ]
            ]
        ]


viewBox : Float -> Float -> Float -> Float -> String
viewBox minX minY width height =
    [ minX, minY, width, height ]
        |> List.map toString
        |> String.join " "


viewGrid : Int -> Float -> Float -> ( Float, Float ) -> Mode -> Grid -> Html Msg
viewGrid resolution canvasSize zoom ( offsetX, offsetY ) mode grid =
    let
        viewSize =
            canvasSize / zoom
    in
        Html.div
            [ HA.class "pixel-grid-container"
            , sizeStyle canvasSize
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
            , Events.onWithStopAndPrevent "touchstart" <| Events.decodeTouchEvent TouchStartOnCanvas
            , Events.onWithStopAndPrevent "touchmove" <| Events.decodeTouchEvent TouchMoveOnCanvas
            , Events.onWithStopAndPrevent "touchend" <| Json.succeed MouseUpOnCanvas
            , Events.onWithStopAndPrevent "mousewheel" <| Events.decodeWheelEvent MouseWheelOnCanvas
            ]
            [ Svg.svg
                [ SA.class "pixel-grid"
                , SA.width <| toString canvasSize
                , SA.height <| toString canvasSize
                , SA.viewBox <| viewBox offsetX offsetY viewSize viewSize
                , SA.shapeRendering "crispEdges"
                ]
                [ viewRects grid
                , viewBorders resolution
                ]
            ]


drawLine : Float -> Float -> Float -> Float -> Svg msg
drawLine x1 y1 x2 y2 =
    Svg.line
        [ SA.x1 <| toString x1
        , SA.y1 <| toString y1
        , SA.x2 <| toString x2
        , SA.y2 <| toString y2
        , HA.attribute "vector-effect" "non-scaling-stroke"
        ]
        []


viewBorders : Int -> Svg msg
viewBorders resolution =
    let
        ns =
            List.range 0 (resolution - 1)

        vertical n =
            drawLine (toFloat n) 0 (toFloat n) (toFloat resolution)

        horizontal n =
            drawLine 0 (toFloat n) (toFloat resolution) (toFloat n)
    in
        Svg.g
            [ SA.class "grid-borders"
            , SA.strokeWidth "1"
            , SA.stroke "white"
            ]
            (List.map vertical ns ++ List.map horizontal ns)


viewRects : Grid -> Svg msg
viewRects grid =
    let
        makeRect col row pixel =
            Svg.rect
                [ SA.width "1"
                , SA.height "1"
                , SA.x <| toString <| toFloat col
                , SA.y <| toString <| toFloat row
                , SA.fill <| ColorUtil.toColorString pixel
                ]
                []

        rects =
            Array2.toList <| Array2.indexedMap makeRect grid
    in
        Svg.g [] rects


viewMenu : Bool -> Bool -> Msg -> String -> Html Msg -> Html Msg
viewMenu selected disabled msg label content =
    Html.a
        [ HA.classList
            [ ( "mode", True )
            , ( "mode--selected", selected )
            , ( "mode--disabled", disabled )
            ]
        , HA.href "#"
        , HA.title label
        , HE.onClick <|
            if disabled then
                NoOp
            else
                msg
        , Events.onWithStopAndPrevent "mousedown" <| Json.succeed NoOp
        , Events.onWithStopAndPrevent "mouseup" <| Json.succeed NoOp
        ]
        [ content ]


viewMenus : Mode -> ImagePaths -> Bool -> Bool -> Html Msg
viewMenus selectedMode images withUndo withRedo =
    let
        modeMenu mode content =
            viewMenu (mode == selectedMode) False (SelectMode mode) content
    in
        Html.div [ HA.class "menu" ]
            [ Html.div
                []
                [ modeMenu Paint "Paint" <| svgIcon images.pencil
                , modeMenu Eraser "Eraser" <| svgIcon images.eraser
                , modeMenu Bucket "Bucket" <| svgIcon images.bucket
                , modeMenu Move "Move" <| svgIcon images.move
                , viewMenu False False ClearCanvas "Clear" <| svgIcon images.trash
                , viewMenu False False ShowSettingsModal "Settings" <| svgIcon images.settings
                , viewMenu False False ShowDownloadModal "Download" <| svgIcon images.download
                ]
            , Html.div
                []
                [ viewMenu False (not withUndo) Undo "Undo" <| svgIcon images.undo
                , viewMenu False (not withRedo) Redo "Redo" <| svgIcon images.redo
                ]
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


viewFrames : Int -> Float -> ImagePaths -> Int -> Frames -> Html Msg
viewFrames resolution size images fps frames =
    Html.div
        [ HA.class "frame-list" ]
    <|
        List.concat
            [ if SelectionArray.length frames > 1 then
                [ viewPreview resolution size fps frames ]
              else
                []
            , List.map (viewFrame resolution size False) <| Array.toList frames.previous
            , [ viewFrame resolution size True frames.current ]
            , List.map (viewFrame resolution size False) <| Array.toList frames.next
            , [ viewAddFrame size images ]
            ]


viewPreview : Int -> Float -> Int -> Frames -> Html Msg
viewPreview resolution size fps frames =
    let
        frameCount =
            SelectionArray.length frames

        keyframeName =
            "preview-" ++ toString fps ++ "-" ++ toString frameCount

        keyframes =
            String.concat
                [ "@keyframes "
                , keyframeName
                , " {"
                , "  from { left: 0; }"
                , "  to { left: -"
                , toString <| (toFloat frameCount) * size
                , "px; }"
                , "}"
                ]

        animation =
            String.concat
                [ keyframeName
                , " "
                , toString <| (toFloat frameCount) / (toFloat fps)
                , "s steps("
                , toString frameCount
                , ") infinite"
                ]

        frameViews =
            List.map (viewThumbnail resolution size []) <|
                SelectionArray.toList frames
    in
        Html.div
            [ HA.class "frame frame-preview"
            , sizeStyle size
            ]
            [ Html.node "style"
                []
                [ Html.text keyframes ]
            , Html.div
                [ HA.class "frame-preview__inner"
                , HA.style
                    [ ( "width", toString (size * toFloat frameCount) ++ "px" )
                    , ( "animation", animation )
                    ]
                ]
                frameViews
            ]


viewFrame : Int -> Float -> Bool -> Frame -> Html Msg
viewFrame resolution size selected frame =
    let
        attrs =
            [ HA.classList
                [ ( "frame", True )
                , ( "frame--normal", not selected )
                , ( "frame--selected", selected )
                ]
            , sizeStyle size
            , Events.onSingleOrDoubleClick
                (SelectFrame frame)
                (ShowFrameModal frame)
            , Events.prepareDoubleClick
            , HA.draggable "true"
            , Events.preventDefault "ondragenter"
            , Events.preventDefault "ondragover"
            , Events.preventDefault "ontouchmove"
            , Events.onDragStart <| SelectFrame frame
            , Events.onDrop <| DropOnFrame frame
            , Events.setDummyDragData
            ]
    in
        viewThumbnail resolution size attrs frame


viewThumbnail : Int -> Float -> List (Html.Attribute msg) -> Frame -> Html msg
viewThumbnail resolution size attrs frame =
    Html.div
        attrs
        [ Svg.svg
            [ SA.width <| toString size
            , SA.height <| toString size
            , SA.viewBox <| viewBox 0 0 (toFloat resolution) (toFloat resolution)
            ]
            [ viewRects frame.grid ]
        ]


viewAddFrame : Float -> ImagePaths -> Html Msg
viewAddFrame frameSize images =
    Html.div
        [ HA.class "frame frame--plus"
        , sizeStyle frameSize
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
        , HA.height 32
        ]
        []



---- PROGRAM ----


main : Program ImagePaths Model Msg
main =
    Html.programWithFlags
        { view = HL.lazy view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
