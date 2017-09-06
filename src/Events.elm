module Events
    exposing
        ( decodeMouseEvent
        , decodeTouchEvent
        , decodeDeltaY
        , onWithStopAndPrevent
        , onDragStart
        , onDrop
        , preventDefault
        , stopPropagation
        , prepareDoubleClick
        , onSingleOrDoubleClick
        , setDummyDragData
        )

import Json.Decode as Json
import Html
import Html.Events as HE
import Html.Attributes as HA


minusPos : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
minusPos ( x0, y0 ) ( x1, y1 ) =
    ( x0 - x1, y0 - y1 )


decodeTouchEvent : (( Float, Float ) -> msg) -> Json.Decoder msg
decodeTouchEvent tagger =
    let
        decodeTarget =
            Json.field "target" decodeOffset

        decodeFirstTouch =
            Json.at [ "touches", "0" ] decodeClientPos
    in
        Json.map tagger <|
            Json.map2 minusPos decodeFirstTouch decodeTarget


decodeOffset : Json.Decoder ( Float, Float )
decodeOffset =
    Json.map2 (,)
        (Json.field "offsetLeft" Json.float)
        (Json.field "offsetTop" Json.float)


decodeClientPos : Json.Decoder ( Float, Float )
decodeClientPos =
    Json.map2 (,)
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


decodeMouseEvent : (( Float, Float ) -> msg) -> Json.Decoder msg
decodeMouseEvent tagger =
    let
        decodeTarget =
            Json.field "currentTarget" decodeOffset
    in
        Json.map tagger <|
            Json.map2 minusPos decodeClientPos decodeTarget


stopAndPrevent : HE.Options
stopAndPrevent =
    { stopPropagation = True
    , preventDefault = True
    }


prevent : HE.Options
prevent =
    { preventDefault = True
    , stopPropagation = False
    }


onWithStopAndPrevent : String -> Json.Decoder msg -> Html.Attribute msg
onWithStopAndPrevent eventName decoder =
    HE.onWithOptions eventName stopAndPrevent decoder


preventDefault : String -> Html.Attribute msg
preventDefault eventAttributeName =
    HA.attribute eventAttributeName "event.preventDefault()"


stopPropagation : String -> Html.Attribute msg
stopPropagation eventAttributeName =
    HA.attribute eventAttributeName "event.stopPropagation()"


onDragStart : msg -> Html.Attribute msg
onDragStart msg =
    HE.on "dragstart" <| Json.succeed msg


onDrop : msg -> Html.Attribute msg
onDrop msg =
    HE.onWithOptions "drop" prevent <| Json.succeed msg


{-| HACK: Double tap support on iOS

iOS does not support `dblclick` event. To detect double tap on iOS:

  - `prepareDoubleClick` sets a flag to the clicked element's `dataset` and
    deletes it in a short amount of time.
  - `onSingleOrDoubleClick` uses a JSON decoder to check if the flag exists
    and returns a message for double click if the flag exists. Otherwise it
    just returns a message for single click.

-}
prepareDoubleClick : Html.Attribute msg
prepareDoubleClick =
    HA.attribute "onclick" prepareScript


prepareScript : String
prepareScript =
    String.concat
        [ "var el = event.currentTarget;"
        , "setTimeout(function () {"
        , "  if (el.dataset.timer) { clearTimeout(parseInt(el.dataset.timer, 10)); }"
        , "  el.dataset.timer = setTimeout(function () {"
        , "    delete el.dataset.clicked;"
        , "  }, 500);"
        , "  el.dataset.clicked = 'true';"
        , "});"
        ]


onSingleOrDoubleClick : msg -> msg -> Html.Attribute msg
onSingleOrDoubleClick singleMessage doubleMessage =
    let
        chooseMessage isDoubleClick =
            if isDoubleClick then
                doubleMessage
            else
                singleMessage
    in
        HE.onWithOptions "click" prevent <|
            Json.map chooseMessage decodeClicked


decodeClicked : Json.Decoder Bool
decodeClicked =
    let
        decodeData =
            Json.map ((==) "true") <|
                Json.at [ "currentTarget", "dataset", "clicked" ] Json.string
    in
        Json.oneOf
            [ decodeData
            , Json.succeed False
            ]


{-| Set dummy data to `event.dataTransfer` on dragstart event for cross-browser
support.

<https://github.com/timruffles/ios-html5-drag-drop-shim#cross-browser-differences-in-html5-dragndrop-api>

-}
setDummyDragData : Html.Attribute msg
setDummyDragData =
    HA.attribute "ondragstart" "event.dataTransfer.setData('text', 'dummy');"


decodeDeltaY : (Float -> msg) -> Json.Decoder msg
decodeDeltaY tagger =
    Json.map tagger <|
        Json.field "deltaY" Json.float
