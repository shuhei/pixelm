module Events
    exposing
        ( decodeMouseEvent
        , decodeTouchEvent
        , onWithStopAndPrevent
        , onDragStart
        , onDrop
        , allowDrop
        , prepareDoubleClick
        , onSingleOrDoubleClick
        )

import Json.Decode as Json
import Html
import Html.Events as HE
import Html.Attributes as HA


minusPos : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
minusPos ( x0, y0 ) ( x1, y1 ) =
    ( x0 - x1, y0 - y1 )


decodeTouchEvent : (( Int, Int ) -> msg) -> Json.Decoder msg
decodeTouchEvent tagger =
    let
        decodeTarget =
            Json.field "target" decodeOffset

        decodeFirstTouch =
            Json.field "touches" <| Json.field "0" decodeClientPos
    in
        Json.map tagger <|
            Json.map2 minusPos decodeFirstTouch decodeTarget


decodeOffset : Json.Decoder ( Int, Int )
decodeOffset =
    Json.map2 (,)
        (Json.field "offsetLeft" Json.int)
        (Json.field "offsetTop" Json.int)



-- Android's Touch.clientX/Y are float instead of int


decodeClientPos : Json.Decoder ( Int, Int )
decodeClientPos =
    Json.map2 (,)
        (Json.map floor <| Json.field "clientX" Json.float)
        (Json.map floor <| Json.field "clientY" Json.float)


decodeMouseEvent : (( Int, Int ) -> msg) -> Json.Decoder msg
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


allowDrop : Html.Attribute msg
allowDrop =
    HA.attribute "ondragover" "event.preventDefault()"


onDragStart : msg -> Html.Attribute msg
onDragStart msg =
    HE.on "dragstart" <| Json.succeed msg


onDrop : msg -> Html.Attribute msg
onDrop msg =
    HE.onWithOptions "drop" prevent <| Json.succeed msg



-- HACK: Double tap support on iOS
-- iOS does not support `dblclick` event. To detect double tap on iOS, the event
-- handler below sets a flag to the clicked element and deletes it in a short
-- amount of time. The other event handler uses a JSON decoder to check if the
-- data exists and returns a message for double click if the flag exists.
-- Otherwise it just returns a message for single click.


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
        , "    el.dataset.clicked = false;"
        , "  }, 500);"
        , "  el.dataset.clicked = true;"
        , "});"
        ]


onSingleOrDoubleClick : msg -> msg -> Html.Attribute msg
onSingleOrDoubleClick singleMessage doubleMessage =
    let
        decodeDoubleOrNot =
            Json.oneOf
                [ decodeClicked
                , Json.succeed False
                ]

        getMessage isDoubleClick =
            if isDoubleClick then
                doubleMessage
            else
                singleMessage
    in
        HE.on "click" <|
            Json.map getMessage decodeDoubleOrNot


decodeClicked : Json.Decoder Bool
decodeClicked =
    let
        decodeClicked =
            Json.field "dataset" <| Json.field "clicked" Json.string

        isTrue x =
            x == "true"
    in
        Json.map isTrue <| Json.field "currentTarget" decodeClicked
