module Events
    exposing
        ( decodeMouseEvent
        , decodeTouchEvent
        , onWithStopAndPrevent
        , onDragStart
        , onDrop
        , allowDrop
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
    HA.attribute "onDragOver" "event.preventDefault()"


onDragStart : msg -> Html.Attribute msg
onDragStart msg =
    HE.on "dragstart" <| Json.succeed msg


onDrop : msg -> Html.Attribute msg
onDrop msg =
    HE.onWithOptions "drop" prevent <| Json.succeed msg
