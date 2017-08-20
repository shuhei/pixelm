module Events exposing (decodeMouseEvent, decodeTouchEvent, onWithStopAndPrevent)

import Json.Decode as Json
import Html
import Html.Events as HE


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


decodeClientPos : Json.Decoder ( Int, Int )
decodeClientPos =
    Json.map2 (,)
        (Json.field "clientX" Json.int)
        (Json.field "clientY" Json.int)


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


onWithStopAndPrevent : String -> Json.Decoder msg -> Html.Attribute msg
onWithStopAndPrevent eventName decoder =
    HE.onWithOptions eventName stopAndPrevent decoder
