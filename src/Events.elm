module Events exposing (decodeMouseEvent, decodeTouchEvent)

import Json.Decode as Json


minusPos : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
minusPos ( x0, y0 ) ( x1, y1 ) =
    ( x0 - x1, y0 - y1 )


decodeTouch : Json.Decoder ( Int, Int )
decodeTouch =
    Json.map2 (,)
        (Json.field "clientX" Json.int)
        (Json.field "clientY" Json.int)


decodeTouchEvent : (( Int, Int ) -> msg) -> Json.Decoder msg
decodeTouchEvent tagger =
    let
        decodeTarget =
            Json.field "target" decodeOffset

        decodeFirstTouch =
            Json.field "touches" <| Json.field "0" decodeTouch
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
