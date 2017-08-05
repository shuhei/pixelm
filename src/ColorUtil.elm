module ColorUtil exposing (..)

import Color exposing (Color)


type alias RGBA =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


toColorString : Color -> String
toColorString color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba("
            ++ toString red
            ++ ","
            ++ toString green
            ++ ","
            ++ toString blue
            ++ ","
            ++ toString alpha
            ++ ")"


transparent : Color
transparent =
    Color.rgba 0 0 0 0
