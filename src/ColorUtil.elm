module ColorUtil exposing
    ( RGBA
    , hsv
    , hue
    , toColorString
    , toHsv
    , transparent
    )

import Color exposing (Color)


type alias HSVA =
    { hue : Float
    , saturation : Float
    , value : Float
    , alpha : Float
    }


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
        ++ String.fromInt red
        ++ ","
        ++ String.fromInt green
        ++ ","
        ++ String.fromInt blue
        ++ ","
        ++ String.fromFloat alpha
        ++ ")"


modFloat : Float -> Float -> Float
modFloat x y =
    let
        z =
            toFloat <| floor (x / y)
    in
    x - z * y


{-| <http://www.rapidtables.com/convert/color/hsv-to-rgb.htm>
-}
hsv : Float -> Float -> Float -> Float -> Color
hsv hueRadian saturation value alpha =
    let
        h =
            modFloat (hueRadian / pi * 180) 360

        c =
            value * saturation

        x =
            c * (1 - abs (modFloat (h / 60) 2 - 1))

        m =
            value - c

        normalize color =
            floor ((m + color) * 255)

        rgb r g b =
            Color.rgba (normalize r) (normalize g) (normalize b) alpha
    in
    if 0 <= h && h < 60 then
        rgb c x 0

    else if 60 <= h && h < 120 then
        rgb x c 0

    else if 120 <= h && h < 180 then
        rgb 0 c x

    else if 180 <= h && h < 240 then
        rgb 0 x c

    else if 240 <= h && h < 300 then
        rgb x 0 c

    else
        rgb c 0 x


toHsv : Color -> HSVA
toHsv color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color

        r =
            toFloat red / 255

        g =
            toFloat green / 255

        b =
            toFloat blue / 255

        cmax =
            max r (max g b)

        cmin =
            min r (min g b)

        delta =
            cmax - cmin
    in
    { hue =
        if delta == 0 then
            0

        else if cmax == r then
            pi / 3 * modFloat ((g - b) / delta) 6

        else if cmax == g then
            pi / 3 * ((b - r) / delta + 2)

        else
            pi / 3 * ((r - g) / delta + 4)
    , saturation =
        if cmax == 0 then
            0

        else
            delta / cmax
    , value = cmax
    , alpha = alpha
    }


hue : Color -> Float
hue color =
    let
        h =
            (Color.toHsl color).hue
    in
    if isNaN h then
        0

    else
        h


transparent : Color
transparent =
    Color.rgba 0 0 0 0
