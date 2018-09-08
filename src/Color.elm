module Color exposing
    ( Color
    , black
    , blue
    , brown
    , charcoal
    , darkBlue
    , darkBrown
    , darkCharcoal
    , darkGray
    , darkGreen
    , darkGrey
    , darkOrange
    , darkPurple
    , darkRed
    , darkYellow
    , gray
    , green
    , grey
    , lightBlue
    , lightBrown
    , lightCharcoal
    , lightGray
    , lightGreen
    , lightGrey
    , lightOrange
    , lightPurple
    , lightRed
    , lightYellow
    , orange
    , purple
    , red
    , rgba
    , toHsl
    , toRgb
    , white
    , yellow
    )


type Color
    = RGBA Int Int Int Float


toRgb : Color -> { red : Int, green : Int, blue : Int, alpha : Float }
toRgb (RGBA r g b a) =
    { red = r
    , green = g
    , blue = b
    , alpha = a
    }


rgba : Int -> Int -> Int -> Float -> Color
rgba r g b a =
    RGBA r g b a


modFloatBy : Int -> Float -> Float
modFloatBy n float =
    let
        integer =
            floor float
    in
    toFloat (modBy n integer) + float - toFloat integer



-- https://github.com/elm/color/blob/master/src/Color/Rgb.elm


toHsl : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsl (RGBA r g b a) =
    let
        cMax =
            max (max r g) b

        cMin =
            min (min r g) b

        c =
            toFloat (cMax - cMin)

        hue =
            degrees 60
                * (if cMax == r then
                    modFloatBy 6 (toFloat (g - b) / c)

                   else if cMax == g then
                    (toFloat (b - r) / c) + 2

                   else
                    {- cMax == b -}
                    (toFloat (r - g) / c) + 4
                  )

        lightness =
            toFloat (cMax + cMin) / 2

        saturation =
            if lightness == 0 then
                0

            else
                c / (1 - abs (2 * lightness - 1))
    in
    { hue = hue
    , saturation = saturation
    , lightness = lightness
    , alpha = a
    }



-- BUILT-IN COLORS


{-| -}
lightRed : Color
lightRed =
    RGBA 239 41 41 1


{-| -}
red : Color
red =
    RGBA 204 0 0 1


{-| -}
darkRed : Color
darkRed =
    RGBA 164 0 0 1


{-| -}
lightOrange : Color
lightOrange =
    RGBA 252 175 62 1


{-| -}
orange : Color
orange =
    RGBA 245 121 0 1


{-| -}
darkOrange : Color
darkOrange =
    RGBA 206 92 0 1


{-| -}
lightYellow : Color
lightYellow =
    RGBA 255 233 79 1


{-| -}
yellow : Color
yellow =
    RGBA 237 212 0 1


{-| -}
darkYellow : Color
darkYellow =
    RGBA 196 160 0 1


{-| -}
lightGreen : Color
lightGreen =
    RGBA 138 226 52 1


{-| -}
green : Color
green =
    RGBA 115 210 22 1


{-| -}
darkGreen : Color
darkGreen =
    RGBA 78 154 6 1


{-| -}
lightBlue : Color
lightBlue =
    RGBA 114 159 207 1


{-| -}
blue : Color
blue =
    RGBA 52 101 164 1


{-| -}
darkBlue : Color
darkBlue =
    RGBA 32 74 135 1


{-| -}
lightPurple : Color
lightPurple =
    RGBA 173 127 168 1


{-| -}
purple : Color
purple =
    RGBA 117 80 123 1


{-| -}
darkPurple : Color
darkPurple =
    RGBA 92 53 102 1


{-| -}
lightBrown : Color
lightBrown =
    RGBA 233 185 110 1


{-| -}
brown : Color
brown =
    RGBA 193 125 17 1


{-| -}
darkBrown : Color
darkBrown =
    RGBA 143 89 2 1


{-| -}
black : Color
black =
    RGBA 0 0 0 1


{-| -}
white : Color
white =
    RGBA 255 255 255 1


{-| -}
lightGrey : Color
lightGrey =
    RGBA 238 238 236 1


{-| -}
grey : Color
grey =
    RGBA 211 215 207 1


{-| -}
darkGrey : Color
darkGrey =
    RGBA 186 189 182 1


{-| -}
lightGray : Color
lightGray =
    RGBA 238 238 236 1


{-| -}
gray : Color
gray =
    RGBA 211 215 207 1


{-| -}
darkGray : Color
darkGray =
    RGBA 186 189 182 1


{-| -}
lightCharcoal : Color
lightCharcoal =
    RGBA 136 138 133 1


{-| -}
charcoal : Color
charcoal =
    RGBA 85 87 83 1


{-| -}
darkCharcoal : Color
darkCharcoal =
    RGBA 46 52 54 1
