module ColorUtilTests exposing (all)

import Color
import ColorUtil
import Expect
import Test exposing (..)


all : Test
all =
    describe "ColorUtil"
        [ test "toColorString" <|
            \() ->
                Expect.equal
                    (ColorUtil.toColorString <| Color.rgba 10 20 30 0.8)
                    "rgba(10,20,30,0.8)"
        , test "transparent" <|
            \() ->
                Expect.equal
                    (Color.toRgb ColorUtil.transparent).alpha
                    0.0
        ]
