module Array2Tests exposing (..)

import Test exposing (..)
import Expect
import Array2 exposing (Array2)


fromList2 : List (List a) -> Array2 a
fromList2 xxs =
    let
        rows =
            List.length xxs

        cols =
            Maybe.withDefault 0 <| Maybe.map List.length <| List.head xxs

        init =
            Array2.initialize cols rows

        tuples =
            List.indexedMap (\y -> List.indexedMap (\x a -> ( ( x, y ), a ))) xxs
    in
        List.foldl (\( ( x, y ), a ) -> Array2.set x y a) init <| List.concat tuples


tuple23 : Array2 ( Int, Int )
tuple23 =
    fromList2
        [ [ ( 0, 0 ), ( 1, 0 ) ]
        , [ ( 0, 1 ), ( 1, 1 ) ]
        , [ ( 0, 2 ), ( 1, 2 ) ]
        ]


all : Test
all =
    describe "Array2"
        [ test "initialize" <|
            \() ->
                Expect.equal tuple23 <|
                    fromList2
                        [ [ ( 0, 0 ), ( 1, 0 ) ]
                        , [ ( 0, 1 ), ( 1, 1 ) ]
                        , [ ( 0, 2 ), ( 1, 2 ) ]
                        ]
        , test "get ok" <|
            \() ->
                Expect.equal (Array2.get 1 2 tuple23) <| Just ( 1, 2 )
        , test "get ng" <|
            \() ->
                Expect.equal (Array2.get 2 3 tuple23) <| Nothing
        , test "set ok" <|
            \() ->
                Expect.equal (Array2.set 1 2 ( 5, 5 ) tuple23) <|
                    fromList2
                        [ [ ( 0, 0 ), ( 1, 0 ) ]
                        , [ ( 0, 1 ), ( 1, 1 ) ]
                        , [ ( 0, 2 ), ( 5, 5 ) ]
                        ]
        , test "set ng" <|
            \() ->
                Expect.equal (Array2.set 10 10 ( 5, 5 ) tuple23) tuple23
        , test "map" <|
            \() ->
                Expect.equal (Array2.map (\( a, b ) -> a + b) tuple23) <|
                    fromList2
                        [ [ 0, 1 ]
                        , [ 1, 2 ]
                        , [ 2, 3 ]
                        ]
        , test "indexedMap" <|
            \() ->
                Expect.equal (Array2.indexedMap (\x y _ -> ( y, x )) tuple23) <|
                    fromList2
                        [ [ ( 0, 0 ), ( 0, 1 ) ]
                        , [ ( 1, 0 ), ( 1, 1 ) ]
                        , [ ( 2, 0 ), ( 2, 1 ) ]
                        ]
        , test "move" <|
            \() ->
                Expect.equal (Array2.move 1 2 tuple23) <|
                    (Array2.set 1 2 ( 0, 0 ) <| Array2.initialize 2 3)
        , test "fill" <|
            \() ->
                let
                    arr2 =
                        fromList2
                            [ [ "a", "a", "a" ]
                            , [ "a", "b", "a" ]
                            , [ "b", "b", "a" ]
                            ]
                in
                    Expect.equal (Array2.fill 0 1 "c" arr2) <|
                        fromList2
                            [ [ "c", "c", "c" ]
                            , [ "c", "b", "c" ]
                            , [ "b", "b", "c" ]
                            ]
        ]
