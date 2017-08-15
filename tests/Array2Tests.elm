module Array2Tests exposing (..)

import Test exposing (..)
import Expect
import Array2 exposing (Array2)


tuple23 : Array2 ( Int, Int )
tuple23 =
    Array2.initialize 2 3 (\x y -> ( x, y ))


all : Test
all =
    describe "Array2"
        [ test "initialize" <|
            \() ->
                Expect.equal tuple23 <|
                    Array2.fromList2
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
                    Array2.fromList2
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
                    Array2.fromList2
                        [ [ 0, 1 ]
                        , [ 1, 2 ]
                        , [ 2, 3 ]
                        ]
        , test "indexedMap" <|
            \() ->
                Expect.equal (Array2.indexedMap (\x y _ -> ( y, x )) tuple23) <|
                    Array2.fromList2
                        [ [ ( 0, 0 ), ( 0, 1 ) ]
                        , [ ( 1, 0 ), ( 1, 1 ) ]
                        , [ ( 2, 0 ), ( 2, 1 ) ]
                        ]
        , test "move" <|
            \() ->
                Expect.equal (Array2.move 1 2 ( 10, 10 ) tuple23) <|
                    Array2.fromList2
                        [ [ ( 10, 10 ), ( 10, 10 ) ]
                        , [ ( 10, 10 ), ( 10, 10 ) ]
                        , [ ( 10, 10 ), ( 0, 0 ) ]
                        ]
        , test "fill" <|
            \() ->
                let
                    arr2 =
                        Array2.fromList2
                            [ [ "a", "a", "a" ]
                            , [ "a", "b", "a" ]
                            , [ "b", "b", "a" ]
                            ]
                in
                    Expect.equal (Array2.fill 0 1 "c" arr2) <|
                        Array2.fromList2
                            [ [ "c", "c", "c" ]
                            , [ "c", "b", "c" ]
                            , [ "b", "b", "c" ]
                            ]
        ]
