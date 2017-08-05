module Array2 exposing (..)

import Array exposing (Array)


type alias Array2 a =
    Array (Array a)


initialize : Int -> Int -> (Int -> Int -> a) -> Array2 a
initialize cols rows f =
    let
        makeRow row =
            Array.initialize cols (\col -> f col row)
    in
        Array.initialize rows makeRow


indexedMap : (Int -> Int -> a -> a) -> Array2 a -> Array2 a
indexedMap f =
    let
        mapRow row =
            Array.indexedMap (\col x -> f col row x)
    in
        Array.indexedMap mapRow
