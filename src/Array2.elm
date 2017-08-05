module Array2 exposing (..)

import Array exposing (Array)


type alias Array2 a =
    Array (Array a)


get : Int -> Int -> Array2 a -> Maybe a
get col row =
    Maybe.andThen (Array.get col) << Array.get row


set : Int -> Int -> a -> Array2 a -> Array2 a
set col row newItem =
    let
        update c r oldItem =
            if col == c && row == r then
                newItem
            else
                oldItem
    in
        indexedMap update


initialize : Int -> Int -> (Int -> Int -> a) -> Array2 a
initialize cols rows f =
    let
        makeRow row =
            Array.initialize cols (\col -> f col row)
    in
        Array.initialize rows makeRow


indexedMap : (Int -> Int -> a -> b) -> Array2 a -> Array2 b
indexedMap f =
    let
        mapRow row =
            Array.indexedMap (\col x -> f col row x)
    in
        Array.indexedMap mapRow


toList2 : Array2 a -> List (List a)
toList2 =
    Array.toList << Array.map Array.toList


toList : Array2 a -> List a
toList =
    List.concat << toList2
