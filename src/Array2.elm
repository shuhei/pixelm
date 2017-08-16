module Array2 exposing (..)

import Dict exposing (Dict)
import Set


type alias Array2 a =
    { rowSize : Int
    , colSize : Int
    , data : Dict ( Int, Int ) a
    }


get : Int -> Int -> Array2 a -> Maybe a
get col row arr2 =
    Dict.get ( col, row ) arr2.data


set : Int -> Int -> a -> Array2 a -> Array2 a
set col row newItem arr2 =
    if inRange col row arr2 then
        { arr2 | data = Dict.insert ( col, row ) newItem arr2.data }
    else
        arr2


inRange : Int -> Int -> Array2 a -> Bool
inRange col row arr2 =
    arr2.colSize > col && col >= 0 && arr2.rowSize > row && row >= 0


initialize : Int -> Int -> Array2 a
initialize cols rows =
    { colSize = cols, rowSize = rows, data = Dict.empty }


map : (a -> b) -> Array2 a -> Array2 b
map f arr2 =
    { arr2 | data = Dict.map (\_ a -> f a) arr2.data }


indexedMap : (Int -> Int -> a -> b) -> Array2 a -> Array2 b
indexedMap f arr2 =
    { arr2 | data = Dict.map (\( col, row ) a -> f col row a) arr2.data }


toList : Array2 a -> List ( ( Int, Int ), a )
toList arr2 =
    Dict.toList arr2.data


foldl : (a -> b -> b) -> b -> Array2 a -> b
foldl f init arr2 =
    Dict.foldl (\_ -> f) init arr2.data


foldr : (a -> b -> b) -> b -> Array2 a -> b
foldr f init arr2 =
    Dict.foldr (\_ -> f) init arr2.data


move : Int -> Int -> Array2 a -> Array2 a
move dx dy arr2 =
    let
        reduce ( x, y ) a prev =
            if inRange (x + dx) (y + dy) arr2 then
                Dict.insert ( x + dx, y + dy ) a prev
            else
                prev
    in
        { arr2 | data = Dict.foldl reduce Dict.empty arr2.data }


fill : Int -> Int -> a -> Array2 a -> Array2 a
fill x y to arr2 =
    let
        fillRegion a ( x, y ) ( visited, arr2 ) =
            if inRange x y arr2 then
                if Set.member ( x, y ) visited then
                    ( visited, arr2 )
                else if get x y arr2 == a then
                    let
                        state =
                            ( Set.insert ( x, y ) visited, set x y to arr2 )

                        neighbors =
                            [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
                    in
                        List.foldl (fillRegion a) state neighbors
                else
                    ( Set.insert ( x, y ) visited, arr2 )
            else
                ( visited, arr2 )
    in
        fillRegion (get x y arr2) ( x, y ) ( Set.empty, arr2 )
            |> Tuple.second
