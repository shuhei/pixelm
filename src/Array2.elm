module Array2 exposing (..)

import Array.Hamt as Array exposing (Array)
import Set


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


map : (a -> b) -> Array2 a -> Array2 b
map =
    Array.map << Array.map


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


fromList2 : List (List a) -> Array2 a
fromList2 =
    Array.fromList << List.map Array.fromList


foldl : (a -> b -> b) -> b -> Array2 a -> b
foldl f =
    Array.foldl (\xs prev -> Array.foldl f prev xs)


foldr : (a -> b -> b) -> b -> Array2 a -> b
foldr f =
    Array.foldr (\xs prev -> Array.foldr f prev xs)


move : Int -> Int -> a -> Array2 a -> Array2 a
move dx dy default arr2 =
    let
        len =
            Array.get 0 arr2
                |> Maybe.map Array.length
                |> Maybe.withDefault 0

        moveCol row x _ =
            Array.get (x - dx) row
                |> Maybe.withDefault default

        moveRow y _ =
            case Array.get (y - dy) arr2 of
                Nothing ->
                    Array.repeat len default

                Just row ->
                    Array.indexedMap (moveCol row) row
    in
        Array.indexedMap moveRow arr2


fill : Int -> Int -> a -> Array2 a -> Array2 a
fill x y to arr2 =
    let
        neighbors x y =
            [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]

        fillRegion a ( x, y ) ( visited, arr2 ) =
            case get x y arr2 of
                Nothing ->
                    ( visited, arr2 )

                Just c ->
                    if Set.member ( x, y ) visited then
                        ( visited, arr2 )
                    else if c == a then
                        List.foldl
                            (fillRegion a)
                            ( Set.insert ( x, y ) visited, set x y to arr2 )
                            (neighbors x y)
                    else
                        ( Set.insert ( x, y ) visited, arr2 )

        start a =
            fillRegion a ( x, y ) ( Set.empty, arr2 ) |> Tuple.second
    in
        Maybe.map start (get x y arr2)
            |> Maybe.withDefault arr2
