module SelectionList exposing (SelectionList, init, updateCurrent, append, selectCurrent, isSingle, get, toArray)

import Array exposing (Array)


type alias SelectionList a =
    { previous : Array a
    , current : a
    , next : Array a
    }


init : a -> SelectionList a
init current =
    { previous = Array.empty
    , current = current
    , next = Array.empty
    }


updateCurrent : a -> SelectionList a -> SelectionList a
updateCurrent current frames =
    { frames | current = current }


append : a -> SelectionList a -> SelectionList a
append frame frames =
    { previous = sandwich frames.previous frames.current frames.next
    , current = frame
    , next = Array.empty
    }


partitionWithItem : a -> List a -> Maybe ( List a, List a )
partitionWithItem item list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x == item then
                Just ( [], xs )
            else
                Maybe.map (\( ys, zs ) -> ( x :: ys, zs )) <|
                    partitionWithItem item xs


sandwich : Array a -> a -> Array a -> Array a
sandwich xs y zs =
    Array.append (Array.push y xs) zs


selectCurrent : a -> SelectionList a -> SelectionList a
selectCurrent item list =
    let
        items =
            Array.toList <| toArray list
    in
        case partitionWithItem item items of
            Nothing ->
                list

            Just ( xs, ys ) ->
                { previous = Array.fromList xs
                , current = item
                , next = Array.fromList ys
                }


isSingle : SelectionList a -> Bool
isSingle list =
    Array.isEmpty list.previous && Array.isEmpty list.next


length : SelectionList a -> Int
length list =
    Array.length list.previous + 1 + Array.length list.next


get : Int -> SelectionList a -> a
get index list =
    let
        i =
            index % length list

        prevSize =
            Array.length list.previous

        maybe =
            if i < prevSize then
                Array.get i list.previous
            else
                Array.get (i - 1 - prevSize) list.next
    in
        Maybe.withDefault list.current maybe


toArray : SelectionList a -> Array a
toArray list =
    sandwich list.previous list.current list.next
