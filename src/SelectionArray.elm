module SelectionArray
    exposing
        ( SelectionArray
        , init
        , select
        , updateCurrent
        , deleteCurrent
        , swapCurrent
        , insertAfterCurrent
        , append
        , isSingle
        , get
        , toArray
        , toList
        , length
        )

import Array.Hamt as Array exposing (Array)


type alias SelectionArray a =
    { previous : Array a
    , current : a
    , next : Array a
    }


init : a -> SelectionArray a
init current =
    { previous = Array.empty
    , current = current
    , next = Array.empty
    }


updateCurrent : (a -> a) -> SelectionArray a -> SelectionArray a
updateCurrent update frames =
    { frames | current = update frames.current }


append : a -> SelectionArray a -> SelectionArray a
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


select : a -> SelectionArray a -> SelectionArray a
select item list =
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


getLast : Array a -> Maybe a
getLast array =
    Array.get (Array.length array - 1) array


deleteCurrent : SelectionArray a -> SelectionArray a
deleteCurrent list =
    case Array.get 0 list.next of
        Just nextCurrent ->
            { list
                | current = nextCurrent
                , next = Array.slice 1 (Array.length list.next) list.next
            }

        Nothing ->
            case getLast list.previous of
                Just nextCurrent ->
                    { list
                        | current = nextCurrent
                        , previous = Array.slice 0 -1 list.previous
                    }

                Nothing ->
                    list


insertAfterCurrent : a -> SelectionArray a -> SelectionArray a
insertAfterCurrent item list =
    { list
        | previous = Array.push list.current list.previous
        , current = item
    }


isSingle : SelectionArray a -> Bool
isSingle list =
    Array.isEmpty list.previous && Array.isEmpty list.next


length : SelectionArray a -> Int
length list =
    Array.length list.previous + 1 + Array.length list.next


get : Int -> SelectionArray a -> a
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


toArray : SelectionArray a -> Array a
toArray list =
    sandwich list.previous list.current list.next


toList : SelectionArray a -> List a
toList list =
    Array.toList <| toArray list


swapCurrent : a -> SelectionArray a -> SelectionArray a
swapCurrent item list =
    case partitionWithItem item (Array.toList list.previous) of
        Nothing ->
            case partitionWithItem item (Array.toList list.next) of
                Nothing ->
                    list

                Just ( xs, ys ) ->
                    { list
                        | previous = sandwich list.previous item (Array.fromList xs)
                        , next = Array.fromList ys
                    }

        Just ( xs, ys ) ->
            { list
                | previous = Array.fromList xs
                , next = sandwich (Array.fromList ys) item list.next
            }
