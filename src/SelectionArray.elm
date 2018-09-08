module SelectionArray exposing
    ( SelectionArray
    , append
    , deleteCurrent
    , get
    , init
    , insertAfterCurrent
    , isSingle
    , length
    , map
    , select
    , swapCurrent
    , toList
    , updateCurrent
    )

import Array exposing (Array)


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
updateCurrent update array =
    { array | current = update array.current }


append : a -> SelectionArray a -> SelectionArray a
append item array =
    { previous = sandwich array.previous array.current array.next
    , current = item
    , next = Array.empty
    }


partitionWithItem : a -> List a -> Maybe ( List a, List a )
partitionWithItem item items =
    case items of
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
select item array =
    case partitionWithItem item (toList array) of
        Nothing ->
            array

        Just ( xs, ys ) ->
            { previous = Array.fromList xs
            , current = item
            , next = Array.fromList ys
            }


getLast : Array a -> Maybe a
getLast array =
    Array.get (Array.length array - 1) array


deleteCurrent : SelectionArray a -> SelectionArray a
deleteCurrent array =
    case Array.get 0 array.next of
        Just nextCurrent ->
            { array
                | current = nextCurrent
                , next = Array.slice 1 (Array.length array.next) array.next
            }

        Nothing ->
            case getLast array.previous of
                Just nextCurrent ->
                    { array
                        | current = nextCurrent
                        , previous = Array.slice 0 -1 array.previous
                    }

                Nothing ->
                    array


insertAfterCurrent : a -> SelectionArray a -> SelectionArray a
insertAfterCurrent item array =
    { array
        | previous = Array.push array.current array.previous
        , current = item
    }


isSingle : SelectionArray a -> Bool
isSingle array =
    Array.isEmpty array.previous && Array.isEmpty array.next


length : SelectionArray a -> Int
length array =
    Array.length array.previous + 1 + Array.length array.next


get : Int -> SelectionArray a -> a
get index array =
    let
        i =
            modBy (length array) index

        prevSize =
            Array.length array.previous

        maybe =
            if i < prevSize then
                Array.get i array.previous

            else
                Array.get (i - 1 - prevSize) array.next
    in
    Maybe.withDefault array.current maybe


toList : SelectionArray a -> List a
toList array =
    Array.toList <|
        sandwich array.previous array.current array.next


swapCurrent : a -> SelectionArray a -> SelectionArray a
swapCurrent item array =
    case partitionWithItem item (Array.toList array.previous) of
        Nothing ->
            case partitionWithItem item (Array.toList array.next) of
                Nothing ->
                    array

                Just ( xs, ys ) ->
                    { array
                        | previous = sandwich array.previous item (Array.fromList xs)
                        , next = Array.fromList ys
                    }

        Just ( xs, ys ) ->
            { array
                | previous = Array.fromList xs
                , next = sandwich (Array.fromList ys) item array.next
            }


map : (a -> b) -> SelectionArray a -> SelectionArray b
map f array =
    { previous = Array.map f array.previous
    , current = f array.current
    , next = Array.map f array.next
    }
