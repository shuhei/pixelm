module SelectionList exposing (..)

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



-- TODO: Better implementation?


partitionWithItem : a -> Array a -> Maybe ( Array a, Array a )
partitionWithItem item array =
    let
        withList list =
            case list of
                [] ->
                    Nothing

                x :: xs ->
                    if x == item then
                        Just ( [], xs )
                    else
                        Maybe.map (\( ys, zs ) -> ( x :: ys, zs )) <|
                            withList xs
    in
        withList (Array.toList array)
            |> Maybe.map (\( xs, ys ) -> ( Array.fromList xs, Array.fromList ys ))


sandwich : Array a -> a -> Array a -> Array a
sandwich xs y zs =
    Array.append (Array.push y xs) zs


selectCurrent : a -> SelectionList a -> SelectionList a
selectCurrent frame frames =
    if frame == frames.current then
        frames
    else
        case partitionWithItem frame frames.previous of
            Nothing ->
                case partitionWithItem frame frames.next of
                    Nothing ->
                        frames

                    Just ( xs, ys ) ->
                        { previous = sandwich frames.previous frames.current xs
                        , current = frame
                        , next = ys
                        }

            Just ( xs, ys ) ->
                { previous = xs
                , current = frame
                , next = sandwich ys frames.current frames.next
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
