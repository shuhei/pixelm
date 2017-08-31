module History exposing (History, initialize, push, undo, redo)

import Array.Hamt as Array exposing (Array)


type History a
    = History Int (Array a) (Array a)


initialize : Int -> History a
initialize size =
    History size Array.empty Array.empty


sameAsHead : a -> History a -> Bool
sameAsHead a (History _ previous next) =
    case Array.get 0 previous of
        Nothing ->
            False

        Just head ->
            head == a


push : a -> History a -> History a
push item ((History size previous next) as history) =
    let
        nextPrevious =
            if sameAsHead item history then
                previous
            else
                Array.append (Array.repeat 1 item) previous
                    |> Array.slice 0 size
    in
        History size nextPrevious Array.empty


undo : a -> History a -> ( Maybe a, History a )
undo item ((History size previous next) as history) =
    case Array.get 0 previous of
        Nothing ->
            ( Nothing, history )

        Just popped ->
            let
                nextHistory =
                    History
                        size
                        (Array.slice 1 (Array.length previous) previous)
                        (Array.append (Array.repeat 1 item) next)
            in
                ( Just popped, nextHistory )


redo : a -> History a -> ( Maybe a, History a )
redo item ((History size previous next) as history) =
    case Array.get 0 next of
        Nothing ->
            ( Nothing, history )

        Just popped ->
            let
                nextHistory =
                    History
                        size
                        (Array.append (Array.repeat 1 item) previous)
                        (Array.slice 1 (Array.length next) next)
            in
                ( Just popped, nextHistory )
