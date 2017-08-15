module History exposing (History, initialize, push, pop)

import Array exposing (Array)


type History a
    = History Int (Array a)


initialize : Int -> History a
initialize size =
    History size Array.empty


sameAsHead : a -> History a -> Bool
sameAsHead a (History _ items) =
    case Array.get 0 items of
        Nothing ->
            False

        Just head ->
            head == a


push : a -> History a -> History a
push a ((History size items) as history) =
    if sameAsHead a history then
        history
    else
        Array.append (Array.fromList [ a ]) items
            |> Array.slice 0 size
            |> History size


pop : History a -> ( Maybe a, History a )
pop (History size items) =
    ( Array.get 0 items
    , History size <| Array.slice 1 size items
    )
