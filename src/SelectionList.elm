module SelectionList exposing (..)


type alias SelectionList a =
    { previous : List a
    , current : a
    , next : List a
    }


init : a -> SelectionList a
init current =
    { previous = []
    , current = current
    , next = []
    }


updateCurrent : a -> SelectionList a -> SelectionList a
updateCurrent current frames =
    { frames | current = current }


append : a -> SelectionList a -> SelectionList a
append frame frames =
    let
        previous =
            List.concat
                [ List.reverse frames.next
                , frames.current :: frames.previous
                ]
    in
        { previous = previous, current = frame, next = [] }


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


selectCurrent : a -> SelectionList a -> SelectionList a
selectCurrent frame frames =
    if frame == frames.current then
        frames
    else
        case partitionWithItem frame <| List.reverse frames.previous of
            Nothing ->
                case partitionWithItem frame frames.next of
                    Nothing ->
                        frames

                    Just ( xs, ys ) ->
                        { previous =
                            List.concat [ List.reverse xs, [ frames.current ], frames.previous ]
                        , current = frame
                        , next = ys
                        }

            Just ( xs, ys ) ->
                { previous = List.reverse xs
                , current = frame
                , next = List.concat [ ys, [ frames.current ], frames.next ]
                }
