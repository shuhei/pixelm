module History exposing
    ( History
    , hasNext
    , hasPrevious
    , initialize
    , push
    , redo
    , undo
    )


type History a
    = History Int (List a) (List a)


initialize : Int -> History a
initialize size =
    History size [] []


sameAsHead : a -> History a -> Bool
sameAsHead a (History _ previous next) =
    case previous of
        [] ->
            False

        x :: _ ->
            x == a


push : a -> History a -> History a
push item ((History size previous next) as history) =
    let
        nextPrevious =
            if sameAsHead item history then
                previous

            else
                List.take size <| item :: previous
    in
    History size nextPrevious []


undo : a -> History a -> Maybe ( a, History a )
undo item ((History size previous next) as history) =
    case previous of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, History size xs (item :: next) )


redo : a -> History a -> Maybe ( a, History a )
redo item ((History size previous next) as history) =
    case next of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, History size (item :: previous) xs )


hasPrevious : History a -> Bool
hasPrevious (History _ previous _) =
    not <| List.isEmpty previous


hasNext : History a -> Bool
hasNext (History _ _ next) =
    not <| List.isEmpty next
