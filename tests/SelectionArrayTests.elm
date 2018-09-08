module SelectionArrayTests exposing (all)

import Array.Hamt as Array exposing (Array)
import Expect
import SelectionArray exposing (SelectionArray)
import Test exposing (..)


fromLists : List a -> a -> List a -> SelectionArray a
fromLists previous current next =
    { previous = Array.fromList previous
    , current = current
    , next = Array.fromList next
    }


all : Test
all =
    describe "SelectionArray"
        [ test "init" <|
            \() ->
                Expect.equal
                    (SelectionArray.init 123)
                    (fromLists [] 123 [])
        , test "select on previous" <|
            \() ->
                Expect.equal
                    (SelectionArray.select 2 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1 ] 2 [ 3, 4, 5 ])
        , test "select on current" <|
            \() ->
                Expect.equal
                    (SelectionArray.select 3 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1, 2 ] 3 [ 4, 5 ])
        , test "select on next" <|
            \() ->
                Expect.equal
                    (SelectionArray.select 4 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1, 2, 3 ] 4 [ 5 ])
        , test "deleteCurrent picks next one" <|
            \() ->
                Expect.equal
                    (SelectionArray.deleteCurrent <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1, 2 ] 4 [ 5 ])
        , test "deleteCurrent fallbacks to previous one" <|
            \() ->
                Expect.equal
                    (SelectionArray.deleteCurrent <|
                        fromLists [ 1, 2 ] 3 []
                    )
                    (fromLists [ 1 ] 2 [])
        , test "deleteCurrent does not make current empty" <|
            \() ->
                Expect.equal
                    (SelectionArray.deleteCurrent <|
                        fromLists [] 3 []
                    )
                    (fromLists [] 3 [])
        , test "length for only current" <|
            \() ->
                Expect.equal
                    (SelectionArray.length <| fromLists [] 3 [])
                    1
        , test "length with prev and next" <|
            \() ->
                Expect.equal
                    (SelectionArray.length <| fromLists [ 1, 2 ] 3 [ 4, 5, 6 ])
                    6
        , test "insertAfterCurrent" <|
            \() ->
                Expect.equal
                    (SelectionArray.insertAfterCurrent 8 <| fromLists [ 1, 2 ] 3 [ 4, 5, 6 ])
                <|
                    fromLists [ 1, 2, 3 ] 8 [ 4, 5, 6 ]
        , test "swapCurrent on previous" <|
            \() ->
                Expect.equal
                    (SelectionArray.swapCurrent 2 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                    )
                <|
                    fromLists [ 1 ] 3 [ 2, 4, 5, 6 ]
        , test "swapCurrent on next" <|
            \() ->
                Expect.equal
                    (SelectionArray.swapCurrent 5 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                    )
                <|
                    fromLists [ 1, 2, 5, 4 ] 3 [ 6 ]
        , test "swapCurrent on current" <|
            \() ->
                Expect.equal
                    (SelectionArray.swapCurrent 3 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                    )
                <|
                    fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        , test "swapCurrent not found" <|
            \() ->
                Expect.equal
                    (SelectionArray.swapCurrent 8 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                    )
                <|
                    fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        ]
