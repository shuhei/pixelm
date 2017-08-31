module SelectionListTests exposing (..)

import Test exposing (..)
import Expect
import SelectionList exposing (SelectionList)
import Array


fromLists : List a -> a -> List a -> SelectionList a
fromLists previous current next =
    { previous = Array.fromList previous
    , current = current
    , next = Array.fromList next
    }


all : Test
all =
    describe "SelectionList"
        [ test "init" <|
            \() ->
                Expect.equal
                    (SelectionList.init 123)
                    (fromLists [] 123 [])
        , test "select on previous" <|
            \() ->
                Expect.equal
                    (SelectionList.select 2 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1 ] 2 [ 3, 4, 5 ])
        , test "select on current" <|
            \() ->
                Expect.equal
                    (SelectionList.select 3 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1, 2 ] 3 [ 4, 5 ])
        , test "select on next" <|
            \() ->
                Expect.equal
                    (SelectionList.select 4 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1, 2, 3 ] 4 [ 5 ])
        , test "deleteCurrent picks next one" <|
            \() ->
                Expect.equal
                    (SelectionList.deleteCurrent <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1, 2 ] 4 [ 5 ])
        , test "deleteCurrent fallbacks to previous one" <|
            \() ->
                Expect.equal
                    (SelectionList.deleteCurrent <|
                        fromLists [ 1, 2 ] 3 []
                    )
                    (fromLists [ 1 ] 2 [])
        , test "deleteCurrent does not make current empty" <|
            \() ->
                Expect.equal
                    (SelectionList.deleteCurrent <|
                        fromLists [] 3 []
                    )
                    (fromLists [] 3 [])
        , test "length for only current" <|
            \() ->
                Expect.equal
                    (SelectionList.length <| fromLists [] 3 [])
                    1
        , test "length with prev and next" <|
            \() ->
                Expect.equal
                    (SelectionList.length <| fromLists [ 1, 2 ] 3 [ 4, 5, 6 ])
                    6
        , test "insertAfterCurrent" <|
            \() ->
                Expect.equal
                    (SelectionList.insertAfterCurrent 8 <| fromLists [ 1, 2 ] 3 [ 4, 5, 6 ])
                <|
                    fromLists [ 1, 2, 3 ] 8 [ 4, 5, 6 ]
        ]
