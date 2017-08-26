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
        , test "selectCurrent on previous" <|
            \() ->
                Expect.equal
                    (SelectionList.selectCurrent 2 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1 ] 2 [ 3, 4, 5 ])
        , test "selectCurrent on current" <|
            \() ->
                Expect.equal
                    (SelectionList.selectCurrent 3 <|
                        fromLists [ 1, 2 ] 3 [ 4, 5 ]
                    )
                    (fromLists [ 1, 2 ] 3 [ 4, 5 ])
        , test "selectCurrent on next" <|
            \() ->
                Expect.equal
                    (SelectionList.selectCurrent 4 <|
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
        ]
