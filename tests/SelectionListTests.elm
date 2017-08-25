module SelectionListTests exposing (..)

import Test exposing (..)
import Expect
import SelectionList exposing (SelectionList)


all : Test
all =
    describe "SelectionList"
        [ test "init" <|
            \() ->
                Expect.equal
                    (SelectionList.init 123)
                    { previous = [], current = 123, next = [] }
        , test "selectCurrent on previous" <|
            \() ->
                Expect.equal
                    (SelectionList.selectCurrent 2
                        { previous = [ 2, 1 ], current = 3, next = [ 4, 5 ] }
                    )
                    { previous = [ 1 ], current = 2, next = [ 3, 4, 5 ] }
        , test "selectCurrent on current" <|
            \() ->
                Expect.equal
                    (SelectionList.selectCurrent 3
                        { previous = [ 2, 1 ], current = 3, next = [ 4, 5 ] }
                    )
                    { previous = [ 2, 1 ], current = 3, next = [ 4, 5 ] }
        , test "selectCurrent on next" <|
            \() ->
                Expect.equal
                    (SelectionList.selectCurrent 4
                        { previous = [ 2, 1 ], current = 3, next = [ 4, 5 ] }
                    )
                    { previous = [ 3, 2, 1 ], current = 4, next = [ 5 ] }
        ]
