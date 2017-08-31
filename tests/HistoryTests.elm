module HistoryTests exposing (..)

import Test exposing (..)
import Expect
import History


all : Test
all =
    describe "History" <|
        [ test "keeps size" <|
            \() ->
                let
                    history =
                        History.initialize 2
                            |> History.push 1
                            |> History.push 2
                            |> History.push 3

                    ( popped1, history1 ) =
                        History.undo 4 history

                    ( popped2, history2 ) =
                        History.undo 3 history1

                    ( popped3, history3 ) =
                        History.undo 2 history2

                    ( popped4, history4 ) =
                        History.redo 2 history3

                    ( popped5, history5 ) =
                        History.redo 3 history4
                in
                    Expect.equal
                        ( popped1, popped2, popped3, popped4, popped5 )
                        ( Just 3, Just 2, Nothing, Just 3, Just 4 )
        , test "ignores same item" <|
            \() ->
                let
                    history =
                        History.initialize 2
                            |> History.push 1
                            |> History.push 1

                    ( popped1, history1 ) =
                        History.undo 2 history

                    ( popped2, history2 ) =
                        History.undo 1 history1

                    ( popped3, history3 ) =
                        History.redo 1 history2

                    ( popped4, history4 ) =
                        History.redo 2 history3
                in
                    Expect.equal
                        ( popped1, popped2, popped3, popped4 )
                        ( Just 1, Nothing, Just 2, Nothing )
        ]
