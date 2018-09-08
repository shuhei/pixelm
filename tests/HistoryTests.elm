module HistoryTests exposing (all)

import Expect
import History
import Test exposing (..)


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
                        History.undo 4 history |> Maybe.withDefault ( 0, history )

                    ( popped2, history2 ) =
                        History.undo 3 history1 |> Maybe.withDefault ( 0, history1 )

                    result3 =
                        History.undo 2 history2

                    ( popped4, history4 ) =
                        History.redo 2 history2 |> Maybe.withDefault ( 0, history2 )

                    ( popped5, history5 ) =
                        History.redo 3 history4 |> Maybe.withDefault ( 0, history4 )
                in
                Expect.equal
                    ( popped1, popped2, result3, popped4, popped5 )
                    ( 3, 2, Nothing, 3, 4 )
        , test "ignores same item" <|
            \() ->
                let
                    history =
                        History.initialize 2
                            |> History.push 1
                            |> History.push 1

                    ( popped1, history1 ) =
                        History.undo 2 history |> Maybe.withDefault ( 0, history )

                    result2 =
                        History.undo 1 history1

                    ( popped3, history3 ) =
                        History.redo 1 history1 |> Maybe.withDefault ( 0, history1 )

                    result4 =
                        History.redo 2 history3
                in
                Expect.equal
                    ( popped1, result2, popped3, result4 )
                    ( 1, Nothing, 2, Nothing )
        ]
