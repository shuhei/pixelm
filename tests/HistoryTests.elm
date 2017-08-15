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
                        History.pop history

                    ( popped2, history2 ) =
                        History.pop history1

                    ( popped3, history3 ) =
                        History.pop history2
                in
                    Expect.equal ( popped1, popped2, popped3 ) ( Just 3, Just 2, Nothing )
        , test "ignores same item" <|
            \() ->
                let
                    history =
                        History.initialize 2
                            |> History.push 1
                            |> History.push 1

                    ( popped1, history1 ) =
                        History.pop history

                    ( popped2, history2 ) =
                        History.pop history1
                in
                    Expect.equal ( popped1, popped2 ) ( Just 1, Nothing )
        ]
