port module Main exposing (..)

import Array2Tests
import ColorUtilTests
import HistoryTests
import SelectionListTests
import Test
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Test.Runner.Node.TestProgram
main =
    let
        tests =
            Test.concat
                [ Array2Tests.all
                , ColorUtilTests.all
                , HistoryTests.all
                , SelectionListTests.all
                ]
    in
        run emit tests


port emit : ( String, Value ) -> Cmd msg
