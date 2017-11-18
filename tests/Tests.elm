module Tests exposing (..)

import Expect exposing (Expectation)
import Inflect
import Test exposing (..)


suite : Test
suite =
    describe "Inflect"
        [ describe "pluralize"
            (let
                cases =
                    [ ( "foo", "foos" )
                    , ( "foos", "foos" )
                    , ( "axis", "axes" )
                    , ( "bus", "buses" )
                    , ( "matrix", "matrices" )
                    , ( "ox", "oxen" )
                    , ( "oxen", "oxen" )
                    , ( "quiz", "quizzes" )
                    , ( "wife", "wives" )
                    , ( "safe", "saves" )
                    , ( "half", "halves" )
                    , ( "equipment", "equipment" )
                    , ( "fish", "fish" )
                    , ( "police", "police" )
                    ]
             in
             cases
                |> List.map
                    (\( a, b ) ->
                        test (a ++ " -> " ++ b) <| \_ -> Inflect.pluralize a |> Expect.equal b
                    )
            )
        , describe "singularize"
            (let
                cases =
                    [ ( "foos", "foo" )
                    , ( "foss", "foss" )
                    , ( "news", "news" )
                    , ( "media", "medium" )
                    , ( "diagnoses", "diagnosis" )
                    , ( "hives", "hive" )
                    , ( "series", "series" )
                    , ( "buses", "bus" )
                    , ( "octopi", "octopus" )
                    , ( "vertices", "vertex" )
                    , ( "databases", "database" )
                    , ( "equipment", "equipment" )
                    , ( "fish", "fish" )
                    , ( "police", "police" )
                    ]
             in
             cases
                |> List.map
                    (\( a, b ) ->
                        test (a ++ " -> " ++ b) <| \_ -> Inflect.singularize a |> Expect.equal b
                    )
            )
        ]
