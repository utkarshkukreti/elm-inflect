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
        , skip <|
            describe "singularize"
                [ test "foos" <| \_ -> Inflect.singularize "foos" |> Expect.equal "foo"
                ]
        ]
