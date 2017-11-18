module Tests exposing (..)

import Expect exposing (Expectation)
import Inflect
import Test exposing (..)


suite : Test
suite =
    describe "Inflect"
        [ describe "pluralize"
            [ test "foo" <| \_ -> Inflect.pluralize "foo" |> Expect.equal "foos"
            ]
        , describe "singularize"
            [ test "foos" <| \_ -> Inflect.singularize "foos" |> Expect.equal "foo"
            ]
        ]
