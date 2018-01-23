module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (..)
import Inflect


suite : Benchmark
suite =
    describe "Inflect"
        [ describe "pluralize"
            [ benchmark "pluralize person" <| \_ -> Inflect.pluralize "person"
            , benchmark "pluralize ox" <| \_ -> Inflect.pluralize "ox"
            , benchmark "pluralize foo" <| \_ -> Inflect.pluralize "foo"
            ]
        , describe "singularize"
            [ benchmark "singularize people" <| \_ -> Inflect.singularize "people"
            , benchmark "singularize oxen" <| \_ -> Inflect.singularize "oxen"
            , benchmark "singularize foos" <| \_ -> Inflect.singularize "foos"
            ]
        ]


main : BenchmarkProgram
main =
    program suite
