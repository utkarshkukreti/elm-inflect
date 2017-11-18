module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (..)
import Inflect


suite : Benchmark
suite =
    describe "Inflect"
        [ describe "pluralize"
            [ benchmark1 "pluralize person" Inflect.pluralize "person"
            , benchmark1 "pluralize ox" Inflect.pluralize "ox"
            , benchmark1 "pluralize foo" Inflect.pluralize "foo"
            ]
        , describe "singularize"
            [ benchmark1 "singularize people" Inflect.singularize "people"
            , benchmark1 "singularize oxen" Inflect.singularize "oxen"
            , benchmark1 "singularize foos" Inflect.singularize "foos"
            ]
        ]


main : BenchmarkProgram
main =
    program suite
