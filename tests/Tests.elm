module Tests exposing (..)

import Char
import Expect exposing (Expectation)
import Inflect
import Test exposing (..)


singularsToPlurals : List ( String, String )
singularsToPlurals =
    [ ( "foo", "foos" )
    , ( "axis", "axes" )
    , ( "bus", "buses" )
    , ( "matrix", "matrices" )
    , ( "ox", "oxen" )
    , ( "quiz", "quizzes" )
    , ( "wife", "wives" )
    , ( "safe", "saves" )
    , ( "half", "halves" )
    , ( "foss", "fosses" )
    , ( "news", "news" )
    , ( "medium", "media" )
    , ( "diagnosis", "diagnoses" )
    , ( "hive", "hives" )
    , ( "series", "series" )
    , ( "octopus", "octopi" )
    , ( "vertex", "vertices" )
    , ( "database", "databases" )
    , ( "equipment", "equipment" )
    , ( "fish", "fish" )
    , ( "police", "police" )
    , ( "person", "people" )
    , ( "child", "children" )
    , ( "zombie", "zombies" )
    ]
        |> List.concatMap
            (\( singular, plural ) ->
                [ ( singular, plural )
                , ( capitalizeFirst singular, capitalizeFirst plural )
                ]
            )


capitalizeFirst : String -> String
capitalizeFirst string =
    case String.uncons string of
        Just ( head, tail ) ->
            String.cons (Char.toUpper head) tail

        Nothing ->
            string


suite : Test
suite =
    describe "Inflect"
        [ describe "pluralize" <|
            List.concatMap
                (\( singular, plural ) ->
                    [ test (singular ++ " -> " ++ plural) <|
                        \_ -> Inflect.pluralize singular |> Expect.equal plural
                    ]
                        ++ (if singular == plural then
                                []
                            else
                                [ test (plural ++ " -> " ++ plural) <|
                                    \_ -> Inflect.pluralize plural |> Expect.equal plural
                                ]
                           )
                )
                singularsToPlurals
        , describe "singularize" <|
            List.concatMap
                (\( singular, plural ) ->
                    [ test (plural ++ " -> " ++ singular) <|
                        \_ -> Inflect.singularize plural |> Expect.equal singular
                    ]
                        ++ (if singular == plural then
                                []
                            else
                                [ test (singular ++ " -> " ++ singular) <|
                                    \_ -> Inflect.singularize singular |> Expect.equal singular
                                ]
                           )
                )
                singularsToPlurals
        , describe "camelize and pascalize" <|
            let
                cases =
                    [ ( "foo bar", "fooBar", "FooBar" )
                    , ( "foo-bar", "fooBar", "FooBar" )
                    , ( "foo-_bar", "fooBar", "FooBar" )
                    , ( "foo -\tbar", "fooBar", "FooBar" )
                    , ( "foo 123 bar", "foo123Bar", "Foo123Bar" )
                    , ( "foo 123 bar456baz", "foo123Bar456Baz", "Foo123Bar456Baz" )
                    ]
                        |> List.concatMap
                            (\( string, camel, pascal ) ->
                                [ ( string, camel, pascal )
                                , ( String.toUpper string, camel, pascal )
                                ]
                            )
            in
            List.concatMap
                (\( string, camel, pascal ) ->
                    [ test ("camelize(" ++ string ++ ") -> " ++ camel) <|
                        \_ -> Inflect.camelize string |> Expect.equal camel
                    , test ("pascalize(" ++ string ++ ") -> " ++ pascal) <|
                        \_ -> Inflect.pascalize string |> Expect.equal pascal
                    ]
                )
                cases
        ]
