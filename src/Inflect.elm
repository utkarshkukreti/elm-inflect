module Inflect exposing (camelize, pascalize, pluralize, singularize)

import Char
import Regex exposing (Regex)


type alias Replacer =
    ( Regex, String -> String )


replace : String -> (Regex.Match -> String) -> Replacer
replace regex_ replacer =
    let
        regex =
            regex_ |> Regex.regex |> Regex.caseInsensitive
    in
    ( regex, Regex.replace Regex.All regex replacer )


replace0 : String -> String -> Replacer
replace0 regex replacement =
    replace regex (always replacement)


replace1 : String -> String -> Replacer
replace1 regex append =
    replace regex
        (\match ->
            case match.submatches of
                (Just a) :: _ ->
                    a ++ append

                _ ->
                    ""
        )


replace2 : String -> String -> Replacer
replace2 regex append =
    replace regex
        (\match ->
            case match.submatches of
                [ a, b ] ->
                    Maybe.withDefault "" a ++ Maybe.withDefault "" b ++ append

                _ ->
                    ""
        )


irregulars : Bool -> List Replacer
irregulars isPlurals =
    [ ( "person", "people" )
    , ( "man", "men" )
    , ( "child", "children" )
    , ( "sex", "sexes" )
    , ( "move", "moves" )
    , ( "zombie", "zombies" )
    ]
        |> List.concatMap
            (\( singular, plural ) ->
                case ( String.uncons singular, String.uncons plural ) of
                    ( Just ( sHead, sTail ), Just ( pHead, pTail ) ) ->
                        let
                            replacement =
                                if isPlurals then
                                    pTail
                                else
                                    sTail
                        in
                        [ replace1 ("(" ++ String.fromChar sHead ++ ")" ++ sTail) replacement
                        , replace1 ("(" ++ String.fromChar pHead ++ ")" ++ pTail) replacement
                        ]

                    _ ->
                        []
            )


plurals : List Replacer
plurals =
    List.reverse <|
        [ replace0 "$" "s"
        , replace0 "s$" "s"
        , replace1 "^(ax|test)is$" "es"
        , replace1 "(octop|vir)us$" "i"
        , replace1 "(octop|vir)i$" "i"
        , replace1 "(alias|status)$" "es"
        , replace1 "(bu)s$" "ses"
        , replace1 "(buffal|tomat)o$" "oes"
        , replace1 "([ti])um$" "a"
        , replace1 "([ti])a$" "a"
        , replace0 "sis$" "ses"
        , replace2 "(?:([^f])fe|([lr])f)$" "ves"
        , replace1 "(hive)$" "s"
        , replace1 "([^aeiouy]|qu)y$" "ies"
        , replace1 "(x|ch|ss|sh)$" "es"
        , replace1 "(matr|vert|ind)(?:ix|ex)$" "ices"
        , replace1 "^(m|l)ouse$" "ice"
        , replace1 "^(m|l)ice$" "ice"
        , replace1 "^(ox)$" "en"
        , replace1 "^(oxen)$" ""
        , replace1 "(quiz)$" "zes"
        ]
            ++ irregulars True


singulars : List Replacer
singulars =
    List.reverse <|
        [ replace0 "s$" ""
        , replace1 "(ss)$" ""
        , replace1 "(n)ews$" "ews"
        , replace1 "([ti])a$" "um"
        , replace1 "((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$" "sis"
        , replace1 "(^analy)(sis|ses)$" "sis"
        , replace1 "([^f])ves$" "fe"
        , replace1 "(hive)s$" ""
        , replace1 "(tive)s$" ""
        , replace1 "([lr])ves$" "f"
        , replace1 "([^aeiouy]|qu)ies$" "y"
        , replace1 "(s)eries$" "eries"
        , replace1 "(m)ovies$" "ovie"
        , replace1 "(x|ch|ss|sh)es$" ""
        , replace1 "^(m|l)ice$" "ouse"
        , replace1 "(bus)(es)?$" ""
        , replace1 "(o)es$" ""
        , replace1 "(shoe)s$" ""
        , replace1 "(cris|test)(is|es)$" "is"
        , replace1 "^(a)x[ie]s$" "xis"
        , replace1 "(octop|vir)(us|i)$" "us"
        , replace1 "(alias|status)(es)?$" ""
        , replace1 "^(ox)en" ""
        , replace1 "(vert|ind)ices$" "ex"
        , replace1 "(matr)ices$" "ix"
        , replace1 "(quiz)zes$" ""
        , replace1 "(database)s$" ""
        ]
            ++ irregulars False


uncountables : List String
uncountables =
    [ "equipment"
    , "information"
    , "rice"
    , "money"
    , "species"
    , "series"
    , "fish"
    , "sheep"
    , "jeans"
    , "police"
    ]


apply : List Replacer -> String -> String
apply replacers string =
    case replacers of
        ( regex, replacer ) :: tail ->
            if Regex.contains regex string then
                replacer string
            else
                apply tail string

        [] ->
            string


{-| Convert a String to its plural form.

    pluralize "foo" == "foos"
    pluralize "axis" == "axes"
    pluralize "bus" == "buses"

-}
pluralize : String -> String
pluralize string =
    if List.member (String.toLower string) uncountables then
        string
    else
        apply plurals string


{-| Convert a String to its singular form.

    singularize "foos" == "foo"
    singularize "axes" == "axis"
    singularize "buses" == "bus"

-}
singularize : String -> String
singularize string =
    if List.member (String.toLower string) uncountables then
        string
    else
        apply singulars string


mapFirst : (Char -> Char) -> String -> String
mapFirst f string =
    case String.uncons string of
        Just ( head, tail ) ->
            String.fromChar (f head) ++ tail

        Nothing ->
            string


{-| Convert a String to camelCase.

    camelize "foo bar" == "fooBar"
    camelize "foo-bar" == "fooBar"
    camelize "foo-_bar" == "fooBar"

-}
camelize : String -> String
camelize =
    pascalize >> mapFirst Char.toLower


{-| Convert a String to PascalCase

    pascalize "foo bar" == "FooBar"
    pascalize "foo-bar" == "FooBar"
    pascalize "foo-_bar" == "FooBar"

-}
pascalize : String -> String
pascalize =
    Regex.find Regex.All (Regex.regex "[a-zA-Z]+|[0-9]+")
        >> List.map (.match >> String.toLower >> mapFirst Char.toUpper)
        >> String.join ""
