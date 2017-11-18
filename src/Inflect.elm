module Inflect exposing (..)

import Regex


replace : String -> (Regex.Match -> String) -> String -> Maybe String
replace regex_ replacer string =
    let
        regex =
            Regex.regex regex_ |> Regex.caseInsensitive
    in
    if Regex.contains regex string then
        Just <| Regex.replace Regex.All regex replacer string
    else
        Nothing


replace0 : String -> String -> String -> Maybe String
replace0 regex replacement =
    replace regex
        (\match -> replacement)


replace1 : String -> String -> String -> Maybe String
replace1 regex append =
    replace regex
        (\match ->
            case match.submatches of
                (Just a) :: _ ->
                    a ++ append

                _ ->
                    ""
        )


replace2 : String -> String -> String -> Maybe String
replace2 regex append =
    replace regex
        (\match ->
            case match.submatches of
                [ a, b ] ->
                    Maybe.withDefault "" a ++ Maybe.withDefault "" b ++ append

                _ ->
                    ""
        )


irregulars : Bool -> List (String -> Maybe String)
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


plurals : List (String -> Maybe String)
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


singulars : List (String -> Maybe String)
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


apply : List (String -> Maybe String) -> String -> String
apply replacers string =
    case replacers of
        head :: tail ->
            case head string of
                Just string ->
                    string

                Nothing ->
                    apply tail string

        [] ->
            string


pluralize : String -> String
pluralize string =
    if List.member (String.toLower string) uncountables then
        string
    else
        apply plurals string


singularize : String -> String
singularize string =
    if List.member (String.toLower string) uncountables then
        string
    else
        apply singulars string
