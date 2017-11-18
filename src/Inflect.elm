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
                [ Just a ] ->
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


plurals : List (String -> Maybe String)
plurals =
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
        |> List.reverse


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


pluralize : String -> String
pluralize string =
    if List.member string uncountables then
        string
    else
        pluralize_ plurals string


pluralize_ : List (String -> Maybe String) -> String -> String
pluralize_ plurals string =
    case plurals of
        head :: tail ->
            case head string of
                Just string ->
                    string

                Nothing ->
                    pluralize_ tail string

        [] ->
            string


singularize : String -> String
singularize =
    identity
