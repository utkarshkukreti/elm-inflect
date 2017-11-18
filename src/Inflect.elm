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


replace1 : String -> (String -> String) -> String -> Maybe String
replace1 regex replacer =
    replace regex
        (\match ->
            case match.submatches of
                [ Just a ] ->
                    replacer a

                _ ->
                    ""
        )


replace2 : String -> (String -> String -> String) -> String -> Maybe String
replace2 regex replacer string =
    Nothing


append : String -> String -> String
append =
    flip (++)


plurals : List (String -> Maybe String)
plurals =
    [ replace0 "$" "s"
    , replace0 "s$" "s"
    , replace1 "^(ax|test)is$" (append "es")
    , replace1 "(octop|vir)us$" (append "i")
    , replace1 "(octop|vir)i$" (append "i")
    , replace1 "(alias|status)$" (append "es")
    , replace1 "(bu)s$" (append "ses")
    , replace1 "(buffal|tomat)o$" (append "oes")
    , replace1 "([ti])um$" (append "a")
    , replace1 "([ti])a$" (append "a")
    , replace0 "sis$" "ses"
    , replace2 "(?:([^f])fe|([lr])f)$" (\x y -> x ++ y ++ "ves")
    , replace1 "(hive)$" (append "s")
    , replace1 "([^aeiouy]|qu)y$" (append "ies")
    , replace1 "(x|ch|ss|sh)$" (append "es")
    , replace1 "(matr|vert|ind)(?:ix|ex)$" (append "ices")
    , replace1 "^(m|l)ouse$" (append "ice")
    , replace1 "^(m|l)ice$" (append "ice")
    , replace1 "^(ox)$" (append "en")
    , replace1 "^(oxen)$" (append "")
    , replace1 "(quiz)$" (append "zes")
    ]
        |> List.reverse


pluralize : String -> String
pluralize =
    pluralize_ plurals


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
