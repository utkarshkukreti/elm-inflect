module Inflect exposing (pluralize, singularize)

import Regex


r : String -> Regex.Regex
r =
    Regex.regex >> Regex.caseInsensitive


replace : Regex.Regex -> (Regex.Match -> String) -> String -> Maybe String
replace regex replacer string =
    if Regex.contains regex string then
        Just <| Regex.replace Regex.All regex replacer string
    else
        Nothing


replace0 : Regex.Regex -> String -> String -> Maybe String
replace0 regex replacement =
    replace regex
        (\match -> replacement)


replace1 : Regex.Regex -> String -> String -> Maybe String
replace1 regex append =
    replace regex
        (\match ->
            case match.submatches of
                (Just a) :: _ ->
                    a ++ append

                _ ->
                    ""
        )


replace2 : Regex.Regex -> String -> String -> Maybe String
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
                        [ replace1 (r ("(" ++ String.fromChar sHead ++ ")" ++ sTail)) replacement
                        , replace1 (r ("(" ++ String.fromChar pHead ++ ")" ++ pTail)) replacement
                        ]

                    _ ->
                        []
            )


plurals : List (String -> Maybe String)
plurals =
    List.reverse <|
        [ replace0 (r "$") "s"
        , replace0 (r "s$") "s"
        , replace1 (r "^(ax|test)is$") "es"
        , replace1 (r "(octop|vir)us$") "i"
        , replace1 (r "(octop|vir)i$") "i"
        , replace1 (r "(alias|status)$") "es"
        , replace1 (r "(bu)s$") "ses"
        , replace1 (r "(buffal|tomat)o$") "oes"
        , replace1 (r "([ti])um$") "a"
        , replace1 (r "([ti])a$") "a"
        , replace0 (r "sis$") "ses"
        , replace2 (r "(?:([^f])fe|([lr])f)$") "ves"
        , replace1 (r "(hive)$") "s"
        , replace1 (r "([^aeiouy]|qu)y$") "ies"
        , replace1 (r "(x|ch|ss|sh)$") "es"
        , replace1 (r "(matr|vert|ind)(?:ix|ex)$") "ices"
        , replace1 (r "^(m|l)ouse$") "ice"
        , replace1 (r "^(m|l)ice$") "ice"
        , replace1 (r "^(ox)$") "en"
        , replace1 (r "^(oxen)$") ""
        , replace1 (r "(quiz)$") "zes"
        ]
            ++ irregulars True


singulars : List (String -> Maybe String)
singulars =
    List.reverse <|
        [ replace0 (r "s$") ""
        , replace1 (r "(ss)$") ""
        , replace1 (r "(n)ews$") "ews"
        , replace1 (r "([ti])a$") "um"
        , replace1 (r "((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$") "sis"
        , replace1 (r "(^analy)(sis|ses)$") "sis"
        , replace1 (r "([^f])ves$") "fe"
        , replace1 (r "(hive)s$") ""
        , replace1 (r "(tive)s$") ""
        , replace1 (r "([lr])ves$") "f"
        , replace1 (r "([^aeiouy]|qu)ies$") "y"
        , replace1 (r "(s)eries$") "eries"
        , replace1 (r "(m)ovies$") "ovie"
        , replace1 (r "(x|ch|ss|sh)es$") ""
        , replace1 (r "^(m|l)ice$") "ouse"
        , replace1 (r "(bus)(es)?$") ""
        , replace1 (r "(o)es$") ""
        , replace1 (r "(shoe)s$") ""
        , replace1 (r "(cris|test)(is|es)$") "is"
        , replace1 (r "^(a)x[ie]s$") "xis"
        , replace1 (r "(octop|vir)(us|i)$") "us"
        , replace1 (r "(alias|status)(es)?$") ""
        , replace1 (r "^(ox)en") ""
        , replace1 (r "(vert|ind)ices$") "ex"
        , replace1 (r "(matr)ices$") "ix"
        , replace1 (r "(quiz)zes$") ""
        , replace1 (r "(database)s$") ""
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
