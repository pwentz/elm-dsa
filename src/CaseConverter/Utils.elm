module CaseConverter.Utils
    exposing
        ( fromTitleWithSeparator
        , isCamel
        , isKebab
        , isSnake
        , isTitle
        , mapFirst
        , mapWords
        , replaceAll
        , splitOn
        , toTitleWithSeparator
        )

import Char
import Regex


splitBy : (Char -> Bool) -> String -> List String
splitBy f str =
    let
        splitOnPred char words =
            if (not << f) char then
                words
                    |> List.head
                    |> Maybe.map (\first -> first ++ String.fromChar char)
                    |> Maybe.map
                        (\updatedFirst ->
                            updatedFirst :: List.drop 1 words
                        )
                    |> Maybe.withDefault [ String.fromChar char ]
            else
                String.fromChar char :: words
    in
    str
        |> String.foldl splitOnPred []
        |> List.reverse


isKebab : String -> Bool
isKebab str =
    let
        strictlyAlphanumeric str =
            str
                |> Regex.find Regex.All (Regex.regex "[^a-zA-Z0-9]")
                |> List.filter (not << (==) "-" << .match)
                |> List.isEmpty

        isLowerCased str =
            splitOn '-' str
                |> List.all (\x -> String.toLower x == x)
    in
    String.contains "-" str
        && isLowerCased str
        && strictlyAlphanumeric str


hasTitleCasedWords : String -> Bool
hasTitleCasedWords str =
    List.length (splitBy Char.isUpper str) > 1


isCamel : String -> Bool
isCamel str =
    let
        strictlyAlphanumeric str =
            Regex.contains
                (Regex.regex "([^a-zA-Z0-9]|_)")
                str

        isFirstCharLower str =
            String.uncons str
                |> Maybe.map (Char.isLower << Tuple.first)
                |> Maybe.withDefault False
    in
    hasTitleCasedWords str
        && isFirstCharLower str
        && (not << strictlyAlphanumeric) str


isTitle : String -> Bool
isTitle str =
    let
        strictlyAlphanumeric str =
            Regex.contains
                (Regex.regex "([^a-zA-Z0-9]|_)")
                str

        hasTitleCasedWords str =
            List.length (splitBy Char.isUpper str) > 1

        isFirstCharUpper str =
            String.uncons str
                |> Maybe.map (Char.isUpper << Tuple.first)
                |> Maybe.withDefault False
    in
    hasTitleCasedWords str
        && isFirstCharUpper str
        && (not << strictlyAlphanumeric) str


isSnake : String -> Bool
isSnake str =
    let
        strictlyAlphanumeric str =
            str
                |> Regex.find Regex.All (Regex.regex "[^a-zA-Z0-9]")
                |> List.filter (not << (==) "_" << .match)
                |> List.isEmpty

        isMonocased x =
            (String.toUpper x == x)
                || (String.toLower x == x)
    in
    String.contains "_" str
        && isMonocased str
        && strictlyAlphanumeric str


splitOn : Char -> String -> List String
splitOn char =
    splitBy ((==) char)
        >> List.map (unconsBy ((String.startsWith << String.fromChar) char))


unconsBy : (String -> Bool) -> String -> String
unconsBy f str =
    if f str then
        String.dropLeft 1 str
    else
        str


mapFirst : (String -> String) -> String -> String
mapFirst f str =
    let
        prefix =
            String.left 1 str
    in
    f prefix ++ String.dropLeft 1 str


joinNeighboringTitleChars : List String -> List String
joinNeighboringTitleChars words =
    case List.head words of
        Nothing ->
            []

        Just next ->
            let
                incomingAcronyms =
                    takeWhileUpper words
            in
            if String.isEmpty incomingAcronyms then
                next :: joinNeighboringTitleChars (List.drop 1 words)
            else
                incomingAcronyms :: joinNeighboringTitleChars (List.drop (String.length incomingAcronyms) words)


takeWhileUpper : List String -> String
takeWhileUpper words =
    let
        recurIfUpper word =
            if String.toUpper word == word then
                word ++ takeWhileUpper (List.drop 1 words)
            else
                ""
    in
    words
        |> List.head
        |> Maybe.map recurIfUpper
        |> Maybe.withDefault ""


fromTitleWithSeparator : Char -> String -> String
fromTitleWithSeparator separator =
    mapWords
        (splitBy Char.isUpper
            >> joinNeighboringTitleChars
            >> List.map String.toLower
            >> String.join (String.fromChar separator)
        )


mapWords : (String -> String) -> String -> String
mapWords f =
    String.words
        >> List.map f
        >> String.join " "


toTitleWithSeparator : Char -> String -> String
toTitleWithSeparator separator =
    mapWords
        (splitOn separator
            >> List.map (mapFirst String.toUpper)
            >> String.concat
        )


replaceAll : ( Char, Char ) -> String -> String
replaceAll ( oldSep, newSep ) =
    splitOn oldSep
        >> String.join (String.fromChar newSep)
