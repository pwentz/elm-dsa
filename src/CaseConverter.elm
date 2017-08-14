module CaseConverter exposing (..)

import Char


toKebab : String -> String
toKebab str =
    if isTitle str then
        titleToKebab str
    else if isCamel str then
        titleToKebab str
    else
        str


titleToKebab : String -> String
titleToKebab titleStr =
    titleStr
        |> splitBy Char.isUpper
        |> joinNeighboringTitleChars
        |> List.map String.toLower
        |> String.join "-"


isCamel : String -> Bool
isCamel str =
    let
        hasTitleCasedWords str =
            List.length (splitBy Char.isUpper str) > 1

        isFirstCharLower str =
            String.uncons str
                |> Maybe.map (Char.isLower << Tuple.first)
                |> Maybe.withDefault False
    in
    hasTitleCasedWords str && isFirstCharLower str


isTitle : String -> Bool
isTitle str =
    let
        hasTitleCasedWords str =
            List.length (splitBy Char.isUpper str) > 1

        isFirstCharUpper str =
            String.uncons str
                |> Maybe.map (Char.isUpper << Tuple.first)
                |> Maybe.withDefault False
    in
    hasTitleCasedWords str && isFirstCharUpper str


splitBy : (Char -> Bool) -> String -> List String
splitBy f str =
    let
        splitOnPred char words =
            if (not << f) char then
                words
                    |> List.head
                    |> Maybe.map (\x -> x ++ String.fromChar char)
                    |> Maybe.map (\x -> x :: List.drop 1 words)
                    |> Maybe.withDefault [ String.fromChar char ]
            else
                String.fromChar char :: words
    in
    str
        |> String.foldl splitOnPred []
        |> List.reverse


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
