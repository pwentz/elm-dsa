module Utils
    exposing
        ( fromTitleWithSeparator
        , isCamel
        , isKebab
        , isSnake
        , isTitle
        , mapWords
        , onPrefix
        , replaceSeparators
        , splitOn
        , toTitleWithSeparator
        )

import Char


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


isKebab : String -> Bool
isKebab str =
    let
        words =
            splitOn '-' str
    in
    List.length words > 1 && List.all (\x -> String.toLower x == x) words


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
    hasTitleCasedWords str
        && isFirstCharLower str
        && (not << String.contains "_") str


isTitle : String -> Bool
isTitle str =
    let
        containsSpecialChars str =
            String.contains "_" str
                || String.contains "-" str

        hasTitleCasedWords str =
            List.length (splitBy Char.isUpper str) > 1

        isFirstCharUpper str =
            String.uncons str
                |> Maybe.map (Char.isUpper << Tuple.first)
                |> Maybe.withDefault False
    in
    hasTitleCasedWords str
        && isFirstCharUpper str
        && (not << containsSpecialChars) str


isSnake : String -> Bool
isSnake str =
    let
        isMonocased x =
            (String.toUpper x == x)
                || (String.toLower x == x)
    in
    List.length (splitOn '_' str) > 1 && isMonocased str


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


onPrefix : (String -> String) -> String -> String
onPrefix f str =
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
            >> List.map (onPrefix String.toUpper)
            >> String.concat
        )


replaceSeparators : ( Char, Char ) -> String -> String
replaceSeparators ( oldSep, newSep ) =
    splitOn oldSep
        >> List.map String.toLower
        >> String.join (String.fromChar newSep)
