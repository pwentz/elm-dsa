module Main exposing (toCamel, toKebab, toSnake, toTitle)

import Utils


type Cased
    = Camel String
    | Kebab String
    | Snake String
    | Title String
    | None String


initCamel : String -> Cased
initCamel str =
    if Utils.isCamel str then
        Camel str
    else
        None str


initSnake : String -> Cased
initSnake str =
    if Utils.isSnake str then
        Snake str
    else
        None str


initTitle : String -> Cased
initTitle str =
    if Utils.isTitle str then
        Title str
    else
        None str


initKebab : String -> Cased
initKebab str =
    if Utils.isKebab str then
        Kebab str
    else
        None str


thenTry : (String -> Cased) -> Cased -> Cased
thenTry f casedStr =
    case casedStr of
        None str ->
            f str

        cased ->
            cased


toCased : String -> Cased
toCased str =
    initCamel str
        |> thenTry initSnake
        |> thenTry initTitle
        |> thenTry initKebab


toCamel : String -> String
toCamel str =
    case toCased str of
        Title str ->
            str
                |> Utils.mapWords (Utils.onPrefix String.toLower)

        Snake str ->
            Utils.splitOn '_' str
                |> List.map (Utils.onPrefix String.toUpper << String.toLower)
                |> String.concat
                |> Utils.onPrefix String.toLower

        Kebab str ->
            str
                |> Utils.toTitleWithSeparator '-'
                |> Utils.onPrefix String.toLower

        Camel str ->
            str

        None str ->
            str


toSnake : String -> String
toSnake str =
    case toCased str of
        Camel str ->
            Utils.fromTitleWithSeparator '_' str

        Title str ->
            Utils.fromTitleWithSeparator '_' str

        Kebab str ->
            Utils.replaceSeparators ( '-', '_' ) str

        Snake str ->
            str

        None str ->
            str


toKebab : String -> String
toKebab str =
    case toCased str of
        Camel str ->
            Utils.fromTitleWithSeparator '-' str

        Title str ->
            Utils.fromTitleWithSeparator '-' str

        Snake str ->
            Utils.replaceSeparators ( '_', '-' ) str

        Kebab str ->
            str

        None str ->
            str


toTitle : String -> String
toTitle str =
    case toCased str of
        Camel str ->
            str
                |> Utils.mapWords (Utils.onPrefix String.toUpper)

        Kebab str ->
            Utils.toTitleWithSeparator '-' str

        Snake str ->
            Utils.toTitleWithSeparator '_' str

        Title str ->
            str

        None str ->
            str
