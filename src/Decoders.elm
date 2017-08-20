module Decoders exposing (..)

import Json.Decode as Json
import Readme exposing (Readme)
import Regex
import Repos exposing (Repo)
import Result


type alias JsonReturn =
    { path : String
    , sha : String
    , url : String
    , format : String
    }


(&) : String -> Json.Decoder a -> Json.Decoder (a -> b) -> Json.Decoder b
(&) key valueDecoder decoder =
    Json.map2 (|>) (Json.field key valueDecoder) decoder


repoContentsDecoder : Json.Decoder ( Maybe Readme, List Repo )
repoContentsDecoder =
    Json.map decodeRepos
        (Json.list <|
            (Json.succeed JsonReturn
                |> (&) "path" Json.string
                |> (&) "sha" Json.string
                |> (&) "url" Json.string
                |> (&) "type" Json.string
            )
        )


repoFileDecoder : Json.Decoder (List Repos.Child)
repoFileDecoder =
    Json.field "tree" <|
        Json.list <|
            Json.map4
                Repos.initContent
                (Json.field "path" Json.string)
                (Json.field "sha" Json.string)
                (Json.field "url" Json.string)
                (Json.field "type" Json.string)


decodeRepos : List JsonReturn -> ( Maybe Readme, List Repo )
decodeRepos repositoryData =
    let
        createRepo { path, sha, url, format } =
            Repos.initRepo path sha url format

        repos =
            repositoryData
                |> List.filterMap (Result.toMaybe << createRepo)

        readMe =
            repositoryData
                |> List.filterMap (Readme.init << .url)
                |> List.head
    in
    ( readMe, repos )
