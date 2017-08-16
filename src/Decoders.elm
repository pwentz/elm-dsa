module Decoders exposing (..)

import Json.Decode as Json
import Json.Decode.Pipeline as JPipe
import Regex


type alias Repo =
    { name : String
    , path : String
    , sha : String
    , size : Int
    , url : String
    , htmlUrl : String
    , gitUrl : String
    , downloadUrl : Maybe String
    , typeOf : String
    }


type alias RepoFile =
    { name : String
    , path : String
    , sha : String
    , url : String
    , format : String
    , code : Maybe String
    }


repoContentsDecoder : Json.Decoder (List Repo)
repoContentsDecoder =
    Json.list
        (JPipe.decode Repo
            |> JPipe.required "name" Json.string
            |> JPipe.required "path" Json.string
            |> JPipe.required "sha" Json.string
            |> JPipe.required "size" Json.int
            |> JPipe.required "url" Json.string
            |> JPipe.required "html_url" Json.string
            |> JPipe.required "git_url" Json.string
            |> JPipe.required "download_url" (Json.nullable Json.string)
            |> JPipe.required "type" Json.string
        )


repoFileDecoder : Json.Decoder (List RepoFile)
repoFileDecoder =
    Json.field "tree" <|
        Json.list <|
            Json.map4
                toRepoFile
                (Json.field "path" Json.string)
                (Json.field "sha" Json.string)
                (Json.field "url" Json.string)
                (Json.field "type" Json.string)


toRepoFile : String -> String -> String -> String -> RepoFile
toRepoFile path sha url format =
    let
        fileName =
            path
                |> Regex.split Regex.All (Regex.regex "/")
                |> (List.head << List.reverse)
                |> Maybe.withDefault path
    in
    RepoFile
        fileName
        path
        sha
        url
        format
        Nothing
