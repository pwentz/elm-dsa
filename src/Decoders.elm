module Decoders exposing (..)

import Json.Decode as Json
import Json.Decode.Pipeline as JPipe
import Regex
import Repos exposing (Repo)
import Result


repoContentsDecoder : Json.Decoder (List Repo)
repoContentsDecoder =
    Json.map (List.filterMap Result.toMaybe)
        (Json.list <|
            (JPipe.decode Repos.initRepo
                |> JPipe.required "path" Json.string
                |> JPipe.required "sha" Json.string
                |> JPipe.required "url" Json.string
                |> JPipe.required "type" Json.string
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
