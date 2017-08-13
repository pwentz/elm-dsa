module Decoders exposing (Repo, repoContentsDecoder)

import Json.Decode as Json
import Json.Decode.Pipeline as JPipe


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
