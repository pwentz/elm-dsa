module Readme
    exposing
        ( Readme
        , code
        , init
        , updateCode
        , url
        )


type alias Payload =
    { code : Maybe String
    , url : String
    }


type Readme
    = Readme Payload


init : String -> Maybe Readme
init url =
    if (String.contains "readme" << String.toLower) url then
        Just <|
            Readme
                { code = Nothing
                , url = url
                }
    else
        Nothing


updateCode : String -> Readme -> Readme
updateCode newCode (Readme payload) =
    Readme
        { payload | code = Just newCode }


code : Readme -> Maybe String
code (Readme { code }) =
    code


url : Readme -> String
url (Readme { url }) =
    url
