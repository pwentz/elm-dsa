module Repos
    exposing
        ( Child
        , Repo
        , Repos
        , addChildren
        , addCodeToChildren
        , all
        , children
        , codeByChild
        , dropChildren
        , empty
        , fromList
        , getRepo
        , initContent
        , initRepo
        , name
        , sha
        , updateReadme
        , updateRepo
        )

import Dict exposing (Dict)
import Readme exposing (Readme)
import Regex


type Repos
    = Repos (Dict String Repo)


type Repo
    = Repo
        { name : String
        , path : String
        , sha : String
        , url : String
        }
        (List Child)


type alias Child =
    { name : String
    , path : String
    , sha : String
    , url : String
    , format : String
    , code : Maybe String
    }


empty : Repos
empty =
    Repos Dict.empty


all : Repos -> List ( String, Repo )
all (Repos repos) =
    Dict.toList repos


fromList : List Repo -> Repos
fromList repos =
    repos
        |> List.map2 (,) (List.map name repos)
        |> Dict.fromList
        |> Repos


getRepo : String -> Repos -> Maybe Repo
getRepo route (Repos repos) =
    repos
        |> Dict.get (nameFromPath route)


updateRepo : String -> (Maybe Repo -> Maybe Repo) -> Repos -> Repos
updateRepo route f (Repos repos) =
    Repos
        (Dict.update (nameFromPath route) f repos)


updateReadme : Maybe Readme -> Repos -> Repos
updateReadme readme (Repos repos) =
    Repos
        repos


nameFromPath : String -> String
nameFromPath path =
    path
        |> Regex.split Regex.All (Regex.regex "/")
        |> (List.head << List.reverse)
        |> Maybe.withDefault path


initRepo : String -> String -> String -> String -> Result String Repo
initRepo path sha url format =
    if (format == "tree") || (format == "dir") then
        Ok <|
            Repo
                { name = nameFromPath path
                , path = path
                , sha = sha
                , url = url
                }
                []
    else
        Err "Repo must be a directory"


initContent : String -> String -> String -> String -> Child
initContent path sha url format =
    Child
        (nameFromPath path)
        path
        sha
        url
        format
        Nothing


codeByChild : Repo -> List ( String, String )
codeByChild (Repo data children) =
    children
        |> List.filterMap .code
        |> List.map2 (,) (List.map .name children)


addChildren : List Child -> Repo -> Repo
addChildren repoFiles (Repo data children) =
    Repo
        data
        (repoFiles ++ children)


addCodeToChildren : List String -> Repo -> Result String Repo
addCodeToChildren codeSnippets (Repo data children) =
    if List.length children == List.length codeSnippets then
        Ok <|
            Repo
                data
                (List.map2
                    (\file code -> { file | code = Just code })
                    children
                    codeSnippets
                )
    else
        Err <|
            "Given repo has "
                ++ (toString << List.length) children
                ++ " children while the given list has "
                ++ (toString << List.length) codeSnippets
                ++ " elements"


dropChildren : (Child -> Bool) -> Repo -> Repo
dropChildren f (Repo { name, path, sha, url } children) =
    Repo
        { name = name
        , path = path
        , sha = sha
        , url = url
        }
        (List.filter (not << f) children)


children : Repo -> List Child
children (Repo _ xs) =
    xs


name : Repo -> String
name (Repo { name } _) =
    name


sha : Repo -> String
sha (Repo { sha } _) =
    sha
