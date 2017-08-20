module ReposTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Repos exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Repo"
        [ describe "addChildren"
            [ test "it takes a list of RepoFiles and a Repo and adds files to Repo" <|
                \_ ->
                    let
                        repoFiles =
                            [ initContent
                                "path/to/SomeFile.rb"
                                "abc123"
                                "api.github.com"
                                "blob"
                            , initContent
                                "path/to/AnotherFile"
                                "dbc123"
                                "api.github.com"
                                "blob"
                            ]

                        repo =
                            initRepo
                                "path/to/SomeAlgorithm"
                                "abc456"
                                "api.github.com/abc456"
                                "dir"

                        res =
                            repo
                                |> Result.map (children << addChildren repoFiles)
                    in
                    case res of
                        Err msg ->
                            Expect.fail msg

                        Ok files ->
                            Expect.equal repoFiles files
            ]
        , describe "codeByChild"
            [ test "it takes a repo and returns a list of tuples of children and their code" <|
                \_ ->
                    let
                        repoFiles =
                            [ initContent
                                "path/to/SomeFile.rb"
                                "abc123"
                                "api.github.com"
                                "blob"
                            , initContent
                                "path/to/AnotherFile"
                                "dbc123"
                                "api.github.com"
                                "blob"
                            ]

                        codes =
                            [ "Hello, World!"
                            , "DROP TABLE USERS;"
                            ]

                        repo =
                            initRepo
                                "path/to/SomeAlgorithm"
                                "abc456"
                                "api.github.com/abc456"
                                "dir"

                        expected =
                            [ ( "SomeFile.rb", "Hello, World!" )
                            , ( "AnotherFile", "DROP TABLE USERS;" )
                            ]

                        res =
                            repo
                                |> Result.map (addChildren repoFiles)
                                |> Result.andThen (addCodeToChildren codes)
                                |> Result.map codeByChild
                    in
                    case res of
                        Err msg ->
                            Expect.fail msg

                        Ok code ->
                            Expect.equal expected code
            ]
        , describe "addCodeToChildren"
            [ test "it takes a repo and some codes and updates children" <|
                \_ ->
                    let
                        repoFiles =
                            [ initContent
                                "path/to/SomeFile.rb"
                                "abc123"
                                "api.github.com"
                                "blob"
                            , initContent
                                "path/to/AnotherFile"
                                "dbc123"
                                "api.github.com"
                                "blob"
                            ]

                        repo =
                            initRepo
                                "path/to/SomeAlgorithm"
                                "abc456"
                                "api.github.com/abc456"
                                "dir"

                        codes =
                            [ "Hello, world!"
                            , "DROP TABLE USERS"
                            ]

                        result =
                            repo
                                |> Result.map (addChildren repoFiles)
                                |> Result.andThen (addCodeToChildren codes)
                                |> Result.map (List.filterMap .code << children)
                    in
                    case result of
                        Err msg ->
                            Expect.fail msg

                        Ok codeRes ->
                            Expect.equal codes codeRes
            ]
        , describe "dropChildren"
            [ test "it takes a (Child -> Bool) fn and returns a repo with children remove where f is false" <|
                \_ ->
                    let
                        toRemove =
                            initContent
                                "path/to/SomeFile.rb"
                                "abc123"
                                "api.github.com"
                                "blob"

                        toKeep =
                            initContent
                                "path/to/AnotherFile"
                                "dbc123"
                                "api.github.com"
                                "blob"

                        repo =
                            initRepo
                                "path/to/SomeAlgorithm"
                                "abc456"
                                "api.github.com/abc456"
                                "dir"

                        result =
                            repo
                                |> Result.map (addChildren [ toRemove, toKeep ])
                                |> Result.map (dropChildren (String.startsWith "abc" << .sha))
                                |> Result.map children
                    in
                    case result of
                        Err msg ->
                            Expect.fail msg

                        Ok files ->
                            Expect.equal [ toKeep ] files
            ]
        , describe "getRepo"
            [ test "it returns a repo when given the pathName" <|
                \_ ->
                    let
                        expectedRepo =
                            initRepo
                                "path/to/SomeAlgorithm"
                                "abc456"
                                "api.github.com/abc456"
                                "dir"

                        repos =
                            [ expectedRepo
                            , initRepo
                                "path/to/AnotherAlgo"
                                "123xyz"
                                "api.github.com/123xyz"
                                "dir"
                            ]

                        res =
                            repos
                                |> List.filterMap Result.toMaybe
                                |> fromList
                                |> getRepo "path/to/SomeAlgorithm"
                    in
                    case expectedRepo of
                        Err msg ->
                            Expect.fail msg

                        Ok expected ->
                            res
                                |> Maybe.map (Expect.equal expected)
                                |> Maybe.withDefault (Expect.fail "Bad input")
            ]
        , describe "updateRepo"
            [ test "it takes a path and a function to act on the repo and returns Repos" <|
                \_ ->
                    let
                        toRemove =
                            initContent
                                "path/to/SomeFile.rb"
                                "abc123"
                                "api.github.com"
                                "blob"

                        toKeep =
                            initContent
                                "path/to/AnotherFile"
                                "dbc123"
                                "api.github.com"
                                "blob"

                        repo =
                            initRepo
                                "path/to/SomeAlgorithm"
                                "abc456"
                                "api.github.com/abc456"
                                "dir"

                        result =
                            repo
                                |> Result.toMaybe
                                |> Maybe.map (addChildren [ toRemove, toKeep ])
                                |> Maybe.map (\x -> fromList [ x ])
                                |> Maybe.map (updateRepo "path/to/SomeAlgorithm" (Maybe.map (dropChildren (String.startsWith "abc" << .sha))))
                                |> Maybe.andThen (getRepo "path/to/SomeAlgorithm")
                                |> Maybe.map children
                    in
                    case result of
                        Nothing ->
                            Expect.fail "Bad input"

                        Just files ->
                            Expect.equal [ toKeep ] files
            ]
        ]
