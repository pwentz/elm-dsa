module DecodersTest exposing (..)

import Decoders exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Json
import Json.Decode.Pipeline as JPipe
import Repos
import Test exposing (..)


suite : Test
suite =
    describe "Decoders"
        [ describe "repoContentsDecoder"
            [ test "it can decode the contents of github repos" <|
                \_ ->
                    let
                        contents =
                            """
                            [
                              { "name": "Fibonacci",
                                "path": "Fibonacci",
                                "sha": "c6f3be5786009ea2",
                                "size": 0,
                                "url": "https://api.github.com/repos/pwentz/dsa-practice/contents/Fibonacci?ref=master",
                                "html_url": "https://github.com/pwentz/dsa-practice/tree/master/Fibonacci",
                                "git_url": "https://api.github.com/repos/pwentz/dsa-practice/git/trees/c6f3be5786009ea2",
                                "download_url": null,
                                "type": "dir",
                                "_links": {
                                  "self": "https://api.github.com/repos/pwentz/dsa-practice/contents/Fibonacci?ref=master",
                                  "git": "https://api.github.com/repos/pwentz/dsa-practice/git/trees/c6f3be5786009ea2",
                                  "html": "https://github.com/pwentz/dsa-practice/tree/master/Fibonacci"
                                }
                              },
                              { "name": "FloydWarshall",
                                "path": "FloydWarshall",
                                "sha": "c16add5071d04e532",
                                "size": 0,
                                "url": "https://api.github.com/repos/pwentz/dsa-practice/contents/FloydWarshall?ref=master",
                                "html_url": "https://github.com/pwentz/dsa-practice/tree/master/FloydWarshall",
                                "git_url": "https://api.github.com/repos/pwentz/dsa-practice/git/trees/c16add5071d04e532",
                                "download_url": null,
                                "type": "dir",
                                "_links": {
                                  "self": "https://api.github.com/repos/pwentz/dsa-practice/contents/FloydWarshall?ref=master",
                                  "git": "https://api.github.com/repos/pwentz/dsa-practice/git/trees/c16add5071d04e532",
                                  "html": "https://github.com/pwentz/dsa-practice/tree/master/FloydWarshall"
                                }
                              }
                            ]
                          """

                        decodedOutput =
                            Json.decodeString repoContentsDecoder contents

                        expected =
                            [ Repos.initRepo
                                "Fibonacci"
                                "c6f3be5786009ea2"
                                "https://api.github.com/repos/pwentz/dsa-practice/contents/Fibonacci?ref=master"
                                "dir"
                            , Repos.initRepo
                                "FloydWarshall"
                                "c16add5071d04e532"
                                "https://api.github.com/repos/pwentz/dsa-practice/contents/FloydWarshall?ref=master"
                                "dir"
                            ]
                    in
                    case decodedOutput of
                        Err msg ->
                            Expect.fail msg

                        Ok res ->
                            Expect.equal res (List.filterMap Result.toMaybe expected)
            ]
        ]
