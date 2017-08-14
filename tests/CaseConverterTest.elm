module CaseConverterTest exposing (..)

import CaseConverter exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "CaseConverter"
        [ describe "toKebab"
            [ test "it can convert title case" <|
                \_ ->
                    let
                        title =
                            "SomeThingVeryAwesome"

                        expected =
                            "some-thing-very-awesome"
                    in
                    Expect.equal expected (toKebab title)
            , test "it can convert title case with acronyms" <|
                \_ ->
                    let
                        title =
                            "SimpleHTTPRequest"

                        expected =
                            "simple-http-request"
                    in
                    Expect.equal expected (toKebab title)
            ]
        ]
