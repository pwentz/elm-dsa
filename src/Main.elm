module Main exposing (..)

import CaseConverter.Main as CaseConverter
import Decoders exposing (Repo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Navigation
import Task
import UrlParser as Url exposing ((</>))


type alias Model =
    { history : List Route
    , repos : List Repo
    , currentRoute : Route
    , currentSnippets : List String
    }


type Route
    = Home
    | Algorithms String
    | NotFound


type Msg
    = GetRepos (Result Http.Error (List Repo))
    | UrlChange Navigation.Location
    | NewUrl String
    | RepoFiles (Result Http.Error (List (Http.Response String)))


view : Model -> Html Msg
view ({ repos, history, currentRoute, currentSnippets } as model) =
    case currentRoute of
        NotFound ->
            homeView model

        Home ->
            homeView model

        Algorithms repoPath ->
            let
                currentRepo =
                    repos
                        |> List.filter ((==) repoPath << .name)
            in
            case currentRepo of
                [] ->
                    notFoundView

                repo :: _ ->
                    div
                        []
                        [ h1
                            []
                            [ (text << .name) repo ]
                        , button
                            [ onClick (NewUrl "/") ]
                            [ text "Home" ]
                        , div
                            []
                            [ code
                                []
                                (List.map (\x -> text x) currentSnippets)
                            ]
                        ]


notFoundView : Html Msg
notFoundView =
    div
        []
        [ p
            []
            [ text "Repo not found!" ]
        ]


homeView : Model -> Html Msg
homeView { repos } =
    div
        []
        [ ul
            []
            (List.map (toRoute << .name) repos)
        ]


toRoute : String -> Html Msg
toRoute repoName =
    let
        url =
            "/algorithms/" ++ repoName
    in
    li [] [ button [ onClick (NewUrl url) ] [ text repoName ] ]


main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \_ -> Sub.none
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        parsedRoute =
            location
                |> Url.parsePath route
                |> Maybe.withDefault Home
    in
    ( Model [ parsedRoute ] [] parsedRoute []
    , getRepos
    )


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map Algorithms (Url.s "algorithms" </> Url.string)
        ]


getRepos : Cmd Msg
getRepos =
    let
        url =
            "https://api.github.com/repos/pwentz/dsa-practice/contents/"

        request =
            Http.get url Decoders.repoContentsDecoder
    in
    Http.send GetRepos request


getRouteDetails : Route -> Model -> Cmd Msg
getRouteDetails route model =
    case route of
        Home ->
            Cmd.none

        NotFound ->
            Cmd.none

        Algorithms routeName ->
            let
                url =
                    "https://api.github.com/repos/pwentz/dsa-practice/contents/" ++ routeName ++ "/"

                request =
                    Http.get url Decoders.repoContentsDecoder
            in
            request
                |> Http.toTask
                |> Task.andThen
                    (\repoContents ->
                        let
                            toRequest url =
                                Http.get url Decoders.repoContentsDecoder
                        in
                        repoContents
                            |> List.map (Http.toTask << toRequest << .url)
                            |> Task.sequence
                    )
                |> Task.andThen
                    (\responses ->
                        let
                            toRequest { url } =
                                Http.get url Decoders.repoContentsDecoder

                            hasFiles { name } =
                                String.contains "src" name
                                    || String.contains "Sources" name
                        in
                        responses
                            |> List.concat
                            |> List.filter hasFiles
                            |> List.map (Http.toTask << toRequest)
                            |> Task.sequence
                    )
                |> Task.andThen
                    (\repoData ->
                        let
                            toRequest { url } =
                                Http.request
                                    { method = "GET"
                                    , headers = [ Http.header "Accept" "application/vnd.github.VERSION.raw" ]
                                    , url = url
                                    , body = Http.emptyBody
                                    , expect = Http.expectStringResponse Ok
                                    , timeout = Nothing
                                    , withCredentials = False
                                    }
                        in
                        repoData
                            |> List.concat
                            |> List.map (Http.toTask << toRequest)
                            |> Task.sequence
                    )
                |> Task.attempt RepoFiles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model
            , Navigation.newUrl url
            )

        UrlChange location ->
            let
                nextRoute =
                    location
                        |> Url.parsePath route
                        |> Maybe.withDefault NotFound
            in
            ( { model
                | history = nextRoute :: model.history
                , currentRoute = nextRoute
              }
            , getRouteDetails nextRoute model
            )

        GetRepos (Err _) ->
            ( model, Cmd.none )

        GetRepos (Ok repos) ->
            let
                dsaRepos =
                    repos
                        |> List.filter (not << String.contains "." << .name)
            in
            ( { model | repos = dsaRepos }, Cmd.none )

        RepoFiles (Err _) ->
            ( model, Cmd.none )

        RepoFiles (Ok snippets) ->
            ( { model | currentSnippets = List.map .body snippets }, Cmd.none )
