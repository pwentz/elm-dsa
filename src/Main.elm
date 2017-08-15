module Main exposing (..)

import CaseConverter.Main as CaseConverter
import Decoders exposing (Repo, RepoFile)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Navigation
import Regex
import Task
import UrlParser as Url exposing ((</>))


type alias Model =
    { history : List Route
    , repos : List Repo
    , currentRoute : Route
    , currentSnippets : List String
    , repoFiles : Dict String (List RepoFile)
    }


type Route
    = Home
    | Algorithms String
    | NotFound


type Msg
    = GetRepos (Result Http.Error (List Repo))
    | UrlChange Navigation.Location
    | NewUrl String
    | RepoFiles (Result Http.Error (List RepoFile))
    | RepoFilesCode (Result Http.Error (List (Http.Response String)))


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
    ( { history = [ parsedRoute ]
      , repos = []
      , currentRoute = parsedRoute
      , currentSnippets = []
      , repoFiles = Dict.empty
      }
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


fileExtensions : List String
fileExtensions =
    [ ".clj", ".swift", ".md" ]


getRouteDetails : Model -> Cmd Msg
getRouteDetails model =
    case model.currentRoute of
        Home ->
            Cmd.none

        NotFound ->
            Cmd.none

        Algorithms routeName ->
            let
                currentRepoSha =
                    model.repos
                        |> List.filter ((==) routeName << .name)
                        |> List.head
                        |> Maybe.map .sha
            in
            case currentRepoSha of
                Nothing ->
                    Cmd.none

                Just sha ->
                    let
                        url =
                            "https://api.github.com/repos/pwentz/dsa-practice/git/trees/" ++ sha ++ "?recursive=1"

                        request =
                            Http.get url Decoders.repoFileDecoder

                        filterFiles files =
                            files
                                |> List.filter
                                    (\file ->
                                        (file.format == "blob")
                                            && (not << String.contains "test" << String.toLower) file.path
                                            && (not << String.contains "spec" << String.toLower) file.path
                                            && List.any (\x -> String.endsWith x file.path) fileExtensions
                                    )
                    in
                    request
                        |> Http.toTask
                        |> Task.map filterFiles
                        |> Task.attempt RepoFiles


fetchCode : Model -> Cmd Msg
fetchCode model =
    case model.currentRoute of
        Home ->
            Cmd.none

        NotFound ->
            Cmd.none

        Algorithms repoName ->
            let
                toRequest path =
                    Http.request
                        { method = "GET"
                        , headers = [ Http.header "Accept" "application/vnd.github.VERSION.raw" ]
                        , url = "https://api.github.com/repos/pwentz/dsa-practice/contents/" ++ path
                        , body = Http.emptyBody
                        , expect = Http.expectStringResponse Ok
                        , timeout = Nothing
                        , withCredentials = False
                        }

                filesToFetch =
                    model.repoFiles
                        |> Dict.get repoName
                        |> Maybe.withDefault []
            in
            filesToFetch
                |> List.map (Http.toTask << toRequest << .path)
                |> Task.sequence
                |> Task.attempt RepoFilesCode


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
            , getRouteDetails model
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

        RepoFiles (Ok files) ->
            case files of
                [] ->
                    model ! []

                (file :: _) as files ->
                    let
                        getParent files =
                            file.path
                                |> Regex.find Regex.All (Regex.regex "/")
                                |> List.head
                                |> Maybe.map ((\idx -> String.left idx file.path) << .index)
                                |> Debug.log "update with repo file: "
                                |> Maybe.withDefault "Unknown"
                    in
                    ( { model
                        | repoFiles =
                            Dict.insert
                                (getParent files)
                                files
                                model.repoFiles
                      }
                    , fetchCode model
                    )

        RepoFilesCode (Err _) ->
            model ! []

        RepoFilesCode (Ok codeFromFiles) ->
            case model.currentRoute of
                Home ->
                    model ! []

                NotFound ->
                    model ! []

                Algorithms repoName ->
                    let
                        filesToUpdate =
                            model.repoFiles
                                |> Dict.get repoName
                                |> Maybe.withDefault []

                        updateFile code file =
                            { file | code = Just code.body }
                    in
                    ( { model
                        | repoFiles =
                            Dict.update
                                repoName
                                (Maybe.map (List.map2 updateFile codeFromFiles))
                                model.repoFiles
                      }
                    , Cmd.none
                    )
