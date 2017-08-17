module Main exposing (..)

import Array
import Decoders
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Navigation
import Regex
import Repos exposing (Repo, Repos)
import Secrets
import Task
import UrlParser as Url exposing ((</>), (<?>))


type alias Model =
    { history : List Route
    , repos : Repos
    , currentRoute : Route
    }


type Route
    = Home
    | Algorithms String (Maybe String)
    | NotFound


type Msg
    = GetRepos (Result Http.Error (List Repo))
    | UrlChange Navigation.Location
    | NewUrl String
    | NewAlgoUrl String
    | RepoFiles (Result Http.Error (List Repos.Child))
    | RepoFilesCode (Result Http.Error (List (Http.Response String)))


view : Model -> Html Msg
view model =
    case model.currentRoute of
        NotFound ->
            notFoundView

        Home ->
            homeView model

        Algorithms repoPath currentFile ->
            case Repos.getRepo repoPath model.repos of
                Nothing ->
                    notFoundView

                Just repo ->
                    let
                        codeSnippets =
                            repo
                                |> Repos.codeByChild

                        fileToRender =
                            currentFile
                                |> Maybe.map String.toLower

                        fileExtension =
                            currentFile
                                |> Maybe.map (Regex.split Regex.All (Regex.regex "\\."))
                                |> Maybe.andThen (List.head << List.reverse)
                                |> Maybe.withDefault ""

                        buttons =
                            codeSnippets
                                |> List.map Tuple.first
                                |> List.map
                                    (\fileName ->
                                        div
                                            []
                                            [ button
                                                [ onClick (NewUrl (buildUrl [ Repos.name repo ] ((Just << (++) "file=") fileName))) ]
                                                [ text fileName ]
                                            ]
                                    )

                        toRender =
                            codeSnippets
                                |> List.filter ((==) (Maybe.withDefault "readme.md" fileToRender) << String.toLower << Tuple.first)
                                |> List.map Tuple.second
                                |> List.head
                                |> Maybe.map
                                    (\c ->
                                        div
                                            []
                                            [ pre
                                                []
                                                [ code
                                                    [ class ("language-" ++ fileExtension) ]
                                                    [ text c ]
                                                ]
                                            ]
                                    )
                                |> Maybe.withDefault (div [] [])

                        _ =
                            Debug.log "PATH: " currentFile
                    in
                    div
                        []
                        [ h1
                            []
                            [ (text << Repos.name) repo ]
                        , button
                            [ onClick (NewUrl "/") ]
                            [ text "Home" ]
                        , div
                            []
                            buttons
                        , div
                            []
                            [ toRender
                            ]
                        ]


buildUrl : List String -> Maybe String -> String
buildUrl chunks params =
    let
        url =
            chunks
                |> String.join "/"
                |> (++) "/algorithms/"
    in
    params
        |> Maybe.map ((++) "?")
        |> Maybe.map ((++) url)
        |> Maybe.withDefault ""


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
            (List.map (toRoute << Tuple.first) (Repos.all repos))
        ]


toRoute : String -> Html Msg
toRoute repoName =
    let
        url =
            "/algorithms/" ++ repoName
    in
    li [] [ button [ onClick (NewAlgoUrl url) ] [ text repoName ] ]


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
      , repos = Repos.empty
      , currentRoute = parsedRoute
      }
    , getRepos
    )


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map Algorithms (Url.s "algorithms" </> Url.string <?> Url.stringParam "file")
        ]


getRepos : Cmd Msg
getRepos =
    let
        url =
            "https://api.github.com/repos/pwentz/dsa-practice/contents?client_id="
                ++ Secrets.githubClientId
                ++ "&client_secret="
                ++ Secrets.githubClientSecret

        request =
            Http.get url Decoders.repoContentsDecoder
    in
    Http.send GetRepos request


fileExtensions : List String
fileExtensions =
    [ ".clj", ".swift", ".md" ]


getRouteDetails : String -> Model -> Cmd Msg
getRouteDetails repoName model =
    let
        currentRepoSha =
            model.repos
                |> Repos.getRepo repoName
                |> Maybe.map Repos.sha
    in
    case currentRepoSha of
        Nothing ->
            Cmd.none

        Just sha ->
            let
                url =
                    "https://api.github.com/repos/pwentz/dsa-practice/git/trees/"
                        ++ sha
                        ++ "?recursive=1&client_id="
                        ++ Secrets.githubClientId
                        ++ "&client_secret="
                        ++ Secrets.githubClientSecret

                request =
                    Http.get url Decoders.repoFileDecoder
            in
            Http.send RepoFiles request


fetchCode : Model -> Cmd Msg
fetchCode model =
    case model.currentRoute of
        Home ->
            Cmd.none

        NotFound ->
            Cmd.none

        Algorithms repoName _ ->
            let
                toRequest url =
                    Http.request
                        { method = "GET"
                        , headers = [ Http.header "Accept" "application/vnd.github.VERSION.raw" ]
                        , url =
                            url
                                ++ "?client_id="
                                ++ Secrets.githubClientId
                                ++ "&client_secret="
                                ++ Secrets.githubClientSecret

                        -- , headers = [ Http.header "Accept" "application/vnd.github.VERSION.html" ]
                        -- , url = "https://api.github.com/repos/pwentz/dsa-practice/contents/" ++ repoName ++ "/" ++ path
                        , body = Http.emptyBody
                        , expect = Http.expectStringResponse Ok
                        , timeout = Nothing
                        , withCredentials = False
                        }

                filesToFetch =
                    model.repos
                        |> Repos.getRepo repoName
                        |> Maybe.map Repos.children
                        |> Maybe.withDefault []
            in
            filesToFetch
                |> List.map (Http.toTask << toRequest << .url)
                |> Task.sequence
                |> Task.attempt RepoFilesCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewAlgoUrl url ->
            let
                hasVisited =
                    model.repos
                        |> Repos.getRepo url
                        |> Maybe.andThen (List.head << Repos.children)
            in
            case hasVisited of
                Nothing ->
                    ( model
                    , Cmd.batch
                        [ getRouteDetails url model
                        , Navigation.newUrl url
                        ]
                    )

                Just _ ->
                    ( model
                    , Navigation.newUrl url
                    )

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
            , Cmd.none
            )

        GetRepos (Err _) ->
            ( model, Cmd.none )

        GetRepos (Ok repos) ->
            ( { model | repos = Repos.fromList repos }, Cmd.none )

        RepoFiles (Err _) ->
            ( model, Cmd.none )

        RepoFiles (Ok files) ->
            case model.currentRoute of
                Home ->
                    model ! []

                NotFound ->
                    model ! []

                Algorithms repoName _ ->
                    let
                        dropWith file =
                            (file.format /= "blob")
                                || (String.contains "test" << String.toLower) file.path
                                || (String.contains "spec" << String.toLower) file.path
                                || not (List.any (\x -> String.endsWith x file.path) fileExtensions)

                        updatedModel =
                            { model
                                | repos =
                                    model.repos
                                        |> Repos.updateRepo repoName
                                            (Maybe.map <|
                                                Repos.dropChildren dropWith
                                                    << Repos.addChildren files
                                            )
                            }
                    in
                    ( updatedModel
                    , fetchCode updatedModel
                    )

        RepoFilesCode (Err _) ->
            model ! []

        RepoFilesCode (Ok responses) ->
            case model.currentRoute of
                Home ->
                    model ! []

                NotFound ->
                    model ! []

                Algorithms repoName _ ->
                    ( { model
                        | repos =
                            model.repos
                                |> Repos.updateRepo repoName
                                    (Maybe.andThen <|
                                        Result.toMaybe
                                            << Repos.addCodeToChildren (List.map .body responses)
                                    )
                      }
                    , Cmd.none
                    )
