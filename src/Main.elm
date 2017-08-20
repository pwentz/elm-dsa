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
import Readme exposing (Readme)
import Regex
import Repos exposing (Repo, Repos)
import Secrets
import Task
import UrlParser as Url exposing ((</>), (<?>))


-- TODO:
-- GET README GIT URL FROM REQUEST IN GET_REPOS AND FETCH CONTENTS


type alias Model =
    { history : List Route
    , repos : ( Maybe Readme, Repos )
    , currentRoute : Route
    }


type Route
    = Home
    | Algorithms String (Maybe String)
    | NotFound


type Msg
    = GetRepos (Result Http.Error ( Maybe Readme, List Repo ))
    | UrlChange Navigation.Location
    | NewUrl String
    | NewAlgoUrl String
    | RepoFiles (Result Http.Error (List Repos.Child))
    | RepoFilesCode (Result Http.Error (List String))


algoRouteWithDefault : (String -> a) -> a -> Route -> a
algoRouteWithDefault f defaultVal route =
    case route of
        Algorithms repoName _ ->
            f repoName

        _ ->
            defaultVal


view : Model -> Html Msg
view model =
    case model.currentRoute of
        NotFound ->
            notFoundView

        Home ->
            homeView model

        Algorithms repoPath currentFile ->
            case Repos.getRepo repoPath ((Tuple.second << .repos) model) of
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
                                                [ onClick (NewUrl (buildAlgoUrl [ Repos.name repo ] ((Just << (++) "file=") fileName))) ]
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


buildAlgoUrl : List String -> Maybe String -> String
buildAlgoUrl chunks params =
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
    let
        contents =
            Tuple.first repos
                |> Maybe.andThen Readme.code
                |> Maybe.map
                    (\md ->
                        div
                            []
                            [ pre
                                [ class "language-markdown" ]
                                [ code
                                    []
                                    [ text md ]
                                ]
                            ]
                    )
                |> Maybe.withDefault (div [] [])
    in
    div
        []
        [ ul
            []
            (List.map (toRoute << Tuple.first) ((Repos.all << Tuple.second) repos))
        , div
            []
            [ contents
            ]
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
      , repos = ( Nothing, Repos.empty )
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
    "https://api.github.com/repos/pwentz/dsa-practice/contents"
        |> flip Http.get Decoders.repoContentsDecoder
        |> Http.toTask
        |> Task.andThen
            (\(( readme, repos ) as payload) ->
                readme
                    |> Maybe.map
                        (\rdme ->
                            rdme
                                |> (codeRequest << Readme.url)
                                |> Http.toTask
                                |> Task.map
                                    (flip (,) repos << Just << flip Readme.updateCode rdme)
                        )
                    |> Maybe.withDefault (Task.succeed payload)
            )
        |> Task.attempt GetRepos


fileExtensions : List String
fileExtensions =
    [ ".clj", ".swift", ".md" ]


getRouteDetails : String -> Model -> Cmd Msg
getRouteDetails repoName model =
    model
        |> (Tuple.second << .repos)
        |> Repos.getRepo repoName
        |> Maybe.map Repos.sha
        |> Maybe.map
            (\sha ->
                (sha ++ "?recursive=1")
                    |> (++) "https://api.github.com/repos/pwentz/dsa-practice/git/trees/"
                    |> flip Http.get Decoders.repoFileDecoder
                    |> Http.send RepoFiles
            )
        |> Maybe.withDefault Cmd.none


codeRequest : String -> Http.Request String
codeRequest url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.github.VERSION.raw" ]
        , url =
            url

        -- ++ "?client_id="
        -- ++ Secrets.githubClientId
        -- ++ "&client_secret="
        -- ++ Secrets.githubClientSecret
        -- , headers = [ Http.header "Accept" "application/vnd.github.VERSION.html" ]
        -- , url = "https://api.github.com/repos/pwentz/dsa-practice/contents/" ++ repoName ++ "/" ++ path
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (Ok << .body)
        , timeout = Nothing
        , withCredentials = False
        }


fetchCode : Model -> Cmd Msg
fetchCode model =
    let
        fetch repoName =
            model
                |> (Tuple.second << .repos)
                |> Repos.getRepo repoName
                |> Maybe.map Repos.children
                |> Maybe.withDefault []
                |> List.map (Http.toTask << codeRequest << .url)
                |> Task.sequence
                |> Task.attempt RepoFilesCode
    in
    model
        |> .currentRoute
        |> algoRouteWithDefault fetch Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewAlgoUrl url ->
            model
                |> (Tuple.second << .repos)
                |> Repos.getRepo url
                |> Maybe.andThen (List.head << Repos.children)
                |> Maybe.map (\_ -> Navigation.newUrl url)
                |> Maybe.withDefault
                    (Cmd.batch
                        [ getRouteDetails url model
                        , Navigation.newUrl url
                        ]
                    )
                |> (,) model

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

        GetRepos (Ok ( readme, repos )) ->
            ( { model | repos = ( readme, Repos.fromList repos ) }, Cmd.none )

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
                                    model
                                        |> (Tuple.second << .repos)
                                        |> Repos.updateRepo repoName
                                            (Maybe.map <|
                                                Repos.dropChildren dropWith
                                                    << Repos.addChildren files
                                            )
                                        |> (,) ((Tuple.first << .repos) model)
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
                            model
                                |> (Tuple.second << .repos)
                                |> Repos.updateRepo repoName
                                    (Maybe.andThen <|
                                        Result.toMaybe
                                            << Repos.addCodeToChildren responses
                                    )
                                |> (,) (Tuple.first model.repos)
                      }
                    , Cmd.none
                    )
