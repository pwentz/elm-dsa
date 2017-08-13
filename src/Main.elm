module Main exposing (..)

import Decoders exposing (Repo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Navigation
import UrlParser as Url exposing ((</>))


type alias Model =
    { history : List (Maybe Route)
    , repos : List Repo
    , currentRoute : Route
    }


type Route
    = Home
    | Algorithms String
    | NotFound


type Msg
    = GetRepos (Result Http.Error (List Repo))
    | UrlChange Navigation.Location
    | NewUrl String


view : Model -> Html Msg
view ({ repos, history, currentRoute } as model) =
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
            Url.parsePath route location
    in
    ( Model [ parsedRoute ] [] (Maybe.withDefault Home parsedRoute)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model
            , Navigation.newUrl url
            )

        UrlChange location ->
            let
                newLocation =
                    Url.parsePath route location
            in
            ( { model
                | history = newLocation :: model.history
                , currentRoute = Maybe.withDefault NotFound newLocation
              }
            , Cmd.none
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
