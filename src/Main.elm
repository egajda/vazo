module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    }


type Page
    = HomePage
    | FundsPage
    | UsersPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url HomePage
    , Nav.pushUrl key (Url.toString url)
    )


updateUrl : Url.Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Url.Parser.parse routeParser url of
        Just (Fund _) ->
            ( { key = model.key, url = url, page = FundsPage }, Cmd.none )

        Just (User _) ->
            ( { key = model.key, url = url, page = UsersPage }, Cmd.none )

        Nothing ->
            case url.path of
                "/funds/" ->
                    ( { key = model.key, url = url, page = FundsPage }, Cmd.none )

                "/users/" ->
                    ( { key = model.key, url = url, page = UsersPage }, Cmd.none )

                _ ->
                    ( { key = model.key, url = url, page = HomePage }, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            updateUrl url model



-- ROUTER


type Route
    = Fund String
    | User String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Fund (s "/fund/" </> string)
        , map User (s "/user/" </> string)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        HomePage ->
            { title = "Pennydrop"
            , body =
                [ node "link" [ rel "stylesheet", href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.css" ] []
                , viewHeader FundsPage
                , text (Url.toString model.url)
                ]
            }

        FundsPage ->
            { title = "Funds"
            , body =
                [ viewHeader FundsPage
                , text model.url.path
                ]
            }

        UsersPage ->
            { title = "Users"
            , body =
                [ viewHeader UsersPage
                , text model.url.path
                ]
            }


viewHeader : Page -> Html msg
viewHeader page =
    let
        logo =
            a [ class "navbar-item", href "/" ] [ img [ src "img/vazo.png" ] [] ]

        stuff =
            case page of
                FundsPage ->
                    [ text "Funds" ]

                UsersPage ->
                    [ text "Users" ]

                HomePage ->
                    [ text "HomePage" ]

        links =
            ul []
                [ navLink FundsPage { url = "/funds", caption = "Funds" }
                , navLink UsersPage { url = "/users", caption = "Users" }
                ]

        navLink : Page -> { url : String, caption : String } -> Html msg
        navLink targetPage { url, caption } =
            li [ classList [ ( "active", page == targetPage ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [ class "navbar is-warning", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand" ]
            [ logo
            , a [ class "navbar-burger burger", attribute "role" "button", attribute "aria-label" "menu", attribute "aria-expanded" "false", attribute "data-target" "navbarBasicExample" ]
                [ span [ attribute "aria-hidden" "true" ] []
                , span [ attribute "aria-hidden" "true" ] []
                , span [ attribute "aria-hidden" "true" ] []
                ]
            ]
        , div [ class "navbar-menu" ]
            [ a [ class "navbar-item " ] [ text "Home" ]
            , a [ class "navbar-item" ] [ text "Funds" ]
            , a [ class "navbar-item" ] [ text "Users" ]
            ]
        ]
