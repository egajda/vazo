module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    , active_nav : Bool
    }


type Page
    = HomePage
    | FundsPage
    | UsersPage


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url HomePage False
    , Nav.pushUrl key (Url.toString url)
    )


updateUrl : Url.Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Url.Parser.parse routeParser url of
        Just (Fund _) ->
            ( { key = model.key, url = url, page = FundsPage, active_nav = False }, Cmd.none )

        Just (User _) ->
            ( { key = model.key, url = url, page = UsersPage, active_nav = False }, Cmd.none )

        Nothing ->
            case url.path of
                "/funds" ->
                    ( { key = model.key, url = url, page = FundsPage, active_nav = False }, Cmd.none )

                "/users" ->
                    ( { key = model.key, url = url, page = UsersPage, active_nav = False }, Cmd.none )

                _ ->
                    ( { key = model.key, url = url, page = HomePage, active_nav = False }, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavToggled


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

        NavToggled ->
            case model.active_nav of
                True ->
                    ( { model | active_nav = False }, Cmd.none )

                False ->
                    ( { model | active_nav = True }, Cmd.none )



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
            { title = "Vazo"
            , body =
                [ div [ class "layout-documentation page-columns" ]
                    [ viewHeader HomePage model.active_nav
                    , text (Url.toString model.url)
                    ]
                ]
            }

        FundsPage ->
            { title = "Funds"
            , body =
                [ div [ class "layout-documentation page-columns" ]
                    [ viewHeader FundsPage model.active_nav
                    , viewFunds model
                    ]
                ]
            }

        UsersPage ->
            { title = "Users"
            , body =
                [ div [ class "layout-documentation page-columns" ]
                    [ viewHeader UsersPage model.active_nav
                    , viewUsers model
                    ]
                ]
            }


viewHeader : Page -> Bool -> Html Msg
viewHeader page active_nav =
    let
        logo =
            a [ class "navbar-item", href "/" ]
                [ img
                    [ src "/img/vazo.png"
                    , attribute "width" "72"
                    , attribute "height" "108"
                    ]
                    []
                ]

        navisactive =
            active_nav

        navLink : String -> String -> Html msg
        navLink name url =
            a [ class "navbar-item", href url ] [ text name ]
    in
    div [ class "container" ]
        [ nav
            [ class "navbar has-shadow is-spaced"
            , attribute "role" "navigation"
            , attribute "aria-label" "main navigation"
            ]
            [ div [ class "navbar-brand" ]
                [ logo
                , div
                    [ class "navbar-burger burger"
                    , onClick NavToggled
                    , classList [ ( "is-active", navisactive ) ]
                    , attribute "role" "button"
                    , attribute "aria-label" "menu"
                    , attribute "disabled" ""
                    , attribute "aria-expanded" "false"
                    ]
                    [ span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    ]
                ]
            , div
                [ class "navbar-menu"
                , classList [ ( "is-active", navisactive ) ]
                ]
                [ div [ class "navbar-start" ]
                    [ navLink "Funds" "/funds"
                    , navLink "Users" "/users"
                    ]
                , div [ class "navbar-end" ]
                    [ div [ class "navbar-item" ]
                        [ div [ class "buttons" ]
                            [ a [ class "is-primary button" ] [ text "Sign up" ]
                            , a [ class "is-light button" ] [ text "Log in" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewFunds : Model -> Html Msg
viewFunds model =
    let
        col obj =
            div [ class "column is-4" ] [ obj ]

        row =
            div [ class "columns is-4" ]
                [ col card
                , col card
                , col card
                ]

        uofm =
            "https://static.timesofisrael.com/www/uploads/2018/09/michigan-e1537585558733.jpg"

        greg =
            "https://avatars1.githubusercontent.com/u/23343587?s=400&v=4"

        card =
            div [ class "card" ]
                [ div [ class "card-image" ]
                    [ figure [ class "image is-4by3" ]
                        [ img
                            [ src uofm ]
                            []
                        ]
                    ]
                , div [ class "card-content" ]
                    [ div [ class "media" ]
                        [ div [ class "media-left" ]
                            [ figure [ class "image is-48x48" ] [ img [ src greg ] [] ] ]
                        , div [ class "media-content" ]
                            [ p [ class "title is-4" ] [ text "Greg Meyer" ]
                            , p [ class "subtitle is-6" ] [ text "@gregjm" ]
                            ]
                        ]
                    ]
                , div [ class "content" ] [ text "Greg's college tuition (read beer money)", br [] [], time [] [ text "11:09PM - 1 Jan 2020" ] ]
                ]
    in
    div
        [ class "section" ]
        [ h1 [ class "title" ] [ text "Funds" ]
        , row
        , row
        , row
        , row
        ]


viewUsers : Model -> Html Msg
viewUsers model =
    let
        col obj =
            div [ class "column is-4" ] [ obj ]

        row =
            div [ class "columns is-4" ]
                [ col card
                , col card
                , col card
                ]

        greg =
            "https://avatars1.githubusercontent.com/u/23343587?s=400&v=4"

        nonlink str =
            p [ class "subtitle is-6" ] [ text str ]

        card =
            div [ class "card" ]
                [ a [ href "/" ]
                    [ div [ class "card-image" ]
                        [ figure [ class "image " ]
                            [ img
                                [ src greg ]
                                []
                            ]
                        ]
                    , div [ class "card-content" ]
                        [ div [ class "media" ]
                            [ div [ class "media-content" ]
                                [ p [ class "title is-4" ] [ text "Greg Meyer" ]
                                , p [ class "subtitle is-6" ] [ text "@gregjm" ]
                                ]
                            ]
                        ]
                    , div [ class "card-content" ]
                        [ nonlink "Over 6 Million Dollars donated to the Spanish Inquistion"
                        , p [ class "subtitle" ] [ text "Most recently funds:" ]
                        , ul []
                            [ li [] [ nonlink "Big Pharma" ]
                            , li [] [ nonlink "Big Vision" ]
                            , li [] [ nonlink "Big Death" ]
                            ]
                        , br [] []
                        , time [] [ nonlink "Joined on 1 Jan 2020" ]
                        ]
                    ]
                ]
    in
    div [ class "section" ]
        [ h1 [ class "title" ] [ text "Users" ]
        , row
        , row
        , row
        , row
        ]
