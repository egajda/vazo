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
    | RegisterPage


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

                "/register" ->
                    ( { key = model.key, url = url, page = RegisterPage, active_nav = False }, Cmd.none )

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
                    [ viewHome model
                    ]
                ]
            }

        FundsPage ->
            { title = "Funds"
            , body =
                [ div [ class "layout-documentation page-columns" ]
                    [ viewHeader FundsPage model.active_nav
                    , banner model
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

        RegisterPage ->
            { title = "Register"
            , body =
                [ div []
                    [ viewRegister model
                    ]
                ]
            }


viewHeader : Page -> Bool -> Html Msg
viewHeader page active_nav =
    let
        logo =
            a [ class "navbar-item", href "/" ]
                [ img
                    [ src "/img/Logo2.png"
                    , attribute "style" "max-height: 2.5rem;"
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
            [ class "navbar is-white has-shadow is-spaced "
            , attribute "role" "navigation"
            , attribute "aria-label" "main navigation"
            , attribute "style" "transition: all 0.3s ease 0s;"
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
                        [ div [ class "field is-grouped" ]
                            [ p [ class "control has-icons-left is-hidden-touch" ]
                                [ input [ class "input", attribute "type" "text", attribute "placeholder" "Search...", attribute "style" "width: 200px" ]
                                    [ span [ class "icon is-left" ]
                                        [ i [ class "fas fa-search" ] [] ]
                                    ]
                                ]
                            , p [ class "control" ]
                                [ a [ class "button", href "/login" ] [ span [ class "icon" ] [ i [ class "fas fa-sign-in-alt" ] [] ], span [] [ text "Login" ] ]
                                ]
                            , p [ class "control" ]
                                [ a [ class "is-primary button", href "/register" ] [ span [ class "icon" ] [ i [ class "fas fa-user-plus" ] [] ], span [] [ text "Register" ] ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


banner : Model -> Html Msg
banner model =
    section [ attribute "style" "margin-top: 3px;", class "hero is-info is-small" ]
        [ div [ class "hero-body" ]
            [ div [ class "container has-text-centered" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        [ h1 [ class "is-size-1" ] [ text "Funds" ] ]
                    ]
                , div [ class "columns " ]
                    [ div [ attribute "style" "padding-down: 3rem;", class "column" ]
                        [ p [ class "is-size-4" ]
                            [ text "See who is making money moves near you" ]
                        ]
                    ]
                ]
            ]
        ]


viewRegister : Model -> Html Msg
viewRegister model =
    section [ class "hero is-alt is-fullheight" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ article [ class "card is-rounded", attribute "style" "max-width: 30rem;" ]
                    [ div [ class "card-content" ]
                        [ h1 [ class "title" ]
                            [ img [ alt "Vazo", src "/img/Logo2.png", attribute "width" "114" ]
                                []
                            ]
                        , p [ class "control has-icon" ]
                            [ input [ class "input", placeholder "Email", type_ "email" ]
                                []
                            , i [ class "fa fa-envelope" ]
                                []
                            ]
                        , p [ class "control has-icon" ]
                            [ input [ class "input", placeholder "Password", type_ "password" ]
                                []
                            , i [ class "fa fa-lock" ]
                                []
                            ]
                        , p [ class "control" ]
                            [ label [ class "checkbox" ]
                                [ input [ type_ "checkbox" ]
                                    []
                                , text "Remember me                "
                                ]
                            ]
                        , p [ class "control" ]
                            [ button [ class "button is-primary is-medium is-fullwidth" ]
                                [ i [ class "fa fa-user" ]
                                    []
                                , text "Login                "
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewHome : Model -> Html Msg
viewHome model =
    section [ class "hero is-medium is-bold" ]
        [ div [ class "hero-head" ] [ viewHeader HomePage model.active_nav ]
        , div [ class "hero-body" ]
            [ div [ class "container has-text-centered" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-offset-3 is-6" ]
                        [ h1 [ class "title" ] [ text "Improve your community one penny at a time" ]
                        , h2 [ class "subtitle" ] [ text "To change the world you gotta start local. We supercharge communities by connecting people, incentivizing collaboration and making it easier to help each other" ]
                        ]
                    , div [ class "column " ]
                        [ img [ attribute "style" "position: relative; top: 3rem; ", src "/img/header.svg" ] []
                        ]
                    ]
                ]
            ]
        , section [ attribute "style" "background: #f7f6f6", class "hero is-small is-primary " ]
            [ div [ class "hero-body", attribute "style" "padding-top: 40px;" ]
                [ div [ class "container" ]
                    [ label
                        [ class "has-text-grey title" ]
                        [ text "Designed to be..." ]
                    , br [] []
                    , br [] []
                    , br [] []
                    , br [] []
                    , br [] []
                    , div [ class "columns " ]
                        [ div [ class "column is-4" ]
                            [ div [ class "card is-shady" ]
                                [ div [ class "card-image has-text-centered" ]
                                    [ img [ attribute "style" "padding: 5rem;", class "image ", src "/img/network.svg" ] []
                                    , div [ class "card-content" ]
                                        [ div [ class "content" ]
                                            [ h1 [ class "has-text-black title" ] [ text "Shared" ]
                                            , p [ class "has-text-black" ] [ text "Alone we are mediocre at best. Together we are unstoppable! The web has an insane power to bring us together. Let's channel that into a force of good!" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "column is-4" ]
                            [ div [ class "card is-shady" ]
                                [ div [ class "card-image has-text-centered" ]
                                    [ img [ attribute "style" "padding: 5rem;", class "image ", src "/img/school.svg" ] []
                                    , div [ class "card-content" ]
                                        [ div [ class "content" ]
                                            [ h1 [ class "has-text-black title" ] [ text "Impactful" ]
                                            , p [ class "has-text-black" ]
                                                [ text "Who knows which organizations are making the biggest impact in your communities better than you? Whether its your schools, religious institutions or local youth-sports teams, you have the power to support and nuture whoever you choose." ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "column is-4" ]
                            [ div [ class "card is-shady" ]
                                [ div [ class "card-image has-text-centered" ]
                                    [ img [ attribute "style" "padding: 5rem;", class "image ", src "/img/padlock.svg" ] []
                                    , div [ class "card-content" ]
                                        [ div [ class "content" ]
                                            [ h1 [ class "has-text-black title" ] [ text "Secure" ]
                                            , p [ class "has-text-black" ]
                                                [ text "It wouldn't be the homepage of a tech company without a padlock. People should be rewarded for giving, not repaid with their sensitive information stored as plaintext. Industry best practices and a genuine concern for our users drives every decision we make, so no we don't use an algorithm my grandma can hack." ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


filter : Html msg
filter =
    div []
        [ aside [ class "menu is-hidden-mobile" ]
            [ h6 [ class "is-size-4 title" ] [ text "Filter" ]
            , p [ class "menu-label" ]
                [ text "Category" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a [ class "is-active" ]
                        [ text "All Funds" ]
                    ]
                ]
            , p [ class "menu-label" ]
                [ text "Location" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a []
                        [ text "Everywhere" ]
                    ]
                , li []
                    [ a []
                        [ text "Michigan" ]
                    , ul []
                        [ li []
                            [ a []
                                [ text "Detroit" ]
                            ]
                        , li []
                            [ a []
                                [ text "Ann Arbor" ]
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
    div [ class "section" ]
        [ div
            [ class "bd-main-container container" ]
            [ div [ class "columns" ]
                [ div [ class "column is-3" ] [ filter ]
                , div [ class "column is-9" ]
                    [ row
                    , row
                    , row
                    , row
                    ]
                ]
            ]
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
        [ div [ class "bd-main-container container" ]
            [ h1 [ class "title" ] [ text "Users" ]
            , row
            , row
            , row
            , row
            ]
        ]
