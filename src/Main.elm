module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
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
    ( Model key url HomePage, Cmd.none )



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
            ( { model | url = url }
            , Cmd.none
            )



-- ROUTER


type Route
    = Fund String
    | User String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Fund (s "fund" </> string)
        , map User (s "user" </> string)
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
                [ viewHeader HomePage
                , text "Home"
                ]
            }

        FundsPage ->
            { title = "Pennydrop"
            , body =
                [ viewHeader FundsPage
                , text "Fund"
                ]
            }

        UsersPage ->
            { title = "Pennydrop"
            , body =
                [ text "Fund"
                ]
            }


viewHeader : Page -> Html msg
viewHeader page =
    let
        logo =
            h1 [] [ text "PennyDrop" ]

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
    nav [] [ logo, links ]
