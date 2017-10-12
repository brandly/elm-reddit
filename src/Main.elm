module Main exposing (..)

import Html exposing (Html, a, button, div, form, h1, input, li, p, pre, span, text, ul)
import Html.Attributes exposing (href, placeholder, rel, style, target)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Json
import Navigation
import Set exposing (Set)
import UrlParser exposing ((</>), Parser, int, map, oneOf, parseHash, s, string, top)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = page
        , subscriptions = subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            Model [ location ] [] "" (parseLocation location) Set.empty
    in
    loadDataForRoute model


loadDataForRoute : Model -> ( Model, Cmd Msg )
loadDataForRoute model =
    case model.route of
        SubredditRoute subreddit ->
            ( { model | subreddit = subreddit }, getPosts subreddit )

        _ ->
            ( model, Cmd.none )


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute top
        , map SubredditRoute (s "r" </> string)
        ]


parseLocation : Navigation.Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


type Route
    = HomeRoute
    | SubredditRoute String
    | NotFoundRoute


type alias Post =
    { title : String
    , url : String
    , permalink : String
    , id : String
    , comments : Int
    , ups : Int
    , selftext : String
    }


type alias Model =
    { history : List Navigation.Location
    , posts : List Post
    , subreddit : String
    , route : Route
    , expandedPosts : Set String
    }


type Msg
    = LoadPosts
    | FetchPosts (Result Http.Error (List Post))
    | UpdateSubreddit String
    | UrlChange Navigation.Location
    | ToggleExpandPost Post


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadPosts ->
            ( model
            , Cmd.batch
                [ getPosts model.subreddit
                , Navigation.newUrl ("#r/" ++ model.subreddit)
                ]
            )

        FetchPosts (Ok json) ->
            ( { model | posts = json }, Cmd.none )

        FetchPosts (Err e) ->
            ( Debug.log (toString e) model, Cmd.none )

        UpdateSubreddit newVal ->
            ( { model | subreddit = newVal }, Cmd.none )

        UrlChange location ->
            loadDataForRoute
                { model
                    | history = location :: model.history
                    , route = parseLocation location
                }

        ToggleExpandPost post ->
            let
                expandedPosts =
                    if Set.member post.id model.expandedPosts then
                        Set.remove post.id model.expandedPosts
                    else
                        Set.insert post.id model.expandedPosts
            in
            ( { model | expandedPosts = expandedPosts }, Cmd.none )


getPosts : String -> Cmd Msg
getPosts subreddit =
    let
        url =
            "https://www.reddit.com/r/" ++ subreddit ++ ".json?count=25"

        request =
            Http.get url decodePostCollection
    in
    Http.send FetchPosts request


decodePostCollection : Json.Decoder (List Post)
decodePostCollection =
    Json.at [ "data", "children" ] (Json.list decodePost)


decodePost : Json.Decoder Post
decodePost =
    Json.map7 Post
        (Json.at [ "data", "title" ] Json.string)
        (Json.at [ "data", "url" ] Json.string)
        (Json.at [ "data", "permalink" ] Json.string)
        (Json.at [ "data", "id" ] Json.string)
        (Json.at [ "data", "num_comments" ] Json.int)
        (Json.at [ "data", "ups" ] Json.int)
        (Json.at [ "data", "selftext" ] Json.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


page : Model -> Html Msg
page model =
    case model.route of
        HomeRoute ->
            viewHome

        SubredditRoute subreddit ->
            viewSubreddit model

        NotFoundRoute ->
            notFoundView


viewHome : Html Msg
viewHome =
    a [ href "#r/javascript" ] [ text "r/javascript" ]


viewSubreddit : Model -> Html Msg
viewSubreddit model =
    let
        postList =
            styled li
                --[ ( "list-style", "none" )
                -- not able to do this??
                [ ( "-webkit-margin-before", "1em" )
                , ( "-webkit-margin-after", "1em" )
                , ( "-webkit-margin-start", "0px" )
                , ( "-webkit-margin-end", "0px" )
                , ( "-webkit-padding-start", "40px" )
                ]

        expanded post =
            Set.member post.id model.expandedPosts
    in
    viewContainer []
        [ h1 []
            [ text ("r/" ++ model.subreddit)
            ]
        , form [ onSubmit LoadPosts ]
            [ input [ placeholder "subreddit", onInput UpdateSubreddit ] []
            ]
        , postList []
            (model.posts |> List.map (\post -> viewPost (expanded post) post))
        ]


viewContainer =
    styled div
        [ ( "width", "100%" )
        , ( "max-width", "700px" )
        , ( "margin", "0 auto" )
        , ( "font-family", "sans-serif" )
        , ( "color", "#444" )
        ]


viewPost : Bool -> Post -> Html Msg
viewPost isExpanded post =
    let
        wrap =
            styled li [ ( "margin", "12px 0" ), ( "line-height", "1.4" ) ]

        ups =
            styled span [ ( "font-weight", "bold" ), ( "padding", "0 3px" ) ]

        hasSelfText =
            String.length post.selftext > 0

        buttonSymbol =
            if isExpanded then
                "-"
            else
                "+"

        null =
            text ""
    in
    wrap []
        [ ups [] [ text (toString post.ups) ]
        , externalLink [ href post.url ] [ text post.title ]
        , if hasSelfText then
            button [ onClick (ToggleExpandPost post) ]
                [ text ("[ " ++ buttonSymbol ++ " ]") ]
          else
            null
        , if hasSelfText && isExpanded then
            p [] [ text post.selftext ]
          else
            null
        ]


notFoundView : Html Msg
notFoundView =
    div []
        [ h1 [] [ text "404" ]
        , a [ href "#" ] [ text "go home" ]
        ]


externalLink : Element msg
externalLink attrs children =
    let
        anchor =
            styled a [ ( "color", "#444" ) ]
    in
    anchor ([ target "_blank", rel "noopener" ] ++ attrs) children


type alias Element msg =
    List (Html.Attribute msg) -> List (Html msg) -> Html msg


styled : Element msg -> List ( String, String ) -> Element msg
styled el css =
    let
        newEl attrs children =
            el
                ([ style css ] ++ attrs)
                children
    in
    newEl
