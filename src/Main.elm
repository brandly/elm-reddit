module Main exposing (..)

import Html exposing (Html, a, button, div, h1, span, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Post =
    { title : String
    , url : String
    , permalink : String
    , id : String
    , comments : Int
    , ups : Int
    }


type alias Model =
    { posts : List Post
    }


initialModel : Model
initialModel =
    { posts = []
    }


type Msg
    = LoadPosts
    | FetchPosts (Result Http.Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadPosts ->
            ( model, getPosts "javascript" )

        FetchPosts (Ok json) ->
            ( { model | posts = json }, Cmd.none )

        FetchPosts (Err e) ->
            ( Debug.log (toString e) model, Cmd.none )


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
    Json.map6 Post
        (Json.at [ "data", "title" ] Json.string)
        (Json.at [ "data", "url" ] Json.string)
        (Json.at [ "data", "permalink" ] Json.string)
        (Json.at [ "data", "id" ] Json.string)
        (Json.at [ "data", "num_comments" ] Json.int)
        (Json.at [ "data", "ups" ] Json.int)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    viewContainer []
        [ h1 []
            [ text "~~ reddit ~~"
            ]
        , button [ onClick LoadPosts ] [ text "load posts" ]
        , div []
            (model.posts |> List.map viewPost)
        ]


viewContainer =
    styled div
        [ ( "width", "100%" )
        , ( "max-width", "700px" )
        , ( "margin", "0 auto" )
        ]


viewPost : Post -> Html Msg
viewPost post =
    let
        wrap =
            styled div [ ( "margin", "12px 0" ), ( "line-height", "1.4" ) ]

        ups =
            styled span [ ( "font-weight", "bold" ), ( "padding", "0 3px" ) ]
    in
    wrap []
        [ ups [] [ text (toString post.ups) ]
        , a [ href post.url ] [ text post.title ]
        ]


styled el css =
    let
        newEl attrs children =
            el
                ([ style css ] ++ attrs)
                children
    in
    newEl
