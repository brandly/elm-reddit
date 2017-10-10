module Main exposing (..)

import Html exposing (Html, button, div, h1, pre, text)
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
    }


type alias Model =
    { name : String
    , posts : List Post
    }


initialModel : Model
initialModel =
    { name = "matt"
    , posts = []
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
    Json.map5 Post
        (Json.at [ "data", "title" ] Json.string)
        (Json.at [ "data", "url" ] Json.string)
        (Json.at [ "data", "permalink" ] Json.string)
        (Json.at [ "data", "id" ] Json.string)
        (Json.at [ "data", "num_comments" ] Json.int)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text ("hello " ++ model.name)
            ]
        , pre [] [ text (toString model) ]
        , button [ onClick LoadPosts ] [ text "load posts" ]
        ]
