module Picshare exposing (main, photoDecoder)

import Browser

import Html exposing (..)
import Html.Attributes exposing (class, src, placeholder, type_, value, disabled)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Http

-- HTTP
fetchFeed : Cmd Msg
fetchFeed =
    Http.get
        { url = baseUrl ++ "feed/1"
        , expect = Http.expectJson LoadFeed photoDecoder
        }
-- DECODER
photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "id" int
        |> required "url" string
        |> required "caption" string
        |> required "liked" bool
        |> required "comments" (list string)
        |> hardcoded ""

-- MODEL
baseUrl : String
baseUrl =
    "https://programming-elm.com/"

type alias Id =
    Int

type alias Photo =
    { id : Id
    , url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }

type alias Model =
    Photo

initialModel : Model
initialModel =
    { id = 1
    , url = baseUrl ++ "1.jpg"
    , caption = "Surfing"
    , liked = False
    , comments = [ "Hoang Do is a bobo boi." ]
    , newComment = ""
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel, fetchFeed )

-- UPDATE
type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment
    | LoadFeed (Result Http.Error Photo)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleLike ->
            ( { model | liked = not model.liked }
            , Cmd.none
            )
        UpdateComment comment ->
            ( { model | newComment = comment }
            , Cmd.none
            )
        SaveComment ->
            ( saveNewComment model
            , Cmd.none
            )
        LoadFeed _ ->
            ( model, Cmd.none )


saveNewComment : Model -> Model
saveNewComment model =
    let
        comment =
            String.trim model.newComment
    in
    case comment of
        "" ->
            model
        _ ->
            { model
                | comments = model.comments ++ [ comment ]
                , newComment = ""
            }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto model ]
        ]

viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
    let
        buttonClass =
            if model.liked then
                "fa-heart"
            else
                "fa-heart-o"
    in
    div [ class "detailed-photo" ]
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
            [ div [ class "like-button"]
                [ i
                    [ class "fa fa-2x"
                    , class buttonClass
                    , onClick ToggleLike
                    ]
                    []
                ]
            , h2 [ class "caption" ] [ text model.caption ]
            , viewComments model
            ]
        ]

viewComments : Model -> Html Msg
viewComments model =
    div []
        [ viewCommentList model.comments
        , form [ class "new-comment", onSubmit SaveComment]
            [ input
                [ type_ "text"
                , placeholder "Add a comment..."
                , value model.newComment
                , onInput UpdateComment
                ]
                []
            , button [ disabled (String.isEmpty model.newComment) ]
                [ text "Submit" ]
            ]
        ]

viewComment : String -> Html Msg
viewComment comment =
    li []
        [ strong [] [ text "Comment:" ]
        , text (" " ++ comment)
        ]

viewCommentList : List String -> Html Msg
viewCommentList comments =
    case comments of
        [] ->
            text ""
        _ ->
            div [ class "comments" ]
                [ ul []
                    (List.map viewComment comments)
                ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
