module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Material
import Material.Scheme as Scheme
import Material.List as Lists
import Material.Layout as Layout
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Color as Color
import Material.Options as Options exposing (css)


--Model


type alias Model =
    { subreddit : SubReddit
    , posts : List Post
    , mdl : Material.Model
    }


type alias SubReddit =
    { name : String
    }


type alias Post =
    { title : String
    , url : String
    , permalink : String
    , id : String
    , comments : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model (SubReddit "Elm") [] Material.model, Cmd.none )



--Update


type Msg
    = OpenReddit (Result Http.Error (List Post))
    | GetReddit
    | UpdateReddit String
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenReddit (Ok json) ->
            ( { model | posts = json }, Cmd.none )

        OpenReddit (Err e) ->
            ( Debug.log (toString e) model, Cmd.none )

        GetReddit ->
            ( model, getInfo model.subreddit.name )

        UpdateReddit string ->
            ( { model | subreddit = (updateSelection string) }, Cmd.none )

        Mdl action_ ->
            Material.update Mdl action_ model


updateSelection : String -> SubReddit
updateSelection string =
    SubReddit string



--View


view : Model -> Html Msg
view model =
    Scheme.topWithScheme Color.Cyan Color.Lime <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader ]
            { header = [ header ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewContent model ]
            }


header : Html Msg
header =
    div []
        [ h5
            [ style
                [ ( "float", "left" )
                , ( "padding-left", "40px" )
                ]
            ]
            [ text "Reddit SPA" ]
        ]


containerStyle : List (Options.Property a b)
containerStyle =
    [ css "margin" "auto"
    , css "padding-left" "10%"
    , css "padding-right" "10%"
    , css "padding-top" "10%"
    , css "padding-bottom" "10%"
    ]


viewContent : Model -> Html Msg
viewContent model =
    Options.div containerStyle
        [ div []
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Options.onInput UpdateReddit
                , Textfield.value model.subreddit.name
                ]
                []
            , Button.render Mdl
                [ 2 ]
                model.mdl
                [ Options.onClick GetReddit
                , Button.raised
                , Button.ripple
                , Button.colored
                , Button.accent
                , css "margin-left" "5px"
                ]
                [ text "Go!" ]
            , h3 [] [ text model.subreddit.name ]
            , h3 [] [ text <| "https://www.reddit.com/r/" ++ model.subreddit.name ]
            , div [ class "wrap-posts" ]
                [ section []
                    [ Lists.ul [] (List.map postView model.posts)
                    ]
                ]
            ]
        ]


postView : Post -> Html Msg
postView post =
    Lists.li []
        [ Lists.content []
            [ a
                [ style
                    [ ( "color", "rgba(0,0,0,0.72)" )
                    , ( "font-weight", "500" )
                    ]
                , href post.url
                , target "_blank"
                ]
                [ text post.title
                ]
            ]
        , Lists.content2 []
            [ a
                [ style
                    [ ( "color", "rgba(0,0,0,0.72)" )
                    , ( "font-weight", "500" )
                    ]
                , href <| "https://www.reddit.com" ++ post.permalink
                , target "_blank"
                ]
                [ text <| "Comments!(" ++ toString post.comments ++ ")" ]
            ]
        ]



--Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--Commands


getInfo : String -> Cmd Msg
getInfo string =
    let
        url =
            "https://www.reddit.com/r/" ++ string ++ "/.json"

        req =
            Http.get url decodeReddit
    in
        Http.send OpenReddit req



--Json


decodeReddit : Json.Decoder (List Post)
decodeReddit =
    Json.at [ "data", "children" ] (Json.list decodePost)


decodePost : Json.Decoder Post
decodePost =
    Json.map5 Post
        (Json.at [ "data", "title" ] Json.string)
        (Json.at [ "data", "url" ] Json.string)
        (Json.at [ "data", "permalink" ] Json.string)
        (Json.at [ "data", "id" ] Json.string)
        (Json.at [ "data", "num_comments" ] Json.int)



--Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
