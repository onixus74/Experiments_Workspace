module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Attr


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



--model


type alias Model =
    { todo : String
    , todos : List String
    }


model : Model
model =
    { todo = ""
    , todos = []
    }



--update


type Msg
    = UpdateTodo String
    | AddTodo
    | RemoveAll
    | RemoveItem String
    | ClearInput


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTodo text ->
            { model | todo = text }

        AddTodo ->
            { model | todos = model.todo :: model.todos }

        RemoveAll ->
            { model | todos = [] }

        RemoveItem text ->
            { model | todos = List.filter (\x -> x /= text) model.todos }

        ClearInput ->
            { model | todo = "" }



--view


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ Attr.attribute "Rel" "stylesheet"
            , Attr.attribute "property" "stylesheet"
            , Attr.attribute "href" "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            ]

        children =
            []
    in
        node tag attrs children


todoItem : String -> Html Msg
todoItem todo =
    li [] [ text todo, button [ onClick (RemoveItem todo) ] [ text "x" ] ]


todoList : List String -> Html Msg
todoList todos =
    let
        child =
            List.map todoItem todos
    in
        ul [] child


view : Model -> Html Msg
view model =
    div [ Attr.class "jumbotron" ]
        [ stylesheet
        , input
            [ Attr.type_ "text"
            , onInput UpdateTodo
            , Attr.value model.todo
            , Attr.class "form-control"
            , onMouseEnter ClearInput
            ]
            []
        , button [ onClick AddTodo, Attr.class "btn btn-primary" ] [ text "Submit" ]
        , button [ onClick RemoveAll, Attr.class "btn btn-danger" ] [ text "Remove All" ]
        , div [] [ todoList model.todos ]
        ]
