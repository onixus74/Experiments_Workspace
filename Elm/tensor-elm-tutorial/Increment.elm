import Html exposing (..)
import Html.Events exposing (..)

main : Program Never Model Msg
main = Html.beginnerProgram{
        model = model,
        view = view,
        update = update
    } 


--model
type alias Model =
    Int 

model : Model
model =
    0


--update
type Msg
    = Inc
    | Dec

update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            model + 1
        Dec ->
            model - 1
            

--view
view : Model -> Html Msg
view model =
   div [][
       button [ onClick Dec ][ text "-" ],
       div [] [ text (toString model) ],
       button [ onClick Inc ][ text "+" ]
   ]