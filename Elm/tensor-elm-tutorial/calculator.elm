module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String exposing (..)


main : Program Never Model Msg
main =
    beginnerProgram { model = init, view = view, update = update }



--model


type alias Calculator =
    { add : Float -> Float -> Float
    , minus : Float -> Float -> Float
    , divide : Float -> Float -> Float
    , times : Float -> Float -> Float
    }


calculator : Calculator
calculator =
    { add = (\x y -> x + y)
    , minus = (\x y -> x - y)
    , divide = (\x y -> x / y)
    , times = (\x y -> x * y)
    }


type alias Model =
    { display : String
    , function : Float -> Float -> Float
    , saveValue : Float
    , append : Bool
    }


init : Model
init =
    { display = ""
    , function = (\x y -> y)
    , saveValue = 0
    , append = False
    }


parseFloat : String -> Float
parseFloat input =
    Result.withDefault 0 (String.toFloat input)


operation : Model -> (Float -> Float -> Float) -> Model
operation model function =
    { model
        | function = function
        , saveValue = (parseFloat model.display)
        , append = False
    }



--update


type Msg
    = None
    | Divide
    | Add
    | Minus
    | Times
    | Equal
    | Decimal
    | Zero
    | Number Int
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            model

        Clear ->
            init

        Number number ->
            updateDisplay model number

        Decimal ->
            decimal model

        Zero ->
            zero model

        Divide ->
            operation model calculator.divide

        Add ->
            operation model calculator.add

        Minus ->
            operation model calculator.minus

        Times ->
            operation model calculator.times

        Equal ->
            equal model


updateDisplay : Model -> Int -> Model
updateDisplay model number =
    if model.append then
        { model | display = model.display ++ toString (number) }
    else
        { model | display = number |> toString }


equal : Model -> Model
equal model =
    if model.append then
        { model
            | display = calculate model
            , saveValue = model.display |> parseFloat
            , append = False
        }
    else
        { model
            | display = calculate model
            , append = False
        }


calculate : Model -> String
calculate model =
    model.function model.saveValue (parseFloat model.display) |> toString


zero : Model -> Model
zero model =
    if String.isEmpty model.display || not model.append then
        { model
            | display = "0"
            , append = False
        }
    else
        { model | display = model.display ++ "0" }


decimal : Model -> Model
decimal model =
    if not (String.isEmpty model.display) || model.append then
        { model
            | display = appendDecimal model.display
        }
    else
        { model | display = model.display ++ "0.", append = True }


appendDecimal : String -> String
appendDecimal string =
    if String.contains "." string then
        string
    else
        string ++ "."



--view


calculatorButton : Msg -> String -> Html Msg
calculatorButton msg buttonText =
    button
        [ class "button"
        , onClick msg
        ]
        [ span [] [ buttonText |> text ]
        ]


calculatorButtonWide : Msg -> String -> Html Msg
calculatorButtonWide msg buttonText =
    button
        [ class "button wide"
        , onClick msg
        ]
        [ span [] [ buttonText |> text ]
        ]


stylesheet : Html a
stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "Rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "calculator-style.css"
            ]

        child =
            []
    in
        node tag attrs child


view : Model -> Html Msg
view model =
    div [ class "calculator" ]
        [ stylesheet
        , div [ class "row" ]
            [ div [ class "col-xs-12" ]
                [ div [ class "display" ]
                    [ div [ class "display-text" ]
                        [ text (model.display)
                        ]
                    , div [ class "buttons" ]
                        [ calculatorButtonWide Clear "Clear"
                        , calculatorButton (Number 7) "7"
                        , calculatorButton (Number 8) "8"
                        , calculatorButton (Number 9) "9"
                        , calculatorButton Divide "/"
                        , calculatorButton (Number 4) "4"
                        , calculatorButton (Number 5) "5"
                        , calculatorButton (Number 6) "6"
                        , calculatorButton Times "x"
                        , calculatorButton (Number 1) "1"
                        , calculatorButton (Number 2) "2"
                        , calculatorButton (Number 3) "3"
                        , calculatorButton Minus "-"
                        , calculatorButton Zero "0"
                        , calculatorButton Decimal "."
                        , calculatorButton Equal "="
                        , calculatorButton Add "+"
                        ]
                    ]
                ]
            ]
        ]
