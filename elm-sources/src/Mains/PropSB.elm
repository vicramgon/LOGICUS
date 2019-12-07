module Mains.PropSB exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Maybe
import String exposing (..)
import List exposing (..)
import File exposing (File)
import File.Select as Select
import Task


import Modules.SintaxSemanticsLP exposing (..)
import Modules.LP_Parser exposing(..)
import Modules.AuxiliarFunctions exposing (..)
import Modules.Algorithms exposing(..)
import Modules.SemanticBoards exposing(..)


-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions=subscriptions}



-- MODEL


type alias Model =
  {  fSet: String,
     formula: String,
     property: String,
     out: String,
     res: String

  }


init : () -> (Model, Cmd Msg)
init _ =
  ({ fSet="",
     formula="",
     property="",
     out = "",
     res = "Results of your query will appear here when you push run."
     
     }, Cmd.none)



-- UPDATE


type Msg
  = ChangefSet String
    | Changeformula String
    | Changeproperty String
    | Changeout
    | TxtRequested
    | TxtSelected File
    | TxtLoaded String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangefSet newContent ->
      ({ model | fSet = newContent }, Cmd.none)

    Changeformula newContent ->
      ({model | formula = newContent}, Cmd.none)

    Changeproperty newContent ->
      ({model | property = newContent}, Cmd.none)

    Changeout -> let formulaSet =  parserSet <| model.fSet in
                 let formula = parserFormula <| model.formula in
                    if model.property == "U_CONS" then 
                        let resultStr = "Formulas Set: " ++ toStringSet(formulaSet) ++ "\n\n" ++ "Query: Is U consistent? " ++ (boolToString <| not <| List.isEmpty <| modelsTab (formulaSet)) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board" in
                            ({model | out = boardToDOTString (semanticBoardAlg (formulaSet)), res=resultStr}, Cmd.none)
                    else if model.property == "U_INCONS" then 
                        let resultStr = "Formulas Set: " ++ toStringSet(formulaSet) ++ "\n\n" ++ "Query: Is U inconsistent? " ++ (boolToString <| List.isEmpty <| modelsTab (formulaSet)) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board" in
                            ({model | out = boardToDOTString (semanticBoardAlg (formulaSet)), res=resultStr}, Cmd.none)
                    else if model.property == "U_MOD" then 
                        let resultStr = "Formulas Set: " ++ toStringSet(formulaSet) ++ "\n\n" ++ "Query: Can you give me the models of U? \n\n" ++ "\t models:" ++ (Debug.toString <| List.map (\x -> toStringSet x) <| modelsTab formulaSet) ++ "\n\n" ++ "\t general models:" ++ (Debug.toString <| List.map (\x -> toStringSet x) <| generalModels <| modelsTab formulaSet) ++ "\n\n"  ++ "Process: click on \" SHOW BOARD\" to see the board. Open branches represents models of U" in
                            ({model | out = boardToDOTString (semanticBoardAlg (formulaSet)), res=resultStr}, Cmd.none)
                    else if model.property == "U_CMOD" then 
                        let resultStr = "Formulas Set: " ++ toStringSet(formulaSet) ++ "\n\n" ++ "Query: Can you give me the countermodels of U? \n\n" ++ "\t countermodels:" ++ (Debug.toString <| List.map (\x -> toStringSet x) <| allSetCounterPropModels formulaSet) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board. \n\n\t The counter models are all posible interpretations that are not in opened branches" in
                            ({model | out = boardToDOTString (semanticBoardAlg (formulaSet)), res=resultStr}, Cmd.none)
                    else if model.property == "F_SAT" then 
                        let resultStr = "Formula: " ++ toStringProp(formula) ++ "\n\n" ++ "Query: Is F satisfactible? " ++ (boolToString <| not <| List.isEmpty <| modelsTab ([formula])) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board." in
                            ({model | out = boardToDOTString (semanticBoardAlg [formula]), res=resultStr}, Cmd.none)
                    else if model.property == "F_UNSAT" then 
                        let resultStr = "Formula: " ++ toStringProp(formula) ++ "\n\n" ++ "Query: Is F unsatisfactible? " ++ (boolToString <| List.isEmpty <| modelsTab ([formula])) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board. \n\n\t If is insatifactible, all branches will be closed" in
                            ({model | out = boardToDOTString (semanticBoardAlg [formula]), res=resultStr}, Cmd.none)
                    else if model.property == "F_TAUT" then 
                        let resultStr = "Formula: " ++ toStringProp(formula) ++ "\n\n" ++ "Query: Is F a tautology? " ++ (boolToString <| List.isEmpty <| modelsTab ([Neg formula])) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board. \n\n\t We have to check if ¬F board is closed" in
                            ({model | out = boardToDOTString (semanticBoardAlg [Neg formula]), res=resultStr}, Cmd.none)
                    else if model.property == "F_MOD" then 
                        let resultStr = "Formula: " ++ toStringProp(formula) ++ "\n\n" ++ "Can you give me the models of F? \n\n" ++ "\t models:" ++ (Debug.toString <| List.map (\x -> toStringSet x)  <| modelsTab [formula]) ++ "\n\n" ++ "\t models:" ++ (Debug.toString <| List.map (\x -> toStringSet x)  <| generalModels <| modelsTab [formula]) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board. \n\n\t We have to check if ¬F board is closed" in
                            ({model | out = boardToDOTString (semanticBoardAlg [formula]), res=resultStr}, Cmd.none)
                    else if model.property == "F_CMOD" then 
                        let resultStr = "Formula: " ++ toStringProp(formula) ++ "\n\n" ++ "Can you give me the countermodels of F? \n\n" ++ "\t countermodels:" ++ (Debug.toString <| List.map (\x -> toStringSet x)  <| modelsTab [Neg formula]) ++ "\n\n" ++ "\t general countermodels:" ++ (Debug.toString <| List.map (\x -> toStringSet x) <| generalModels <| modelsTab [Neg formula]) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board. \n\n\t Countermodels of F are the models of ¬F" in
                            ({model | out = boardToDOTString (semanticBoardAlg [Neg formula]), res=resultStr}, Cmd.none)   
                    else if model.property == "F_CONS" then 
                        let resultStr = "Formulas Set: " ++ toStringSet(formulaSet) ++ "\n\n" ++ "Formula: " ++ toStringProp(formula) ++ "\n\n" ++ "Query: Is F consecuence of U? " ++ (boolToString <| List.isEmpty <| modelsTab (formulaSet ++ [Neg formula])) ++ "\n\n" ++ "Process: click on \" SHOW BOARD\" to see the board. \n\n\t U ⊨ F only if U ∪ {¬F} is insatisfactible" in
                            ({model | out = boardToDOTString (semanticBoardAlg (formulaSet ++ [Neg formula])), res=resultStr}, Cmd.none)
                    else
                        (model, Cmd.none)
    TxtRequested ->
      ( model
      , Select.file ["text/plain charset=utf-8"] TxtSelected
      )

    TxtSelected file ->
      ( model
      , Task.perform TxtLoaded (File.toString file)
      )

    TxtLoaded content ->
      ( { model | fSet = content }
      , Cmd.none
      )
      

-- VIEW

view : Model -> Html Msg
view model = 
    Html.main_ [] [
        div[Html.Attributes.class "leftView"][
            div[][
                h2 [] [Html.text "Set of Formulas (U)"],
                textarea [Html.Attributes.id "FormSet", placeholder "Introduce the formulas of the set. Don\'t forget to put a semicolon \';\' after each one.\n\nEx: p->q; q&(q|(p<->r));", onInput ChangefSet, value model.fSet][],
                div[Html.Attributes.id "up_file"][
                    button [Html.Attributes.id "template", Html.Attributes.class "buttFSet"][Html.text "Template File"],
                    button [Html.Attributes.class "buttFSet", onClick TxtRequested][Html.text "Choose File"]
                ]
            ],

            div[][
                h2 [] [Html.text "Single Formula (F)"],
                input [Html.Attributes.id "Formula",  Html.Attributes.type_ "text", placeholder "Introduce the formula. Ex: (p->q)<->(q&(q|p))",  onInput Changeformula][]
            ],

            div[][
                 h2 [] [Html.text "Check Property"],
                select [Html.Attributes.id "Query", onInput Changeproperty][
                      option [value ""] [Html.text "WHAT DO YOU WANT TO CHECK?"]
                    , option [value "U_CONS"] [Html.text ("U CONSISTENCY")]
                    , option [value "U_INCONS"] [Html.text ("U INCONSISTENCY")]
                    , option [value "U_MOD"] [Html.text ("U MODELS")]
                    , option [value "U_CMOD"] [Html.text ("U COUNTERMODELS")]
                    , option [value "F_SAT"] [Html.text ("F SATISFACTIBILITY")]
                    , option [value "F_UNSAT"] [Html.text ("F UNSATISFACTIBILITY")]
                    , option [value "F_TAUT"] [Html.text ("F TAUTOLOGY")]
                    , option [value "F_MOD"] [Html.text ("F MODELS")]
                    , option [value "F_CMOD"] [Html.text ("F COUNTERMODELS")]
                    , option [value "F_CONS"] [Html.text ("F IS LOGIC CONSECUENCE OF U")]
                ]
            ],

            div[][
                button [Html.Attributes.id "run_butt", onClick Changeout][Html.text "RUN"]
            ]
        ],

        div [Html.Attributes.class "rightView"][
           div[][
               div[Html.Attributes.class "grid_3_4"][
                    h2 [] [Html.text "Results"]
               ],

               div [Html.Attributes.class "grid_1_4"][
                    button [Html.Attributes.id "sGraph"]
                    [Html.text "SHOW GRAPH"]
               ],

             textarea [Html.Attributes.id "resTA", Html.Attributes.readonly True, value model.res][]
           ],
            textarea [Html.Attributes.id "outGTA", Html.Attributes.readonly True, value model.out][]   
        ],

        div[Html.Attributes.id "popUp"][
            button [Html.Attributes.id "closepopUp"] [Html.text "✘"],
            div[Html.Attributes.id "graph"][]
        ]
        
    ]

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none