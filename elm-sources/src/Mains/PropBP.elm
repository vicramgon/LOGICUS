module Mains.PropBP exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import File exposing (File)
import File.Select as Select
import Task
import Regex
import Maybe exposing (withDefault)
import Modules.LPBig_Parser exposing (expandFormBigProp, expandSetBigProp, toStringBigProp, toStringSetBigProp, toStringBigPropFile, toStringSetBigPropFile, parseBigProp, parseSetBigProp)
import String exposing (replace)

-- Filtered file content
-- Remove comments

pattern = "(---((?!(---))(.|\r|\n|\t))*---)"
regex = Maybe.withDefault Regex.never <| Regex.fromString pattern 
fileFilter s =  replace ";" ";\n" <| replace " " "" <| replace "\n" "" <| replace "\r" "" <| replace "\t" "" <| Regex.replace regex (\_ -> "") s

-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions=subscriptions}



-- MODEL


type alias Model =
  {  fSet: String,
     out: String,
     res: String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ({ fSet="",
     out = "",
     res = "Results of your query will appear here when you push run."
    }, Cmd.none)



-- UPDATE


type Msg
  = ChangefSet String
    | ChangeOut
    | TxtRequested
    | TxtSelected File
    | TxtLoaded String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangefSet newContent ->
      ({ model | fSet = newContent }, Cmd.none)

    ChangeOut ->  ({model | out=(toStringSetBigPropFile <| expandSetBigProp <| model.fSet), res = (toStringSetBigProp <| expandSetBigProp <| model.fSet)}, Cmd.none)
    
    TxtRequested ->
      ( model
      , Select.file ["text/plain charset=utf-8"] TxtSelected
      )

    TxtSelected file ->
      ( model
      , Task.perform TxtLoaded (File.toString file)
      )

    TxtLoaded content ->
      ( { model | fSet = (fileFilter <| content) }
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
                 h2 [] [Html.text "Some propositional models for CSP Problems"],
                 button [Html.Attributes.class "buttExamples", Html.Attributes.id "Nqueens"][Html.text "N Queens"],
                 button [Html.Attributes.class "buttExamples", Html.Attributes.id "Sudoku"][Html.text "Sudoku"],
                 button [Html.Attributes.class "buttExamples", Html.Attributes.id "Gcoloring"][Html.text "Map coloring"],
                 button [Html.Attributes.class "buttExamples", Html.Attributes.id "SendMM"][Html.text "Send more money "]

            ],

            div[][
                button [Html.Attributes.id "run_butt", onClick ChangeOut][Html.text "RUN"]
            ]
        ],

        div [Html.Attributes.class "rightView"][
           div[][
               div[Html.Attributes.class "grid_3_4"][
                    h2 [] [Html.text "Results"]
               ],

               div [Html.Attributes.class "grid_1_4"][
                    button [Html.Attributes.id "toFile"]
                    [Html.text "TO FILE"]
               ],

             textarea [Html.Attributes.id "resTA", Html.Attributes.readonly True, value model.res][]
           ],
            textarea [Html.Attributes.id "outGTA", Html.Attributes.readonly True, value model.out][]   
        ]
    ]

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- TODO: Finalize view adding some buttons for the examples for N queens, sudoku, graph coloration, ....