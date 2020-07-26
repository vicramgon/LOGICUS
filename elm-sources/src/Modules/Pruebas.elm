module Modules.Pruebas exposing (..)


import Tuple exposing (first)
import Html exposing (Html, text)
import Modules.SintaxSemanticsLPO exposing (Term(..), FormulaLPO(..), formTree, formTree2DOT)
import Modules.LPO_Parser exposing (parserFormula)

main : Html msg
main = text <| formTree2DOT <| formTree <| Maybe.withDefault Insat <| Tuple.first <| parserFormula <| "exists{x}((_+(y;y;) = _·(x;'0;)) | Menor('1; y;))"