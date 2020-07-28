module Modules.Pruebas exposing (..)


import Tuple exposing (first)
import Html exposing (Html, text)
import Modules.SintaxSemanticsLPO exposing (Term(..), FormulaLPO(..), formTree, formTree2DOT)
import Modules.LPO_Parser exposing (parserFormula)

main : Html msg
main = text <| formTree2DOT <| formTree <| Maybe.withDefault Insat <| Tuple.first <| parserFormula <| "forall{x}forall{y}(O[x;] & O[y;] & ¬(x=y) -> ¬(_f[x;]=_f[y;])) & forall{x}(I[x;]->exists{y}(O[y;]&_f[y;]=x))"