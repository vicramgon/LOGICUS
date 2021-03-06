module Logicus.Pruebas exposing (..)

import Html exposing (Html, text)
import Logicus.SintaxSemanticsLPO exposing (Term(..), FormulaLPO(..))
import Logicus.IO_LPO exposing (extractReadFLPO, fromStringToFLPO, formTree)

main : Html msg
main = text <| formTree <| extractReadFLPO <| fromStringToFLPO 
        <| "FORALL{x} FORALL{y} (O[x;] AND O[y;] AND NOT(x=y) IMPLIES NOT(_f[x;]=_f[y;])) AND FORALL{x} (I[x;] IMPLIES EXISTS{y} (O[y;] AND _f[y;]=x))"


