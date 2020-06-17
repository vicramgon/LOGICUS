module Modules.MCS exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)

import Modules.LP_Parser exposing (..)
import Modules.AuxiliarFunctions exposing (powerset)
import Modules.LPNormalForms exposing (..)
import Modules.SintaxSemanticsLP exposing (..)
import Modules.LPClausalForms exposing (propClause)
import Modules.AuxiliarFunctions exposing (unionLs)

maximalSets : List (List Prop) -> List (List Prop)
maximalSets pss = 
    filter (\ps -> not (List.any (\ps2 -> (ps2 /= ps) && List.all (\p -> List.member p ps2) ps) pss)) pss

con_delta : List Prop -> List (List Prop)
con_delta delta=  List.filter (\x -> isConsistence x) <| powerset <| delta

max_dom_delta : Prop -> List Prop -> List (List Prop)
max_dom_delta a delta = 
    let max_dom_delta_ = maximalSets <| List.filter (\c -> not(isConsecuence c (Neg a)))  <| con_delta delta in
        case max_dom_delta_ of
            [] -> [[a]]
            _ ->  List.map (\s -> s ++ [a]) <| max_dom_delta_



maximalConsitentSets : List Prop -> List (List Prop)
maximalConsitentSets delta = 
    case delta of
        a::deltap -> unionLs (List.filter (\c -> isConsecuence c (Neg a)) <| maximalConsitentSets deltap) (max_dom_delta a deltap)
        [] -> []

d: List Prop 
d = parserSet <| "¬p&q&r; ¬r&(p <-> ¬q); p&r&¬q; (r <-> q); ¬p&¬q&¬r;"

formulas : String
formulas = String.join "\n" <| List.map toStringProp d
result : String
result = String.join "\n" <| List.map toStringSet <| maximalConsitentSets <| d

--main = textarea [value formulas][]
main = textarea [value result][]

 
