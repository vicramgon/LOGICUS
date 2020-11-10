module Logicus.UnificationLPO exposing (..)

import Logicus.SintaxSemanticsLPO exposing (..)
import Dict exposing (..)
import List

termMGU : Term -> Term -> Substitution
termMGU t1 t2 =
    case t1 of
        (Var x) ->
            case t2 of
                (Var y) ->  
                    if x == y then
                        Dict.empty
                    else
                        Dict.fromList [(x, t2)]
                    

                (Func g gts) -> 
                    if List.member t1 (varsInTerm t2) then
                        Dict.empty
                    else 
                        Dict.fromList [(x, t2)]

        (Func f fts) -> 
            case t2 of
                (Var y) -> 
                    if List.member t2 (varsInTerm t1) then
                        Dict.empty
                    else 
                        Dict.fromList [(y, t1)]

                (Func g gts) ->
                    if f == g then 
                        listTermMGU fts gts
                    else
                        Dict.empty

listTermMGU : List(Term) -> List(Term) -> Substitution
listTermMGU lt1 lt2 =
    case (lt1, lt2) of
        ([],[]) -> Dict.empty
        ([], r::rs) -> Dict.empty
        (t::ts, []) -> Dict.empty
        (t::ts, r::rs) -> 
            let
                s1 = termMGU t r
            in
                let
                    s2 = listTermMGU (List.map (\x -> applySubsToTerm s1 x) ts) (List.map (\x -> applySubsToTerm s1 x) rs)
                in
                    finalizelistTermMGU (subsComposition s2 s1)

finalizelistTermMGU : Substitution -> Substitution
finalizelistTermMGU s = 
    let 
        sf = Dict.filter (\x tr -> (Var x) /= tr) s
    in 
        case List.head <|  List.filter (\x -> List.member (Var x) (varsInListTerm <| Dict.values <| Dict.remove x sf)) <| Dict.keys sf of
            Just y -> finalizelistTermMGU (subsComposition (Dict.remove y sf) (Dict.fromList [(y, Maybe.withDefault (Var "fail") <| Dict.get y sf)]))
            Nothing -> sf 

atomsMGU :  FormulaLPO -> FormulaLPO -> Substitution
atomsMGU a1 a2 =
    case (a1, a2) of
        (Pred p1 ts1, Pred p2 ts2) -> 
            if p1 == p2 then
                listTermMGU ts1 ts2
            else 
                Dict.empty
        _ -> Dict.empty
     
                
            