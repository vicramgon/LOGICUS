module Modules.SintaxSemanticsLPO exposing (..)

-- IN PROGRESS

import List exposing (concat)
import Modules.AuxiliarFunctions as Aux exposing (unique)
import Maybe exposing (Maybe(..))
import Dict exposing (Dict)
import Set exposing (Set)




-----------
-- TYPES --
-----------

type Term = Var String
          | Func String (List Term)

type alias Variable = Term

type FormulaLPO = Pred String (List Term)
                | Equal Term Term
                | Neg FormulaLPO
                | Conj FormulaLPO FormulaLPO
                | Disj FormulaLPO FormulaLPO
                | Impl FormulaLPO FormulaLPO
                | Equi FormulaLPO FormulaLPO
                | Exists Variable FormulaLPO
                | Forall Variable FormulaLPO
                | Insat

type alias Substitution = Dict String Term

type alias Universe comparable = List comparable

type alias Interpretation comparable =
    { interpretsFunction : Dict String (Dict (List comparable) comparable)
    , interpretsPredicate: Dict String (Set (List comparable))
    }

-------------
-- METHODS --
-------------

getVarSymb : Variable -> String
getVarSymb t =
    case t of
        Var x -> x
        _ -> ""

isVariable : Term -> Bool
isVariable t =
    case t of
        Var _ -> True
        _ -> False

varsInTerm : Term -> List (Variable)
varsInTerm t =
    case t of
        Var x -> [Var x]
        Func _ ts -> varsInListTerm ts

varsInListTerm : List Term -> List Variable
varsInListTerm ts = Aux.unique <| List.concat <| List.map varsInTerm ts

varsInFormula : FormulaLPO -> List Variable
varsInFormula f =
    case f of
        Pred _ terms -> varsInListTerm terms
        Equal t1 t2 -> varsInListTerm [t1, t2]
        Neg p -> varsInFormula p
        Conj p q -> Aux.unique <| varsInFormula p ++ varsInFormula q
        Disj p q -> Aux.unique <| varsInFormula p ++ varsInFormula q
        Impl p q -> Aux.unique <| varsInFormula p ++ varsInFormula q
        Equi p q -> Aux.unique <| varsInFormula p ++ varsInFormula q
        Exists _ p -> varsInFormula p
        Forall _ p -> varsInFormula p
        Insat -> []

subsDomain : Substitution -> List Variable
subsDomain x = List.map Var <| Dict.keys x

applySubsToVar : Substitution -> Variable -> Term
applySubsToVar s x = Maybe.withDefault x <| Dict.get (getVarSymb x) s

applySubsToTerm : Substitution -> Term -> Term
applySubsToTerm s t=
    case t of
        Var _ -> applySubsToVar s t
        Func sf ts -> Func sf (List.map (\term -> applySubsToTerm s term) ts)

applySubsToFormula : Substitution -> FormulaLPO -> FormulaLPO
applySubsToFormula s f =
    case f of
        Pred n ts ->  Pred n <| List.map (\t -> applySubsToTerm s t) ts
        Equal t1 t2 -> Equal (applySubsToTerm s t1)  (applySubsToTerm s t2)
        Neg p ->  Neg <| applySubsToFormula s p
        Conj p q -> Conj (applySubsToFormula s p) (applySubsToFormula s q)
        Disj p q -> Disj (applySubsToFormula s p) (applySubsToFormula s q)
        Impl p q -> Impl (applySubsToFormula s p) (applySubsToFormula s q)
        Equi p q -> Equi (applySubsToFormula s p) (applySubsToFormula s q)
        Exists v p -> 
            let 
                s2 = Dict.filter (\ k _ -> k /= (getVarSymb v)) s
            in
                Exists v (applySubsToFormula s2 p)
        Forall v p ->  
            let 
                s2 = Dict.filter (\ k _ -> k /= (getVarSymb v)) s
            in
                Forall v (applySubsToFormula s2 p)
        Insat -> Insat

checkWFF : FormulaLPO -> Bool
checkWFF x = checkWFFAux x []

checkWFFAux : FormulaLPO -> List Variable -> Bool
checkWFFAux x ls =
    case x of
        Pred _ _ -> True
        Equal _ _-> True
        Neg p -> checkWFFAux p ls
        Conj p q -> checkWFFAux p ls && checkWFFAux q ls
        Disj p q -> checkWFFAux p ls && checkWFFAux q ls
        Impl p q -> checkWFFAux p ls && checkWFFAux q ls
        Equi p q -> checkWFFAux p ls && checkWFFAux q ls
        Exists v p -> not(List.member v ls) && checkWFFAux p ls && checkWFFAux p (ls ++ [v])
        Forall v p -> not(List.member v ls) && checkWFFAux p ls && checkWFFAux p (ls ++ [v])
        Insat -> True 

varIsFreeInFLPO : Variable -> FormulaLPO -> Bool 
varIsFreeInFLPO v f=
    case f of
        Pred _ terms -> List.member v (varsInListTerm terms)
        Equal t1 t2 -> List.member v (varsInListTerm [t1, t2])
        Neg p -> varIsFreeInFLPO v p
        Conj p q -> varIsFreeInFLPO v p || varIsFreeInFLPO v q
        Disj p q -> varIsFreeInFLPO v p || varIsFreeInFLPO v q
        Impl p q -> varIsFreeInFLPO v p || varIsFreeInFLPO v q
        Equi p q -> varIsFreeInFLPO v p || varIsFreeInFLPO v q
        Exists var p -> not(var == v) && varIsFreeInFLPO v p
        Forall var p -> not(var == v) && varIsFreeInFLPO v p
        Insat -> True 

varIsLinkedInFLPO : Variable -> FormulaLPO -> Bool 
varIsLinkedInFLPO v f=
    case f of
        Pred _ _ -> False
        Equal _ _ -> False
        Neg p -> varIsLinkedInFLPO v p
        Conj p q -> varIsLinkedInFLPO v p || varIsLinkedInFLPO v q
        Disj p q -> varIsLinkedInFLPO v p || varIsLinkedInFLPO v q
        Impl p q -> varIsLinkedInFLPO v p || varIsLinkedInFLPO v q
        Equi p q -> varIsLinkedInFLPO v p || varIsLinkedInFLPO v q
        Exists var p -> (var == v && List.member v (varsInFormula p)) || varIsLinkedInFLPO v p
        Forall var p -> (var == v && List.member v (varsInFormula p)) || varIsLinkedInFLPO v p
        Insat -> False

isOpenFLPO : FormulaLPO -> Bool
isOpenFLPO f =
    case f of
        Pred _ _ -> True
        Equal _ _ -> True
        Neg p -> isOpenFLPO p
        Conj p q -> isOpenFLPO p && isOpenFLPO q
        Disj p q -> isOpenFLPO p && isOpenFLPO q
        Impl p q -> isOpenFLPO p && isOpenFLPO q
        Equi p q -> isOpenFLPO p && isOpenFLPO q
        Exists _ _ -> False
        Forall _ _ -> False
        Insat -> True

isClosedFLPO : FormulaLPO -> Bool
isClosedFLPO f = List.all (\v -> varIsLinkedInFLPO v f) (varsInFormula f)

universalClausureFLPO : FormulaLPO -> FormulaLPO
universalClausureFLPO f = renameVars <| universalClausureFLPOAux f (List.filter (\ v -> varIsFreeInFLPO v f) <| varsInFormula f)
    
universalClausureFLPOAux : FormulaLPO -> List Variable -> FormulaLPO
universalClausureFLPOAux f ls =
    case ls of
        [] -> f
        x::xs -> universalClausureFLPOAux (Forall x f) xs

existencialClausureFLPO : FormulaLPO -> FormulaLPO
existencialClausureFLPO f = renameVars <| existencialClausureFLPOAux f (List.filter (\ v -> varIsFreeInFLPO v f) <| varsInFormula f)
    
existencialClausureFLPOAux : FormulaLPO -> List Variable -> FormulaLPO
existencialClausureFLPOAux f ls =
    case ls of
        [] -> f
        x::xs -> existencialClausureFLPOAux (Exists x f) xs

renameVars : FormulaLPO -> FormulaLPO
renameVars f = Tuple.first <| renameVarsAux f  Dict.empty

renameVarsAux : FormulaLPO -> Dict String Int -> (FormulaLPO, Dict String Int)
renameVarsAux f vars =
    let varsSub = Dict.map (\ k v ->  Var (k ++ "_" ++ String.fromInt v)) vars in
        case f of
            Pred n terms ->  (Pred n <| List.map (\ t -> applySubsToTerm varsSub t) terms, vars)
            Equal t1 t2 -> (Equal (applySubsToTerm varsSub t1) (applySubsToTerm varsSub t2), vars)
            Neg p ->  
                let
                    (f1, d1) = renameVarsAux p vars
                in
                    (Neg f1 , d1)
            Conj p q -> 
                let
                    (f1, d1) = renameVarsAux p vars
                in
                    let
                        (f2,d2) = renameVarsAux q d1
                    in
                        (Conj f1 f2, d2)
            Disj p q -> 
                let
                    (f1, d1) = renameVarsAux p vars
                in
                    let
                        (f2,d2) = renameVarsAux q d1
                    in
                        (Disj f1 f2, d2)
            Impl p q -> 
                let
                    (f1, d1) = renameVarsAux p vars
                in
                    let
                        (f2,d2) = renameVarsAux q d1
                    in
                        (Impl f1 f2, d2)
            Equi p q -> 
                let
                    (f1, d1) = renameVarsAux p vars
                in
                    let
                        (f2,d2) = renameVarsAux q d1
                    in
                        (Equi f1 f2, d2)
            Exists v p -> 
                let 
                    vSymb = getVarSymb v 
                in
                    let 
                        vNewIndex = (Maybe.withDefault 0 <| Dict.get vSymb vars) + 1
                    in
                        let
                            newVars = Dict.insert vSymb vNewIndex vars
                        in
                            let
                                (f1, d1) = renameVarsAux p newVars
                            in
                                (Exists (Var (vSymb ++ "_" ++ String.fromInt vNewIndex)) f1, d1)
            Forall v p -> 
                let 
                    vSymb = getVarSymb v 
                in
                    let 
                        vNewIndex = (Maybe.withDefault 0 <| Dict.get vSymb vars) + 1
                    in
                        let
                            newVars = Dict.insert vSymb vNewIndex vars
                        in
                            let
                                (f1, d1) = renameVarsAux p newVars
                            in
                                (Forall (Var (vSymb ++ "_" ++ String.fromInt vNewIndex)) f1, d1)
            Insat -> (Insat, vars)

{-compose : Substitution -> Substitution -> Substitution
compose s1 s2 =  reduce <|  List.map (\(v,t) -> (v, applySubsToTerm s2 t)) s1 ++  List.filter (\(v, _) -> not(List.member v (subsDomain s1))) s2 -}
