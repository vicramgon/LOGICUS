module Modules.SintaxSemanticsLPO exposing (..)

-- IN PROGRESS

import List exposing (concat)
import Modules.AuxiliarFunctions as Aux exposing (unique)
import Maybe exposing (Maybe(..))
import Dict exposing (Dict)



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
        Func _ ts -> varsInList ts

varsInList : List Term -> List Variable
varsInList ts = Aux.unique <| List.concat <| List.map varsInTerm ts

eye : Substitution
eye = Dict.empty

subsDomain : Substitution -> List Variable
subsDomain x = List.map Var <| Dict.keys x

applySubsToVar : Substitution -> Variable -> List Variable -> Term
applySubsToVar s x aV =
    if List.member x aV then
        x
    else
        Maybe.withDefault x <| Dict.get (getVarSymb x) s

applySubsToTerm : Substitution -> Term -> List Variable -> Term
applySubsToTerm s t aV= 
    case t of
        Var _ -> applySubsToVar s t aV
        Func sf ts -> Func sf (List.map (\term -> applySubsToTerm s term aV) ts)


applySubsToFormula : Substitution -> FormulaLPO -> FormulaLPO
applySubsToFormula s f = applySubsToFormulaAux s f []

applySubsToFormulaAux : Substitution -> FormulaLPO -> List Variable -> FormulaLPO
applySubsToFormulaAux s f aV = 
    case f of
        Pred n ts ->  Pred n <| List.map (\t -> applySubsToTerm s t aV) ts

        Equal t1 t2 -> Equal (applySubsToTerm s t1 aV)  (applySubsToTerm s t2 aV)
        Neg p ->  Neg <| applySubsToFormulaAux s p aV 
        Conj p q -> Conj (applySubsToFormulaAux s p aV) (applySubsToFormulaAux s q aV)
        Disj p q -> Disj (applySubsToFormulaAux s p aV) (applySubsToFormulaAux s q aV)

        Impl p q -> Impl (applySubsToFormulaAux s p aV) (applySubsToFormulaAux s q aV)
        Equi p q -> Equi (applySubsToFormulaAux s p aV) (applySubsToFormulaAux s q aV)
        Exists v p -> Exists v (applySubsToFormulaAux s p (aV ++ [v])) 
        Forall v p ->  Forall v (applySubsToFormulaAux s p (aV ++ [v])) 
        Insat -> Insat



{- compose : Substitution -> Substitution -> Substitution 
compose s1 s2 =  reduce <|  List.map (\(v,t) -> (v, applySubsToTerm s2 t)) s1 ++  List.filter (\(v, _) -> not(List.member v (subsDomain s1))) s2 -}

