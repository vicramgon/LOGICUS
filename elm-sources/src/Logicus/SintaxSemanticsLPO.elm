module Logicus.SintaxSemanticsLPO exposing (..)

-- IN PROGRESS

import List exposing (concat)
import Logicus.AuxiliarFunctions as Aux exposing (unique)
import Maybe exposing (Maybe(..), andThen)
import Dict exposing (Dict)
import Maybe.Extra exposing (isNothing, values, combine)

import String





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

type alias SetLPO = List FormulaLPO

type alias Substitution = Dict String Term

type alias Universe a = List a

type alias Interpretation a =
    { i_const : Dict String a
     , i_funct : Dict String ((List a) -> a)
     , i_pred : Dict String ((List a) -> Bool)
    }

type alias L_estructure a = (Universe a , Interpretation a)

-------------
-- METHODS --
-------------

getVarSymb : Variable -> String
getVarSymb t =
    case t of
        Var x -> x
        _ -> ""

isConstTerm : Term -> Bool 
isConstTerm t =
    case t of
        Func _ [] -> True
        Func _ terms -> List.all (\x -> isConstTerm x) terms
        _ -> False

getConstSymb : Variable -> String
getConstSymb t =
    case t of
        Func x [] -> x
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
                s2 = Dict.filter (\ k _ -> k /=  getVarSymb v) s
            in
                Exists v (applySubsToFormula s2 p)
        Forall v p ->  
            let 
                s2 = Dict.filter (\ k _ -> k /=  getVarSymb v) s
            in
                Forall v (applySubsToFormula s2 p)
        Insat -> Insat

-- subsComposition s1 s2 calculates s2 o s1
subsComposition : Substitution -> Substitution -> Substitution
subsComposition s1 s2 =
    let
        s1l = Dict.toList s1
        s2l = Dict.toList s2
    in
        Dict.fromList <| (List.filter (\(x1, tr) -> (Var x1) /= tr) <| List.map (\(x1, t1) -> (x1, applySubsToTerm s2 t1)) s1l) ++ (List.filter (\(x2,_) -> not(Dict.member x2 s1)) s2l)
    

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
renameVars f = Tuple.first <| renameVarsAux Dict.empty Dict.empty f

renameVarsAux : Dict String Int -> Dict String Int -> FormulaLPO -> (FormulaLPO, Dict String Int)
renameVarsAux act mem  f =
    case f of
        Pred n terms -> 
            let
                s = Dict.map (\ k v -> Var (k ++ "_" ++ String.fromInt v)) act
            in
                (Pred n <| List.map (\ t -> applySubsToTerm s t) terms, mem)
    
        Equal t1 t2 -> 
            let
                s = Dict.map (\ k v -> Var (k ++ "_" ++ String.fromInt v)) act
            in
                (Equal (applySubsToTerm s t1) (applySubsToTerm s t2), mem)

        Neg g ->
            let
                (ng, nmem) = renameVarsAux act mem g
            in
                (Neg ng, nmem)
        
        Conj g h ->
            let
                (ng, nmem) = renameVarsAux act mem g
            in
                let
                    (nh, nmem2) = renameVarsAux act nmem h
                in
                    (Conj ng nh, nmem2)

        Disj g h ->
            let
                (ng, nmem) = renameVarsAux act mem g
            in
                let
                    (nh, nmem2) = renameVarsAux act nmem h
                in
                    (Disj ng nh, nmem2)
        Impl g h ->
            let
                (ng, nmem) = renameVarsAux act mem g
            in
                let
                    (nh, nmem2) = renameVarsAux act nmem h
                in
                    (Impl ng nh, nmem2)
        Equi g h ->
            let
                (ng, nmem) = renameVarsAux act mem g
            in
                let
                    (nh, nmem2) = renameVarsAux act nmem h
                in
                    (Equi ng nh, nmem2)
        Forall (Var x) g ->
            let
                xind = (Maybe.withDefault 0 <| Dict.get x mem) + 1
            in
                let
                    nact = Dict.insert x xind act 
                    nmem = Dict.insert x xind mem
                in
                    let
                        (ng, nmem2) = renameVarsAux nact nmem g
                    in
                        (Forall (Var (x ++ "_" ++  String.fromInt xind)) ng, nmem2)

        Exists (Var x) g ->
            let
                xind = (Maybe.withDefault 0 <| Dict.get x mem) + 1
            in
                let
                    nact = Dict.insert x xind act 
                    nmem = Dict.insert x xind mem
                in
                    let
                        (ng, nmem2) = renameVarsAux nact nmem g
                    in
                        (Exists (Var (x ++ "_" ++  String.fromInt xind)) ng, nmem2)

        _ -> (Insat, mem)

interpretsTerm : Term -> Interpretation a ->  Maybe a
interpretsTerm t i =
    case t of
        Var _ -> Nothing
        Func s [] -> Dict.get s i.i_const
        Func s args ->         
            let f = Dict.get s i.i_funct in
                case f of
                    Nothing -> Nothing
                    Just x -> 
                        let ls = List.map (\t2 -> interpretsTerm t2 i) args in
                            if List.any isNothing ls then
                                Nothing
                            else
                                Just <| x (values ls)

interpretsFLPO : FormulaLPO -> L_estructure a -> Maybe Bool
interpretsFLPO f estr = interpretsFLPOAux (universalClausureFLPO f) estr

interpretsFLPOAux : FormulaLPO -> L_estructure a -> Maybe Bool
interpretsFLPOAux f (m,i) =
    case f of
        Pred sp terms -> 
            let ip = Dict.get sp i.i_pred in
                case ip of
                    Nothing -> Nothing
                    Just x -> 
                        let ls = List.map (\t -> interpretsTerm t i) terms in
                            if List.any isNothing ls then
                                Nothing
                            else
                                Just <| x (values ls)
        Equal t1 t2-> 
            let it1 = interpretsTerm t1 i
                it2 = interpretsTerm t2 i
            in 
                if isNothing it1 || isNothing it2 then
                    Nothing
                else Just  (it1 == it2)

        Neg f1 -> andThen (\if1 -> Just (not if1)) (interpretsFLPOAux f1 (m,i))
        Conj f1 f2 -> 
            let if1 = interpretsFLPOAux f1 (m,i)
                if2 = interpretsFLPOAux f2 (m,i)
            in 
                case if1 of
                    Nothing -> Nothing
                    Just x ->   
                        case if2 of
                            Nothing -> Nothing 
                            Just y -> Just (x && y)
        Disj f1 f2 -> 
            let if1 = interpretsFLPOAux f1 (m,i)
                if2 = interpretsFLPOAux f2 (m,i)
            in 
                case if1 of
                    Nothing -> Nothing
                    Just x ->   
                        case if2 of
                            Nothing -> Nothing 
                            Just y -> Just (x || y)
        Impl f1 f2 -> 
            let if1 = interpretsFLPOAux f1 (m,i)
                if2 = interpretsFLPOAux f2 (m,i)
            in 
                case if1 of
                    Nothing -> Nothing
                    Just x ->   
                        case if2 of
                            Nothing -> Nothing 
                            Just y -> Just (not x || y)
        Equi f1 f2 -> 
            let if1 = interpretsFLPOAux f1 (m,i)
                if2 = interpretsFLPOAux f2 (m,i)
            in 
                case if1 of
                    Nothing -> Nothing
                    Just x ->   
                        case if2 of
                            Nothing -> Nothing 
                            Just y -> Just (x == y)
        Exists v f1 ->
            let ls = combine <| List.map (\o -> interpretsFLPOAux (applySubsToFormula (Dict.singleton (getVarSymb v) (Func o [])) f1) (m,i)) (Dict.keys i.i_const) in
                case ls of
                    Nothing -> Nothing
                    Just li -> Just <| List.any (\x -> x) li
        Forall v f1 -> 
            let ls = combine <| List.map (\o -> interpretsFLPOAux (applySubsToFormula (Dict.singleton (getVarSymb v) (Func o [])) f1) (m,i)) (Dict.keys i.i_const)in
                case ls of
                    Nothing -> Nothing
                    Just li -> Just <| List.all (\x -> x) li
        Insat -> Just False

interpretsSetLPO : SetLPO -> L_estructure a -> Maybe Bool
interpretsSetLPO fs estr = interpretsFLPOAux (universalClausureFLPO <| lpoSetConjunction fs) estr

lpoSetConjunction : SetLPO -> FormulaLPO
lpoSetConjunction fs = 
    case fs of
        f::xs -> List.foldl (\x ac -> Conj ac x) f xs
        _ -> Insat
    
       


