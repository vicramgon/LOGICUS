module Logicus.HerbrandLPO exposing (..)
import Logicus.SintaxSemanticsLPO as LPO exposing (FormulaLPO(..), Term(..), SetLPO, isOpenFLPO, lpoSetConjunction, varsInFormula, applySubsToFormula, getVarSymb)
import Logicus.AuxiliarFunctions as Aux exposing (uniqueConcatList)
import String exposing (repeat)
import Dict
import List
import Logicus.SintaxSemanticsLP as LP exposing (FormulaLP(..))
import Logicus.IO_LPO exposing (toStringFLPO)
import List.Extra exposing (cartesianProduct)

type alias Signature = (List String, List (String, Int), List (String, Int))

unionSignatures2 : Signature -> Signature -> Signature
unionSignatures2 (cs1, fs1, ps1) (cs2, fs2, ps2) =
    let
        cs = List.Extra.unique <| cs1 ++ cs2
        fs =  List.Extra.unique <| fs1 ++ fs2
        ps = List.Extra.unique <| ps1 ++ ps2
    in
        (cs, fs, ps)

unionSignatures : List Signature -> Signature
unionSignatures ls = List.foldl unionSignatures2 ([],[], []) ls

signatureTerm : Term -> Signature
signatureTerm t =
    case t of
        Var _ -> ([], [], [])
        Func f [] -> ([f], [], [])
        Func f terms -> List.foldl (\ x ac -> unionSignatures2 ac (signatureTerm x)) ([], [(f, List.length terms)], []) terms

signatureFormulaLPO : FormulaLPO -> Maybe Signature
signatureFormulaLPO f =
    if isOpenFLPO f then 
        Just <| signatureFormulaLPOAux f
    else 
        Nothing

signatureFormulaLPOAux : FormulaLPO -> Signature
signatureFormulaLPOAux f =
    case f of
        LPO.Pred p terms -> List.foldl (\ x ac -> unionSignatures2 ac (signatureTerm x)) ([], [], [(p, List.length terms)]) terms
        LPO.Equal t1 t2 -> List.foldl (\ x ac -> unionSignatures2 ac (signatureTerm x)) ([], [], [("=", 2)]) [t1, t2]
        LPO.Neg x -> signatureFormulaLPOAux x
        LPO.Conj x y -> unionSignatures2 (signatureFormulaLPOAux x) (signatureFormulaLPOAux y)
        LPO.Disj x y -> unionSignatures2 (signatureFormulaLPOAux x) (signatureFormulaLPOAux y)
        LPO.Impl x y -> unionSignatures2 (signatureFormulaLPOAux x) (signatureFormulaLPOAux y)
        LPO.Equi x y -> unionSignatures2 (signatureFormulaLPOAux x) (signatureFormulaLPOAux y)
        LPO.Forall _ x -> signatureFormulaLPOAux x
        LPO.Exists _ x -> signatureFormulaLPOAux x
        LPO.Insat -> ([],[],[])

signatureLPOSet : SetLPO -> Maybe Signature
signatureLPOSet ls =
    if List.all isOpenFLPO ls then
        Just <| List.foldl (\ f ac -> unionSignatures2 ac (signatureFormulaLPOAux f)) ([], [], []) ls
    else
        Nothing


signatureHerbrandUniverse : Signature -> Int -> List Term
signatureHerbrandUniverse (cs, fs, ps) n =
    if n <= 0 then
        if List.isEmpty cs then  
            [Func "a" []]
        else
            List.map (\x -> Func x []) cs
    else 
        let
            uH_prev = signatureHerbrandUniverse (cs,fs,ps) (n-1)
        in
            List.foldl (\x ac -> uniqueConcatList ac x) uH_prev <| List.map (\ (f, a)-> List.map (\ ts -> Func f ts) (List.Extra.cartesianProduct (List.repeat a uH_prev))) fs
           

formulaLPOHerbrandUniverse : FormulaLPO -> Int -> Maybe(List Term)
formulaLPOHerbrandUniverse f n = 
    if isOpenFLPO f then
        Just <| formulaLPOHerbrandUniverseAux f n
    else 
        Nothing

formulaLPOHerbrandUniverseAux : FormulaLPO -> Int -> List Term
formulaLPOHerbrandUniverseAux f n = signatureHerbrandUniverse (signatureFormulaLPOAux f) n


lpoSetHerbrandUniverse : List FormulaLPO -> Int ->  Maybe(List Term)
lpoSetHerbrandUniverse fs n =
    if List.all isOpenFLPO fs then
        Just <| List.foldl (\ x ac -> uniqueConcatList ac (formulaLPOHerbrandUniverseAux x n)) [] fs
    else
        Nothing
        
   
signatureHerbrandBase : Signature -> Int -> List FormulaLPO 
signatureHerbrandBase (cs, fs, ps) n =
    let
        uH = signatureHerbrandUniverse (cs, fs, ps) n
    in
        List.foldl (\ x ac -> uniqueConcatList ac x) [] <| List.map (\ (p, a) -> List.map (\ ts -> Pred p ts)  (List.Extra.cartesianProduct (List.repeat a uH))) ps


formulaLPOHerbrandBase : FormulaLPO -> Int -> Maybe(List FormulaLPO)
formulaLPOHerbrandBase f n =
    if isOpenFLPO f then 
        Just <| formulaLPOHerbrandBaseAux f n
    else
        Nothing

formulaLPOHerbrandBaseAux : FormulaLPO -> Int -> List FormulaLPO
formulaLPOHerbrandBaseAux f n =
    let
        s = signatureFormulaLPOAux f
    in
        signatureHerbrandBase s n

lpoSetHerbrandBase : SetLPO -> Int -> Maybe(List FormulaLPO)
lpoSetHerbrandBase fs n =
    formulaLPOHerbrandBase (lpoSetConjunction fs) n

signatureHerbrandInterpSets : Signature -> Int -> List (List FormulaLPO)
signatureHerbrandInterpSets s n = Aux.powerset <| signatureHerbrandBase s n


formulaLPOHerbrandInterpSets : FormulaLPO -> Int -> Maybe(List (List FormulaLPO))
formulaLPOHerbrandInterpSets f n =
    if isOpenFLPO f then
        Just <| formulaLPOHerbrandInterpSetsAux f n 
    else 
        Nothing 

formulaLPOHerbrandInterpSetsAux : FormulaLPO -> Int -> List (List FormulaLPO)
formulaLPOHerbrandInterpSetsAux f n = 
    let
        s = signatureFormulaLPOAux f
    in
        signatureHerbrandInterpSets s n

lpoSetHerbrandInterpSets : SetLPO -> Int -> Maybe(List (List FormulaLPO))
lpoSetHerbrandInterpSets fs n =
    if List.all isOpenFLPO fs then 
        Just <| lpoSetHerbrandInterpSetsAux fs n
    else 
        Nothing

lpoSetHerbrandInterpSetsAux : SetLPO -> Int ->  List (List FormulaLPO)
lpoSetHerbrandInterpSetsAux fs n =
    let
        s = Maybe.withDefault ([], [], []) <| signatureLPOSet fs
    in
        signatureHerbrandInterpSets s n


interpretsHerbrandFLPO : FormulaLPO -> List FormulaLPO -> List Term -> Maybe Bool
interpretsHerbrandFLPO f iH uH = 
    if isOpenFLPO f then
        Just <| interpretsHerbrandFLPOAux (List.foldr (\x ac -> Forall x ac) f (varsInFormula f)) iH uH
    else
        Nothing
interpretsHerbrandFLPOAux : FormulaLPO -> List FormulaLPO -> List Term -> Bool
interpretsHerbrandFLPOAux f iH uH =
    case f of
        LPO.Pred _ _ -> List.member f iH

        LPO.Equal _ _-> List.member f iH

        LPO.Neg f1 -> not (interpretsHerbrandFLPOAux f1 iH uH)
        
        LPO.Conj f1 f2 -> 
            let if1 = interpretsHerbrandFLPOAux f1 iH uH
                if2 = interpretsHerbrandFLPOAux f2 iH uH
            in 
                (if1 && if2)

        LPO.Disj f1 f2 -> 
            let if1 = interpretsHerbrandFLPOAux f1 iH uH
                if2 = interpretsHerbrandFLPOAux f2 iH uH
            in 
                (if1 || if2)
        LPO.Impl f1 f2 -> 
            let if1 = interpretsHerbrandFLPOAux f1 iH uH
                if2 = interpretsHerbrandFLPOAux f2 iH uH
            in 
                (not if1 || if2)
        LPO.Equi f1 f2 -> 
            let if1 = interpretsHerbrandFLPOAux f1 iH uH
                if2 = interpretsHerbrandFLPOAux f2 iH uH
            in 
                (if1 == if2)
        
        LPO.Forall v f1 -> List.all (\x -> x) <| List.map (\o -> interpretsHerbrandFLPOAux (applySubsToFormula (Dict.singleton (getVarSymb v) o) f1) iH uH) uH
    
        _ ->  False

formulaLPOHerbrandModels : FormulaLPO -> Int -> Maybe (List Term, List (List FormulaLPO))
formulaLPOHerbrandModels f n =
    if isOpenFLPO f then
        Just <| formulaLPOHerbrandModelsAux f n
        
    else 
        Nothing

formulaLPOHerbrandModelsAux : FormulaLPO -> Int -> (List Term, List (List FormulaLPO))
formulaLPOHerbrandModelsAux f n =
    let
            uH = formulaLPOHerbrandUniverseAux f n
        in
            let
                iHs = List.filter (\ iH -> Maybe.withDefault False <| interpretsHerbrandFLPO  f iH uH) <| formulaLPOHerbrandInterpSetsAux f n
            in  
                (uH, iHs)
lpoSetHerbrandModels : SetLPO -> Int -> Maybe (List Term, List (List FormulaLPO))
lpoSetHerbrandModels fs n =
    if List.all isOpenFLPO fs then
        Just <| formulaLPOHerbrandModelsAux (lpoSetConjunction fs) n
    else
        Nothing


formulaLPOHerbrandExtension  : FormulaLPO -> Int -> Maybe (List FormulaLP)
formulaLPOHerbrandExtension f n =
    if isOpenFLPO f then
        Just <| formulaLPOHerbrandExtensionAux f n 
    else 
        Nothing


formulaLPOHerbrandExtensionAux : FormulaLPO -> Int -> List FormulaLP
formulaLPOHerbrandExtensionAux f n =
    let
        uH = formulaLPOHerbrandUniverseAux f n
        vs = varsInFormula f
    in
        let
            substitutions = List.map (\x -> List.map2 (\ y z -> (getVarSymb y, z)) vs x) <| cartesianProduct <| List.repeat (List.length vs) uH
        in
            List.map (\xs -> Maybe.withDefault LP.Insat <| formulaLPOToLP <| applySubsToFormula (Dict.fromList xs) f) substitutions
        
lpoSetHerbrandExtension : SetLPO -> Int -> Maybe (List FormulaLP)
lpoSetHerbrandExtension fs n =
    if List.all isOpenFLPO fs then
        Just <| List.foldl(\x ac-> Aux.uniqueConcatList ac (formulaLPOHerbrandExtensionAux x n)) [] fs
    else
        Nothing

formulaLPOToLP : FormulaLPO -> Maybe(FormulaLP)
formulaLPOToLP f =
    case f of
        LPO.Pred _ _-> Just <| LP.Atom (toStringFLPO f)
        LPO.Equal _ _ -> Just <| LP.Atom (toStringFLPO f)
        LPO.Neg x -> 
            case formulaLPOToLP x of
                Just f1 -> Just <| LP.Neg f1
                Nothing -> Nothing
        LPO.Conj x y ->  
            case formulaLPOToLP x of
                Nothing -> Nothing
                Just f1 -> 
                    case formulaLPOToLP y of
                        Nothing -> Nothing
                        Just f2  -> Just <| LP.Conj f1 f2
        LPO.Disj x y ->
            case formulaLPOToLP x of
                Nothing -> Nothing
                Just f1 -> 
                    case formulaLPOToLP y of
                        Nothing -> Nothing
                        Just f2  -> Just <| LP.Disj f1 f2
        LPO.Impl x y -> 
            case formulaLPOToLP x of
                Nothing -> Nothing
                Just f1 -> 
                    case formulaLPOToLP y of
                        Nothing -> Nothing
                        Just f2  -> Just <| LP.Impl f1 f2
        LPO.Equi x y -> 
            case formulaLPOToLP x of
                Nothing -> Nothing
                Just f1 -> 
                    case formulaLPOToLP y of
                        Nothing -> Nothing
                        Just f2  -> Just <| LP.Equi f1 f2
        LPO.Forall _ _ -> Nothing
        LPO.Exists _ _ -> Nothing
        LPO.Insat -> Just LP.Insat




    