module Logicus.NormalFormsLPO exposing (..)

import Logicus.SintaxSemanticsLPO exposing (FormulaLPO(..), Term(..), getVarSymb)
import List.Extra
import List

import Dict
import Dict exposing (Dict)
import Logicus.SintaxSemanticsLPO exposing (applySubsToFormula)


type Cuantifier
    = F String
    | E String

isFCuantifier : Cuantifier -> Bool
isFCuantifier c =
    case c of
        F _ -> True
        E _ -> False

isECuantifier : Cuantifier -> Bool
isECuantifier c = not <| isFCuantifier c
contraryCuantifier : Cuantifier -> Cuantifier
contraryCuantifier c =
    case c of
        F x -> E x
        E x -> F x

applyCuantifier : Cuantifier -> FormulaLPO -> FormulaLPO
applyCuantifier c f=
    case c of
        F x -> Forall (Var x) f
        E x -> Exists (Var x) f

sortWithFirstE : List Cuantifier -> List Cuantifier -> List Cuantifier
sortWithFirstE l1 l2 = sortWithFirstEAux l1 l2 []

sortWithFirstEAux : List Cuantifier -> List Cuantifier -> List Cuantifier -> List Cuantifier 
sortWithFirstEAux l1 l2 res =
    case l1 of
        [] -> res ++ l2
        (E x)::ls1 -> sortWithFirstEAux ls1 l2 (res ++ [E x])
        _ ->
            case l2 of
                [] -> res ++ l1 
                (E x)::ls2 -> sortWithFirstEAux l1 ls2 (res ++ [E x])
                _ ->
                    let
                        (xs1, ys1) = List.Extra.break (\x -> isECuantifier x) l1
                        (xs2, ys2) = List.Extra.break (\x -> isECuantifier x) l2 
                    in
                        if List.isEmpty ys2 then
                            res ++ l1 ++ xs2
                        else if List.isEmpty ys1 then 
                            res ++ l2 ++ xs1
                        else if List.length xs2 < List.length xs1 then
                            sortWithFirstEAux l1 ys2 (res ++ xs2)
                        else
                            sortWithFirstEAux ys1 l2 (res ++ xs1)

sortWithFirstF : List Cuantifier -> List Cuantifier -> List Cuantifier
sortWithFirstF l1 l2 = sortWithFirstFAux l1 l2 []

sortWithFirstFAux : List Cuantifier -> List Cuantifier -> List Cuantifier -> List Cuantifier 
sortWithFirstFAux l1 l2 res =
    case l1 of
        [] -> res ++ l2
        (F x)::ls1 -> sortWithFirstFAux ls1 l2 (res ++ [F x])
        _ ->
            case l2 of
                [] -> res ++ l1 
                (F x)::ls2 -> sortWithFirstFAux l1 ls2 (res ++ [F x])
                _ ->
                    let
                        (xs1, ys1) = List.Extra.break (\x -> isFCuantifier x) l1
                        (xs2, ys2) = List.Extra.break (\x -> isFCuantifier x) l2 
                    in
                        if List.isEmpty ys2 then
                            res ++ l1 ++ xs2
                        else if List.isEmpty ys1 then 
                            res ++ l2 ++ xs1
                        else if List.length xs2 < List.length xs1 then
                            sortWithFirstFAux l1 ys2 (res ++ xs2)
                        else
                            sortWithFirstFAux ys1 l2 (res ++ xs1)

toPrenexForm : FormulaLPO -> FormulaLPO
toPrenexForm f =
    case f of
        Pred _ _ -> f
        Equal _ _ -> f
        Neg g ->
            let
                (cuants1, f1) = toPrenexAux g False
            in
                let
                    cuants = List.map contraryCuantifier cuants1
                in
                    List.foldr (\x ac -> applyCuantifier x ac) (Neg f1) cuants
        Conj g h ->
            let
                (cuants1, f1) = toPrenexAux g True
                (cuants2, f2) = toPrenexAux h True
            in
                let
                    cuants = sortWithFirstE cuants1 cuants2
                in
                    List.foldr (\x ac -> applyCuantifier x ac) (Conj f1 f2) cuants   

        Disj g h ->
            let
                (cuants1, f1) = toPrenexAux g True
                (cuants2, f2) = toPrenexAux h True
            in
                let
                    cuants = sortWithFirstE cuants1 cuants2
                in
                    List.foldr (\x ac -> applyCuantifier x ac) (Disj f1 f2) cuants   

        Impl g h ->
            let
                (cuants1, f1) = toPrenexAux g False
                (cuants2, f2) = toPrenexAux h True
            in
                let
                    cuants = sortWithFirstE (List.map contraryCuantifier cuants1) cuants2
                in
                    List.foldr (\x ac -> applyCuantifier x ac) (Impl f1 f2) cuants   
                
            
        Equi g h -> 
            toPrenexForm (Conj (Impl g h) (Impl h g))

        Forall v g -> Forall v (toPrenexForm g)
        Exists v g -> Exists v (toPrenexForm g)
        Insat -> Insat


toPrenexAux : FormulaLPO -> Bool -> (List Cuantifier, FormulaLPO)
toPrenexAux f cent =
    case f of
        Pred _ _ -> ([], f)
        Equal _ _ -> ([], f)
        Neg g ->
            let
                (cuants1, f1) = toPrenexAux g (not cent)
            in
                let
                    cuants = List.map contraryCuantifier cuants1
                in
                    (cuants, Neg f1)
        Conj g h ->
            let
                (cuants1, f1) = toPrenexAux g cent
                (cuants2, f2) = toPrenexAux h cent
            in
                let
                    cuants = 
                        if cent then 
                            sortWithFirstE cuants1 cuants2
                        else
                            sortWithFirstF cuants1 cuants2
                in
                    (cuants, Conj f1 f2)  

        Disj g h ->
            let
                (cuants1, f1) = toPrenexAux g cent
                (cuants2, f2) = toPrenexAux h cent
            in
                let
                    cuants = 
                        if cent then 
                            sortWithFirstE cuants1 cuants2
                        else
                            sortWithFirstF cuants1 cuants2
                in
                    (cuants, Disj f1 f2)   

        Impl g h ->
            let
                (cuants1, f1) = toPrenexAux g (not cent)
                (cuants2, f2) = toPrenexAux h cent
            in
                let
                    cuants = 
                        if cent then 
                            sortWithFirstE (List.map contraryCuantifier cuants1) cuants2
                        else
                            sortWithFirstF (List.map contraryCuantifier cuants1) cuants2
                in
                    (cuants, Impl f1 f2)     
                
            
        Equi g h -> 
            toPrenexAux (Conj (Impl g h) (Impl h g)) cent

        Forall v g ->
            let
                (cuants1, f1) = toPrenexAux g cent
            in
                ( F (getVarSymb v)::cuants1, f1)
        Exists v g -> 
             let
                (cuants1, f1) = toPrenexAux g cent
            in
                ( E (getVarSymb v)::cuants1, f1)
        
        Insat -> ([], f)

containsCuantifiers : FormulaLPO -> Bool
containsCuantifiers f =
    case f of
        Pred _ _ -> False
        Equal _ _ -> False
        Neg g -> containsCuantifiers g
        Conj g h -> containsCuantifiers g || containsCuantifiers h
        Disj g h -> containsCuantifiers g || containsCuantifiers h
        Impl g h -> containsCuantifiers g || containsCuantifiers h
        Equi g h -> containsCuantifiers g || containsCuantifiers h
        Forall _ _ -> True
        Exists _ _ -> True
        Insat -> False

isInPrenexForm : FormulaLPO -> Bool
isInPrenexForm f =
    case f of
        Pred _ _ -> True
        Equal _ _ -> True
        Neg g -> not(containsCuantifiers g)
        Conj g h -> not(containsCuantifiers g) &&  not(containsCuantifiers h)
        Disj g h -> not(containsCuantifiers g) &&  not(containsCuantifiers h)
        Impl g h -> not(containsCuantifiers g) &&  not(containsCuantifiers h)
        Equi g h -> not(containsCuantifiers g) &&  not(containsCuantifiers h)
        Forall _ g -> isInPrenexForm g
        Exists _ g -> isInPrenexForm g
        Insat -> True

extractHeadCuantifiers : FormulaLPO -> Maybe (List Cuantifier, FormulaLPO)
extractHeadCuantifiers f =
    if isInPrenexForm f then
        Just <| extractHeadCuantifiersAux f 
    else 
        Nothing

extractHeadCuantifiersAux : FormulaLPO -> (List Cuantifier, FormulaLPO)
extractHeadCuantifiersAux f =
    case f of
        Forall x g ->  
            let
                (cuants, h) = extractHeadCuantifiersAux g
            in
                (F (getVarSymb x)::cuants, h)
        Exists x g ->  
            let
                (cuants, h) = extractHeadCuantifiersAux g
            in
                (E (getVarSymb x)::cuants, h)
        _ -> ([], f)

getSkolemSubs : List Cuantifier -> Dict String Term 
getSkolemSubs cS =
    let
        (subs, _, _) = 
            List.foldl 
                (\ c (res, lF, nE) -> 
                    case c of
                        F x -> (res, lF ++ [Var x], nE)
                        E x -> 
                            let
                                nres = Dict.insert x (Func ("f_" ++ String.fromInt (nE+1)) lF) res
                            in
                                (nres, lF, nE + 1)
                ) 
                (Dict.empty, [], 0) 
                cS    
    in
        subs

toSkolemForm : FormulaLPO -> FormulaLPO
toSkolemForm f = 
    if isInPrenexForm f then
        let
            (lc, g) = extractHeadCuantifiersAux f
        in
            applySubsToFormula (getSkolemSubs lc) g
    else 
        let
            fPrenex = toPrenexForm f
        in
            toSkolemForm fPrenex


type alias Literal = FormulaLPO
literal : FormulaLPO -> Bool
literal f =
    case f of
        Pred _ _ -> True

        Equal _ _ -> True

        Neg (Pred _ _) -> True
            
        _ -> False

literalComplement : Literal -> Literal
literalComplement f=
    case f of
        Pred _ _ -> Neg f
            
        Equal _ _ -> Neg f

        Neg (Pred n ts) -> Pred n ts
            
        _ -> f

containsEquiv : FormulaLPO -> Bool
containsEquiv f =
    case f of
        Pred _ _-> False

        Equal _ _ -> False
            
        Neg x -> containsEquiv x

        Conj x y -> containsEquiv x || containsEquiv y

        Disj x y -> containsEquiv x || containsEquiv y

        Impl x y -> containsEquiv x || containsEquiv y

        Equi _ _ -> True

        Forall _ g -> containsEquiv g

        Exists _ g -> containsEquiv g

        Insat -> False 

remEquiv : FormulaLPO -> FormulaLPO
remEquiv f =
    case f of
        Pred _ _-> f

        Equal _ _ -> f
            
        Neg x -> Neg (remEquiv x)

        Conj x y -> Conj (remEquiv x) (remEquiv y)

        Disj x y -> Disj (remEquiv x) (remEquiv y)

        Impl x y -> Impl (remEquiv x) (remEquiv y)

        Equi x y -> Conj (Impl (remEquiv x) (remEquiv y)) (Impl (remEquiv y) (remEquiv x))

        Forall v g -> Forall v (remEquiv g)

        Exists v g -> Exists v (remEquiv g)

        Insat -> Insat


containsImpl : FormulaLPO -> Bool
containsImpl f =
    case f of
        Pred _ _-> False

        Equal _ _ -> False
            
        Neg x -> containsEquiv x

        Conj x y -> containsImpl x || containsImpl y

        Disj x y -> containsImpl x || containsImpl y

        Impl _ _ -> True

        Equi x y -> containsImpl x || containsImpl y

        Forall _ g -> containsImpl g

        Exists _ g -> containsImpl g

        Insat -> False 
    
remImpl : FormulaLPO -> FormulaLPO
remImpl f =
    case f of
        Pred _ _-> f

        Equal _ _ -> f
            
        Neg x -> Neg (remImpl x)

        Conj x y -> Conj (remImpl x) (remImpl y)

        Disj x y -> Disj (remImpl x) (remImpl y)

        Impl x y -> Disj (Neg (remImpl x)) (remImpl y)

        Equi x y -> Conj (Impl (remImpl x) (remImpl y)) (Impl (remImpl y) (remImpl x))

        Forall v g -> Forall v (remImpl g)

        Exists v g -> Exists v (remImpl g)

        Insat -> Insat

interiorizeNeg : FormulaLPO -> FormulaLPO
interiorizeNeg f=
    case f of
        Pred _ _-> f

        Equal _ _ -> f

        Neg x ->  interiorizeNegAux x

        Conj x y -> Conj (interiorizeNeg x) (interiorizeNeg y)

        Disj x y -> Disj (interiorizeNeg x) (interiorizeNeg y)

        Impl x y -> Impl (interiorizeNeg x) (interiorizeNeg y)

        Equi x y -> Equi (interiorizeNeg x) (interiorizeNeg y)

        Forall v g -> Forall v (interiorizeNeg g)

        Exists v g -> Exists v (interiorizeNeg g)

        Insat -> Insat

interiorizeNegAux : FormulaLPO -> FormulaLPO
interiorizeNegAux f=
    case f of
        Pred _ _-> Neg f

        Equal _ _ -> Neg f

        Neg x -> interiorizeNeg x

        Conj x y -> Disj (interiorizeNegAux x) (interiorizeNegAux  y)

        Disj x y -> Conj (interiorizeNegAux x) (interiorizeNegAux y)

        Impl x y -> Conj (interiorizeNeg x) (interiorizeNegAux  y)

        Equi x y -> Disj (interiorizeNegAux  (Impl x y)) (interiorizeNegAux  (Impl x y)) 

        Forall v g -> Exists v (interiorizeNegAux g)

        Exists v g -> Forall v (interiorizeNegAux g)

        Insat -> Insat


toNNF : FormulaLPO -> FormulaLPO
toNNF f = interiorizeNeg <| remImpl <| remEquiv <| toSkolemForm f
        

interiorizeDisj : FormulaLPO -> FormulaLPO
interiorizeDisj f =
    case f of
        (Disj (Conj f1 f2) g) -> interiorizeDisj (Conj (Disj (interiorizeDisj f1) (interiorizeDisj g)) (Disj (interiorizeDisj f2) (interiorizeDisj g)))

        (Disj g (Conj f1 f2)) -> interiorizeDisj (Conj (Disj (interiorizeDisj g) (interiorizeDisj f1)) (Disj (interiorizeDisj g) (interiorizeDisj f2)))

        (Conj f1 f2)-> Conj (interiorizeDisj f1)  (interiorizeDisj f2) 

        _ -> f


toCNF : FormulaLPO -> FormulaLPO
toCNF f = interiorizeDisj <| toNNF <| toSkolemForm f


interiorizeConj : FormulaLPO -> FormulaLPO  
interiorizeConj f= 
    case f of
        (Conj (Disj f1 f2) g) ->  interiorizeConj (Disj (Conj (interiorizeConj f1) (interiorizeConj g)) (Conj (interiorizeConj f2) (interiorizeConj g)))

        (Conj g (Disj f1 f2)) ->   interiorizeConj (Disj (Conj (interiorizeConj g) (interiorizeConj f1)) (Conj (interiorizeConj g) (interiorizeConj f2)))
    
        (Disj f1 f2)-> Disj (interiorizeConj f1)  (interiorizeConj f2) 

        _ -> f

toDNF : FormulaLPO -> FormulaLPO
toDNF f = interiorizeConj <| toNNF <| toSkolemForm f
        

    
   
    




                            
