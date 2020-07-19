module Modules.SemanticBoards exposing (..)

import List exposing (..)

import Modules.SintaxSemanticsLP exposing (..)
import Modules.LPNormalForms exposing (..)
import Modules.AuxiliarFunctions exposing (..)

import Modules.LP_Parser exposing (..)
import Html exposing (..)

-- Definition of isDobleNeg that represents if a formula is a doble negation

isDobleNeg : Prop -> Bool
isDobleNeg x =
    case x of
        Neg (Neg (Atom f)) -> True
        
        other -> False

-- Definition of isAlpha that represents if a formula is an alpha formula

isAlpha : Prop -> Bool
isAlpha x =
    case x of
        Neg (Neg _) -> True
        Conj _ _ -> True
        Neg (Disj _ _) -> True
        Neg (Impl _ _) -> True
        Equi _ _ -> True
        other -> False

-- Definition of isBeta that represents if a formula is a beta formula

isBeta : Prop -> Bool
isBeta x =
    case x of
        Disj _ _ -> True
        Impl _ _ -> True
        Neg (Conj _ _) -> True
        Neg (Equi _ _) -> True 
        other -> False

-- Definition of formulaComponents that represents the components of a formula

formulaComponents : Prop -> List Prop
formulaComponents x =
    case x of
        Atom p -> [Atom p]
        Neg (Atom p) -> [Neg (Atom p)]
        Neg (Neg f) -> [f]
        Conj f g -> [f, g]
        Neg (Impl f g) -> [f, Neg g]
        Neg (Disj f g) -> [Neg f, Neg g]
        Disj f g -> [f, g]
        Impl f g -> [Neg f , g]
        Neg (Conj f g) -> [Neg f, Neg g]
        Equi f g -> [Conj f g, Conj (Neg f) (Neg g)]
        Neg (Equi f g) -> [Conj f (Neg g), Conj (Neg f) g]
            
            
-- Definition of isLiteralSet that represents if the argument is a literal set.

isLiteralSet : List Prop -> Bool
isLiteralSet fs = List.all (\x -> literal x) fs

-- Definition of hasContradiction that represents if the argument has a formula and the negation of it.

hasContradiction : List Prop -> Bool
hasContradiction fs = List.any (\x -> member (Neg x) fs) fs

-- Definition of dnexpansion that represents de epansion of the argument with the doble negation of a formula that is in the 

dnExpansion : List Prop -> Prop -> List (List Prop)
dnExpansion fs f = [unionLs (deleteLs fs f) (formulaComponents f)]

-- Definition of alphaExpansion that represents de expansion of the argument with the expansion of an alpha formula

alphaExpansion : List Prop -> Prop -> List (List Prop)
alphaExpansion fs f = [unionLs (deleteLs fs f) (formulaComponents f)]

-- Definition of betaExpansion that represents the expansion of the argument with de expansion of a beta formula

betaExpansion : List Prop -> Prop -> List (List Prop)
betaExpansion fs f = List.map (\x -> unionLs (deleteLs fs f) [x]) (formulaComponents f)

-- definition of sucessors that representes the List of sucessor of a formula set

sucessors : List Prop -> List (List Prop)
sucessors fs = case List.head (List.filter (isDobleNeg) fs) of
    Just f -> dnExpansion fs f 

    Nothing -> case List.head (List.filter (isAlpha) fs) of
        Just f -> alphaExpansion fs f
            
        Nothing -> case  List.head (List.filter (isBeta) fs) of
            Just f -> betaExpansion fs f
        
            Nothing -> [fs]

-- definition of modelsTab that representes the models of a set of formulas using semantic boards
modelsTab : List Prop -> List (List Prop)     
modelsTab fs = if hasContradiction fs then
                    []
                else if isLiteralSet fs then
                    [fs]
                else 
                    List.concat <| List.map (\gs -> modelsTab gs) (sucessors fs)

generalModels : List (List Prop) -> List (List Prop) 
generalModels ps =  let generalModelsAux ms ls = case List.head ms of
                                                    Nothing -> ls
                                                    
                                                    Just m -> if List.any (\x -> isSubSet x m) ls then 
                                                                    generalModelsAux (deleteFirstLs ms) ls
                                                            else
                                                                    generalModelsAux (deleteFirstLs ms) (m::(List.filter (\x -> not (isSubSet m x)) ls ))
                    in
                        generalModelsAux ps []
            
         


-- definition of isTautBoard  that represents if a formula is a tautology using semantic boards
isTautBoard : Prop -> Bool
isTautBoard f = List.isEmpty <| modelsTab [Neg f]

-- definition of isConsecuenceBoard that solve logic consecuence using semantic boards.

isConsecuenceBoard : List Prop -> Prop -> Bool
isConsecuenceBoard fs f = List.isEmpty <| modelsTab <| (Neg f)::fs