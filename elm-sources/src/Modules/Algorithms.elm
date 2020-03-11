module Modules.Algorithms exposing (semanticBoardAlg, boardToDOTString)

import List exposing (..)
import List.Extra exposing (..)
import Graph as G exposing (..)
import String exposing (..)
import Tuple exposing (..)

import IntDict exposing (..)

import Modules.SintaxSemanticsLP as SLP exposing (..)
import Modules.LPNormalForms exposing (..)
import Modules.AuxiliarFunctions as Aux exposing (..)
import Modules.SemanticBoards as SB exposing (..)

import Modules.LP_Parser exposing (..)
import Html exposing (..)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- SEMANTIC BOARDS (PROPOSITIONAL)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


semanticBoardAlg : List Prop -> Graph String String
semanticBoardAlg ps = semanticBoardAlgAux (G.insert ({ node = Node (0) (SLP.toStringSet ps), incoming = IntDict.empty, outgoing = IntDict.empty }) G.empty) [(0,ps)] 0

semanticBoardAlgAux : Graph String String -> List ((Int, List Prop))-> Int -> Graph String String
semanticBoardAlgAux g os nIdCounter = let ac = List.head os in
                                case ac of
                                    Nothing -> g
                
                                    Just (idAn, an) -> if  SB.hasContradiction an  then
                                                        let newNode =  { node = Node (nIdCounter + 1) "X", incoming = IntDict.singleton idAn "Contradiction", outgoing = IntDict.empty} in
                                                        let ng = G.insert newNode g in
                                                            semanticBoardAlgAux ng (Aux.deleteFirstLs os) (nIdCounter + 1)
                                                       else if SB.isLiteralSet an then 
                                                         let newNode =  { node = Node (nIdCounter + 1) "O", incoming = IntDict.singleton idAn "Model", outgoing = IntDict.empty} in
                                                         let ng = G.insert newNode g in
                                                            semanticBoardAlgAux ng (Aux.deleteFirstLs os) (nIdCounter + 1)
                                                       else
                                                        let (textBase, yBase) = extractBase an in 
                                                        let suc = SB.sucessors an in
                                                        let sucBase = List.concat <| SB.sucessors [yBase] in
                                                        let newProp =  List.map (\(x,y) -> (nIdCounter + x + 1, y)) <|indexedMap Tuple.pair suc in
                                                        let newPropBase = List.Extra.zip newProp sucBase in
                                                        let newNodes = if List.length suc == 1 then
                                                        
                                                                            List.map (\((x,y), z) -> { node = Node (x) (SLP.toStringSet y), 
                                                                                                        incoming = IntDict.singleton idAn (textBase ++  (SLP.toStringSet sucBase) ++ " ]"), 
                                                                                                        outgoing = IntDict.empty }) <| newPropBase
                                                                        else
                                                                            List.map (\((x,y), z) -> { node = Node (x) (SLP.toStringSet y), 
                                                                                             incoming = IntDict.singleton idAn (textBase ++  SLP.toStringSet [z] ++ " ]"), 
                                                                                             outgoing = IntDict.empty }) <| newPropBase

                                                        in
                                                        let ng = insertNodes newNodes g in
                                                            semanticBoardAlgAux ng (newProp ++ (Aux.deleteFirstLs os)) (nIdCounter + (List.length suc))
                                                        
extractBase : List Prop -> (String, Prop)
extractBase an = case List.head (List.filter (isDobleNeg) an) of
                        
                        Just y -> ("dN [" ++ SLP.toStringProp  y ++ " ⇒ ", y)
                
                        Nothing -> case List.head (List.filter (isAlpha) an) of
                                        
                                        Just z -> ("α [" ++ SLP.toStringProp z ++ " ⇒ " , z)

                                        Nothing -> case List.head (List.filter (isBeta) an) of
                                            
                                            Just w -> ("β [" ++ SLP.toStringProp w ++ " ⇒ ", w)

                                            Nothing -> ("", Atom "")

insertNodes : List (NodeContext n e) -> Graph n e -> Graph n e
insertNodes ls g = case List.head ls of
    Nothing -> g

    Just x -> G.insert x (insertNodes (Aux.deleteFirstLs ls) g)
        
boardToDOTString : Graph String String -> String
boardToDOTString g = "graph G { " ++ toStringNodes (nodes g) ++ toStringEdges2 (List.reverse (edges g)) ++ "}"

toStringNodes : List (Node String) -> String
toStringNodes ns = List.foldl (++) " " <| List.map (\x ->toStringNode x) <| List.reverse <| ns

toStringEdges : List (Edge String) -> String
toStringEdges es = List.foldl (++) " " <| List.map (\x -> String.fromInt x.from ++ " -> " ++ String.fromInt x.to ++ " [ fontcolor=\"grey\", label = \"" ++ x.label ++ "\"]; ") es

toStringEdges2 : List (Edge String) -> String
toStringEdges2 es = let (edg, rel) = toStringEdgesAux es 0 "" "OPERATIONS \\l" in
                        edg ++ "OPERATIONS [ fontcolor=\"#32465F\", fontname=\"times-bold\", shape = \"plaintext\", label = \"" ++ rel ++ "\"];" 


toStringEdgesAux es i ac rel = case List.head es of
    Just x -> let ac_p = ac ++ String.fromInt x.from ++ " -- " ++ String.fromInt x.to ++ " [style=\"setlinewidth(2)\", color=\"#32465F\", fontcolor=\"#32465F\", fontname=\"times-bold\", label = \"" ++ "(" ++ String.fromInt i ++ ")" ++ "\"]; " in
              let rel_p = rel ++ "(" ++ String.fromInt i ++ ")- " ++ x.label ++ "\\l " in
                toStringEdgesAux (Aux.deleteFirstLs es) (i+1) ac_p rel_p
        
    Nothing -> (ac, rel)
        

toStringNode : Node String -> String
toStringNode x =    if x.label == "X" then
                        String.fromInt x.id ++ " [ fontcolor=\"#32465F\", fontname=\"times-bold\", shape = \"plaintext\", label = \"" ++ "✖" ++ "\"]; "
                    else if x.label == "O" then
                        String.fromInt x.id ++ " [ fontcolor=\"#32465F\", fontname=\"times-bold\", shape = \"plaintext\", label = \"" ++ "◯" ++ "\"]; "
                    else
                        String.fromInt x.id ++ " [ style=\"setlinewidth(2)\", shape=\"box\", color=\"#32465F\", fontcolor=\"#32465F\", fontname=\"times-bold\" , label = \"" ++ x.label ++ "\"]; " 

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- NF FORMS USING ALGEBRA (PROPOSITIONAL)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


toCNFAlgebraAlg : Prop -> String
toCNFAlgebraAlg p = toNFAlgebraAlgAux p [] 1 'C'

toDNFAlgebraAlg : Prop -> String
toDNFAlgebraAlg p = toNFAlgebraAlgAux p [] 1 'D'

-- Definition of toNFAlgebraAlgAux that is the alghorithm that shows how to do CNF/DNF of a Formula (detailed dtep by step)
toNFAlgebraAlgAux : Prop -> List String -> Int -> Char -> String
toNFAlgebraAlgAux p s i c_d= 
    if containsEquiv p then
        let p_ = remOneEquiv p in 
            toNFAlgebraAlgAux (p_) (s ++ ["(", String.fromInt i, ") Remove equivalence", "\n", "Result: ", toStringProp p_]) (i+1) c_d
    else if containsImpl p then 
        let p_ = remOneImpl p in 
            toNFAlgebraAlgAux (p_) (s ++ ["(", String.fromInt i, ") Remove implication", "\n", "Result: ", toStringProp p_]) (i+1) c_d
    else if deMorganIsApplicable p then
        let p_ = applyOneDeMorgan p in
            toNFAlgebraAlgAux p_ (s ++ ["(", String.fromInt i, ") Interiorize negation (de Morgan)", "\n", "Result: ", toStringProp p_]) (i+1) c_d
    else if areDobleNegation p then
        let p_ = removeDoubleNegations p in
            toNFAlgebraAlgAux p_ (s ++ ["(", String.fromInt i, ") Remove double negations", "\n", "Result: ", toStringProp p_]) (i+1) c_d
    else if ((c_d == 'C') && (isApplicableDistribORReAND p)) then
        let p_ = applyOneDistrOrReAnd p in
            toNFAlgebraAlgAux p_ (s ++ ["(", String.fromInt i, ") Distributive OR regard to AND", "\n", "Result: ", toStringProp p_]) (i+1) c_d
    else if ((c_d == 'D') && (isApplicableDistribANDReOR p)) then
        let p_ = applyOneDistrAndReOr p in
            toNFAlgebraAlgAux p_ (s ++ ["(", String.fromInt i, ") Distributive AND regard to OR", "\n", "Result: ", toStringProp p_]) (i+1) c_d
    else if (c_d == 'C') then
        let p_ = disjunctionSetToConjunction <| List.map (literalsSetToDisjunctions) <| absortionNF (List.map (literalsNNF) <| extractTermsCNF <| p) [] in
           String.concat <| s ++ 
            ["(", String.fromInt i, ") Idempotence and Absorption laws", "\n", "Result: ", toStringProp p_] ++ 
                ["\n" , "\n", "CNF Formula: ", toStringProp p_]
    else
        let p_ = conjunctionSetToDisjunction <| List.map (literalsSetToConjunctions) <| absortionNF (List.map (literalsNNF) <| extractTermsDNF <| p) [] in
           String.concat <| s ++ 
            ["(", String.fromInt i, ") Idempotence and Absorption laws", "\n", "Result: ", toStringProp p_] ++ 
                ["\n" , "\n", "DNF Formula: ", toStringProp p_]


remOneEquiv : Prop -> Prop
remOneEquiv f =
    case f of
        Atom x-> Atom x
            
        Neg x -> Neg (remOneEquiv x)

        Conj x y -> if containsEquiv x then Conj (remOneEquiv x) y else Conj x (remOneEquiv y)

        Disj x y -> if containsEquiv x then Disj (remOneEquiv x) y else Disj x (remOneEquiv y)

        Impl x y -> if containsEquiv x then Impl (remOneEquiv x) y else Impl x (remOneEquiv y)

        Equi x y -> Conj (Impl (remEquiv x) (remEquiv y)) (Impl (remEquiv y) (remEquiv x))

remOneImpl : Prop -> Prop
remOneImpl f =
    case f of
            
        Neg x -> Neg (remOneImpl x)

        Conj x y -> if containsImpl x then Conj (remOneImpl x) y else Conj x (remOneImpl y)

        Disj x y -> if containsImpl x then Disj (remOneImpl x) y else Disj x (remOneImpl y)

        Impl x y -> Disj (Neg x) y

        _ -> f

applyOneDeMorgan : Prop -> Prop 
applyOneDeMorgan f =
    case f of

        Neg (Conj x y) -> Disj (Neg x) (Neg y)

        Neg (Disj x y) -> Conj (Neg x) (Neg y)

        Neg x -> Neg(applyOneDeMorgan x) 

        Conj x y -> if deMorganIsApplicable x then Conj (applyOneDeMorgan x) y else Conj x (applyOneDeMorgan y)

        Disj x y -> if deMorganIsApplicable x then Disj (applyOneDeMorgan x) y else Disj x (applyOneDeMorgan y)

        Impl x y -> Disj (Neg x) y

        _ -> f

removeDoubleNegations : Prop -> Prop
removeDoubleNegations f =
    case f of
        Neg (Neg x) -> x

        Neg x -> Neg (removeDoubleNegations x)

        Conj x y -> Conj (removeDoubleNegations x) (removeDoubleNegations y)

        Disj x y -> Disj (removeDoubleNegations x) (removeDoubleNegations y)

        _ -> f

areDobleNegation : Prop -> Bool
areDobleNegation f =
    case f of
        Neg (Neg x) -> True

        Neg x -> areDobleNegation x

        Conj x y -> (areDobleNegation x) || (areDobleNegation y)

        Disj x y -> (areDobleNegation x) || (areDobleNegation y)

        _ -> False

isApplicableDistribORReAND : Prop -> Bool
isApplicableDistribORReAND f =
    case f of
        Atom _ -> False

        Neg x -> (isApplicableDistribORReAND x)

        Conj x y -> (isApplicableDistribORReAND x) || (isApplicableDistribORReAND y)

        Disj (Conj _ _) _-> True 

        Disj _ (Conj _ _)-> True

        Disj x y -> (isApplicableDistribORReAND x) || (isApplicableDistribORReAND y)
        
        _ -> False

isApplicableDistribANDReOR : Prop -> Bool
isApplicableDistribANDReOR f =
    case f of
        Atom _ -> False

        Neg x -> (isApplicableDistribANDReOR x)

        Conj (Disj _ _) _-> True 

        Conj _ (Disj _ _)-> True

        Conj x y -> (isApplicableDistribANDReOR x) || (isApplicableDistribANDReOR y)

        Disj x y -> (isApplicableDistribANDReOR x) || (isApplicableDistribANDReOR y)
        
        _ -> False

applyOneDistrOrReAnd : Prop -> Prop
applyOneDistrOrReAnd f = 
    case f of
        (Disj (Conj f1 f2) g) -> (Conj (Disj f1 g) (Disj f2 g))
        
        (Disj g (Conj f1 f2)) -> (Conj (Disj g f1) (Disj g f2))

        (Disj x y) -> if isApplicableDistribORReAND x then Disj (applyOneDistrOrReAnd x) y else Disj x (applyOneDistrOrReAnd y)

        Conj x y -> if isApplicableDistribORReAND x then Conj (applyOneDistrOrReAnd x) y else Conj x (applyOneDistrOrReAnd y)
        
        _ -> f

applyOneDistrAndReOr : Prop -> Prop
applyOneDistrAndReOr f = 
    case f of
        (Conj (Disj f1 f2) g) -> (Disj (Conj f1 g) (Conj f2 g))
        
        (Conj g (Disj f1 f2)) -> (Disj (Conj g f1) (Conj g f2))

        (Conj x y) -> if isApplicableDistribANDReOR x then Conj (applyOneDistrAndReOr x) y else Conj x (applyOneDistrAndReOr y)

        Disj x y -> if isApplicableDistribANDReOR x then Disj (applyOneDistrAndReOr x) y else Disj x (applyOneDistrAndReOr y)
        
        _ -> f


extractTermsCNF : Prop -> List (Prop)
extractTermsCNF p =
    case p of
        Conj f1 f2 -> (extractTermsCNF f1) ++ (extractTermsCNF f2)      
        _ -> [p]

extractTermsDNF : Prop -> List (Prop)
extractTermsDNF p =
    case p of
        Disj f1 f2 -> (extractTermsDNF f1) ++ (extractTermsDNF f2)      
        _ -> [p]

literalsSetToDisjunctions : List Prop -> Prop
literalsSetToDisjunctions ls = 
    case ls of
        (x::y::xs) -> List.foldr (\x_ ac -> Disj x_ ac) x (y::xs)
        [x] -> x
        other -> Disj (Atom "p") (Neg (Atom "p"))

literalsSetToConjunctions : List Prop -> Prop
literalsSetToConjunctions ls = 
    case ls of
        (x::y::xs) -> List.foldr (\x_ ac -> Conj x_ ac) x (y::xs)
        [x] -> x
        other -> Disj (Atom "p") (Neg (Atom "p"))

disjunctionSetToConjunction : List Prop -> Prop
disjunctionSetToConjunction ls = 
     case ls of
        (x::y::xs) -> List.foldr (\x_ ac -> Conj x_ ac) x (y::xs)
        [x] -> x
        other -> Disj (Atom "p") (Neg (Atom "p"))

conjunctionSetToDisjunction : List Prop -> Prop
conjunctionSetToDisjunction ls = 
     case ls of
        (x::y::xs) -> List.foldr (\x_ ac -> Disj x_ ac) x (y::xs)
        [x] -> x
        other -> Disj (Atom "p") (Neg (Atom "p"))

absortionNF : List (List Prop) -> List (List Prop) -> List (List Prop)
absortionNF in_s out_s = 
    case in_s of
        [] -> out_s
        (x::xs)-> 
            if not (List.any (\e -> Aux.isSubSet e x) out_s) then
                absortionNF (xs) (List.foldr (\e ac -> if Aux.isSubSet x e then ac else e::ac) [x] out_s)
            else
                absortionNF (xs) (out_s)

main =  text <| toDNFAlgebraAlg <| parserFormula "(¬p & q) -> (q | r) -> p"


