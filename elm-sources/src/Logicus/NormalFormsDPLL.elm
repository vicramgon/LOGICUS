module Logicus.NormalFormsDPLL exposing (..)

import List exposing (..)
import Html exposing (..)

import Logicus.SintaxSemanticsLP exposing (..)
import Logicus.AuxiliarFunctions exposing (..)
import Logicus.IO_LP exposing (..)
import Set
import Graph exposing (NodeId, Node, Edge, fromNodesAndEdges)
import Graph.DOT exposing (outputWithStyles, defaultStyles)
import Dict exposing (Dict)



areEquiv : FormulaLP -> FormulaLP -> Bool
areEquiv f g = validity (Equi f g)

type alias Literal  = FormulaLP

literal : FormulaLP -> Bool
literal f =
    case f of
        Atom _ -> True

        Neg (Atom _) -> True
            
        _ -> False

literalComplement : Literal -> Literal
literalComplement f=
    case f of
        Atom x -> Neg (Atom x)
            
        Neg (Atom x) -> Atom x

        _ -> f

containsEquiv : FormulaLP -> Bool
containsEquiv f =
    case f of
        Atom _-> False
            
        Neg x -> containsEquiv x

        Conj x y -> containsEquiv x || containsEquiv y

        Disj x y -> containsEquiv x || containsEquiv y

        Impl x y -> containsEquiv x || containsEquiv y

        Equi _ _ -> True

        Insat -> False 

remEquiv : FormulaLP -> FormulaLP
remEquiv f =
    case f of
        Atom x-> Atom x
            
        Neg x -> Neg (remEquiv x)

        Conj x y -> Conj (remEquiv x) (remEquiv y)

        Disj x y -> Disj (remEquiv x) (remEquiv y)

        Impl x y -> Impl (remEquiv x) (remEquiv y)

        Equi x y -> Conj (Impl (remEquiv x) (remEquiv y)) (Impl (remEquiv y) (remEquiv x))

        Insat -> Insat


containsImpl : FormulaLP -> Bool
containsImpl f =
    case f of
        Atom _-> False
            
        Neg x -> containsImpl x

        Conj x y -> containsImpl x || containsImpl y

        Disj x y -> containsImpl x || containsImpl y

        Impl _ _ -> True

        Equi x y -> containsImpl x || containsImpl y

        Insat -> False
    
remImpl : FormulaLP -> FormulaLP
remImpl f =
    case f of
        Atom x-> Atom x
            
        Neg x -> Neg (remImpl x)

        Conj x y -> Conj (remImpl x) (remImpl y)

        Disj x y -> Disj (remImpl x) (remImpl y)

        Impl x y-> Disj (Neg (remImpl x)) (remImpl y)

        Equi x y-> Equi (remImpl x) (remImpl y)

        Insat -> Insat

interiorizeNeg : FormulaLP -> FormulaLP
interiorizeNeg f=
    case f of
        Atom x -> Atom x

        Neg x ->  interiorizeNegAux x

        Conj x y -> Conj (interiorizeNeg x) (interiorizeNeg y)

        Disj x y -> Disj (interiorizeNeg x) (interiorizeNeg y)

        Impl x y -> Impl (interiorizeNeg x) (interiorizeNeg y)

        Equi x y -> Equi (interiorizeNeg x) (interiorizeNeg y)

        Insat -> Insat

interiorizeNegAux : FormulaLP -> FormulaLP
interiorizeNegAux f=
    case f of
        Atom x -> Neg (Atom x)

        Neg x -> x

        Conj x y -> Disj (interiorizeNegAux x) (interiorizeNegAux  y)

        Disj x y -> Conj (interiorizeNegAux x) (interiorizeNegAux y)

        Impl x y -> Conj x (interiorizeNeg y)

        Equi x y -> Disj (Conj (interiorizeNeg x) y) (Conj x (interiorizeNeg y)) 

        Insat -> Insat


toNNF : FormulaLP -> FormulaLP
toNNF f = interiorizeNeg <| remImpl <| remEquiv f
        

interiorizeDisj : FormulaLP -> FormulaLP
interiorizeDisj f =
    case f of
        (Disj (Conj f1 f2) g) -> interiorizeDisj (Conj (Disj (interiorizeDisj f1) (interiorizeDisj g)) (Disj (interiorizeDisj f2) (interiorizeDisj g)))

        (Disj g (Conj f1 f2)) -> interiorizeDisj (Conj (Disj (interiorizeDisj g) (interiorizeDisj f1)) (Disj ( interiorizeDisj g) (interiorizeDisj f2)))

        (Conj f1 f2)-> Conj (interiorizeDisj f1)  (interiorizeDisj f2) 

        _ -> f


toCNF : FormulaLP -> FormulaLP 
toCNF f = interiorizeDisj (toNNF f)


interiorizeConj : FormulaLP -> FormulaLP   
interiorizeConj f= 
    case f of
        (Conj (Disj f1 f2) g) ->  interiorizeConj (Disj (Conj (interiorizeConj f1) (interiorizeConj g)) (Conj (interiorizeConj f2) (interiorizeConj g)))

        (Conj g (Disj f1 f2)) ->   interiorizeConj (Disj (Conj (interiorizeConj g) (interiorizeConj f1)) (Conj (interiorizeConj g) (interiorizeConj f2)))
    
        (Disj f1 f2)-> Disj (interiorizeConj f1)  (interiorizeConj f2) 

        _ -> f

toDNF : FormulaLP -> FormulaLP 
toDNF f = interiorizeConj (toNNF f)             


type alias Clause = List Literal

toClauseCNF : FormulaLP -> List Clause
toClauseCNF x =
    case x of
        Conj f g ->
            unionLs (toClauseCNF f) (toClauseCNF g)

        _ ->
            [ toClauseCNFAux x ]

toClauseCNFAux : FormulaLP -> Clause
toClauseCNFAux x =
    if literal x then
        [ x ]

    else
        case x of
            Disj f g ->
                unionLs (toClauseCNFAux f) (toClauseCNFAux g)

            _ ->
                []

toClause : FormulaLP -> List Clause
toClause x =
    toClauseCNF <| toCNF x


areEqClauses : Clause -> Clause -> Bool

areEqClauses a b =
     List.all (\x -> member x a) b && List.all (\x -> member x b) a


filterEqClauses : List Clause -> List Clause
filterEqClauses xs =
    uniqueBy areEqClauses xs

haveContraryLiteral : Clause -> Bool
haveContraryLiteral x =
    List.any (\y -> member (Neg y) x) x


filterTautClauses : List Clause -> List Clause
filterTautClauses xs =
    List.filter (\x -> not (haveContraryLiteral x)) xs


setClauses : List FormulaLP -> List Clause
setClauses xs =
    filterTautClauses <| filterEqClauses <| concat <| List.map toClause xs


symbolsClause : Clause -> List PSymb
symbolsClause x = Set.toList <| setSymbols x


symbolsClauseSet : List Clause -> List PSymb
symbolsClauseSet xs =
    unique <| concat <| List.map symbolsClause xs


clauseInterpretations : Clause -> List Interpretation
clauseInterpretations x =
    powerset <| symbolsClause x


clauseSetInterpretations : List Clause -> List Interpretation
clauseSetInterpretations xs =
    powerset <| symbolsClauseSet xs


isLiteralModel : Interpretation -> Literal -> Bool
isLiteralModel i l =
    case l of
        Atom x ->
            member x i

        Neg (Atom x) ->
            not <| member x i

        _ -> False

isClausalModel : Interpretation -> Clause -> Bool
isClausalModel i c =
    any (\y -> y) <| List.map (\l -> isLiteralModel i l) c



clausalModels : Clause -> List Interpretation
clausalModels c =
    List.filter (\x -> isClausalModel x c) <| clauseInterpretations c


isClausalSetModel : Interpretation -> List Clause -> Bool
isClausalSetModel i cs =
    all (\y -> y) <| List.map (\c -> isClausalModel i c) cs



clausalSetModels : List Clause -> List Interpretation
clausalSetModels cs =
    List.filter (\x -> isClausalSetModel x cs) <| clauseSetInterpretations cs


validClause : Clause -> Bool
validClause =
    haveContraryLiteral


insatisfactibleClause : Clause -> Bool
insatisfactibleClause =
    List.isEmpty



satisfactibleClause : Clause -> Bool
satisfactibleClause c =
    not <| insatisfactibleClause c



validClauseSet : List Clause -> Bool
validClauseSet cs =
    List.all (\x -> x) <| List.map validClause cs



consistenceClauseSet : List Clause -> Bool
consistenceClauseSet cs =
    not <| List.isEmpty <| clausalSetModels cs



inconsistenceClauseSet : List Clause -> Bool
inconsistenceClauseSet cs =
    List.isEmpty <| clausalSetModels cs



-- Definition of propValidwidthClauses that represents if the clause set of a formula is valid


propValidByClauses : FormulaLP -> Bool
propValidByClauses p =
    validClauseSet <| toClause <| p

isConsecuenceByClauses : List FormulaLP -> FormulaLP -> Bool
isConsecuenceByClauses fs f =
    let
        s1 = setClauses fs
        s2 = toClause f
    in
    
    List.isEmpty <|
        List.filter
            (\x ->
                not (isClausalSetModel x s2)
                    && isClausalSetModel x s1
            )
        <|
            clauseSetInterpretations (s1 ++ s2)


-- DPLL Algorithm
literalPsymb : Literal -> Maybe PSymb
literalPsymb l =
    case l of 
        Atom x -> Just x
        Neg (Atom x) -> Just x
        _ -> Nothing 

dpllGraph : List Clause -> String
dpllGraph clauses =
    let myStyles =
            { defaultStyles | node = "shape=box, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
        nodes = [Node 0 ("{" ++ (String.join "," <| List.map (\x -> toStringFLPSet x) clauses) ++ "}")]
        psymbs = List.foldl (\ c ac ->  merge2 ac (Dict.fromList <| List.map (\x-> (x,1)) (Set.toList <| setSymbols c)) (\v1 v2 -> Maybe.withDefault 0 v1 + Maybe.withDefault 0 v2)) Dict.empty clauses
        
    in
        String.replace "\n\n" "\n" <| outputWithStyles myStyles (\x -> Just x) (\x-> Just x) <|  uncurry fromNodesAndEdges <| dpllGraphAux clauses psymbs 0 (nodes, [])

dpllGraphAux : List Clause -> Dict PSymb Int-> NodeId -> (List (Node String), List(Edge String)) -> (List (Node String), List(Edge String))
dpllGraphAux clauses psymbs nidp (nodes, edges) =
    let
        nid = List.length nodes
    in
        case  List.head <| List.filter (\x -> List.length x == 1) clauses of
            Just [l] -> 
                let
                    (new_clauses, new_psymbs) = 
                        List.foldl (\x (cl, ps) -> 
                            
                            if List.member l x then 
                                (cl, Dict.insert (Maybe.withDefault "" (literalPsymb l)) ((Maybe.withDefault 1 <| Dict.get (Maybe.withDefault "" (literalPsymb l)) ps)-1) ps)
                            else 
                                let
                                    new_x = List.filter (\ y -> y /= literalComplement l) x
                                in
                                    if new_x == x then 
                                        (cl ++ [new_x], ps)
                                    else
                                        (cl ++ [new_x], Dict.insert (Maybe.withDefault "" (literalPsymb l)) ((Maybe.withDefault 1 <| Dict.get (Maybe.withDefault "" (literalPsymb l)) ps)-1) ps)
                        ) ([], psymbs)  clauses
                in
                    if List.length new_clauses == 0 then 
                        (nodes ++ [Node nid "{}"], edges ++ [Edge nidp nid (toStringFLP l)])
                    else if List.any (\x -> List.length x == 0) new_clauses then 
                        (nodes ++ [Node nid "{□}"], edges ++ [Edge nidp nid (toStringFLP l)])
                    else
                        dpllGraphAux new_clauses (Dict.remove (Maybe.withDefault "" (literalPsymb l)) new_psymbs) nid (nodes ++ [Node nid ("{" ++ (String.join "," <| List.map (\x -> toStringFLPSet x) new_clauses) ++ "}")], edges ++ [Edge nidp nid (toStringFLP l)])
            _ -> 
                case List.head <| List.sortBy (\(_,v)-> v) <| List.filter (\ (_,v) -> v > 0) <| Dict.toList psymbs of
                    Just (lsymb, _) ->
                        let
                            (nodes1 , edges1) = dpllGraphAux (clauses ++ [[Atom lsymb]]) psymbs nidp (nodes, edges)
                        in
                            dpllGraphAux (clauses ++ [[Neg (Atom lsymb)]]) psymbs nidp (nodes1 , edges1)

                    Nothing -> (nodes ++ [Node nid "{}"], edges ++ [Edge nidp nid ""])


dpllModels : List Clause -> List (List Literal)
dpllModels clauses =
    let
        psymbs = List.foldl (\ c ac ->  merge2 ac (Dict.fromList <| List.map (\x-> (x,1)) (Set.toList <| setSymbols c)) (\v1 v2 -> Maybe.withDefault 0 v1 + Maybe.withDefault 0 v2)) Dict.empty clauses
    in
        dpllModelsAux clauses psymbs []

    

dpllModelsAux : List Clause -> Dict PSymb Int-> List Literal -> List (List Literal)
dpllModelsAux clauses psymbs actualModel =
    case  List.head <| List.filter (\x -> List.length x == 1) clauses of
        Just [l] -> 
            let
                (new_clauses, new_psymbs) = 
                    List.foldl (\x (cl, ps) -> 
                        
                        if List.member l x then 
                            (cl, Dict.insert (Maybe.withDefault "" (literalPsymb l)) ((Maybe.withDefault 1 <| Dict.get (Maybe.withDefault "" (literalPsymb l)) ps)-1) ps)
                        else 
                            let
                                new_x = List.filter (\ y -> y /= literalComplement l) x
                            in
                                if new_x == x then 
                                    (cl ++ [new_x], ps)
                                else
                                    (cl ++ [new_x], Dict.insert (Maybe.withDefault "" (literalPsymb l)) ((Maybe.withDefault 1 <| Dict.get (Maybe.withDefault "" (literalPsymb l)) ps)-1) ps)
                    ) ([], psymbs)  clauses
            in
                if List.any (\x -> List.length x == 0) new_clauses then 
                    []
                else
                    dpllModelsAux new_clauses (Dict.remove (Maybe.withDefault "" (literalPsymb l)) new_psymbs) (actualModel ++ [l])
        _ -> 
            case List.head <| List.sortBy (\(_,v)-> v) <| List.filter (\ (_,v) -> v > 0) <| Dict.toList psymbs of
                Just (lsymb, _) ->
                    dpllModelsAux (clauses ++ [[Atom lsymb]]) psymbs actualModel ++ dpllModelsAux (clauses ++ [[Neg (Atom lsymb)]]) psymbs actualModel

                Nothing -> [actualModel]


toStringClauseSet : List Clause -> String
toStringClauseSet clauses =
    "{" ++ (String.join "," <| List.map (\x -> toStringFLPSet x) clauses) ++ "}"

toLatexClauseSet : List Clause -> String
toLatexClauseSet clauses =
    "$\\left\\lbrace" ++ (String.join ", \\, " <| List.map (\x -> toLatexClause x) clauses) ++ "\\right\\rbrace$"

toLatexClause : Clause -> String
toLatexClause clause = 
  "\\left\\lbrace " ++ (String.join ", \\," <| List.map toLatexLiteral clause) ++ "\\right\\rbrace"

toLatexLiteral : Literal -> String                  
toLatexLiteral l =
    case l of    
        Atom p -> p
        Neg (Atom p) -> "\\neg " ++ p
        _ -> ""                             
                                    

                        

    



        