module Modules.Algorithms exposing (semanticBoardAlg, boardToDOTString)

import List exposing (..)
import List.Extra exposing (..)
import Graph as G exposing (..)

import IntDict exposing (..)

import Modules.SintaxSemanticsLP as SLP exposing (..)
import Modules.LPNormalForms exposing (..)
import Modules.AuxiliarFunctions as Aux exposing (..)
import Modules.SemanticBoards as SB exposing (..)

import Modules.LP_Parser exposing (..)
import Html exposing (..)

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
boardToDOTString g = "digraph G { " ++ toStringNodes (nodes g) ++ toStringEdges2 (List.reverse (edges g)) ++ "}"

toStringNodes : List (Node String) -> String
toStringNodes ns = List.foldl (++) " " <| List.map (\x ->toStringNode x) <| List.reverse <| ns

toStringEdges : List (Edge String) -> String
toStringEdges es = List.foldl (++) " " <| List.map (\x -> String.fromInt x.from ++ " -> " ++ String.fromInt x.to ++ " [ fontcolor=\"grey\", label = \"" ++ x.label ++ "\"]; ") es

toStringEdges2 : List (Edge String) -> String
toStringEdges2 es = let (edg, rel) = toStringEdgesAux es 0 "" "OPERATIONS \\l" in
                        edg ++ "OPERATIONS [shape = \"plaintext\", label = \"" ++ rel ++ "\"];" 


toStringEdgesAux es i ac rel = case List.head es of
    Just x -> let ac_p = ac ++ String.fromInt x.from ++ " -> " ++ String.fromInt x.to ++ " [ label = \"" ++ "(" ++ String.fromInt i ++ ")" ++ "\"]; " in
              let rel_p = rel ++ "(" ++ String.fromInt i ++ ")- " ++ x.label ++ "\\l " in
                toStringEdgesAux (Aux.deleteFirstLs es) (i+1) ac_p rel_p
        
    Nothing -> (ac, rel)
        

toStringNode : Node String -> String
toStringNode x =    if x.label == "X" then
                        String.fromInt x.id ++ " [ shape = \"plaintext\", label = \"" ++ "✖" ++ "\"]; "
                    else if x.label == "O" then
                        String.fromInt x.id ++ " [ shape = \"plaintext\", label = \"" ++ "◯" ++ "\"]; "
                    else
                        String.fromInt x.id ++ " [ shape = \"box\" , label = \"" ++ x.label ++ "\"]; " 

main = text <| boardToDOTString <| semanticBoardAlg <| parserSet "(p | q -> r)&(t -> ¬r)&¬(¬t->q);"

