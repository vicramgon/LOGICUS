module Modules.SintaxSemanticsLPO exposing (..)

-- IN PROGRESS

import List exposing (concat)
import Modules.AuxiliarFunctions as Aux exposing (unique)
import Graph exposing (Graph(..), Node, Edge, NodeId, fromNodesAndEdges)
import Graph.DOT exposing (outputWithStyles, defaultStyles)
import Maybe exposing (Maybe(..))

import Html exposing (Html, text)

type Term = Var String
          | Const String
          | Func String (List Term)

type alias Variable = Term
varsInTerm : Term -> List (Variable)
varsInTerm t =
    case t of
        Var x -> [Var x]
        Const _ -> []
        Func _ ts -> Aux.unique <| List.concat <| List.map varsInTerm <| ts

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

toStringTerm : Term -> String
toStringTerm x =
    case x of
        Var s-> s
        Const s -> s
        Func n params -> n ++ " (" ++ (String.join ", " <| List.map toStringTerm params) ++ ")"

toStringFormula : FormulaLPO -> String
toStringFormula x =
    case x of
        Pred n params -> n ++ " (" ++ (String.join ", " <| List.map toStringTerm params) ++ ")"
        Equal t1 t2 -> toStringTerm t1 ++ " = " ++ toStringTerm t2
        Neg p -> "¬ " ++ toStringFormula p
        Conj p q -> "( " ++ toStringFormula p ++ " ∧ "  ++ toStringFormula q ++ " )"
        Disj p q -> "( " ++ toStringFormula p ++ " ∨ "  ++ toStringFormula q ++ " )"
        Impl p q -> "( " ++ toStringFormula p ++ " ⟶ "  ++ toStringFormula q ++ " )"
        Equi p q -> "( " ++ toStringFormula p ++ " ⟷ "  ++ toStringFormula q ++ " )"
        Exists v p -> "∃" ++ toStringTerm v ++ " " ++ toStringFormula p  
        Forall v p -> "∀" ++ toStringTerm v ++ " " ++ toStringFormula p  
        Insat -> "⊥"

formTree : FormulaLPO -> Graph String ()
formTree x =
    case x of
        Pred _ _ -> fromNodesAndEdges [Node 0 (toStringFormula x)] []
        Equal _ _ -> fromNodesAndEdges [Node 0 (toStringFormula x)] []
        Neg p-> 
            let (nodes, edges) = formTreeAux p 1 in
                fromNodesAndEdges (Node 0 (toStringFormula x)::nodes) (Edge 0 1 ()::edges)
        Conj p q -> 
            let 
                (nodes1, edges1) = formTreeAux p 1
                (nodes2, edges2) = formTreeAux q 2
            in
                fromNodesAndEdges (Node 0 (toStringFormula x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 2 ()] ++ edges1 ++ edges2)
        Disj p q -> 
            let 
                (nodes1, edges1) = formTreeAux p 1
                (nodes2, edges2) = formTreeAux q 2
            in
                fromNodesAndEdges (Node 0 (toStringFormula x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 2 ()] ++ edges1 ++ edges2)
        Impl p q -> 
            let 
                (nodes1, edges1) = formTreeAux p 1
                (nodes2, edges2) = formTreeAux q 2
            in
                fromNodesAndEdges (Node 0 (toStringFormula x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 2 ()] ++ edges1 ++ edges2)
        Equi p q -> 
            let 
                (nodes1, edges1) = formTreeAux p 1
                (nodes2, edges2) = formTreeAux q 2
            in
                fromNodesAndEdges (Node 0 (toStringFormula x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 2 ()] ++ edges1 ++ edges2)
        Exists _ p ->
            let (nodes, edges) = formTreeAux p 1 in
                fromNodesAndEdges (Node 0 (toStringFormula x)::nodes) (Edge 0 1 ()::edges)
        Forall _ p ->
            let (nodes, edges) = formTreeAux p 1 in
                fromNodesAndEdges (Node 0 (toStringFormula x)::nodes) (Edge 0 1 ()::edges)
        Insat -> fromNodesAndEdges [Node 0 (toStringFormula x)] []

formTreeAux : FormulaLPO -> NodeId -> (List (Node String), List (Edge ()))
formTreeAux x nodeid=
    case x of
        Pred _ _ ->  ([Node nodeid (toStringFormula x)], [])
        Equal _ _ -> ([Node nodeid (toStringFormula x)], [])
        Neg p -> 
            let nextid = Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "1" in
                let (nodes, edges) = formTreeAux p nextid in
                (Node nodeid (toStringFormula x)::nodes, Edge nodeid nextid ()::edges)
        Conj p q -> 
            let 
                nextid1 =  Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "1"
                nextid2 = Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "2"
            in
                let 
                    (nodes1, edges1) = formTreeAux p nextid1
                    (nodes2, edges2) = formTreeAux q nextid2
                in
                   ( Node nodeid (toStringFormula x)::(nodes1 ++ nodes2),  [Edge nodeid nextid1 (), Edge nodeid nextid2 ()] ++ edges1 ++ edges2)
        Disj p q -> 
            let 
                nextid1 =  Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "1"
                nextid2 = Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "2"
            in
                let 
                    (nodes1, edges1) = formTreeAux p nextid1
                    (nodes2, edges2) = formTreeAux q nextid2
                in
                   ( Node nodeid (toStringFormula x)::(nodes1 ++ nodes2),  [Edge nodeid nextid1 (), Edge nodeid nextid2 ()] ++ edges1 ++ edges2)
        Impl p q -> 
            let 
                nextid1 =  Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "1"
                nextid2 = Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "2"
            in
                let 
                    (nodes1, edges1) = formTreeAux p nextid1
                    (nodes2, edges2) = formTreeAux q nextid2
                in
                   ( Node nodeid (toStringFormula x)::(nodes1 ++ nodes2),  [Edge nodeid nextid1 (), Edge nodeid nextid2 ()] ++ edges1 ++ edges2)
        Equi p q -> 
           let 
                nextid1 =  Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "1"
                nextid2 = Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "2" 
            in
                let 
                    (nodes1, edges1) = formTreeAux p nextid1
                    (nodes2, edges2) = formTreeAux q nextid2
                in
                   ( Node nodeid (toStringFormula x)::(nodes1 ++ nodes2),  [Edge nodeid nextid1 (), Edge nodeid nextid2 ()] ++ edges1 ++ edges2)
        Exists _ p -> 
            let nextid = Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "1" in
                let (nodes, edges) = formTreeAux p nextid in
                (Node nodeid (toStringFormula x)::nodes, Edge nodeid nextid ()::edges)
        Forall _ p -> 
            let nextid = Maybe.withDefault 0 <| String.toInt <| String.fromInt nodeid ++ "1" in
                let (nodes, edges) = formTreeAux p nextid in
                (Node nodeid (toStringFormula x)::nodes, Edge nodeid nextid ()::edges)
        Insat -> ([Node nodeid (toStringFormula x)], [])

formTree2DOT : Graph String () -> String
formTree2DOT ft =
    let myStyles =
            { defaultStyles | node = "shape=plaintext, color=black", edge = "dir=none"}
    in 
        outputWithStyles myStyles (\x -> Just x) (\_ -> Nothing) ft


ejemplo : FormulaLPO
ejemplo = Exists (Var "x") (Disj (Equal (Func "+" [ Var "y", Var "y"]) (Func "·" [ Var "x", Const "0"])) (Pred "<" [ Const "1",  Var "y"]))
main : Html msg
main = text <| formTree2DOT <| formTree ejemplo

