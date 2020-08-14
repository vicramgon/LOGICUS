module Logicus.SemanticBoardsLP exposing (..)

import Logicus.SintaxSemanticsLP exposing (FormulaLP(..))
import Logicus.AuxiliarFunctions exposing (uniqueConcatList, isSubSetList, deleteFirstLs, uncurry)
import List.Extra exposing (remove)
import List exposing (member)
import Graph exposing (NodeId, Node, Edge, fromNodesAndEdges)
import Logicus.IO_LP exposing (toStringFLPSet, toStringFLP)
import Graph.DOT exposing (outputWithStyles, defaultStyles)




isInsat : FormulaLP -> Bool 
isInsat x =
    case x of
        Insat -> True
        Neg (Insat) -> True
        _ -> False

-- Definition of isDobleNeg that represents if a formula is a doble negation

isDobleNeg : FormulaLP -> Bool
isDobleNeg x =
    case x of
        Neg ( Neg ( Atom _ ) ) -> True
        
        _ -> False

-- Definition of isAlpha that represents if a formula is an alpha formula

isAlpha : FormulaLP -> Bool
isAlpha x =
    case x of
        Neg (Neg _) -> True
        Conj _ _ -> True
        Neg (Disj _ _) -> True
        Neg (Impl _ _) -> True
        Equi _ _ -> True
        _ -> False

-- Definition of isBeta that represents if a formula is a beta formula

isBeta : FormulaLP -> Bool
isBeta x =
    case x of
        Disj _ _ -> True
        Impl _ _ -> True
        Neg (Conj _ _) -> True
        Neg (Equi _ _) -> True 
        _ -> False

-- Definition of formulaComponents that represents the components of a formula

formulaComponents : FormulaLP -> List FormulaLP
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
        Equi f g -> [Impl f g, Impl g f]
        Neg (Equi f g) -> [Neg(Impl f g), Neg(Impl g f)]
        _ -> [Insat]

-- Definition of isLiteral that represents if a formula is a isLiteral 

isLiteral : FormulaLP -> Bool
isLiteral x =
    case x of 
        Atom _ -> True
        Neg (Atom _) -> True
        _ -> False

            
-- Definition of isLiteralSet that represents if the argument is a isLiteral set.

isLiteralSet : List FormulaLP -> Bool
isLiteralSet fs = List.all (\x -> isLiteral x) fs

-- Definition of hasContradiction that represents if the argument has a formula and the negation of it.

hasContradiction : List FormulaLP -> Bool
hasContradiction fs = List.any (\x -> member (Neg x) fs || isInsat x) fs

-- Definition of dnexpansion that represents de epansion of the argument with the doble negation of a formula that is in the set

dnExpansion : List FormulaLP -> FormulaLP -> List (List FormulaLP)
dnExpansion fs f = [uniqueConcatList (remove f fs) (formulaComponents f)]

-- Definition of alphaExpansion that represents de expansion of the argument with the expansion of an alpha formula

alphaExpansion : List FormulaLP -> FormulaLP -> List (List FormulaLP)
alphaExpansion fs f = [uniqueConcatList (remove f fs) (formulaComponents f)]

-- Definition of betaExpansion that represents the expansion of the argument with de expansion of a beta formula

betaExpansion : List FormulaLP -> FormulaLP -> List (List FormulaLP)
betaExpansion fs f = 
    let fs2 = remove f fs in
        List.map (\x -> uniqueConcatList fs2 [x]) (formulaComponents f)

-- definition of sucessors that representes the List of sucessor of a formula set

sucessors : List FormulaLP -> List (List FormulaLP)
sucessors fs = case List.head (List.filter isDobleNeg fs) of
    Just f -> dnExpansion fs f 

    Nothing -> case List.head (List.filter  isAlpha fs) of
        Just f -> alphaExpansion fs f
            
        Nothing -> case  List.head (List.filter  isBeta fs) of
            Just f -> betaExpansion fs f
        
            Nothing -> [fs]


makeSBoard : List FormulaLP -> String 
makeSBoard fs = 
    let myStyles =
            { defaultStyles | node = "shape=box, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
    in
        String.replace "\n\n" "\n" <| outputWithStyles myStyles (\x -> Just x) (\x-> Just x) <|  uncurry fromNodesAndEdges <| makeSBoardAux fs 0
        
        
makeSBoardAux : List FormulaLP -> NodeId -> (List (Node String), List (Edge String))
makeSBoardAux fs nid =
    let 
        actualNode = Node nid (toStringFLPSet fs)
    in
        if  hasContradiction fs then
                ([actualNode, Node (nid + 1) "╳"], [Edge  nid  (nid + 1) ""])

        else if isLiteralSet fs then
                    ([actualNode, Node (nid + 1) "◯"], [Edge  nid  (nid + 1) ""])
        else
            case List.head (List.filter isDobleNeg fs) of
                Just f ->
                    let
                        edgeLabel = "dN : " ++  toStringFLP f ++ "\n ⟹ " ++ (toStringFLPSet <| formulaComponents f)
                        (nodes, edges) = makeSBoardAux (List.concat <| dnExpansion fs f) (nid + 1) 
                    in
                        (actualNode::nodes,  Edge nid (nid + 1) edgeLabel::edges)
                Nothing ->
                    case List.head (List.filter isAlpha fs) of
                        Just f ->
                            let
                                edgeLabel = "α : " ++  toStringFLP f ++ "\n ⟹ " ++ (toStringFLPSet <| formulaComponents f)
                                (nodes, edges) = makeSBoardAux (List.concat <| alphaExpansion fs f) (nid + 1) 
                            in
                                (actualNode::nodes,  Edge nid (nid + 1) edgeLabel::edges)
                        Nothing ->
                            case List.head (List.filter  isBeta fs) of
                                Just f ->
                                    case formulaComponents f of
                                        [f1, f2] -> 
                                            let
                                                edgeLabel1 = " β: " ++  toStringFLP f ++ "\n ⟹ {" ++  toStringFLP f1 ++ "}"
                                                edgeLabel2 = " β: " ++  toStringFLP f ++ "\n ⟹ {" ++  toStringFLP f2 ++ "}"
                                                nfs =  remove f fs
                                            in
                                                let (nodes1, edges1) = makeSBoardAux (uniqueConcatList nfs [f1]) (nid + 1) in
                                                    let nextid = nid + List.length nodes1 + 1 in
                                                        let (nodes2, edges2) = makeSBoardAux (uniqueConcatList nfs [f2]) nextid in
                                                            (actualNode::(nodes1 ++ nodes2), [Edge nid (nid + 1) edgeLabel1, Edge nid nextid edgeLabel2] ++ edges1 ++ edges2)
                                        _ -> ([],[])
                                _ -> ([],[])



                


            

-- definition of modelsTab that representes the models of a set of formulas using semantic boards
modelsTab : List FormulaLP -> List (List FormulaLP)     
modelsTab fs = if hasContradiction fs then
                    []
                else if isLiteralSet fs then
                    [fs]
                else 
                    List.concat <| List.map (\gs -> modelsTab gs) (sucessors fs)

generalModels : List (List FormulaLP) -> List (List FormulaLP) 
generalModels ps =  let generalModelsAux ms ls = case List.head ms of
                                                    Nothing -> ls
                                                    
                                                    Just m -> if List.any (\x -> isSubSetList x m) ls then 
                                                                    generalModelsAux (deleteFirstLs ms) ls
                                                            else
                                                                    generalModelsAux (deleteFirstLs ms) (m:: List.filter (\x -> not (isSubSetList m x)) ls )
                    in
                        generalModelsAux ps []
            

-- definition of isConsecuenceBoard that solve logic consecuence using semantic boards.

isConsecuenceBoard : List FormulaLP -> FormulaLP -> Bool
isConsecuenceBoard fs f = List.isEmpty <| modelsTab <|  Neg f::fs