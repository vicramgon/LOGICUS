module Logicus.SemanticBoardsLPO exposing (..)

import Logicus.SintaxSemanticsLPO exposing (FormulaLPO(..), Term(..))
import Logicus.AuxiliarFunctions exposing (uncurry)
import Logicus.IO_LPO exposing (toStringFLPO)
import List.Extra exposing (getAt)
import List exposing (member)
import Graph exposing (NodeId, Node, Edge, fromNodesAndEdges)
import Graph.DOT exposing (outputWithStyles, defaultStyles)
import Dict exposing (Dict, fromList)
import Logicus.SintaxSemanticsLPO exposing (getVarSymb)
import Logicus.SintaxSemanticsLPO exposing (applySubsToFormula)
import Dict
import Dict
import Logicus.IO_LPO exposing (toStringTerm)
import List
import List
import Dict
import Logicus.AuxiliarFunctions exposing (uniqueConcatList)
import Logicus.SintaxSemanticsLPO exposing (varsInTerm)


isInsat : FormulaLPO -> Bool 
isInsat x =
    case x of
        Insat -> True
        Neg (Insat) -> True
        Neg (Equal t1 t2) -> t1 == t2
        _ -> False

isLiteral : FormulaLPO -> Bool
isLiteral x =
    case x of
        Pred _ _ -> True
        Neg (Pred _ _) -> True
        Equal _ _ -> True
        Neg (Equal t1 t2) -> t1 /= t2
        _ -> False

isDobleNeg : FormulaLPO -> Bool
isDobleNeg x =
    case x of
        Neg ( Neg ( _ ) ) -> True
        _ -> False


isAlpha : FormulaLPO -> Bool
isAlpha x =
    case x of
        Conj _ _ -> True
        Neg (Disj _ _) -> True
        Neg (Impl _ _) -> True
        Equi _ _ -> True
        _ -> False

isBeta : FormulaLPO -> Bool
isBeta x =
    case x of
        Disj _ _ -> True
        Impl _ _ -> True
        Neg (Conj _ _) -> True
        Neg (Equi _ _) -> True 
        _ -> False

isGamma : FormulaLPO -> Bool
isGamma x =
    case x of
        Forall _ _ -> True
        Neg (Exists _ _) -> True 
        _ -> False 

isDelta : FormulaLPO -> Bool
isDelta x =
    case x of
        Exists _ _ -> True
        Neg (Forall _ _) -> True 
        _ -> False 

formuladNComponents : FormulaLPO -> Maybe (FormulaLPO)
formuladNComponents x =
    case x of
        Neg (Neg f) -> Just f
        _ -> Nothing

formulaAlphaBetaComponents : FormulaLPO -> Maybe (FormulaLPO, FormulaLPO)
formulaAlphaBetaComponents x =
    case x of                                   
        Conj f g -> Just (f, g)
        Neg (Impl f g) -> Just (f, Neg g)
        Neg (Disj f g) -> Just (Neg f, Neg g)
        Disj f g -> Just (f, g)
        Impl f g -> Just (Neg f , g)
        Neg (Conj f g) -> Just (Neg f, Neg g)
        Equi f g -> Just (Impl f g, Impl g f)
        Neg (Equi f g) -> Just (Neg(Impl f g), Neg(Impl g f))
        _ -> Nothing

formulaGammaDeltaComponents : FormulaLPO -> Term -> Maybe (FormulaLPO)
formulaGammaDeltaComponents x t =
    case x of
        Forall v f -> Just <| applySubsToFormula (Dict.fromList [(getVarSymb v, t)]) f
        Neg (Exists v f) -> Just <| Neg <| applySubsToFormula (Dict.fromList [(getVarSymb v, t)]) f
        Exists v f -> Just <| applySubsToFormula (Dict.fromList [(getVarSymb v, t)]) f
        Neg (Forall v f) -> Just <| Neg <| applySubsToFormula (Dict.fromList [(getVarSymb v, t)]) f
        _ -> Nothing

clasifyFormula : FormulaLPO -> Int 
clasifyFormula x =
    if isDobleNeg x then 0
    else if isAlpha x then 1
    else if isBeta x then 2
    else if isGamma x then 3
    else if isDelta x then 4
    else 5

constantTermsInFormula : FormulaLPO -> List(Term)
constantTermsInFormula x = 
    case x of
        Pred _ terms -> List.filter (\ t -> List.isEmpty <| varsInTerm t) terms
        Equal t1 t2 -> List.filter (\ t -> List.isEmpty <| varsInTerm t) [t1, t2]
        Neg p -> constantTermsInFormula p
        Conj p q -> uniqueConcatList (constantTermsInFormula p) (constantTermsInFormula q)
        Disj p q -> uniqueConcatList (constantTermsInFormula p) (constantTermsInFormula q)
        Impl p q -> uniqueConcatList (constantTermsInFormula p) (constantTermsInFormula q)
        Equi p q -> uniqueConcatList (constantTermsInFormula p) (constantTermsInFormula q)
        Exists _ p -> constantTermsInFormula p
        Forall _ p -> constantTermsInFormula p
        Insat -> []

generateNewConstant : Int -> List Term -> Term
generateNewConstant i ls =
    let name = "c" ++ String.fromInt i in
        if List.member (Func name []) ls then
            generateNewConstant (i+1) ls
        else 
            Func name []


makeSBoard : List FormulaLPO -> Int -> List Term -> String 
makeSBoard fs  pmax m = 
    let myStyles =
            { defaultStyles | node = "shape=box, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
        sbnodes = List.foldl (\ f ac -> ac ++ [Node (List.length ac) (toStringFLPO f)]) [] fs 
        sbedges = List.map (\ x -> Edge x (x+1) "") <| List.range 0 ((List.length fs)-1)
        dictfs = Dict.fromList <| List.map (\ x -> (x , (clasifyFormula (Maybe.withDefault Insat <| List.Extra.getAt x fs), 0))) <| List.range 0 ((List.length fs)-1)
    in
        String.replace "\n\n" "\n" <| outputWithStyles myStyles (\x -> Just x) (\x-> Just x) <|  uncurry fromNodesAndEdges <| makeSBoardAux fs dictfs m pmax ((List.length fs)-1) (sbnodes, sbedges)
        


makeSBoardAux : List FormulaLPO -> Dict Int (Int, Int) -> List Term -> Int -> NodeId -> (List (Node String), List (Edge String)) -> (List (Node String), List (Edge String))
makeSBoardAux fs dictfs m pmax nidp (sbnodes, sbedges) =
    let 
        nid = List.length sbnodes 
    in
        if pmax == 0 then
            (sbnodes ++ [Node nid "◯"], sbedges ++ [Edge  nidp  nid ""])
        else 
            case List.head <| Dict.keys <| Dict.filter (\ _ (v1, v2) -> v1 == 0 && v2 == 0) dictfs of
                Just i ->
                    let
                        f = Maybe.withDefault Insat <| formuladNComponents <| Maybe.withDefault Insat <| List.Extra.getAt i fs
                        edgeLabel = "dN : 『" ++  String.fromInt (i+1) ++ "』"
                    in
                        let
                            newsbnodes = sbnodes ++ [Node nid (toStringFLPO f)]
                            newsbedges = sbedges ++ [Edge  nidp  nid edgeLabel]
                            newfs = fs ++ [f]
                            newdictfs = Dict.insert i (0, 1) <| Dict.insert ((List.length newfs)-1) (clasifyFormula f, 0) dictfs
                        in 
                            if isInsat f || List.any (\x -> x ==  Maybe.withDefault (Neg f) (formuladNComponents (Neg f))) fs then 
                                (newsbnodes ++ [Node (nid + 1) "╳"], newsbedges ++ [Edge  nid  (nid + 1) ""])
                            else 
                                makeSBoardAux newfs  newdictfs m (pmax-1) nid (newsbnodes, newsbedges)
                Nothing ->
                    case List.head <| Dict.keys <| Dict.filter (\ _ (v1, v2) -> v1 == 1 && v2 == 0) dictfs of
                        Just i ->
                            let
                                (f, g) = Maybe.withDefault (Insat, Insat) <| formulaAlphaBetaComponents <| Maybe.withDefault Insat <| List.Extra.getAt i fs
                                edgeLabelf = "α₁ :『" ++  String.fromInt (i+1) ++ "』"
                                edgeLabelg = "α₂ :『" ++  String.fromInt (i+1) ++ "』"
                                
                            in
                                let
                                    newsbnodes = sbnodes ++ [Node nid (toStringFLPO f), Node (nid + 1) (toStringFLPO g)]
                                    newsbedges = sbedges ++ [Edge  nidp  nid edgeLabelf, Edge  nid (nid + 1) edgeLabelg]
                                    newfs = fs ++ [f, g]
                                    newdictfs = 
                                        Dict.insert i (1, 1) 
                                            <| Dict.insert ((List.length newfs)-2) (clasifyFormula f, 0)
                                            <| Dict.insert ((List.length newfs)-1) (clasifyFormula g, 0) dictfs
                                in 
                                    if isInsat f || isInsat g || List.any (\x -> x == Maybe.withDefault (Neg f) (formuladNComponents (Neg f)) || x == Maybe.withDefault (Neg f) (formuladNComponents (Neg g))) fs  then 
                                        (newsbnodes ++ [Node (nid + 2) "╳"], newsbedges ++ [Edge  (nid+1)  (nid + 2) ""])
                                    else 
                                        makeSBoardAux newfs  newdictfs m (pmax-1) nid (newsbnodes, newsbedges)
                                
                        Nothing ->
                            case List.head <| Dict.keys <| Dict.filter (\ _ (v1, v2) -> v1 == 2 && v2 == 0) dictfs of
                                Just i ->
                                    let
                                        (f, g) = Maybe.withDefault (Insat, Insat) <| formulaAlphaBetaComponents <| Maybe.withDefault Insat <| List.Extra.getAt i fs
                                        edgeLabelf = "β₁ :『" ++  String.fromInt (i+1) ++ "』"
                                        edgeLabelg = "β₂ :『" ++  String.fromInt (i+1) ++ "』"
                                        
                                    in

                                        let
                                            newsbnodes1 = sbnodes ++ [Node nid (toStringFLPO f)] 
                                            newsbedges1 = sbedges ++ [Edge  nidp  nid edgeLabelf]
                                            newfs1 = fs ++ [f]
                                            newdictfs1 = 
                                                Dict.insert i (2, 1) 
                                                    <| Dict.insert ((List.length newfs1)-1) (clasifyFormula f, 0) dictfs
                                        in 
                                            let 
                                                (sbnodes2, sbedges2) = 
                                                    if isInsat f || List.any (\x -> x == Maybe.withDefault (Neg f) (formuladNComponents (Neg f))) fs  then 
                                                        (newsbnodes1 ++ [Node (nid + 1) "╳"], newsbedges1 ++ [Edge  nid (nid + 1) ""])
                                                    else
                                                        makeSBoardAux newfs1 newdictfs1 m (pmax-1) nid (newsbnodes1, newsbedges1)
                                            in
                                                let
                                                    nid2 = List.length sbnodes2
                                                in
                                                    let
                                                        newsbnodes2 = sbnodes2 ++ [Node nid2 (toStringFLPO g)]  
                                                        newsbedges2 = sbedges2 ++ [Edge  nidp  nid2 edgeLabelg]
                                                        newfs2 = fs ++ [g]
                                                        newdictfs2 = 
                                                            Dict.insert i (2, 1) 
                                                                <| Dict.insert ((List.length newfs2)-1) (clasifyFormula g, 0) dictfs

                                                    in
                                                        if isInsat g || List.any (\x -> x == Maybe.withDefault (Neg g) (formuladNComponents (Neg g))) fs  then 
                                                            (newsbnodes2 ++ [Node (nid2 + 1) "╳"], newsbedges2 ++ [Edge  nid2 (nid2 + 1) ""])
                                                        else
                                                            makeSBoardAux newfs2 newdictfs2 m (pmax-1) nid2 (newsbnodes2, newsbedges2)
                                Nothing ->
                                    case  List.head <| Dict.keys <| Dict.filter (\ _ (v1, v2) -> v1 == 4 && v2 == 0) dictfs of
                                        Just i ->
                                            let
                                                newC = generateNewConstant 1 m
                                                f = Maybe.withDefault Insat <| formulaGammaDeltaComponents (Maybe.withDefault Insat <| List.Extra.getAt i fs) newC
                                                edgeLabel = "δ : 『" ++  String.fromInt (i+1) ++ " / " ++ toStringTerm newC ++"』"
                                            in
                                                let
                                                    newsbnodes = sbnodes ++ [Node nid (toStringFLPO f)]
                                                    newsbedges = sbedges ++ [Edge  nidp  nid edgeLabel]
                                                    newfs = fs ++ [f]
                                                    newdictfs = Dict.insert i (4, 1) <| Dict.insert ((List.length newfs)-1) (clasifyFormula f, 0) dictfs
                                                in 
                                                    if isInsat f || List.any (\x -> x == Maybe.withDefault (Neg f) (formuladNComponents (Neg f))) fs then 
                                                        (newsbnodes ++ [Node (nid + 1) "╳"], newsbedges ++ [Edge  nid  (nid + 1) ""])
                                                    else 
                                                        makeSBoardAux newfs newdictfs (uniqueConcatList (m ++ [newC]) (constantTermsInFormula f)) (pmax-1) nid (newsbnodes, newsbedges)
                                        Nothing ->
                                            case  List.head <| List.sortBy (\ (_,(_, v2)) -> v2) <| Dict.toList <| Dict.filter (\ _ (v1, _) -> v1 == 3) dictfs of
                                                Just (i, (_, iv)) ->
                                                    let
                                                        (nm, cs) = 
                                                            case List.Extra.getAt iv m of
                                                                Just c -> (m, c)

                                                                Nothing -> 
                                                                    let newC = generateNewConstant 1 m in
                                                                        (m ++ [newC], newC)
                                                    in
                                                        let
                                                            f = Maybe.withDefault Insat <| formulaGammaDeltaComponents (Maybe.withDefault Insat <| List.Extra.getAt i fs) cs
                                                            edgeLabel = "𝛾 : 『" ++  String.fromInt (i+1) ++ " / " ++ toStringTerm cs ++"』"
                                                        in
                                                            let
                                                                newsbnodes = sbnodes ++ [Node nid (toStringFLPO f)]
                                                                newsbedges = sbedges ++ [Edge  nidp  nid edgeLabel]
                                                                newfs = fs ++ [f]
                                                                newdictfs = Dict.insert i (3, iv + 1) <| Dict.insert ((List.length newfs)-1) (clasifyFormula f, 0) dictfs
                                                            in 
                                                                if isInsat f || List.any (\x -> x == Maybe.withDefault (Neg f) (formuladNComponents (Neg f))) fs then 
                                                                    (newsbnodes ++ [Node (nid + 1) "╳"], newsbedges ++ [Edge  nid  (nid + 1) ""])
                                                                else 
                                                                    makeSBoardAux newfs newdictfs (uniqueConcatList nm (constantTermsInFormula f)) (pmax-1) nid (newsbnodes, newsbedges)
                                                Nothing -> (sbnodes ++ [Node nid "◯"], sbedges ++ [Edge  nidp  nid ""])


-- TO DO : Semantic Boards in LPO with =
     
       




