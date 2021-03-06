module Logicus.ResolutionLP exposing (..)

import Logicus.SintaxSemanticsLP exposing (FormulaLP(..))
import Logicus.NormalFormsDPLL exposing (Literal, Clause,  literalComplement, areEqClauses, haveContraryLiteral, toStringClause)
import Logicus.AuxiliarFunctions as Aux
import Graph exposing (Node, Edge, fromNodesAndEdges, Graph)
import Logicus.IO_LP exposing (toStringFLP)

-- RESOLUTES BETWEEN CLAUSES

resolute : Clause -> Clause -> Literal-> Clause
resolute c1 c2 l =
    Aux.unionLs (List.filter (\x -> x /= l) c1) (List.filter (\x -> x /= literalComplement l) c2)

clausesResolutes : Clause -> Clause -> List Clause
clausesResolutes c1 c2 =
    List.foldl (\l ac ->  if List.member (literalComplement l) c2 then ac ++ [resolute c1 c2 l] else ac) [] c1

setClausesCResolutesWithClause : Clause ->  List Clause -> List (Clause, List Clause)
setClausesCResolutesWithClause c ls = setClausesCResolutesWithClauseAux c ls [] []

setClausesCResolutesWithClauseAux : Clause -> List Clause -> List Clause -> List (Clause, List Clause) -> List (Clause, List Clause)
setClausesCResolutesWithClauseAux c ls ac res = 
    case ls of
        [] -> res
        x::xs ->
            let
                resolutes_x = List.filter (\ y -> List.all (\z -> not(areEqClauses y z)) ac) (clausesResolutes c x)  
            in
                setClausesCResolutesWithClauseAux c xs (ac ++ resolutes_x) (res ++ List.map(\y -> (y, [c, x])) resolutes_x)


-- GENERAL RESOLUTION (LESS LENGTH HEURISTIC)

clauseSetResolutionGraph : List(Clause) -> Graph Clause Literal 
clauseSetResolutionGraph ls =
    let
        opened = List.map (\x -> (x, [])) <| List.sortBy (\x -> List.length x) ls 
    in 
        let 
            resPath = opened ++ (clauseSetResolutionAux [] opened)
        in
            let 
                (nodes, edges) = buildResolutionGraph resPath [] []  
            in 
                fromNodesAndEdges nodes edges


clauseSetResolutionAux :  List (Clause, List Clause, List Clause) ->  List (Clause, List Clause) -> List (Clause, List Clause)
clauseSetResolutionAux closed opened =
    case opened of
        [] -> []
        x::xs ->
            case x of 
                ([], px) -> 
                    let 
                        ys = List.filter (\ y -> not(List.isEmpty <| Tuple.second y)) <| (List.map (\(c, ps, _) -> (c, ps)) closed) ++ xs
                    in
                        recoverResolutionPath px ys [] ++ [x]
                (c, ps) ->
                    let 
                        opendedClauses = List.map (Tuple.first) xs

                    in
                        let
                            newclosed = closed ++ [(c, ps, opendedClauses)]
                            resolutesXWithClosed = clauseResolutesWithClosed c closed
                            resolutesXWithOpened = setClausesCResolutesWithClause c opendedClauses
                        in
                            let
                                newopened = 
                                    List.sortBy (\(y, _) -> List.length y ) <|
                                        List.foldl 
                                            (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                            xs
                                            (resolutesXWithClosed ++ resolutesXWithOpened)
                            in
                                clauseSetResolutionAux newclosed newopened

clauseResolutesWithClosed : Clause -> List (Clause, List Clause, List Clause) -> List (Clause, List Clause)
clauseResolutesWithClosed c closed =
    let
        ls = List.foldl  (\ (x, _, xs) ac -> if List.all (\ y -> not (areEqClauses c y)) xs then ac ++ [x] else ac) [] closed  
    in
        setClausesCResolutesWithClause c ls

recoverResolutionPath : List(Clause) -> List (Clause, List(Clause)) -> List (Clause, List(Clause)) -> List (Clause, List(Clause))
recoverResolutionPath  xs ys res =
    case xs of
        [] -> res
        z::zs ->
            let
                cs = List.filter (\ x -> (Tuple.first x) == z) ys
                
            in
                let
                    nxs = (List.concat <| List.map (Tuple.second) cs) ++ zs
                    nres = Aux.uniqueConcatList cs res
                in
                recoverResolutionPath  nxs ys nres 
            

buildResolutionGraph : List (Clause, List(Clause)) -> List (Int, Clause) -> List (Int, Int, Literal) -> (List (Node Clause), List(Edge Literal))
buildResolutionGraph resPath nodes edges =
    case resPath of
        [] -> (List.map (\ (id, c) -> Node id c) nodes , List.map (\ (id1, id2, l) -> Edge id1 id2 l) edges)
        (x, [p1, p2])::xs -> 
            let 
                idnodex = List.length nodes
            in 
                let
                    nodex =  (idnodex, x)
                    edgesx = List.foldl (\ (id, l) ac -> if (areEqClauses l p1) || (areEqClauses l p2) then ac ++ [(id, idnodex, (getResolutionLiteralForGraph l x))] else ac) [] nodes
                in
                    buildResolutionGraph xs (nodes ++ [nodex]) (edges ++ edgesx)
        x::xs -> 
            buildResolutionGraph xs (nodes ++ [((List.length nodes), Tuple.first x)]) edges
    
getResolutionLiteralForGraph : Clause -> Clause -> Literal
getResolutionLiteralForGraph c1 c2 = Maybe.withDefault Insat <| List.head <| List.filter (\x -> not (List.member x c2)) c1

-- LINEAR RESOLUTION (LESS LENGTH HEURISTIC)

clauseSetLinearResolutionAux :  List (Clause, List Clause) -> List (Clause, List Clause)
clauseSetLinearResolutionAux clauses =
    case clauses of 
        [] -> []
        _::xs ->
            case clauseSetLinearResolutionAux2 clauses of
                [] -> clauseSetLinearResolutionAux xs
                ls -> ls


clauseSetLinearResolutionAux2 :  List (Clause, List Clause) -> List (Clause, List Clause)
clauseSetLinearResolutionAux2 clauses =
    case clauses of
        [] -> []
        x::xs ->
            case x of 
                ([], px) -> 
                    let 
                        ys = List.filter (\ y -> not(List.isEmpty <| Tuple.second y)) <| clauses
                    in
                        recoverResolutionPath px ys [] ++ [x]
                (c, _) ->
                    let 
                        resolutes_c = 
                            List.sortBy (\(y, _) -> List.length y ) <|
                                List.foldr (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _) -> not (areEqClauses y z)) clauses then ac ++ [(y, pys)] else ac)
                                    [] 
                                    (setClausesCResolutesWithClause c <| List.map (Tuple.first) xs)

                    in
                        clauseSetLinearResolutionAux3 clauses resolutes_c

clauseSetLinearResolutionAux3 :  List (Clause, List Clause) -> List (Clause, List Clause) -> List (Clause, List Clause) 
clauseSetLinearResolutionAux3 clauses resolutes =
    case resolutes of
        [] -> []
        x::xs -> 
            case clauseSetLinearResolutionAux2 (x::clauses) of
                [] -> clauseSetLinearResolutionAux3 clauses xs
                ls -> ls

 
clauseSetLinearResolutionGraph : List(Clause) -> Graph Clause Literal 
clauseSetLinearResolutionGraph ls =
    let
        clauses = List.map (\x -> (x, [])) <| List.sortBy (\x -> List.length x) ls 
    in 
        let 
            resPath = clauses ++ (clauseSetLinearResolutionAux clauses)
        in
            let 
                (nodes, edges) = buildResolutionGraph resPath [] []  
            in 
                fromNodesAndEdges nodes edges


-- POSITIVE RESOLUTION (LESS LENGTH HEURISTIC)

clauseSetPositiveResolutionGraph : List(Clause) -> Graph Clause Literal 
clauseSetPositiveResolutionGraph ls =
    let
        opened = List.map (\x -> (x, [])) <| List.sortBy (\x -> List.length x) ls 
    in 
        let 
            resPath = opened ++ (clauseSetPositiveResolutionAux [] opened)
        in
            let 
                (nodes, edges) = buildResolutionGraph resPath [] []  
            in 
                fromNodesAndEdges nodes edges

clauseSetPositiveResolutionAux :  List (Clause, List Clause, List Clause) ->  List (Clause, List Clause) -> List (Clause, List Clause)
clauseSetPositiveResolutionAux closed opened =
    case opened of
        [] -> []
        x::xs ->
            case x of 
                ([], px) -> 
                    let 
                        ys = List.filter (\ y -> not(List.isEmpty <| Tuple.second y)) <| (List.map (\(c, ps, _) -> (c, ps)) closed) ++ xs
                    in
                        recoverResolutionPath px ys [] ++ [x]
                (c, ps) ->
                    if isPositiveClause c then
                        let 
                            opendedClauses = List.map (Tuple.first) xs

                        in
                            let
                                newclosed = closed ++ [(c, ps, opendedClauses)]
                                resolutesXWithClosed = clauseResolutesWithClosed c closed
                                resolutesXWithOpened = setClausesCResolutesWithClause c opendedClauses
                            in
                                let
                                    newopened = 
                                        List.sortBy (\(y, _) -> List.length y ) <|
                                            List.foldl 
                                                (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                                xs
                                                (resolutesXWithClosed ++ resolutesXWithOpened)
                                in
                                    clauseSetPositiveResolutionAux newclosed newopened
                    else
                        let 
                            opendedPositiveClauses = List.filter (isPositiveClause) <| List.map (Tuple.first) xs

                        in
                            let
                                newclosed = closed ++ [(c, ps, opendedPositiveClauses)]
                                resolutesXWithClosed = clauseResolutesWithClosed c <| List.filter (\(cx, _, _) -> isPositiveClause cx) closed
                                resolutesXWithOpened = setClausesCResolutesWithClause c opendedPositiveClauses

                            in
                                let
                                    newopened = 
                                        List.sortBy (\(y, _) -> List.length y ) <|
                                            List.foldl 
                                                (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                                xs
                                                (resolutesXWithClosed ++ resolutesXWithOpened)
                                in
                                    clauseSetPositiveResolutionAux newclosed newopened



isPositiveLiteral : Literal -> Bool
isPositiveLiteral l =
    case l of
       Atom _ -> True
       _ -> False

isPositiveClause : Clause -> Bool 
isPositiveClause c = List.all isPositiveLiteral c

-- NEGATIVE RESOLUTION (LESS LENGTH HEURISTIC)

clauseSetNegativeResolutionGraph : List(Clause) -> Graph Clause Literal 
clauseSetNegativeResolutionGraph ls =
    let
        opened = List.map (\x -> (x, [])) <| List.sortBy (\x -> List.length x) ls 
    in 
        let 
            resPath = opened ++ (clauseSetNegativeResolutionAux [] opened)
        in
            let 
                (nodes, edges) = buildResolutionGraph resPath [] []  
            in 
                fromNodesAndEdges nodes edges

clauseSetNegativeResolutionAux :  List (Clause, List Clause, List Clause) ->  List (Clause, List Clause) -> List (Clause, List Clause)
clauseSetNegativeResolutionAux closed opened =
    case opened of
        [] -> []
        x::xs ->
            case x of 
                ([], px) -> 
                    let 
                        ys = List.filter (\ y -> not(List.isEmpty <| Tuple.second y)) <| (List.map (\(c, ps, _) -> (c, ps)) closed) ++ xs
                    in
                        recoverResolutionPath px ys [] ++ [x]
                (c, ps) ->
                    if isNegativeClause c then
                        let 
                            opendedClauses = List.map (Tuple.first) xs

                        in
                            let
                                newclosed = closed ++ [(c, ps, opendedClauses)]
                                resolutesXWithClosed = clauseResolutesWithClosed c closed
                                resolutesXWithOpened = setClausesCResolutesWithClause c opendedClauses
                            in
                                let
                                    newopened = 
                                        List.sortBy (\(y, _) -> List.length y ) <|
                                            List.foldl 
                                                (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                                xs
                                                (resolutesXWithClosed ++ resolutesXWithOpened)
                                in
                                    clauseSetNegativeResolutionAux newclosed newopened
                    else
                        let 
                            opendedNegativeClauses = List.filter (isNegativeClause) <| List.map (Tuple.first) xs

                        in
                            let
                                newclosed = closed ++ [(c, ps, opendedNegativeClauses)]
                                resolutesXWithClosed = clauseResolutesWithClosed c <| List.filter (\(cx, _, _) -> isNegativeClause cx) closed
                                resolutesXWithOpened = setClausesCResolutesWithClause c opendedNegativeClauses

                            in
                                let
                                    newopened = 
                                        List.sortBy (\(y, _) -> List.length y ) <|
                                            List.foldl 
                                                (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                                xs
                                                (resolutesXWithClosed ++ resolutesXWithOpened)
                                in
                                    clauseSetNegativeResolutionAux newclosed newopened



isNegativeLiteral : Literal -> Bool
isNegativeLiteral l =
    case l of
       Neg(Atom _) -> True
       _ -> False

isNegativeClause : Clause -> Bool 
isNegativeClause c = List.all isNegativeLiteral c


-- ENTRY RESOLUTION (LESS LENGTH HEURISTIC)

clauseSetEntryResolutionGraph : List(Clause) -> Graph Clause Literal 
clauseSetEntryResolutionGraph ls =
    let
        opened = List.map (\x -> (x, [])) <| List.sortBy (\x -> List.length x) ls 
    in 
        let 
            resPath = opened ++ (clauseSetEntryResolutionAux [] opened)
        in
            let 
                (nodes, edges) = buildResolutionGraph resPath [] []  
            in 
                fromNodesAndEdges nodes edges

clauseSetEntryResolutionAux :  List (Clause, List Clause, List Clause) ->  List (Clause, List Clause) -> List (Clause, List Clause)
clauseSetEntryResolutionAux closed opened =
    case opened of
        [] -> []
        x::xs ->
            case x of 
                ([], px) -> 
                    let 
                        ys = List.filter (\ y -> not(List.isEmpty <| Tuple.second y)) <| (List.map (\(c, ps, _) -> (c, ps)) closed) ++ xs
                    in
                        recoverResolutionPath px ys [] ++ [x]
                (c, ps) ->
                    if List.isEmpty ps then
                        let 
                            opendedClauses = List.map (Tuple.first) xs

                        in
                            let
                                newclosed = closed ++ [(c, ps, opendedClauses)]
                                resolutesXWithClosed = clauseResolutesWithClosed c closed
                                resolutesXWithOpened = setClausesCResolutesWithClause c opendedClauses
                            in
                                let
                                    newopened = 
                                        List.sortBy (\(y, _) -> List.length y ) <|
                                            List.foldl 
                                                (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                                xs
                                                (resolutesXWithClosed ++ resolutesXWithOpened)
                                in
                                    clauseSetEntryResolutionAux newclosed newopened
                    else
                        let 
                            opendedEntryClauses = List.map (Tuple.first) <| List.filter (\ y -> List.isEmpty <| Tuple.second y) xs

                        in
                            let
                                newclosed = closed ++ [(c, ps, opendedEntryClauses)]
                                resolutesXWithClosed = clauseResolutesWithClosed c <| List.filter (\(_, px, _) -> List.isEmpty px) closed
                                resolutesXWithOpened = setClausesCResolutesWithClause c opendedEntryClauses

                            in
                                let
                                    newopened = 
                                        List.sortBy (\(y, _) -> List.length y ) <|
                                            List.foldl 
                                                (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                                xs
                                                (resolutesXWithClosed ++ resolutesXWithOpened)
                                in
                                    clauseSetEntryResolutionAux newclosed newopened




{--clauseSetWithFilterResolutionAux :  (List (Clause, List Clause, List Clause), List (Clause, List Clause, List Clause)) ->  
                                    (List (Clause, List Clause), List (Clause, List Clause)) -> 
                                    ((Clause, List Clause, List Clause) -> Bool) -> 
                                    ((Clause, List Clause) -> Bool) -> 
                                    List (Clause, List Clause)
clauseSetWithFilterResolutionAux (closedT, closedF) (openedT, openedF) closedFilter openedFilter=
    let 
        varT = List.head <| openedT
        varF = List.head <| openedF
    in


    
    
    if (List.isEmpty openedT) && (List.isEmpty openedF) then 
        []
    else
        

    case openedT of
        [] -> 
            case openedF of
                [] -> []
                x::xs ->
                    case x of 
                        ([], px) -> 
                            let 
                                ys = List.filter (\ y -> not(List.isEmpty <| Tuple.second y)) <| (List.map (\(c, ps, _) -> (c, ps)) closed) ++ xs
                            in
                                recoverResolutionPath px ys [] ++ [x]
                        
                        (c, ps) ->
                            let
                                opendedFClauses = List.map (Tuple.first) xs

                            in
                                let
                                    newclosedF = closedF ++ [(c, ps, opendedFClauses)]
                                    (resolutesXWithClosedT, resolutesXWithClosedF) = clauseResolutesWithClosed c closed
                                    resolutesXWithOpened = setClausesCResolutesWithClause c opendedClauses
                                in
                                    let
                                        newopened = 
                                            List.sortBy (\(y, _) -> List.length y ) <|
                                                List.foldl 
                                                    (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                                    xs
                                                    (resolutesXWithClosed ++ resolutesXWithOpened)
                                    in
                                        clauseSetEntryResolutionAux newclosed newopened
                        else
                            let 
                                opendedEntryClauses = List.map (Tuple.first) <| List.filter (\ y -> List.isEmpty <| Tuple.second y) xs

                            in
                                let
                                    newclosed = closed ++ [(c, ps, opendedEntryClauses)]
                                    resolutesXWithClosed = clauseResolutesWithClosed c <| List.filter (\(_, px, _) -> List.isEmpty px) closed
                                    resolutesXWithOpened = setClausesCResolutesWithClause c opendedEntryClauses

                                in
                                    let
                                        newopened = 
                                            List.sortBy (\(y, _) -> List.length y ) <|
                                                List.foldl 
                                                    (\ (y, pys) ac -> if not(haveContraryLiteral y) && List.all (\(z, _) -> not (areEqClauses y z)) ac && List.all (\(z, _, _) -> not (areEqClauses y z)) closed then ac ++ [(y, pys)] else ac)
                                                    xs
                                                    (resolutesXWithClosed ++ resolutesXWithOpened)
                                    in
                                        clauseSetEntryResolutionAux newclosed newopened--}








