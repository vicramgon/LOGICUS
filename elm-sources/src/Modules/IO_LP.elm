module Modules.IO_LP exposing(fromStringToFLP, checkFLP, extractReadFLP, fromStringToSetLP, toStringFLP, toStringFLPSet, formTree)

import Char
import Set
import String
import Maybe
import Parser exposing (Parser, run, variable, oneOf, succeed, spaces, (|.), (|=), symbol, lazy, andThen)
import Modules.SintaxSemanticsLP exposing (PSymb, FormulaLP(..))
import Graph exposing (Graph(..), Node, Edge, NodeId, fromNodesAndEdges)
import Graph.DOT exposing (outputWithStyles, defaultStyles)
import Modules.AuxiliarFunctions exposing (cleanSpaces)

------------------------------------------------------------------------------------------------------------
--                                                 PARSER                                                 --
------------------------------------------------------------------------------------------------------------

fromStringToFLP : String -> (Maybe (FormulaLP), String)
fromStringToFLP x =
  if x == "" then
        (Maybe.Nothing, "Argument is empty")
  else
    case run parserFLP ("(" ++ cleanSpaces x ++ ")") of
      
      Ok y-> (Maybe.Just y, "")

      Err y -> (Maybe.Nothing, Debug.toString y)

checkFLP : (Maybe (FormulaLP), String) -> Bool
checkFLP (flp, _) =
  case flp of
    Nothing ->  False 
    _ -> True 

extractReadFLP : (Maybe (FormulaLP), String) -> FormulaLP
extractReadFLP (flp, _) = Maybe.withDefault Insat flp



fromStringToSetLP : String -> List (Maybe(FormulaLP), String)
fromStringToSetLP  x =  List.map fromStringToFLP <| String.split "\\\\" x

typeVar : Parser PSymb
typeVar =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c
    , reserved = Set.fromList []
    }

parserFLP : Parser FormulaLP
parserFLP =
  oneOf 
  [ 
    succeed Atom
      |. spaces
      |= typeVar
      |. spaces

  , succeed identity 
    |. symbol "("
    |. spaces
    |= lazy(\_ -> expression)
    |. spaces
    |. symbol ")"
    |. spaces  
  
  , succeed Neg
    |.spaces
    |.symbol "NEG"
    |.spaces
    |= lazy(\_ -> parserFLP)

  , succeed Insat
    |.spaces
    |.symbol "INSAT"
    |.spaces
  ]

expression : Parser FormulaLP
expression =
  parserFLP |> andThen (expressionAux [])

type Operator = AndOp | OrOp | ImplOp | EquivOp

operator : Parser Operator
operator = 
    oneOf
    [ Parser.map (\_ -> AndOp) (symbol "AND")
    , Parser.map (\_ -> OrOp) (symbol "OR")
    , Parser.map (\_ -> ImplOp) (symbol "IMPLIES")
    , Parser.map (\_ -> EquivOp) (symbol "EQUIV")
    ]

expressionAux : List (FormulaLP, Operator) -> FormulaLP -> Parser FormulaLP
expressionAux revOps expr =
  oneOf
    [ succeed Tuple.pair
        |. spaces
        |= operator
        |. spaces
        |= parserFLP
        |> andThen (\(op, newExpr) -> expressionAux ((expr,op) :: revOps) newExpr)
    , lazy (\_ -> succeed (finalize revOps expr))
    ]

finalize : List (FormulaLP, Operator) -> FormulaLP -> FormulaLP
finalize revOps finalExpr =
  case revOps of
    [] ->
      finalExpr

      -- AND EXPRESSIONS CASES
      -- And operation have the maximum priorty, so module have a unique case

    (expr, AndOp) :: otherRevOps ->
      finalize otherRevOps (Conj expr finalExpr)

      -- OR EXPRESSIONS CASES
      -- Or have the second maximum priority, so we need to determine how parser's going to do if it searches an and after, and if it searches something different.

    (expr, OrOp) :: (expr2, AndOp) :: otherRevOps ->
      Disj (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, OrOp) :: otherRevOps ->
      finalize otherRevOps (Disj expr finalExpr)

    -- IMPLICATION EXPRESSIONS CASES

    (expr, ImplOp) :: (expr2, AndOp) :: otherRevOps ->
      Impl (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, ImplOp) :: (expr2, OrOp) :: otherRevOps ->
       Impl (finalize ( (expr2, OrOp) :: otherRevOps) expr) finalExpr

    (expr, ImplOp) :: otherRevOps ->
      finalize otherRevOps (Impl expr finalExpr)

    -- EQUIVALATION EXPRESSIONS CASES

    (expr, EquivOp) :: (expr2, AndOp) :: otherRevOps ->
      Equi (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: (expr2, OrOp) :: otherRevOps ->
       Equi (finalize ( (expr2, OrOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: (expr2, ImplOp) :: otherRevOps ->
       Equi (finalize ( (expr2, ImplOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: otherRevOps ->
      finalize otherRevOps (Equi expr finalExpr)

------------------------------------------------------------------------------------------------------------
--                                            REPRESENTATION                                              --
------------------------------------------------------------------------------------------------------------

toStringFLP : FormulaLP -> String
toStringFLP prop =
    case prop of
        Atom p -> p
        Neg p -> "¬ " ++ toStringFLP p
        Conj p q -> "( " ++ toStringFLP p ++ " ∧ "  ++ toStringFLP q ++ " )"
        Disj p q -> "( " ++ toStringFLP p ++ " ∨ "  ++ toStringFLP q ++ " )"
        Impl p q -> "( " ++ toStringFLP p ++ " ⟶ "  ++ toStringFLP q ++ " )"
        Equi p q -> "( " ++ toStringFLP p ++ " ⟷ "  ++ toStringFLP q ++ " )"
        Insat -> "⊥"
        
toStringFLPSet : List FormulaLP -> String
toStringFLPSet xs = "{" ++ (String.join ", " <| List.map toStringFLP xs) ++ "}" 

formTree : FormulaLP -> String
formTree x =  formTree2DOT <| formTree1 x

formTree1 : FormulaLP -> Graph String ()
formTree1 x =
    case x of
        Atom psymb -> fromNodesAndEdges [Node 0 psymb] []
        Neg p -> 
            let (nodes, edges) = formTreeAux p 1 in
                fromNodesAndEdges (Node 0 (toStringFLP x)::nodes) (Edge 0 1 ()::edges)
        Conj p q -> 
            let 
                (nodes1, edges1) = formTreeAux p 1 in
                let nextid = List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        fromNodesAndEdges (Node 0 (toStringFLP x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 nextid ()] ++ edges1 ++ edges2)
        Disj p q -> 
            let 
                (nodes1, edges1) = formTreeAux p 1 in
                let nextid = List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        fromNodesAndEdges (Node 0 (toStringFLP x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 nextid ()] ++ edges1 ++ edges2)
        Impl p q -> 
            let 
                (nodes1, edges1) = formTreeAux p 1 in
                let nextid = List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        fromNodesAndEdges (Node 0 (toStringFLP x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 nextid ()] ++ edges1 ++ edges2)
        Equi p q -> 
           let 
                (nodes1, edges1) = formTreeAux p 1 in
                let nextid = List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        fromNodesAndEdges (Node 0 (toStringFLP x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 nextid ()] ++ edges1 ++ edges2)
        Insat -> fromNodesAndEdges [Node 0 (toStringFLP x)] []

formTreeAux : FormulaLP -> NodeId -> (List (Node String), List (Edge ()))
formTreeAux x nodeid=
    case x of
        Atom psymb -> ([Node nodeid psymb], [])
        Neg p -> 
            let (nodes, edges) = formTreeAux p (nodeid + 1) in
                (Node nodeid (toStringFLP x)::nodes, Edge nodeid (nodeid + 1) ()::edges)
        Conj p q -> 
            let (nodes1, edges1) = formTreeAux p (nodeid + 1) in 
                let nextid = nodeid + List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        ( Node nodeid (toStringFLP x)::(nodes1 ++ nodes2),  [Edge nodeid (nodeid + 1) (), Edge nodeid nextid ()] ++ edges1 ++ edges2)
        Disj p q -> 
            let (nodes1, edges1) = formTreeAux p (nodeid + 1) in 
                let nextid = nodeid + List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        ( Node nodeid (toStringFLP x)::(nodes1 ++ nodes2),  [Edge nodeid (nodeid + 1) (), Edge nodeid nextid ()] ++ edges1 ++ edges2)
        Impl p q -> 
            let (nodes1, edges1) = formTreeAux p (nodeid + 1) in 
                let nextid = nodeid + List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        ( Node nodeid (toStringFLP x)::(nodes1 ++ nodes2),  [Edge nodeid (nodeid + 1) (), Edge nodeid nextid ()] ++ edges1 ++ edges2)
        Equi p q -> 
          let (nodes1, edges1) = formTreeAux p (nodeid + 1) in 
                let nextid = nodeid + List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        ( Node nodeid (toStringFLP x)::(nodes1 ++ nodes2),  [Edge nodeid (nodeid + 1) (), Edge nodeid nextid ()] ++ edges1 ++ edges2)
        Insat -> ([Node nodeid (toStringFLP x)], [])

formTree2DOT : Graph String () -> String
formTree2DOT ft =
    let myStyles =
            { defaultStyles | node = "shape=plaintext, color=black", edge = "dir=none"}
    in 
        outputWithStyles myStyles (\x -> Just x) (\_ -> Nothing) ft


