module Modules.IO_LPO exposing(fromStringToFLPO, fromStringToSetLPO, checkFLPO, extractReadFLPO, fromStringToSubstitutionLPO,
                               checkSubstitutionLPO, extractReadSubstitutionLPO, toStringFLPO, toStringSLOP, toLatexFLPO, toLatexSLOP, formTree)

import Char
import Set
import String
import Maybe
import Char.Extra exposing (isSpace)
import Parser exposing (Parser, run, variable, oneOf, succeed, spaces, (|.), (|=), symbol, lazy, andThen, loop, Step(..), map, Trailing(..), sequence)
import Modules.SintaxSemanticsLPO exposing (Term(..), FormulaLPO(..), Variable, Substitution)
import Dict
import Graph exposing (Graph(..), Node, Edge, NodeId, fromNodesAndEdges)
import Graph.DOT exposing (outputWithStyles, defaultStyles)
import Modules.AuxiliarFunctions exposing (cleanSpaces)

------------------------------------------------------------------------------------------------------------
--                                                 PARSERS                                                --
------------------------------------------------------------------------------------------------------------

fromStringToFLPO : String -> (Maybe (FormulaLPO), String)
fromStringToFLPO x =
  if x == "" then
        (Maybe.Nothing, "Argument is empty")
  else
    case run parserFLPO ("(" ++ cleanSpaces x ++ ")") of

      Ok y-> (Maybe.Just y, "")

      Err y -> (Maybe.Nothing, Debug.toString y)

checkFLPO : (Maybe (FormulaLPO), String) -> Bool
checkFLPO (flpo, _) =
  case flpo of
    Nothing ->  False
    _ -> True

extractReadFLPO : (Maybe (FormulaLPO), String) -> FormulaLPO
extractReadFLPO (flpo, _) = Maybe.withDefault Insat flpo



fromStringToSetLPO : String -> List (Maybe(FormulaLPO), String)
fromStringToSetLPO  x =  List.map fromStringToFLPO <| String.split "\\\\" x

parseVar : Parser Variable
parseVar = succeed Var
            |= variable
                { start = Char.isLower
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.fromList []
                }


parseTerm : Parser Term
parseTerm =
    oneOf [
        succeed Func
            |. symbol "_"
            |= variable
                { start =  \c -> c /= '[' && c /= ']' && c /= ';' && c/= '(' && c/= ')' && c/= '{' && c/= '}' && c/= ',' && not (isSpace c)
                , inner = \c -> c /= '[' && c /= ']' && c /= ';' && c/= '(' && c/= ')' && c/= '{' && c/= '}' && c/= ',' && not (isSpace c)
                , reserved = Set.fromList []
                }
            |= parseParams
        , parseVar
    ]

parseParams : Parser (List Term)
parseParams =
    oneOf [
        succeed identity
            |. symbol "["
            |= loop [] listTerm
            |. symbol "]"
        , succeed []
        ]


listTerm : List Term -> Parser (Step (List Term) (List Term))
listTerm revTerms =
  oneOf
    [ succeed (\term-> Loop (term :: revTerms))
        |= parseTerm
        |. spaces
        |. symbol ";"
        |. spaces
    , succeed ()
        |> map (\_ -> Done (List.reverse revTerms))
    ]

parserFLPO : Parser FormulaLPO
parserFLPO =
  oneOf
  [ succeed Exists
    |.symbol "EXISTS"
    |.spaces
    |.symbol "{"
    |.spaces
    |= parseVar
    |.spaces
    |.symbol "}"
    |.spaces
    |= lazy(\_ -> parserFLPO)
    |.spaces

  , succeed Forall
    |.symbol "FORALL"
    |.spaces
    |.symbol "{"
    |.spaces
    |= parseVar
    |.spaces
    |.symbol "}"
    |.spaces
    |= lazy(\_ -> parserFLPO)
    |.spaces

  , succeed Equal
    |= parseTerm
    |. spaces
    |. symbol "="
    |. spaces
    |= parseTerm
    |. spaces

  , succeed Neg
    |.symbol "NOT"
    |.spaces
    |= lazy(\_ -> parserFLPO)

  , succeed Insat
    |.symbol "INSAT"
    |.spaces

  , succeed identity
    |. symbol "("
    |. spaces
    |= lazy(\_ -> expression)
    |. spaces
    |. symbol ")"
    |. spaces

    ,succeed Pred
        |= variable
            { start = \c -> not (Char.isLower c || c == '_' || c == '!' || c == '(' || c == ')' || c == '[' || c == ']' || c== '{' || c== '}' || isSpace c)
            , inner = \c -> Char.isAlphaNum c || not (c == '(' || c == ')' || c == '[' || c == ']' || c== '{' || c== '}' || isSpace c)
            , reserved = Set.fromList ["NOT", "AND", "OR", "IMPLIES", "EQUIV", "EXISTS", "FORALL", "INSAT" ]
            }
        |= parseParams
        |.spaces
  ]

expression : Parser FormulaLPO
expression =
  parserFLPO |> andThen (expressionAux [])

type Operator = AndOp | OrOp | ImplOp | EquivOp

operator : Parser Operator
operator =
    oneOf

    [ Parser.map (\_ -> AndOp) (symbol "AND")
    , Parser.map (\_ -> OrOp) (symbol "OR")
    , Parser.map (\_ -> ImplOp) (symbol "IMPLIES")
    , Parser.map (\_ -> EquivOp) (symbol "EQUIV")
    ]

expressionAux : List (FormulaLPO, Operator) -> FormulaLPO -> Parser FormulaLPO
expressionAux revOps expr =
  oneOf
    [ succeed Tuple.pair
        |. spaces
        |= operator
        |. spaces
        |= parserFLPO
        |> andThen (\(op, newExpr) -> expressionAux ((expr,op) :: revOps) newExpr)
    , lazy (\_ -> succeed (finalize revOps expr))
    ]

finalize : List (FormulaLPO, Operator) -> FormulaLPO -> FormulaLPO
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


fromStringToSubstitutionLPO : String -> (Maybe Substitution, String)
fromStringToSubstitutionLPO x =
  if x == "" then
        (Maybe.Nothing, "Argument is empty")
  else
    case run parserSubstitution x of

      Ok y-> (Maybe.Just y, "")

      Err y -> (Maybe.Nothing, Debug.toString y)

checkSubstitutionLPO : (Maybe Substitution, String) -> Bool
checkSubstitutionLPO  (subs, _) =
  case subs of
    Nothing ->  False
    _ -> True

extractReadSubstitutionLPO : (Maybe Substitution, String) -> Substitution

extractReadSubstitutionLPO (subs, _) = Maybe.withDefault Dict.empty subs

parserSubstitution : Parser Substitution
parserSubstitution =
  succeed Dict.fromList
    |.spaces
    |= Parser.sequence
      { start = "{"
      , separator = ","
      , end = "}"
      , spaces = spaces
      , item = parserSubsChange
      , trailing = Forbidden -- demand a trailing semi-colon
      }

parserSubsChange : Parser (String, Term)
parserSubsChange =
  succeed Tuple.pair
    |= variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList []
        }
    |. symbol "/"
    |= parseTerm
    |. spaces

------------------------------------------------------------------------------------------------------------
--                                            REPRESENTATION                                              --
------------------------------------------------------------------------------------------------------------

toStringTerm : Term -> String
toStringTerm x =
    case x of
        Var s-> s
        Func n params -> if List.isEmpty params then
                            n
                         else
                            n ++ " (" ++ (String.join ", " <| List.map toStringTerm params) ++ ")"

toStringFLPO : FormulaLPO -> String
toStringFLPO x =
    case x of
        Pred n params -> n ++ " (" ++ (String.join ", " <| List.map toStringTerm params) ++ ")"
        Equal t1 t2 -> "(" ++ toStringTerm t1 ++ " = " ++ toStringTerm t2 ++ ")"
        Neg p -> "¬ " ++ toStringFLPO p
        Conj p q -> "( " ++ toStringFLPO p ++ " ∧ "  ++ toStringFLPO q ++ " )"
        Disj p q -> "( " ++ toStringFLPO p ++ " ∨ "  ++ toStringFLPO q ++ " )"
        Impl p q -> "( " ++ toStringFLPO p ++ " → "  ++ toStringFLPO q ++ " )"
        Equi p q -> "( " ++ toStringFLPO p ++ " ↔ "  ++ toStringFLPO q ++ " )"
        Exists v p -> "∃ " ++ toStringTerm v ++ " " ++ toStringFLPO p
        Forall v p -> "∀ " ++ toStringTerm v ++ " " ++ toStringFLPO p
        Insat -> "⊥"

toStringSLOP : List FormulaLPO -> String
toStringSLOP xs = "{" ++ (String.join ", " <| List.map toStringFLPO xs) ++ "}"

toLatexFLPO : FormulaLPO -> String
toLatexFLPO x = "$" ++ toLatexFLPOAux x ++ "$"

toLatexFLPOAux : FormulaLPO -> String
toLatexFLPOAux x =
    case x of
        Pred n params ->  n ++ "\\left(" ++ (String.join ", " <| List.map toStringTerm params) ++ "\\right)"
        Equal t1 t2 ->  toStringTerm t1 ++ " = " ++ toStringTerm t2
        Neg p -> "\\neg " ++ toLatexFLPOAux p
        Conj p q -> "\\left( " ++ toLatexFLPOAux p ++ " \\wedge "  ++ toLatexFLPOAux q ++ " \\right)"
        Disj p q -> "\\left( " ++ toLatexFLPOAux p ++ " \\vee "  ++ toLatexFLPOAux q ++ " \\right)"
        Impl p q -> "\\left( " ++ toLatexFLPOAux p ++ "\\rightarrow "  ++ toLatexFLPOAux q ++ " \\right)"
        Equi p q -> "\\left( " ++ toLatexFLPOAux p ++ "\\leftrightarrow "  ++ toLatexFLPOAux q ++ " \\right)"
        Exists v p -> "\\exists " ++ toStringTerm v ++ "\\," ++ toLatexFLPOAux p
        Forall v p -> "\\forall " ++ toStringTerm v ++ "\\," ++ toLatexFLPOAux p
        Insat -> "\\perp "

toLatexSLOP : List FormulaLPO -> String
toLatexSLOP xs = "$ \\left\\lbrace" ++ (String.join ", " <| List.map toLatexFLPOAux xs) ++ "\\right\\rbrace $"

formTree : FormulaLPO -> String
formTree x =  String.replace "\n\n" "\n" <| formTree2DOT <| formTree1 x

formTree1 : FormulaLPO -> Graph String ()
formTree1 x =
    case x of
        Pred _ _ -> fromNodesAndEdges [Node 0 (toStringFLPO x)] []
        Equal _ _ -> fromNodesAndEdges [Node 0 (toStringFLPO x)] []
        Neg p->
            let (nodes, edges) = formTreeAux p 1 in
                fromNodesAndEdges (Node 0 (toStringFLPO x)::nodes) (Edge 0 1 ()::edges)
        Conj p q ->
            let
                (nodes1, edges1) = formTreeAux p 1 in
                let nextid = List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        fromNodesAndEdges (Node 0 (toStringFLPO x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 nextid ()] ++ edges1 ++ edges2)
        Disj p q ->
            let
                (nodes1, edges1) = formTreeAux p 1 in
                let nextid = List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        fromNodesAndEdges (Node 0 (toStringFLPO x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 nextid ()] ++ edges1 ++ edges2)
        Impl p q ->
            let
                (nodes1, edges1) = formTreeAux p 1 in
                let nextid = List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        fromNodesAndEdges (Node 0 (toStringFLPO x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 nextid ()] ++ edges1 ++ edges2)
        Equi p q ->
            let
                (nodes1, edges1) = formTreeAux p 1 in
                let nextid = List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        fromNodesAndEdges (Node 0 (toStringFLPO x)::(nodes1 ++ nodes2)) ([Edge 0 1 (), Edge 0 nextid ()] ++ edges1 ++ edges2)
        Exists _ p ->
            let (nodes, edges) = formTreeAux p 1 in
                fromNodesAndEdges (Node 0 (toStringFLPO x)::nodes) (Edge 0 1 ()::edges)
        Forall _ p ->
            let (nodes, edges) = formTreeAux p 1 in
                fromNodesAndEdges (Node 0 (toStringFLPO x)::nodes) (Edge 0 1 ()::edges)
        Insat -> fromNodesAndEdges [Node 0 (toStringFLPO x)] []

formTreeAux : FormulaLPO -> NodeId -> (List (Node String), List (Edge ()))
formTreeAux x nodeid=
    case x of
        Pred _ _ ->  ([Node nodeid (toStringFLPO x)], [])
        Equal _ _ -> ([Node nodeid (toStringFLPO x)], [])
        Neg p ->
            let (nodes, edges) = formTreeAux p (nodeid + 1) in
                (Node nodeid (toStringFLPO x)::nodes, Edge nodeid (nodeid + 1) ()::edges)
        Conj p q ->
            let (nodes1, edges1) = formTreeAux p (nodeid + 1) in
                let nextid = nodeid + List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        ( Node nodeid (toStringFLPO x)::(nodes1 ++ nodes2),  [Edge nodeid (nodeid + 1) (), Edge nodeid nextid ()] ++ edges1 ++ edges2)
        Disj p q ->
            let (nodes1, edges1) = formTreeAux p (nodeid + 1) in
                let nextid = nodeid + List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        ( Node nodeid (toStringFLPO x)::(nodes1 ++ nodes2),  [Edge nodeid (nodeid + 1) (), Edge nodeid nextid ()] ++ edges1 ++ edges2)
        Impl p q ->
            let (nodes1, edges1) = formTreeAux p (nodeid + 1) in
                let nextid = nodeid + List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        ( Node nodeid (toStringFLPO x)::(nodes1 ++ nodes2),  [Edge nodeid (nodeid + 1) (), Edge nodeid nextid ()] ++ edges1 ++ edges2)
        Equi p q ->
            let (nodes1, edges1) = formTreeAux p (nodeid + 1) in
                let nextid = nodeid + List.length nodes1 + 1 in
                    let (nodes2, edges2) = formTreeAux q nextid in
                        ( Node nodeid (toStringFLPO x)::(nodes1 ++ nodes2),  [Edge nodeid (nodeid + 1) (), Edge nodeid nextid ()] ++ edges1 ++ edges2)
        Exists _ p ->
            let (nodes, edges) = formTreeAux p (nodeid + 1) in
                (Node nodeid (toStringFLPO x)::nodes, Edge nodeid (nodeid + 1) ()::edges)
        Forall _ p ->
            let (nodes, edges) = formTreeAux p (nodeid + 1) in
                (Node nodeid (toStringFLPO x)::nodes, Edge nodeid (nodeid + 1) ()::edges)
        Insat -> ([Node nodeid (toStringFLPO x)], [])

formTree2DOT : Graph String () -> String
formTree2DOT ft =
    let myStyles =
            { defaultStyles | node = "shape=plaintext, color=black", edge = "dir=none"}
    in
        outputWithStyles myStyles (\x -> Just x) (\_ -> Nothing) ft
