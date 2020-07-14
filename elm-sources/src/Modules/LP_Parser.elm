module Modules.LP_Parser exposing(parserProp, parserPropSet)

import Char
import Set
import String
import Maybe

import Parser exposing (Parser, run, variable, oneOf, succeed, spaces, (|.), (|=), symbol, lazy, andThen)


import Modules.SintaxSemanticsLP exposing (PSymb, Prop, atomProp, negProp, conjProp, disjProp, implProp, equiProp)

parserProp : String -> (Maybe (Prop), String)
parserProp x =
  if x == "" then
        (Maybe.Nothing, "Argument is empty")
  else
    case run lpParser ("(" ++ x ++ ")") of
      
      Ok y-> (Maybe.Just y, "")

      Err y -> (Maybe.Nothing, Debug.toString y)

parserPropSet : String -> List (Maybe(Prop), String)
parserPropSet x =  List.map parserProp <| String.split ";" x

typeVar : Parser PSymb
typeVar =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c
    , reserved = Set.fromList []
    }

lpParser : Parser Prop
lpParser =
  oneOf 
  [ 
    succeed atomProp
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
  
  , succeed negProp
    |.spaces
    |.symbol "¬"
    |.spaces
    |= lazy(\_ -> lpParser)
  ]

expression : Parser Prop
expression =
  lpParser |> andThen (expressionAux [])

type Operator = AndOp | OrOp | ImplOp | EquivOp

operator : Parser Operator
operator = 
    oneOf
    [ Parser.map (\_ -> AndOp) (symbol "&")
    , Parser.map (\_ -> OrOp) (symbol "|")
    , Parser.map (\_ -> ImplOp) (symbol "->")
    , Parser.map (\_ -> EquivOp) (symbol "<->")
    ]

expressionAux : List (Prop, Operator) -> Prop -> Parser Prop
expressionAux revOps expr =
  oneOf
    [ succeed Tuple.pair
        |. spaces
        |= operator
        |. spaces
        |= lpParser
        |> andThen (\(op, newExpr) -> expressionAux ((expr,op) :: revOps) newExpr)
    , lazy (\_ -> succeed (finalize revOps expr))
    ]

finalize : List (Prop, Operator) -> Prop -> Prop
finalize revOps finalExpr =
  case revOps of
    [] ->
      finalExpr

      -- AND EXPRESSIONS CASES
      -- And operation have the maximum priorty, so module have a unique case

    (expr, AndOp) :: otherRevOps ->
      finalize otherRevOps (conjProp expr finalExpr)

      -- OR EXPRESSIONS CASES
      -- Or have the second maximum priority, so we need to determine how parser's going to do if it searches an and after, and if it searches something different.

    (expr, OrOp) :: (expr2, AndOp) :: otherRevOps ->
      disjProp (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, OrOp) :: otherRevOps ->
      finalize otherRevOps (disjProp expr finalExpr)

    -- IMPLICATION EXPRESSIONS CASES

    (expr, ImplOp) :: (expr2, AndOp) :: otherRevOps ->
      implProp (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, ImplOp) :: (expr2, OrOp) :: otherRevOps ->
       implProp (finalize ( (expr2, OrOp) :: otherRevOps) expr) finalExpr

    (expr, ImplOp) :: otherRevOps ->
      finalize otherRevOps (implProp expr finalExpr)

    -- EQUIVALATION EXPRESSIONS CASES

    (expr, EquivOp) :: (expr2, AndOp) :: otherRevOps ->
      equiProp (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: (expr2, OrOp) :: otherRevOps ->
       equiProp (finalize ( (expr2, OrOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: (expr2, ImplOp) :: otherRevOps ->
       equiProp (finalize ( (expr2, ImplOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: otherRevOps ->
      finalize otherRevOps (equiProp expr finalExpr)