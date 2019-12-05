module Modules.LP_Parser exposing(parserFormula, parserSet)

import Browser
import Char
import Html
import Parser exposing (..)
import Set
import Maybe
import String exposing (..)

import Modules.SintaxSemanticsLP exposing (..)
import Modules.AuxiliarFunctions exposing (..)

parserFormula: String -> Prop
parserFormula x =
  if x == "" then
        Atom " "
  else
    case ( run lpParser ("(" ++ x ++ ")")) of
      
      Ok y-> y

      Err y -> Atom "ERROR"

parserSet : String -> List Prop
parserSet x =  List.map parserFormula<| init  <| split ";" x

typeVar : Parser PSymb
typeVar =
  variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList []
    }

lpParser : Parser Prop
lpParser =
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

    (expr, AndOp) :: otherRevOps ->
      finalize otherRevOps (Conj expr finalExpr)

      -- OR EXPRESSIONS CASES

    (expr, OrOp) :: [] ->
      finalize [] (Disj expr finalExpr)

    (expr, OrOp) :: (expr2, AndOp) :: otherRevOps ->
      Disj (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, OrOp) :: (expr2, OrOp) :: otherRevOps ->
      finalize ((expr2, OrOp) :: otherRevOps) (Disj expr finalExpr)

    (expr, OrOp) :: (expr2, ImplOp) :: otherRevOps ->
      finalize ((expr2, ImplOp) :: otherRevOps) (Disj expr finalExpr)

    (expr, OrOp) :: (expr2, EquivOp) :: otherRevOps ->
      finalize ((expr2, EquivOp) :: otherRevOps) (Disj expr finalExpr)

    -- IMPLICATION EXPRESSIONS CASES

    (expr, ImplOp) :: [] ->
      finalize  [] (Impl expr finalExpr)

    (expr, ImplOp) :: (expr2, AndOp) :: otherRevOps ->
      Impl (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, ImplOp) :: (expr2, OrOp) :: otherRevOps ->
       Impl (finalize ( (expr2, OrOp) :: otherRevOps) expr) finalExpr

    (expr, ImplOp) :: (expr2, ImplOp) :: otherRevOps ->
      finalize ((expr2, ImplOp) :: otherRevOps) (Impl expr finalExpr)

    (expr, ImplOp) :: (expr2, EquivOp) :: otherRevOps ->
      finalize ((expr2, EquivOp) :: otherRevOps) (Impl expr finalExpr)

    -- EQUIVALATION EXPRESSIONS CASES

    (expr, EquivOp) :: [] ->
      finalize  [] (Equi expr finalExpr)

    (expr, EquivOp) :: (expr2, AndOp) :: otherRevOps ->
      Equi (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: (expr2, OrOp) :: otherRevOps ->
       Equi (finalize ( (expr2, OrOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: (expr2, ImplOp) :: otherRevOps ->
       Equi (finalize ( (expr2, ImplOp) :: otherRevOps) expr) finalExpr

    (expr, EquivOp) :: (expr2, EquivOp) :: otherRevOps ->
      finalize ((expr2, EquivOp) :: otherRevOps) (Equi expr finalExpr)