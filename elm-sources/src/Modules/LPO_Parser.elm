module Modules.LPO_Parser exposing(parserFormula, parserFormulaSet)

import Char
import Set
import String
import Maybe

import Parser exposing (Parser, run, variable, oneOf, succeed, spaces, (|.), (|=), symbol, lazy, andThen, loop, Step(..), map)


import Modules.SintaxSemanticsLPO exposing (Term(..), FormulaLPO(..), formTree, formTree2DOT)
import Tuple exposing (first)
import Html exposing (Html, text)

parserFormula : String -> (Maybe (FormulaLPO), String)
parserFormula x =
  if x == "" then
        (Maybe.Nothing, "Argument is empty")
  else
    case run lpoParser ("(" ++ x ++ ")") of
      
      Ok y-> (Maybe.Just y, "")

      Err y -> (Maybe.Nothing, Debug.toString y)

parserFormulaSet : String -> List (Maybe(FormulaLPO), String)
parserFormulaSet x =  List.map parserFormula <| String.split ";" x

parseVar : Parser Term
parseVar = succeed Var
            |= variable
                { start = Char.isLower 
                , inner = Char.isAlphaNum
                , reserved = Set.fromList ["exists", "forall"]
                }


parseTerm: Parser Term 
parseTerm =
    oneOf [
        parseVar
        , succeed Const
            |. symbol "\'"
            |= variable
                { start = Char.isAlphaNum
                , inner = Char.isAlphaNum
                , reserved = Set.fromList []
                }
        , succeed Func
            |. symbol "_"
            |= variable
                { start =  \c -> c /= '(' && c /= ')'
                , inner = \c -> c /= '(' && c /= ')'
                , reserved = Set.fromList []
                }
            |. symbol "("
            |= listTerms
            |. symbol ")"
    ]

listTerms : Parser (List Term)
listTerms =
  loop [] listTermAux

listTermAux : List Term -> Parser (Step (List Term) (List Term))
listTermAux revTerms =
  oneOf
    [ succeed (\term-> Loop (term :: revTerms))
        |= parseTerm
        |. spaces
        |. symbol ";"
        |. spaces
    , succeed ()
        |> map (\_ -> Done (List.reverse revTerms))
    ]

lpoParser : Parser FormulaLPO
lpoParser =
  oneOf 
  [ succeed Exists
    |.symbol "exists"
    |.spaces
    |.symbol "{"
    |.spaces
    |= parseVar
    |.spaces
    |.symbol "}"
    |.spaces
    |= lazy(\_ -> lpoParser)
    |.spaces
  
  , succeed Forall
    |.symbol "forall"
    |.spaces
    |.symbol "{"
    |.spaces
    |= parseVar
    |.spaces
    |.symbol "}"
    |.spaces
    |= lazy(\_ -> lpoParser)
    |.spaces

    ,succeed Pred
        |= variable
            { start = Char.isUpper
            , inner = Char.isAlphaNum
            , reserved = Set.fromList []
            }
        |.spaces
        |. symbol "("
        |.spaces
        |= listTerms
        |.spaces
        |. symbol ")"
        |.spaces

  , succeed Equal
    |= parseTerm
    |. spaces
    |. symbol "="
    |. spaces
    |= parseTerm
    |. spaces  
  
  , succeed Neg
    |.symbol "¬"
    |.spaces
    |= lazy(\_ -> lpoParser)

  , succeed Insat
    |.spaces
    |.symbol "!"
    |.spaces

  , succeed identity 
    |. symbol "("
    |. spaces
    |= lazy(\_ -> expression)
    |. spaces
    |. symbol ")"
    |. spaces  
  ]

expression : Parser FormulaLPO
expression =
  lpoParser |> andThen (expressionAux [])

type Operator = AndOp | OrOp | ImplOp | EquivOp

operator : Parser Operator
operator = 
    oneOf
    [ Parser.map (\_ -> AndOp) (symbol "&")
    , Parser.map (\_ -> OrOp) (symbol "|")
    , Parser.map (\_ -> ImplOp) (symbol "->")
    , Parser.map (\_ -> EquivOp) (symbol "<->")
    ]

expressionAux : List (FormulaLPO, Operator) -> FormulaLPO -> Parser FormulaLPO
expressionAux revOps expr =
  oneOf
    [ succeed Tuple.pair
        |. spaces
        |= operator
        |. spaces
        |= lpoParser
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

ejemplo : String
ejemplo = "exists{x}(_suma(y,y) = _producto(x,'0) | menor('1, y))"
main : Html msg
main = text <| formTree2DOT <| formTree <| Maybe.withDefault Insat <| Tuple.first <| parserFormula <| "exists{x}((_+(y;y;) = _·(x;'0;)) | Menor('1; y;))"