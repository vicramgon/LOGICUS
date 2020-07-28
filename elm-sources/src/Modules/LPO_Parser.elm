module Modules.LPO_Parser exposing(parserFormula, parserFormulaSet, parseTerm)

import Char
import Set
import String
import Maybe
import Char.Extra exposing (isSpace)
import Parser exposing (Parser, run, variable, oneOf, succeed, spaces, (|.), (|=), symbol, lazy, andThen, loop, Step(..), map)


import Modules.SintaxSemanticsLPO exposing (Term(..), FormulaLPO(..))


parserFormula : String -> (Maybe (FormulaLPO), String)
parserFormula x =
  if x == "" then
        (Maybe.Nothing, "Argument is empty")
  else
    case run lpoParser ("(" ++ x ++ ")") of
      
      Ok y-> (Maybe.Just y, "")

      Err y -> (Maybe.Nothing, Debug.toString y)

parserFormulaSet : String -> List (Maybe(FormulaLPO), String)
parserFormulaSet x =  List.map parserFormula <| String.split "//" x

parseVar : Parser Term
parseVar = succeed Var
            |= variable
                { start = Char.isLower 
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.fromList ["exists", "forall"]
                }


parseTerm: Parser Term 
parseTerm =
    oneOf [
        succeed Func
            |. symbol "_"
            |= variable
                { start =  \c -> c /= '[' && c /= ']' && c /= ';' && c/= '(' && c/= ')' && not (isSpace c)
                , inner = \c -> c /= '[' && c /= ']' && c /= ';' && c/= '(' && c/= ')' && not (isSpace c)
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
    |.symbol "!"
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
            { start = \c -> not (Char.isLower c || c == '_' || c == '!' || c == '(' || c == ')' || c == '[' || c == ']' || isSpace c)
            , inner = \c -> Char.isAlphaNum c || not ( c == '!' || c == '(' || c == ')' || c == '[' || c == ']'  || isSpace c)
            , reserved = Set.fromList []
            }
        |= parseParams
        |.spaces 
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

