module Modules.LPBig_Parser exposing(expandFormBigProp, expandSetBigProp, toStringBigProp, toStringSetBigProp, toStringBigPropFile, toStringSetBigPropFile, parseBigProp, parseSetBigProp, conjPropToSet)

import Parser exposing (..)
import List exposing (head, repeat, length, map, filter, all)
import Tuple exposing (first, second)
import Maybe.Extra exposing (isNothing)
import Maybe exposing (withDefault)
import Set exposing (fromList)
import String exposing (replace, split, concat, join)
import List.Extra exposing (last, dropWhile, zip, cartesianProduct)
import Regex

import Modules.A_Expressions exposing (A_Expr, evaluateAExpr, parseAExpr, toStringAExpr, expressionA)
import Modules.AuxiliarFunctions exposing (deleteFirstLs, init)

import Html exposing (text)



type Comparator = EQ | NE | GT | LT | GE | LE


type alias Condition = {comp : Comparator, fmember : A_Expr, smember : A_Expr }

createCondition : A_Expr -> Comparator -> A_Expr -> Condition
createCondition f c s = {comp = c, fmember = f, smember = s }

type alias Ident = { name : String, values : List Int}

createIdent : String -> List Int -> Ident
createIdent str li = {name = str, values = li}

type BigProp =  Atom String
                | Neg BigProp
                | Conj BigProp BigProp
                | Disj BigProp BigProp
                | Impl BigProp BigProp
                | Equi BigProp BigProp
                | BAnd (List Ident) (List Condition) BigProp
                | BOr  (List Ident) (List Condition) BigProp
                | Error String

baseAtomBigProp : Parser String
baseAtomBigProp = 
    variable
    { start = Char.isLower
    , inner = \c -> Char.isLower c || Char.isDigit c
    , reserved = Set.fromList []
    }

nameIdentBigProp : Parser String
nameIdentBigProp = 
    variable
    { start = Char.isUpper
    , inner = Char.isUpper
    , reserved = Set.fromList []
    }

valuesIdentBigProp : Parser (List Int)
valuesIdentBigProp =
     Parser.sequence
        { start = ":"
        , separator = ","
        , end = "#"
        , spaces = spaces
        , item = oneOf
                    [ succeed negate
                         |. symbol "-"
                         |= int
                      , int
                    ]
        , trailing = Optional
        }


identBigProp : Parser Ident
identBigProp =
    succeed createIdent
        |= nameIdentBigProp
        |= valuesIdentBigProp


listIdentBigProp : Parser (List Ident)
listIdentBigProp = 
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = spaces
        , item = identBigProp
        , trailing = Optional -- demand a trailing semi-colon
        }

compConditionBigProp : Parser Comparator
compConditionBigProp = oneOf 
                            [
                              succeed GE
                                |. symbol ">"
                                |. symbol "="
                            , succeed LE
                                |. symbol "<"
                                |. symbol "="
                            , succeed NE
                                |. symbol "!"
                                |. symbol "="
                            , succeed GT
                                |. symbol ">"
                            , succeed LT
                                |. symbol "<"
                            ,succeed EQ
                                |. symbol "="
                            ]

conditionBigProp : Parser Condition
conditionBigProp =
    succeed createCondition
        |= expressionA
        |= compConditionBigProp
        |= expressionA

listconditionBigProp : Parser (List Condition)
listconditionBigProp = oneOf [
                                Parser.sequence
                                    { start = "{"
                                    , separator = ","
                                    , end = "}"
                                    , spaces = spaces
                                    , item = conditionBigProp
                                    , trailing = Optional -- demand a trailing semi-colon
                                    }
                                , succeed []
                                    |. symbol "#"
                            ]

atomVarBigProp : Parser String
atomVarBigProp = getChompedString <|
                    succeed ()
                    |. baseAtomBigProp
                    |. chompWhile (\c -> Char.isUpper c || c == '_' || Char.isDigit c)
                        


termBigProp : Parser BigProp
termBigProp = oneOf 
                    [
                    succeed Atom 
                        |= atomVarBigProp
                    ,succeed BAnd
                        |. symbol "&"
                        |. symbol "_"
                        |= listIdentBigProp
                        |= listconditionBigProp
                        |. symbol "("
                        |= lazy(\_ -> expressionBigProp)
                        |. symbol ")"
                    , succeed BOr
                        |. symbol "|"
                        |. symbol "_"
                        |= listIdentBigProp
                        |= listconditionBigProp
                        |. symbol "("
                        |= lazy(\_ -> expressionBigProp)
                        |. symbol ")"
                    , succeed identity 
                        |. symbol "("
                        |= lazy(\_ -> expressionBigProp)
                        |. symbol ")"
                    , succeed Neg 
                        |. symbol "¬"
                        |= lazy(\_ -> termBigProp)
                    ]

expressionBigProp: Parser BigProp
expressionBigProp = termBigProp |> andThen (expressionBigPropAux [])

type Operator = AndOp | OrOp | ImplOp | EquivOp

operator : Parser Operator
operator = 
    oneOf
    [ Parser.map (\_ -> AndOp) (symbol "&")
    , Parser.map (\_ -> OrOp) (symbol "|")
    , Parser.map (\_ -> ImplOp) (symbol "->")
    , Parser.map (\_ -> EquivOp) (symbol "<->")
    ]

expressionBigPropAux : List (BigProp, Operator) -> BigProp -> Parser BigProp
expressionBigPropAux revOps expr =
  oneOf
    [ succeed Tuple.pair
        |= operator
        |= termBigProp
        |> andThen (\(op, newExpr) -> expressionBigPropAux ((expr,op) :: revOps) newExpr)
    , lazy (\_ -> succeed (finalize revOps expr))
    ]

finalize : List (BigProp, Operator) -> BigProp -> BigProp
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

toStringIdentBigProp : Ident -> String
toStringIdentBigProp i = i.name ++  (replace "[" ":"<| replace "]" "#" <| Debug.toString <| i.values) 

toStringListIdentBigProp: List Ident -> String
toStringListIdentBigProp lid = let aux lp str = case lp of
                                                [] -> "{" ++ str ++ "}" 
                                                i::[] -> "{" ++ str ++ toStringIdentBigProp i ++ "}"
                                                i::rlp -> aux rlp (str ++ (toStringIdentBigProp i) ++ ",")
                                in
                                    aux lid ""

toStringComparator : Comparator -> String
toStringComparator c = case c of
                         EQ -> "="
                         NE -> "!="
                         GT -> ">"
                         LT -> "<"
                         GE -> ">="
                         LE -> "<="
        

toStringConditionBigProp : Condition -> String
toStringConditionBigProp c = (toStringAExpr c.fmember) ++ (toStringComparator c.comp) ++ (toStringAExpr c.smember)

toStringListConditionBigProp: List Condition -> String
toStringListConditionBigProp lcond = let aux lp str = case lp of
                                                [] -> "{" ++ str ++ "}" 
                                                i::[] -> "{" ++ str ++ toStringConditionBigProp i ++ "}"
                                                i::rlp -> aux rlp (str ++ (toStringConditionBigProp i) ++ ",")
                                in
                                    aux lcond ""
        


parseBigProp : String -> BigProp
parseBigProp str = if str == "" then
                     Error "Empty expression"
                 else
                    case ( run expressionBigProp str) of
      
                        Ok y-> y

                        Err err -> Error ("Syntax Error: " ++ (Debug.toString <| err) ++ str)

parseSetBigProp : String -> List BigProp
parseSetBigProp str =  List.map (parseBigProp) <| init <| split ";" str

expandBigProp : BigProp -> Maybe BigProp
expandBigProp prop = case prop of
    Atom p -> Just <| Atom p
    Neg p ->  case expandBigProp p of
        Nothing -> Nothing
        Just m -> Just <| Neg <| m
    Conj p q -> case expandBigProp p of
        Nothing -> Nothing
        Just m -> case expandBigProp q of
            Nothing -> Nothing
            Just m2 ->  Just <| Conj m m2
    Disj p q -> case expandBigProp p of
        Nothing -> Nothing
        Just m -> case expandBigProp q of
            Nothing -> Nothing
            Just m2 ->  Just <| Disj m m2
    Impl p q -> case expandBigProp p of
        Nothing -> Nothing
        Just m -> case expandBigProp q of
            Nothing -> Nothing
            Just m2 ->  Just <| Impl m m2
    Equi p q -> case expandBigProp p of
        Nothing -> Nothing
        Just m -> case expandBigProp q of
            Nothing -> Nothing
            Just m2 ->  Just <| Equi m m2
    
    BAnd li lc p -> expandBForm "&" li lc p 

    BOr  li lc p -> expandBForm "|" li lc p

    Error err -> Just <| Error err
        


expandBForm : String -> List Ident -> List Condition -> BigProp -> Maybe BigProp
expandBForm s li lc p = let keys = List.map (\x -> x.name) li in
                        let values = List.map (\x -> x.values) li in
                        let posibilities = filter (\x -> all (\y -> y) <| List.map (\y -> evalCond y (keys, x)) lc) <| cartesianProduct <| values in 
                                if posibilities == [] then
                                    Nothing
                                else
                                    Just <| expandFormula keys posibilities p s

                                         

evalCond : Condition -> (List String, List Int) -> Bool
evalCond cond (lids, lvals) = let fmemberVal = evaluateAExpr cond.fmember (zip lids lvals) in
                  let smemberVal = evaluateAExpr cond.smember (zip lids lvals) in
                        case fmemberVal of
                            Nothing -> False

                            Just fv -> case smemberVal of
                                Nothing -> False
                                    
                                Just sv -> case cond.comp of
                                                EQ -> fv == sv
                                                NE -> not <| fv == sv
                                                GT -> fv > sv
                                                LT -> fv < sv
                                                GE -> fv >= sv
                                                LE -> fv <= sv

adecuateString s = replace "{}" "#" <| replace "⟷" "<->" <| replace "⟶" "->" <| replace "∨" "|" <| replace "∧" "&" <| replace " " "" <| replace "\n" "" <| replace "\r" "" <| replace "\t" "" s

replaceVars : (List String, List Int) -> BigProp -> BigProp
replaceVars (lids, lvals) p = case head lids of
    Nothing -> p
    Just id -> let val = withDefault 0 <| head lvals in
                   replaceVars (deleteFirstLs lids, deleteFirstLs lvals) (parseBigProp <| adecuateString <| replace id (String.fromInt val) <| toStringBigProp p)

expandFormula : List String -> List (List Int) -> BigProp -> String -> BigProp
expandFormula lis llv prop symb = let aux lids llvals p s res = case llvals of
                                                                    [] -> Error "No posibilities to expand the formula"
                                                            
                                                                    lvals::[] -> case (expandBigProp <| replaceVars (lids, lvals) p) of
                                                                        Nothing -> parseBigProp <| adecuateString <| Regex.replace (withDefault Regex.never <| Regex.fromString "&$") (\_ -> "") <| res
                                                                            
                                                                        Just exp -> parseBigProp <| adecuateString <| res ++ "(" ++ (toStringBigProp <| exp)  ++ ")"

                                                                    lvals::rlvals -> case (expandBigProp <| replaceVars (lids, lvals) p) of
                                                                        Nothing ->  aux (lids) (rlvals) p s res
                                                                            
                                                                        Just exp -> aux (lids) (rlvals) p s (res ++ "(" ++ (toStringBigProp <| exp) ++ ")" ++ symb )
                                  in
                                    aux lis llv prop symb ""

expandFormBigProp: String -> BigProp
expandFormBigProp x = case expandBigProp <|parseBigProp  <| adecuateString x of
    Nothing -> Error "No-expansion-returned"
    
    Just y -> y
         

expandSetBigProp : String -> List BigProp
expandSetBigProp x =  List.map (expandFormBigProp) <| init <| split ";" x

toStringBigProp : BigProp -> String
toStringBigProp prop = case prop of 
        Atom p -> p
        Neg p -> "¬ " ++ (toStringBigProp p) 
        Conj p q -> "( " ++ (toStringBigProp p) ++ " ∧ "  ++ (toStringBigProp q) ++ " )"
        Disj p q -> "( " ++ (toStringBigProp p) ++ " ∨ "  ++ (toStringBigProp q) ++ " )"
        Impl p q -> "( " ++ (toStringBigProp p) ++ " ⟶ "  ++ (toStringBigProp q) ++ " )"
        Equi p q -> "( " ++ (toStringBigProp p) ++ " ⟷ "  ++ (toStringBigProp q) ++ " )"
        BAnd li lc p -> "∧_" ++ (toStringListIdentBigProp li) ++ (toStringListConditionBigProp lc) ++ "(" ++ toStringBigProp p ++ ")"
        BOr  li lc p -> "∨_" ++ (toStringListIdentBigProp li) ++ (toStringListConditionBigProp lc) ++ "(" ++ toStringBigProp p ++ ")"
        Error err -> "Error:" ++ err

toStringBigPropFile : BigProp -> String
toStringBigPropFile x = adecuateString <| toStringBigProp x

toStringSetBigProp : List BigProp -> String
toStringSetBigProp xs = "{" ++ (join "," <| List.map (\x -> toStringBigProp x ++ "\n") <| xs) ++ "}"

toStringSetBigPropFile : List BigProp -> String
toStringSetBigPropFile xs = join "" <| List.map (\x -> toStringBigPropFile x ++ ";\n") <| xs

conjPropToSet : BigProp -> List BigProp
conjPropToSet p = case p of
    Conj p1 p2 -> List.concat [conjPropToSet p1, conjPropToSet p2]
    _ -> [p]

main = text <| Debug.toString <| toStringSetBigProp <| expandSetBigProp "&_ {I:0,1#}#(|_ {J:0,1#} # (pIJ))"