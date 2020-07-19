module Modules.B_Expressions exposing (B_Expr, evaluateBExpr, expressionB, parseBExpr, toStringB_Expression)

import Modules.A_Expressions exposing (A_Expr, expressionA, evaluateAExpr, toStringAExpr)
import Modules.AuxiliarFunctions exposing (uncurry)
import Parser exposing (Parser, run, oneOf, succeed, (|.), (|=), symbol, lazy, andThen)
import List.Extra exposing (zip)

type Comparator
    = EQ
    | NE
    | GT
    | LT
    | GE
    | LE

type alias Condition =
    { comp : Comparator
    , fmember : A_Expr
    , smember : A_Expr
    }

createCondition : A_Expr -> Comparator -> A_Expr -> Condition
createCondition f c s =
    { comp = c, fmember = f, smember = s }

compCondition : Parser Comparator
compCondition=
    oneOf
        [ succeed GE
            |. symbol ">="
        , succeed LE
            |. symbol "<="
        , succeed NE
            |. symbol "!"
            |. symbol "="
        , succeed GT
            |. symbol ">"
        , succeed LT
            |. symbol "<"
        , succeed EQ
            |. symbol "="
        ]

condition : Parser Condition
condition =
    succeed createCondition
        |= expressionA
        |= compCondition
        |= expressionA

evalCond : Condition -> ( List String, List Int ) -> Bool
evalCond cond ( lids, lvals ) =
    let
        fmemberVal =
            evaluateAExpr cond.fmember (zip lids lvals)
    in
    let
        smemberVal =
            evaluateAExpr cond.smember (zip lids lvals)
    in
    case fmemberVal of
        Nothing ->
            False

        Just fv ->
            case smemberVal of
                Nothing ->
                    False

                Just sv ->
                    case cond.comp of
                        EQ ->
                            fv == sv

                        NE ->
                            not <| fv == sv

                        GT ->
                            fv > sv

                        LT ->
                            fv < sv

                        GE ->
                            fv >= sv

                        LE ->
                            fv <= sv
type B_Expr = T 
            | F
            | And B_Expr B_Expr
            | Or B_Expr B_Expr
            | Not B_Expr
            | Cond Condition
            | Error String

type B_Operator = AndOp | OrOp

operatorB : Parser B_Operator
operatorB = 
    oneOf
    [ Parser.map (\_ -> AndOp) (symbol "AND")
    , Parser.map (\_ -> OrOp) (symbol "OR")
    ]
         
evaluateBExpr : B_Expr -> (List String , List Int) -> Maybe (Bool)
evaluateBExpr expr (ls, li)= case expr of
    T -> Just True
    F -> Just False
    And e1 e2 -> 
        case evaluateBExpr e1 (ls, li) of
            Nothing -> Nothing
            Just y ->
                case evaluateBExpr e2 (ls, li) of
                   Nothing -> Nothing
                   Just z -> Just <| y && z
    Or e1 e2 -> 
        case evaluateBExpr e1 (ls, li) of
            Nothing -> Nothing
            Just y ->
                case evaluateBExpr e2 (ls, li) of
                   Nothing -> Nothing
                   Just z -> Just <| y || z
    Not e -> 
        case evaluateBExpr e  (ls, li) of
            Nothing -> Nothing
            Just y -> Just <| not y
    
    Cond c -> Just <| evalCond c (ls, li)

    Error _ -> Nothing

parseBExpr : String -> B_Expr
parseBExpr str =  if str == "" then
                     Error "Empty expression"
                 else
                    case run expressionB str of
      
                        Ok y-> y

                        Err _ -> Error "Syntax Error"

termBExpr : Parser B_Expr
termBExpr = oneOf 
                [
                 succeed T
                    |. symbol "T"
                , succeed F
                    |. symbol "F"
                , succeed Not
                    |. symbol "NOT"
                    |= lazy (\_ -> expressionB)
                ,succeed Cond
                    |. symbol "["
                    |= condition
                    |. symbol  "]"
                , succeed identity
                    |. symbol "("
                    |= lazy (\_ -> expressionB)
                    |. symbol  ")"
                ]


expressionB : Parser B_Expr
expressionB = termBExpr |> andThen (expressionBAux []) 

expressionBAux : List (B_Expr, B_Operator) -> B_Expr -> Parser B_Expr
expressionBAux revOps bExpr= oneOf 
                                [
                                    succeed Tuple.pair
                                      |= operatorB
                                      |= termBExpr
                                      |> andThen (\(op, newExpr) -> expressionBAux ((bExpr,op) :: revOps) newExpr)
                                    
                                    , lazy (\_ -> succeed (finalize revOps bExpr))
                                ]

finalize : List (B_Expr, B_Operator) -> B_Expr -> B_Expr
finalize revOps finalExpr = case revOps of
    [] -> finalExpr

    -- And operation have the maximum priorty, so module have a unique case

    (expr, AndOp) :: otherRevOps ->
        finalize otherRevOps (And expr finalExpr)

    
    -- Or have the second maximum priority, so we need to determine how parser's going to do if it searches a module after and if it searches something different
        
    (expr, OrOp) :: (expr2, AndOp):: otherRevOps ->
        Or (finalize ( (expr2, AndOp) :: otherRevOps) expr) finalExpr
    
    (expr, OrOp) :: otherRevOps ->
        finalize otherRevOps (Or expr finalExpr)

toStringComparator : Comparator -> String
toStringComparator c =
    case c of
        EQ ->
            "="

        NE ->
            "!="

        GT ->
            ">"

        LT ->
            "<"

        GE ->
            ">="

        LE ->
            "<="

toStringCondition : Condition -> String
toStringCondition c =
    toStringAExpr c.fmember ++ toStringComparator c.comp ++ toStringAExpr c.smember

toStringB_Expression : B_Expr-> String
toStringB_Expression bexpr = "{" ++ toStringB_ExpressionAux bexpr ++ "}"

toStringB_ExpressionAux : B_Expr-> String
toStringB_ExpressionAux bexpr =
    case bexpr of
        T -> "T"
        F -> "F"
        And e1 e2 -> "(" ++ toStringB_ExpressionAux e1 ++ "AND" ++ toStringB_ExpressionAux e2 ++ ")"
        Or e1 e2 -> "(" ++ toStringB_ExpressionAux e1 ++ "OR" ++ toStringB_ExpressionAux e2 ++ ")"
        Not e -> "( NOT" ++ toStringB_ExpressionAux e ++ ")"
        Cond c -> "[" ++ toStringCondition c ++ "]"
        Error s -> s
    
            
    