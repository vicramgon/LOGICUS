module Modules.A_Expressions exposing (A_Expr, evaluateAExpr, expressionA, parseAExpr, toStringAExpr)

import List exposing (filter, head)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (isNothing)
import Parser exposing (..)
import Set exposing (fromList)
import Tuple exposing (first, second)
import Html exposing (text)


type A_Expr
    = Number Int
    | Var String
    | Add A_Expr A_Expr
    | Dif A_Expr A_Expr
    | Mul A_Expr A_Expr
    | Div A_Expr A_Expr
    | Mod A_Expr A_Expr
    | Error String


varsInA_Expr : A_Expr -> List String
varsInA_Expr expr =
    case expr of
        Number i ->
            []

        Var s ->
            [ s ]

        Add e1 e2 ->
            varsInA_Expr e1 ++ varsInA_Expr e2

        Dif e1 e2 ->
            varsInA_Expr e1 ++ varsInA_Expr e2

        Mul e1 e2 ->
            varsInA_Expr e1 ++ varsInA_Expr e2

        Div e1 e2 ->
            varsInA_Expr e1 ++ varsInA_Expr e2

        Mod e1 e2 ->
            varsInA_Expr e1 ++ varsInA_Expr e2

        Error s ->
            []


evaluateAExpr : A_Expr -> List ( String, Int ) -> Maybe Int
evaluateAExpr expr ls =
    case expr of
        Number i ->
            Just i

        Var s ->
            case head <| filter (\x -> s == first x) ls of
                Nothing ->
                    Nothing

                Just t ->
                    Just (second t)

        Add e1 e2 ->
            if isNothing (evaluateAExpr e1 ls) || isNothing (evaluateAExpr e2 ls) then
                Nothing

            else
                Just (withDefault 0 (evaluateAExpr e1 ls) + withDefault 0 (evaluateAExpr e2 ls))

        Dif e1 e2 ->
            if isNothing (evaluateAExpr e1 ls) || isNothing (evaluateAExpr e2 ls) then
                Nothing

            else
                Just (withDefault 0 (evaluateAExpr e1 ls) - withDefault 0 (evaluateAExpr e2 ls))

        Mul e1 e2 ->
            if isNothing (evaluateAExpr e1 ls) || isNothing (evaluateAExpr e2 ls) then
                Nothing

            else
                Just (withDefault 0 (evaluateAExpr e1 ls) * withDefault 0 (evaluateAExpr e2 ls))

        Div e1 e2 ->
            if isNothing (evaluateAExpr e1 ls) || isNothing (evaluateAExpr e2 ls) then
                Nothing

            else
                Just (withDefault 0 (evaluateAExpr e1 ls) // withDefault 0 (evaluateAExpr e2 ls))

        Mod e1 e2 ->
            if isNothing (evaluateAExpr e1 ls) || isNothing (evaluateAExpr e2 ls) then
                Nothing

            else
                Just (modBy (withDefault 1 (evaluateAExpr e2 ls)) (withDefault 0 (evaluateAExpr e1 ls)))

        Error s ->
            Nothing


parseAExpr : String -> A_Expr
parseAExpr str =
    if str == "" then
        Error "Empty expression"

    else
        case run expressionA str of
            Ok y ->
                y

            Err y ->
                Error "Syntax Error"


numberParser : Parser Int
numberParser =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]


varAExpr : Parser String
varAExpr =
    variable
        { start = Char.isUpper
        , inner = Char.isUpper
        , reserved = Set.fromList []
        }


termAExpr : Parser A_Expr
termAExpr =
    oneOf
        [ succeed Number
            |= numberParser
        , succeed Var
            |= varAExpr
        , succeed identity
            |. symbol "("
            |= lazy (\_ -> expressionA)
            |. symbol ")"
        ]


expressionA : Parser A_Expr
expressionA =
    termAExpr |> andThen (expressionAAux [])


type A_Operator
    = AddOp
    | DifOp
    | MulOp
    | DivOp
    | ModOp


operatorA : Parser A_Operator
operatorA =
    oneOf
        [ Parser.map (\_ -> AddOp) (symbol "+")
        , Parser.map (\_ -> DifOp) (symbol "-")
        , Parser.map (\_ -> MulOp) (symbol "*")
        , Parser.map (\_ -> DivOp) (symbol "/")
        , Parser.map (\_ -> ModOp) (symbol "%")
        ]


expressionAAux : List ( A_Expr, A_Operator ) -> A_Expr -> Parser A_Expr
expressionAAux revOps aExpr =
    oneOf
        [ succeed Tuple.pair
            |= operatorA
            |= termAExpr
            |> andThen (\( op, newExpr ) -> expressionAAux (( aExpr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps aExpr))
        ]


finalize : List ( A_Expr, A_Operator ) -> A_Expr -> A_Expr
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        -- Module operation have the maximum priorty, so module have a unique case
        ( expr, ModOp ) :: otherRevOps ->
            finalize otherRevOps (Mod expr finalExpr)

        -- Division have the second maximum priority, so we need to determine how parser's going to do if it searches a module after and if it searches something different
        ( expr, DivOp ) :: ( expr2, ModOp ) :: otherRevOps ->
            Div (finalize (( expr2, ModOp ) :: otherRevOps) expr) finalExpr

        ( expr, DivOp ) :: otherRevOps ->
            finalize otherRevOps (Div expr finalExpr)

        -- Multiply cases
        ( expr, MulOp ) :: ( expr2, ModOp ) :: otherRevOps ->
            Mul (finalize (( expr2, ModOp ) :: otherRevOps) expr) finalExpr

        ( expr, MulOp ) :: ( expr2, DivOp ) :: otherRevOps ->
            Mul (finalize (( expr2, DivOp ) :: otherRevOps) expr) finalExpr

        ( expr, MulOp ) :: otherRevOps ->
            finalize otherRevOps (Mul expr finalExpr)

        -- Differece cases
        ( expr, DifOp ) :: ( expr2, ModOp ) :: otherRevOps ->
            Dif (finalize (( expr2, ModOp ) :: otherRevOps) expr) finalExpr

        ( expr, DifOp ) :: ( expr2, DivOp ) :: otherRevOps ->
            Dif (finalize (( expr2, DivOp ) :: otherRevOps) expr) finalExpr

        ( expr, DifOp ) :: ( expr2, MulOp ) :: otherRevOps ->
            Dif (finalize (( expr2, MulOp ) :: otherRevOps) expr) finalExpr

        ( expr, DifOp ) :: otherRevOps ->
            finalize otherRevOps (Dif expr finalExpr)

        -- Add cases
        ( expr, AddOp ) :: ( expr2, ModOp ) :: otherRevOps ->
            Add (finalize (( expr2, ModOp ) :: otherRevOps) expr) finalExpr

        ( expr, AddOp ) :: ( expr2, DivOp ) :: otherRevOps ->
            Add (finalize (( expr2, DivOp ) :: otherRevOps) expr) finalExpr

        ( expr, AddOp ) :: ( expr2, MulOp ) :: otherRevOps ->
            Add (finalize (( expr2, MulOp ) :: otherRevOps) expr) finalExpr

        ( expr, AddOp ) :: ( expr2, DifOp ) :: otherRevOps ->
            Add (finalize (( expr2, DifOp ) :: otherRevOps) expr) finalExpr

        ( expr, AddOp ) :: otherRevOps ->
            finalize otherRevOps (Add expr finalExpr)


toStringAExpr : A_Expr -> String
toStringAExpr aExpr =
    case aExpr of
        Number p ->
            String.fromInt p

        Var p ->
            p

        Add p q ->
           "(" ++ toStringAExpr p ++ "+" ++ toStringAExpr q ++ ")"

        Dif p q ->
            "(" ++ toStringAExpr p ++ "-" ++ toStringAExpr q ++ ")"

        Mul p q ->
            "(" ++ toStringAExpr p ++ "*" ++ toStringAExpr q ++ ")"

        Div p q ->
            "(" ++ toStringAExpr p ++ "/" ++ toStringAExpr q ++ ")"

        Mod p q ->
            "(" ++ toStringAExpr p ++ "%" ++ toStringAExpr q ++ ")"

        Error p ->
            "Error:" ++ p


main = text <| toStringAExpr <| parseAExpr <| "4*7%2+5+3"
