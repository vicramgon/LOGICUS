module Modules.LPBig_Parser exposing (conjPropToSet, expandFormBigProp, expandSetBigProp, parseBigProp, parseSetBigProp, toStringBigProp, toStringBigPropFile, toStringSetBigProp, toStringSetBigPropFile, toProp, toSolverNotation, setBigPSymbols)

import List exposing (all, filter, head, map)
import List.Extra exposing (cartesianProduct, zip)
import Maybe exposing (withDefault)
import Modules.A_Expressions exposing (A_Expr, evaluateAExpr, expressionA, toStringAExpr)
import Modules.AuxiliarFunctions exposing (deleteFirstLs, init, unique)
import Modules.LP_Parser exposing (parserFormula)
import Modules.SintaxSemanticsLP exposing (Prop, toStringProp)
import Modules.LPClausalForms exposing (setClauses)
import Modules.B_Expressions exposing (..)
import Parser exposing (..)
import Regex
import Set exposing (fromList)
import String exposing (concat, join, replace, split)


type alias Ident =
    { name : String
    , values : List Int
    }


createIdent : String -> List Int -> Ident
createIdent str li =
    { name = str, values = li }


type BigProp
    = Atom String
    | Neg BigProp
    | Conj BigProp BigProp
    | Disj BigProp BigProp
    | Impl BigProp BigProp
    | Equi BigProp BigProp
    | BAnd (List Ident) B_Expr BigProp
    | BOr (List Ident) B_Expr BigProp
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
    oneOf
        [ Parser.sequence
            { start = ":"
            , separator = ","
            , end = "#"
            , spaces = spaces
            , item =
                oneOf
                    [ succeed negate
                        |. symbol "-"
                        |= int
                    , int
                    ]
            , trailing = Optional
            }
        , succeed List.range
            |. symbol "["
            |= oneOf
                [ succeed negate
                    |. symbol "-"
                    |= int
                , int
                ]
            |. symbol ":"
            |= oneOf
                [ succeed negate
                    |. symbol "-"
                    |= int
                , int
                ]
            |. symbol "]"
        ]


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

atomVarBigProp : Parser String
atomVarBigProp =
    getChompedString <|
        succeed ()
            |. baseAtomBigProp
            |. chompWhile (\c -> Char.isUpper c || c == '_' || Char.isDigit c)


termBigProp : Parser BigProp
termBigProp =
    oneOf
        [ succeed Atom
            |= atomVarBigProp
        , succeed BAnd
            |. symbol "&"
            |. symbol "_"
            |= listIdentBigProp
            |. symbol "{"
            |= expressionB
            |. symbol "}"
            |. symbol "("
            |= lazy (\_ -> expressionBigProp)
            |. symbol ")"
        , succeed BOr
            |. symbol "|"
            |. symbol "_"
            |= listIdentBigProp
            |. symbol "{"
            |= expressionB
            |. symbol "}"
            |. symbol "("
            |= lazy (\_ -> expressionBigProp)
            |. symbol ")"
        , succeed identity
            |. symbol "("
            |= lazy (\_ -> expressionBigProp)
            |. symbol ")"
        , succeed Neg
            |. symbol "¬"
            |= lazy (\_ -> termBigProp)
        ]


expressionBigProp : Parser BigProp
expressionBigProp =
    termBigProp |> andThen (expressionBigPropAux [])


type Operator
    = AndOp
    | OrOp
    | ImplOp
    | EquivOp


operator : Parser Operator
operator =
    oneOf
        [ Parser.map (\_ -> AndOp) (symbol "&")
        , Parser.map (\_ -> OrOp) (symbol "|")
        , Parser.map (\_ -> ImplOp) (symbol "->")
        , Parser.map (\_ -> EquivOp) (symbol "<->")
        ]


expressionBigPropAux : List ( BigProp, Operator ) -> BigProp -> Parser BigProp
expressionBigPropAux revOps expr =
    oneOf
        [ succeed Tuple.pair
            |= operator
            |= termBigProp
            |> andThen (\( op, newExpr ) -> expressionBigPropAux (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


finalize : List ( BigProp, Operator ) -> BigProp -> BigProp
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        -- AND EXPRESSIONS CASES
        -- And operation have the maximum priorty, so module have a unique case
        ( expr, AndOp ) :: otherRevOps ->
            finalize otherRevOps (Conj expr finalExpr)

        -- OR EXPRESSIONS CASES
        -- Or have the second maximum priority, so we need to determine how parser's going to do if it searches an and after, and if it searches something different.
        ( expr, OrOp ) :: ( expr2, AndOp ) :: otherRevOps ->
            Disj (finalize (( expr2, AndOp ) :: otherRevOps) expr) finalExpr

        ( expr, OrOp ) :: otherRevOps ->
            finalize otherRevOps (Disj expr finalExpr)

        -- IMPLICATION EXPRESSIONS CASES
        ( expr, ImplOp ) :: ( expr2, AndOp ) :: otherRevOps ->
            Impl (finalize (( expr2, AndOp ) :: otherRevOps) expr) finalExpr

        ( expr, ImplOp ) :: ( expr2, OrOp ) :: otherRevOps ->
            Impl (finalize (( expr2, OrOp ) :: otherRevOps) expr) finalExpr

        ( expr, ImplOp ) :: otherRevOps ->
            finalize otherRevOps (Impl expr finalExpr)

        -- EQUIVALATION EXPRESSIONS CASES
        ( expr, EquivOp ) :: ( expr2, AndOp ) :: otherRevOps ->
            Equi (finalize (( expr2, AndOp ) :: otherRevOps) expr) finalExpr

        ( expr, EquivOp ) :: ( expr2, OrOp ) :: otherRevOps ->
            Equi (finalize (( expr2, OrOp ) :: otherRevOps) expr) finalExpr

        ( expr, EquivOp ) :: ( expr2, ImplOp ) :: otherRevOps ->
            Equi (finalize (( expr2, ImplOp ) :: otherRevOps) expr) finalExpr

        ( expr, EquivOp ) :: otherRevOps ->
            finalize otherRevOps (Equi expr finalExpr)


toStringIdentBigProp : Ident -> String
toStringIdentBigProp i =
    i.name ++ (replace "[" ":" <| replace "]" "#" <| Debug.toString <| i.values)


toStringListIdentBigProp : List Ident -> String
toStringListIdentBigProp lid =
    let
        aux lp str =
            case lp of
                [] ->
                    "{" ++ str ++ "}"

                i :: [] ->
                    "{" ++ str ++ toStringIdentBigProp i ++ "}"

                i :: rlp ->
                    aux rlp (str ++ toStringIdentBigProp i ++ ",")
    in
    aux lid ""


parseBigProp : String -> BigProp
parseBigProp str =
    if str == "" then
        Error "Empty expression"

    else
        case run expressionBigProp str of
            Ok y ->
                y

            Err err ->
                Error ("Syntax Error: " ++ (Debug.toString <| err) ++ str)


parseSetBigProp : String -> List BigProp
parseSetBigProp str =
    List.map parseBigProp <| init <| split ";" str


expandBigProp : BigProp -> Maybe BigProp
expandBigProp prop =
    case prop of
        Atom p ->
            Just <| Atom p

        Neg p ->
            case expandBigProp p of
                Nothing ->
                    Nothing

                Just m ->
                    Just <| Neg <| m

        Conj p q ->
            case expandBigProp p of
                Nothing ->
                    Nothing

                Just m ->
                    case expandBigProp q of
                        Nothing ->
                            Nothing

                        Just m2 ->
                            Just <| Conj m m2

        Disj p q ->
            case expandBigProp p of
                Nothing ->
                    Nothing

                Just m ->
                    case expandBigProp q of
                        Nothing ->
                            Nothing

                        Just m2 ->
                            Just <| Disj m m2

        Impl p q ->
            case expandBigProp p of
                Nothing ->
                    Nothing

                Just m ->
                    case expandBigProp q of
                        Nothing ->
                            Nothing

                        Just m2 ->
                            Just <| Impl m m2

        Equi p q ->
            case expandBigProp p of
                Nothing ->
                    Nothing

                Just m ->
                    case expandBigProp q of
                        Nothing ->
                            Nothing

                        Just m2 ->
                            Just <| Equi m m2

        BAnd li lc p ->
            expandBForm "&" li lc p

        BOr li lc p ->
            expandBForm "|" li lc p

        Error err ->
            Just <| Error err


expandBForm : String -> List Ident -> B_Expr -> BigProp -> Maybe BigProp
expandBForm s li bexpr p =
    let
        keys =
            List.map (\x -> x.name) li
    in
    let
        values =
            List.map (\x -> x.values) li
    in
    let
        posibilities =
            filter (\x -> Maybe.withDefault False (evaluateBExpr bexpr ( keys, x ))) <| cartesianProduct <| values
    in
    if posibilities == [] then
        Nothing

    else
        Just <| expandFormula keys posibilities p s


adecuateString : String -> String
adecuateString s =
    replace "{}" "#" <| replace "⟷" "<->" <| replace "⟶" "->" <| replace "∨" "|" <| replace "∧" "&" <| replace " " "" <| replace "\n" "" <| replace "\u{000D}" "" <| replace "\t" "" s


replaceVars : ( List String, List Int ) -> BigProp -> BigProp
replaceVars ( lids, lvals ) p =
    case head lids of
        Nothing ->
            p

        Just id ->
            let
                val =
                    withDefault 0 <| head lvals
            in
            replaceVars ( deleteFirstLs lids, deleteFirstLs lvals ) (parseBigProp <| adecuateString <| replace id (String.fromInt val) <| toStringBigProp p)


expandFormula : List String -> List (List Int) -> BigProp -> String -> BigProp
expandFormula lis llv prop symb =
    let
        aux lids llvals p s res =
            case llvals of
                [] ->
                    Error "No posibilities to expand the formula"

                lvals :: [] ->
                    case expandBigProp <| replaceVars ( lids, lvals ) p of
                        Nothing ->
                            parseBigProp <| adecuateString <| Regex.replace (withDefault Regex.never <| Regex.fromString "&$") (\_ -> "") <| res

                        Just exp ->
                            parseBigProp <| adecuateString <| res ++ "(" ++ (toStringBigProp <| exp) ++ ")"

                lvals :: rlvals ->
                    case expandBigProp <| replaceVars ( lids, lvals ) p of
                        Nothing ->
                            aux lids rlvals p s res

                        Just exp ->
                            aux lids rlvals p s (res ++ "(" ++ (toStringBigProp <| exp) ++ ")" ++ symb)
    in
    aux lis llv prop symb ""


expandFormBigProp : String -> BigProp
expandFormBigProp x =
    case expandBigProp <| parseBigProp <| adecuateString x of
        Nothing ->
            Error "No-expansion-returned"

        Just y ->
            y


expandSetBigProp : String -> List BigProp
expandSetBigProp x =
    List.map expandFormBigProp <| init <| split ";" x


toStringBigProp : BigProp -> String
toStringBigProp prop =
    case prop of
        Atom p ->
            p

        Neg p ->
            "¬ " ++ toStringBigProp p

        Conj p q ->
            "( " ++ toStringBigProp p ++ " ∧ " ++ toStringBigProp q ++ " )"

        Disj p q ->
            "( " ++ toStringBigProp p ++ " ∨ " ++ toStringBigProp q ++ " )"

        Impl p q ->
            "( " ++ toStringBigProp p ++ " ⟶ " ++ toStringBigProp q ++ " )"

        Equi p q ->
            "( " ++ toStringBigProp p ++ " ⟷ " ++ toStringBigProp q ++ " )"

        BAnd li lc p ->
            "∧_" ++ toStringListIdentBigProp li ++ toStringB_Expression lc ++ "(" ++ toStringBigProp p ++ ")"

        BOr li lc p ->
            "∨_" ++ toStringListIdentBigProp li ++ toStringB_Expression lc ++ "(" ++ toStringBigProp p ++ ")"

        Error err ->
            "Error:" ++ err


toStringBigPropFile : BigProp -> String
toStringBigPropFile x =
    adecuateString <| toStringBigProp x


toStringSetBigProp : List BigProp -> String
toStringSetBigProp xs =
    "{" ++ (join "," <| List.map (\x -> toStringBigProp x ++ "\n") <| xs) ++ "}"


toStringSetBigPropFile : List BigProp -> String
toStringSetBigPropFile xs =
    join "" <| List.map (\x -> toStringBigPropFile x ++ ";\n") <| xs


conjPropToSet : BigProp -> List BigProp
conjPropToSet p =
    case p of
        Conj p1 p2 ->
            List.concat [ conjPropToSet p1, conjPropToSet p2 ]

        _ ->
            [ p ]


symbInBigProp : BigProp -> List String
symbInBigProp f =
    case f of
        Atom p ->
            [ p ]

        Neg p ->
            symbInBigProp p

        Conj p q ->
            symbInBigProp p ++ symbInBigProp q

        Disj p q ->
            symbInBigProp p ++ symbInBigProp q

        Impl p q ->
            symbInBigProp p ++ symbInBigProp q

        Equi p q ->
            symbInBigProp p ++ symbInBigProp q

        Error _ ->
            []

        _ ->
            case expandBigProp f of
                Nothing ->
                    []

                Just y ->
                    symbInBigProp <| y


distinctSymbInBigProp : BigProp -> List String
distinctSymbInBigProp p =
    unique (symbInBigProp p)


setBigPSymbols : List BigProp -> List String
setBigPSymbols xs =
    unique <| List.concat <| List.map distinctSymbInBigProp xs

toProp : BigProp -> Prop 
toProp a = parserFormula <|  adecuateString <| toStringBigProp a

toSolverNotation : List BigProp -> String
toSolverNotation bps = String.replace " " "" <| String.replace "\"" "" <| String.replace  "]" "" <| String.replace  "[" "" <| String.replace  "],[" ";" <| Debug.toString <| List.map (\x -> List.map toStringProp x) <| setClauses <| List.map toProp bps
