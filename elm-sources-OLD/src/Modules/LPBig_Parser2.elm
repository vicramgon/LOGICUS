module Modules.LPBig_Parser2 exposing (parseBigProp, parseSetBigProp, toProp, checkCorrectReadFormula, checkCorrectReadFormulaSet)

import List exposing (filter, map)
import List.Extra exposing (cartesianProduct, zip)
import Maybe exposing (withDefault)
import Modules.SintaxSemanticsLP exposing (Prop(..))
import Modules.B_Expressions exposing (B_Expr, evaluateBExpr, expressionB)
import Parser exposing (Parser, run, variable, oneOf, succeed, spaces, (|.), (|=), symbol, lazy, andThen, int, Trailing(..), getChompedString, chompWhile)
import Set exposing (fromList)
import String exposing (replace, split)
import Dict exposing (Dict)

--import Html exposing (Html)

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
    | Insat


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
        { start = \c -> c == '_'
        , inner =  \c -> Char.isUpper c || Char.isDigit c
        , reserved = Set.fromList []
        }


valuesIdentBigProp : Parser (List Int)
valuesIdentBigProp =
    oneOf
        [ Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
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
        , trailing = Forbidden
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
        , succeed Insat
            |. symbol "!"
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


parseBigProp : String -> (Maybe BigProp, String)
parseBigProp str =
    if str == "" then
        (Nothing, "Empty expression")

    else
        case run expressionBigProp <| String.replace " " "" <| String.replace "\n" "" <| str of
            Ok y ->
                (Just y, "")

            Err err ->
                (Nothing, "Syntax Error: " ++ (Debug.toString <| err) ++ str)


parseSetBigProp : String -> List (Maybe BigProp, String)
parseSetBigProp str =
    List.map parseBigProp <| split ";" str 

replaceVars : List (String, Int) -> String -> String
replaceVars var_val s =
    case var_val of
        [] -> s
        (x, y)::xs -> replaceVars xs (String.replace x (String.fromInt y) s)

expandBigPropAux : Dict String Int -> BigProp -> BigProp    
expandBigPropAux var_val prop =
    case prop of
        Atom p ->
            Atom <| replaceVars (Dict.toList var_val) p

        Neg p ->
            Neg (expandBigPropAux var_val p)

        Conj p q ->
            Conj (expandBigPropAux var_val p) (expandBigPropAux var_val q)
        
        Disj p q ->
            Disj (expandBigPropAux var_val p) (expandBigPropAux var_val q)

        Impl p q ->
            Impl (expandBigPropAux var_val p) (expandBigPropAux var_val q)

        Equi p q ->
            Conj (expandBigPropAux var_val p) (expandBigPropAux var_val q)

        BAnd li lc p ->
            expandBForm True var_val li lc p

        BOr li lc p -> 
            expandBForm False var_val li lc p

        Insat ->
            Insat


expandBForm : Bool -> Dict String Int -> List Ident -> B_Expr -> BigProp -> BigProp
expandBForm op var_val li bexpr p =
        let
                keys =List.map (\x -> x.name) li
            in
            let
                values =
                    List.map (\x -> x.values) li
            in
            let
                posibilities =
                    filter (\x -> Maybe.withDefault False (evaluateBExpr bexpr ( keys, x ))) <| cartesianProduct <| values
            in
            case posibilities of
                [] -> Insat
                [v]-> expandBigPropAux (Dict.union  (Dict.fromList <| List.Extra.zip keys v) var_val) p
                _ -> if op then
                        expandBAnd var_val keys posibilities p
                     else
                        expandBOr var_val keys posibilities p


expandBAnd : Dict String Int -> List String -> List (List Int) -> BigProp -> BigProp
expandBAnd var_val keys posibilities p =
    case posibilities of
        [] -> Insat
        [v] -> expandBigPropAux (Dict.union  (Dict.fromList <| List.Extra.zip keys v) var_val) p
        v::xs -> Conj (expandBigPropAux (Dict.union  (Dict.fromList <| List.Extra.zip keys v) var_val) p) (expandBAnd var_val keys xs p)

expandBOr: Dict String Int -> List String -> List (List Int) -> BigProp -> BigProp
expandBOr var_val keys posibilities p =
    case posibilities of
        [] -> Insat
        [v] -> expandBigPropAux (Dict.union  (Dict.fromList <| List.Extra.zip keys v) var_val) p
        v::xs -> Disj (expandBigPropAux (Dict.union  (Dict.fromList <| List.Extra.zip keys v) var_val) p) (expandBAnd var_val keys xs p)

toProp : BigProp -> Prop
toProp p = toPropAux <| expandBigPropAux Dict.empty p


toPropAux : BigProp -> Prop
toPropAux x = 
    case x of
        Atom p ->
            Modules.SintaxSemanticsLP.Atom p

        Neg p ->
            Modules.SintaxSemanticsLP.Neg (toPropAux p)

        Conj p q ->
            Modules.SintaxSemanticsLP.Conj (toPropAux p) (toPropAux q)
        
        Disj p q ->
            Modules.SintaxSemanticsLP.Disj (toPropAux p) (toPropAux q)

        Impl p q ->
            Modules.SintaxSemanticsLP.Impl (toPropAux p) (toPropAux q)

        Equi p q ->
            Modules.SintaxSemanticsLP.Equi (toPropAux p) (toPropAux q)

        _ ->
            Modules.SintaxSemanticsLP.Insat

checkCorrectReadFormula : (Maybe BigProp, String) -> Bool
checkCorrectReadFormula (_, s) = s == ""

checkCorrectReadFormulaSet : List (Maybe BigProp, String) -> Bool
checkCorrectReadFormulaSet lp = List.all checkCorrectReadFormula lp

{-main : Html msg
main =
    let ls = parseSetBigProp <| "&_{_I{0,1,2},_J{0,1,2}}{[_I=_J]}(p_I_J); &_{_I{0,1,2},_J{0,1,2}}{[_I!=_J]}(¬p_I_J)" in
       Html.text <| String.join "," <| List.map (\(x,y) -> if y /= "" then y else Debug.toString <| toProp <| Maybe.withDefault Insat x) ls-}
       
