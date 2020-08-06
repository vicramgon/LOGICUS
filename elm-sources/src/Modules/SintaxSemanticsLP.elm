module Modules.SintaxSemanticsLP exposing (
    PSymb, FormulaLP(..), Interpretation, LPSet,
    valuation, truthTable, models, countermodels, satisfactibility, 
    validity, insatisfactibility, isSetModel, allSetModels, 
    allSetCounterModels, isConsistent, isInconsistent, isConsecuence, setSymbols, symbInProp)

import List
import Set
import Modules.AuxiliarFunctions as Aux
import Maybe exposing (Maybe(..))

-----------
-- TYPES --
-----------
type alias PSymb = String 

type FormulaLP = Atom PSymb
          | Neg FormulaLP
          | Conj FormulaLP FormulaLP
          | Disj FormulaLP FormulaLP
          | Impl FormulaLP FormulaLP
          | Equi FormulaLP FormulaLP
          | Insat
type alias LPSet = List FormulaLP

type alias Interpretation = List PSymb


-------------
-- METHODS --
-------------

valuation : FormulaLP -> Interpretation -> Bool
valuation pr i =
    case pr of
        Atom p -> List.member p i
        Neg p -> not (valuation p i)
        Conj p q -> valuation p i &&  valuation q i
        Disj p q ->   valuation p i ||  valuation q i
        Impl p q ->   not (valuation p i) ||  valuation q i
        Equi p q ->   valuation (Impl p q) i &&  valuation (Impl q p) i
        Insat -> Basics.False

symbInProp : FormulaLP -> List PSymb
symbInProp x = List.sort <| Set.toList <| symbInPropAux x

symbInPropAux : FormulaLP -> Set.Set PSymb
symbInPropAux f=
    case f of
        Atom p -> Set.singleton p
        Neg p -> symbInPropAux p
        Conj p q -> Set.union (symbInPropAux p ) (symbInPropAux q)
        Disj p q -> Set.union (symbInPropAux p ) (symbInPropAux q)
        Impl p q -> Set.union (symbInPropAux p ) (symbInPropAux q)
        Equi p q -> Set.union (symbInPropAux p ) (symbInPropAux q)
        Insat -> Set.empty


allInterpretations : FormulaLP -> List Interpretation
allInterpretations x =  Aux.powerset (symbInProp x)

truthTable : FormulaLP -> List (Interpretation, Bool)
truthTable x = List.map (\xs ->  (xs,valuation x xs)) <| allInterpretations x

models : FormulaLP -> List Interpretation
models x = List.filter (\y -> valuation x y) (allInterpretations x)

countermodels : FormulaLP -> List Interpretation
countermodels x = List.filter (\y -> not(valuation x y)) (allInterpretations x)

satisfactibility : FormulaLP -> Bool
satisfactibility x = List.any (\xs-> valuation x xs) (allInterpretations x)
validity : FormulaLP -> Bool
validity x = List.all (\xs-> valuation x xs) (allInterpretations x)
insatisfactibility : FormulaLP -> Bool
insatisfactibility x = List.all (\xs-> not(valuation x xs)) (allInterpretations x)

setSymbols : List FormulaLP -> Set.Set PSymb
setSymbols xs = List.foldr (\x acc -> Set.union acc (symbInPropAux x)) Set.empty xs

allSetInterpretations : List FormulaLP -> List Interpretation
allSetInterpretations xs = Aux.powerset <| Set.toList <| setSymbols xs

isSetModel : List FormulaLP -> Interpretation -> Bool
isSetModel xs i =  List.all (\x -> valuation x i) xs

allSetModels : List FormulaLP -> List Interpretation
allSetModels xs = List.filter (isSetModel xs) (allSetInterpretations xs)

allSetCounterModels : List FormulaLP -> List Interpretation
allSetCounterModels xs = List.filter (\x -> not(isSetModel xs x)) <| allSetInterpretations xs

isConsistent : List FormulaLP -> Bool
isConsistent xs = List.any (\x -> isSetModel xs x) <| allSetInterpretations xs

isInconsistent: List FormulaLP -> Bool
isInconsistent xs = not(isConsistent xs)

isConsecuence : List FormulaLP -> FormulaLP -> Bool
--isConsecuence xs x = List.all (\y -> valuation x y) <| allSetModels xs
isConsecuence xs x = isInconsistent (xs ++ [Neg x])


