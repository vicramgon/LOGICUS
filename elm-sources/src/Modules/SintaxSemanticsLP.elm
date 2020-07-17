module Modules.SintaxSemanticsLP exposing (
    PSymb, Prop, Interpretation,PropSet, atomProp, negProp, conjProp, 
    disjProp, implProp, equiProp, insatProp,
    valuation, truthTable, models, countermodels, satisfactibility, 
    validity, insatisfactibility, isSetModel, allSetModels, 
    allSetCounterModels, isConsistent, isInconsistent, isConsecuence, toStringProp, toStringSet)
import List
import Set
import Modules.AuxiliarFunctions as Aux 


-----------
-- TYPES --
-----------
type alias PSymb = String 

type Prop = Atom PSymb
          | Neg Prop
          | Conj Prop Prop
          | Disj Prop Prop
          | Impl Prop Prop
          | Equi Prop Prop
          | Insat

type alias Interpretation = List PSymb
type alias PropSet = List Prop

-------------
-- METHODS --
-------------

atomProp: PSymb -> Prop
atomProp = Atom

negProp: Prop -> Prop
negProp = Neg

conjProp: Prop -> Prop -> Prop
conjProp = Conj 

disjProp: Prop -> Prop -> Prop
disjProp = Disj

implProp: Prop -> Prop -> Prop
implProp = Impl

equiProp: Prop -> Prop -> Prop
equiProp = Equi

insatProp: Prop
insatProp = Insat

valuation : Prop -> Interpretation -> Bool
valuation pr i =
    case pr of
        Atom p -> List.member p i
        Neg p -> not (valuation p i)
        Conj p q -> valuation p i &&  valuation q i
        Disj p q ->   valuation p i ||  valuation q i
        Impl p q ->   not (valuation p i) ||  valuation q i
        Equi p q ->   valuation (Impl p q) i &&  valuation (Impl q p) i
        Insat -> Basics.False

symbInProp : Prop -> Set.Set PSymb

symbInProp f=
    case f of
        Atom p -> Set.singleton p
        Neg p -> symbInProp p
        Conj p q -> Set.union (symbInProp p ) (symbInProp q)
        Disj p q -> Set.union (symbInProp p ) (symbInProp q)
        Impl p q -> Set.union (symbInProp p ) (symbInProp q)
        Equi p q -> Set.union (symbInProp p ) (symbInProp q)
        Insat -> Set.empty

allInterpretations : Prop -> List Interpretation
allInterpretations x =  Aux.powerset <| List.sort <| Set.toList <| symbInProp x

truthTable : Prop -> List (Interpretation, Bool)
truthTable x = List.map (\xs ->  (xs,valuation x xs)) <| allInterpretations x

models : Prop -> List Interpretation
models x = List.filter (\y -> valuation x y) (allInterpretations x)

countermodels : Prop -> List Interpretation
countermodels x = List.filter (\y -> not(valuation x y)) (allInterpretations x)

satisfactibility : Prop -> Bool
satisfactibility x = List.any (\xs-> valuation x xs) (allInterpretations x)
validity : Prop -> Bool
validity x = models x== allInterpretations x
insatisfactibility : Prop -> Bool
insatisfactibility x = List.isEmpty (models x)

setSymbols : List Prop -> Set.Set PSymb
setSymbols xs = List.foldr (\x acc -> Set.union acc (symbInProp x)) Set.empty xs

allSetInterpretations : List Prop -> List Interpretation
allSetInterpretations xs = Aux.powerset <| Set.toList <| setSymbols xs

isSetModel : List Prop -> Interpretation -> Bool
isSetModel xs i =  List.all (\x -> valuation x i) xs

allSetModels : List Prop -> List Interpretation
allSetModels xs = List.filter (isSetModel xs) (allSetInterpretations xs)

allSetCounterModels : List Prop -> List Interpretation
allSetCounterModels xs = List.filter (\x -> not(isSetModel xs x)) <| allSetInterpretations xs

isConsistent : List Prop -> Bool
isConsistent xs = List.any (\x -> isSetModel xs x) <| allSetInterpretations xs

isInconsistent: List Prop -> Bool
isInconsistent xs = not(isConsistent xs)

isConsecuence : List Prop -> Prop -> Bool
--isConsecuence xs x = List.all (\y -> valuation x y) <| allSetModels xs
isConsecuence xs x = isInconsistent (xs ++ [Neg x])


toStringProp : Prop -> String
toStringProp prop =
    case prop of
        Atom p -> p
        Neg p -> "¬ " ++ toStringProp p
        Conj p q -> "( " ++ toStringProp p ++ " ∧ "  ++ toStringProp q ++ " )"
        Disj p q -> "( " ++ toStringProp p ++ " ∨ "  ++ toStringProp q ++ " )"
        Impl p q -> "( " ++ toStringProp p ++ " ⟶ "  ++ toStringProp q ++ " )"
        Equi p q -> "( " ++ toStringProp p ++ " ⟷ "  ++ toStringProp q ++ " )"
        Insat -> "⊥"
        
toStringSet : List Prop -> String
toStringSet xs = "{" ++ toStringListPropAux xs  ++ "}" 

toStringListPropAux : List Prop -> String
toStringListPropAux xs = case xs of
    [] -> ""

    x::[] -> toStringProp x

    x::ys -> toStringProp x ++ "," ++ toStringListPropAux ys
   
        