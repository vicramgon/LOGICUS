module Modules.SintaxSemanticsLP exposing (..)

import List

import Modules.AuxiliarFunctions as Aux 

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import Set exposing (..)


--Definition of types for propositional symbols ("PSymb") and for propositional formulas ("Prop"). 
--Propositional formulas can be an atomic formula, a negation os a formula, a conjunction of two formulas, an union of two formula, a implication of two formular o an equivalence of two formulas.

type alias PSymb = String 

type Prop = Atom PSymb
          | Neg Prop
          | Conj Prop Prop
          | Disj Prop Prop
          | Impl Prop Prop
          | Equi Prop Prop

type alias Interpretation = List PSymb
type alias PropSet = List Prop

-- Definition of how we're going to show the formulas

-- Function for the serching of all propositional symbols in a formula

-- Definition of type Interpretations that represents the interpretations as an atoms list



-- Definition of function valuation that represents the valuation of a formula with a some interpretation

valuation : Prop -> Interpretation -> Bool
valuation pr i =
    case pr of
        Atom p -> List.member p i
        Neg p -> not (valuation p i)
        Conj p q -> valuation p i &&  valuation q i
        Disj p q ->   valuation p i ||  valuation q i
        Impl p q ->   not (valuation p i) ||  valuation q i
        Equi p q ->   valuation (Impl p q) i &&  valuation (Impl q p) i

-- Definition of function allInterpretations that represents all posibles interpretations of some propsitional formula



symbInProp : Prop -> Set PSymb

symbInProp f=
    case f of
        Atom p -> Set.singleton p
        Neg p -> symbInProp p
        Conj p q -> Set.union (symbInProp p ) (symbInProp q)
        Disj p q -> Set.union (symbInProp p ) (symbInProp q)
        Impl p q -> Set.union (symbInProp p ) (symbInProp q)
        Equi p q -> Set.union (symbInProp p ) (symbInProp q)

allInterpretations : Prop -> List Interpretation
allInterpretations x =  Aux.powerset <| List.sort <| Set.toList <| symbInProp x

truthTable : Prop -> List (Interpretation, Bool)
truthTable x = List.map (\xs ->  (xs,valuation x xs)) <| allInterpretations x

-- Definition of all models of a some propositional formula

models : Prop -> List Interpretation
models x = List.filter (\y -> valuation x y) (allInterpretations x)

countermodels : Prop -> List Interpretation
countermodels x = List.filter (\y -> not(valuation x y)) (allInterpretations x)

-- Definition of satisfactibility, insatisfactibility and validity of a formula

satisfactibility : Prop -> Bool
satisfactibility x = List.any (\xs-> valuation x xs) (allInterpretations x)
validity : Prop -> Bool
validity x = models x== allInterpretations x
insatisfactibility : Prop -> Bool
insatisfactibility x = List.isEmpty (models x)



-- Definition of setSymbols function that represents all symbols in a set of propositional formulas


setSymbols : List Prop -> Set PSymb
setSymbols xs = List.foldr (\x acc -> Set.union acc (symbInProp x)) Set.empty xs


-- Definition of setInterpretations function that represents all posible interpretations of a set of propositional formulas

allSetInterpretations : List Prop -> List Interpretation
allSetInterpretations xs = Aux.powerset <| Set.toList <| setSymbols xs

-- Definition of setModel function that represents if an interpretation is model forall formulas in the set

isSetModel : List Prop -> Interpretation -> Bool
isSetModel xs i =  List.all (\x -> valuation x i) xs


-- Definition of allSetModels function that represents all models of a set
allSetModels : List Prop -> List Interpretation
allSetModels xs = List.filter (isSetModel xs) (allSetInterpretations xs)

allSetCounterModels : List Prop -> List Interpretation
allSetCounterModels xs = List.filter (\x -> not(isSetModel xs x)) <| allSetInterpretations xs

--allSetCounterPropModels : List Prop -> List (List Prop)
--allSetCounterPropModels xs =  List.map (\x -> List.map(\y -> Atom y) x ++ (List.map (\y -> Neg (Atom y)) <|List.filter (\y -> not (List.member y x)) <| setSymbols xs))<| allSetCounterModels xs

-- Definition of consitency and inconsistency of sets



isConsistent : List Prop -> Bool
isConsistent xs = List.any (\x -> isSetModel xs x) <| allSetInterpretations xs

b = [Impl (Atom "a") (Atom "b"), Neg(Atom "b")]
rf = Neg(Atom "a")
result = Debug.toString <| isConsecuence b rf
main = textarea [value result][] 
isInconsistent: List Prop -> Bool
isInconsistent xs = not(isConsistent xs)

-- Definition of logic consecuence


isConsecuence : List Prop -> Prop -> Bool

--isConsecuence xs x = List.all (\y -> valuation x y) <| allSetModels xs
isConsecuence xs x = isInconsistent (xs ++ [Neg x])