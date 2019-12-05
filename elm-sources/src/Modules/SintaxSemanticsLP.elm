module Modules.SintaxSemanticsLP exposing (..)

import List exposing (..)

import Modules.AuxiliarFunctions as Aux exposing (..)


--Definition of types for propositional symbols ("PSymb") and for propositional formulas ("Prop"). 
--Propositional formulas can be an atomic formula, a negation os a formula, a conjunction of two formulas, an union of two formula, a implication of two formular o an equivalence of two formulas.

type alias PSymb = String 

type Prop = Atom PSymb
          | Neg Prop
          | Conj Prop Prop
          | Disj Prop Prop
          | Impl Prop Prop
          | Equi Prop Prop

-- Definition of how we're going to show the formulas

toStringProp : Prop -> String
toStringProp prop =
    case prop of
        Atom p -> p
        Neg p -> "¬ " ++ (toStringProp p) 
        Conj p q -> "( " ++ (toStringProp p) ++ " ∧ "  ++ (toStringProp q) ++ " )"
        Disj p q -> "( " ++ (toStringProp p) ++ " ∨ "  ++ (toStringProp q) ++ " )"
        Impl p q -> "( " ++ (toStringProp p) ++ " ⟶ "  ++ (toStringProp q) ++ " )"
        Equi p q -> "( " ++ (toStringProp p) ++ " ⟷ "  ++ (toStringProp q) ++ " )"


-- Function for the serching of all propositional symbols in a formula (allowed an not allowed reply elements)
symbInProp : Prop -> List PSymb

symbInProp f=
    case f of
        Atom p -> [p]
        Neg p -> symbInProp p
        Conj p q -> symbInProp p ++ symbInProp q
        Disj p q -> symbInProp p ++ symbInProp q
        Impl p q -> symbInProp p ++ symbInProp q
        Equi p q -> symbInProp p ++ symbInProp q

distinctSymbInProp : Prop -> List PSymb
distinctSymbInProp p = unique (symbInProp p)

-- Definition of type Interpretations that represents the interpretations as an atoms list

type alias Interpretation = List PSymb

-- Definition of function valuation that represents the valuation of a formula with a some interpretation

valuation : Prop -> Interpretation -> Bool
valuation pr i =
    case pr of
    Atom p -> List.member p i
    Neg p -> not (valuation p i)
    Conj p q -> (valuation p i) && (valuation q i)
    Disj p q ->  (valuation p i) || (valuation q i)
    Impl p q ->  (not (valuation p i)) || (valuation q i)
    Equi p q ->  (valuation (Impl p q) i) && (valuation (Impl q p) i)

-- Definition of function allInterpretations that represents all posibles interpretations of some propsitional formula

subsets : List PSymb -> List (List PSymb)
subsets xs = 
    if (List.isEmpty xs)  then [[]]
    else 
        let xss = subsets (fromJustLPsymb (List.tail xs))
        in 
            (subsetsAux (fromJustPsymb (List.head xs)) xss) ++ xss

subsetsAux : PSymb -> List (List PSymb) -> List (List PSymb)
subsetsAux x xss = List.map (\ys -> x::ys) xss


fromJustLPsymb : Maybe (List PSymb) -> List PSymb
fromJustLPsymb xs = 
    case xs of 
        Nothing -> []
        Just value -> value

fromJustPsymb : Maybe PSymb -> PSymb
fromJustPsymb x = 
    case x of
        Nothing -> ""
        Just value -> value


allInterpretations : Prop -> List Interpretation
allInterpretations x =  subsets (distinctSymbInProp x)

-- Definition of all models of a some propositional formula

models : Prop -> List Interpretation
models x = List.filter (\y -> valuation x y) (allInterpretations x)

-- Definition of satisfactibility, insatisfactibility and validity of a formula

satisfactibility : Prop -> Bool
satisfactibility x = not (insatisfactibility x)

insatisfactibility : Prop -> Bool
insatisfactibility x = List.isEmpty (models x)

validity : Prop -> Bool
validity x = (models x) == (allInterpretations x)

-- Definition of setSymbols function that represents all symbols in a set of propositional formula

setSymbols : List Prop -> List PSymb
setSymbols xs = List.map distinctSymbInProp xs |> List.concat |> unique

-- Definition of setInterpretations function that represents all posible interpretations of a set of propositional formulas

allSetInterpretations : List Prop -> List Interpretation
allSetInterpretations xs = List.map allInterpretations xs |> List.concat |> unique

-- Definition of setModel function that represents if an interpretation is model forall formulas in the set

setModel : List Prop -> Interpretation -> Bool
setModel xs i =  List.filter (\x -> not (valuation x i)) xs |> List.isEmpty

-- Definition of allSetModels function that represents all models of a set
allSetModels : List Prop -> List Interpretation
allSetModels xs = List.filter (setModel xs) (allSetInterpretations xs)

allSetCounterModels : List Prop -> List Interpretation
allSetCounterModels xs = List.filter (\x -> not(setModel xs x)) <| allSetInterpretations xs

allSetCounterPropModels : List Prop -> List (List Prop)
allSetCounterPropModels xs =  List.map (\x -> (List.map(\y -> Atom y) x) ++ (List.map (\y -> Neg (Atom y)) <|List.filter (\y -> not (member y x)) <| setSymbols xs))<| allSetCounterModels xs

-- Definition of consitency and inconsistency of sets

isConsistence : List Prop -> Bool
isConsistence xs = not (isInconsistence xs)

isInconsistence : List Prop -> Bool
isInconsistence xs = allSetModels xs |> List.isEmpty

-- Definition of logic consecuence

isConsecuence : List Prop -> Prop -> Bool
isConsecuence xs x = List.filter (\y -> not(valuation x y)) (allSetModels xs) |> List.isEmpty

-- ToString of a Formulas set
toStringSet : List Prop -> String
toStringSet xs = "{" ++ toStringListPropAux xs  ++ "}" 

toStringListPropAux : List Prop -> String
toStringListPropAux xs = case List.head xs of
    Nothing -> ""

    Just y -> if List.length xs == 1 then toStringProp y else  toStringProp y ++ "," ++ toStringListPropAux (Aux.deleteFirstLs xs)
        