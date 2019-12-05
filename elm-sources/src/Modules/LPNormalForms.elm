module Modules.LPNormalForms exposing (..)

import List exposing (..)
import Html exposing (..)

import Modules.SintaxSemanticsLP exposing (..)
import Modules.AuxiliarFunctions exposing (..)

-- Definition of areEquiv that repesents if two formulas are equivalent

areEquiv : Prop -> Prop -> Bool
areEquiv f g = validity (Equi f g)

-- Definition of remEquiv that returns an equivalent formula that not have equivalencies

remEquiv : Prop -> Prop
remEquiv f =
    case f of
        Atom x-> Atom x
            
        Neg x -> Neg (remEquiv x)

        Conj x y -> Conj (remEquiv x) (remEquiv y)

        Disj x y -> Disj (remEquiv x) (remEquiv y)

        Impl x y -> Impl (remEquiv x) (remEquiv y)

        Equi x y -> Conj (Impl (remEquiv x) (remEquiv y)) (Impl (remEquiv y) (remEquiv x))


-- Definition of remImpl that returns an equivalent formula that not have implications. This function will not be applicated to equivalencies

remImpl : Prop -> Prop
remImpl f =
    case f of
        Atom x-> Atom x
            
        Neg x -> Neg (remImpl x)

        Conj x y -> Conj (remImpl x) (remImpl y)

        Disj x y -> Disj (remImpl x) (remImpl y)

        Impl x y-> Disj (Neg (remImpl x)) (remImpl y)

        Equi x y-> Atom ""                              -- This option never will be chosen

-- Definition interiorizeNeg that returns an equivalent formula that negations are aplicated only to atomical formulas. This function will not be applicated to equivalencies neither implications

interiorizeNeg : Prop -> Prop
interiorizeNeg f=
    case f of
        Atom x -> Atom x

        Neg x ->  interiorizeNegAux x

        Conj x y -> Conj (interiorizeNeg x) (interiorizeNeg y)

        Disj x y -> Disj (interiorizeNeg x) (interiorizeNeg y)

        other -> Atom ""                                 -- This option never will be chosen

interiorizeNegAux : Prop -> Prop
interiorizeNegAux f=
    case f of
        Atom x -> Neg (Atom x)

        Neg x -> x

        Conj x y -> Disj (interiorizeNegAux x) (interiorizeNegAux  y)

        Disj x y -> Conj (interiorizeNegAux x) (interiorizeNegAux y)

        other -> Atom ""                                     -- This option never will be chosen

-- Definition of negativeNF  that returns an equivalent formula that is in negative normal formula (NNF)

toNNF : Prop -> Prop
toNNF f = interiorizeNeg <| remImpl <| remEquiv f

-- Definition of literal that checks if a formula is a literal
literal : Prop -> Bool
literal f =
    case f of
        Atom x -> True

        Neg (Atom x) -> True
            
        other -> False

-- Definition of Literal type as synonim of Prop

type alias Literal = Prop

-- Definition of complement that returns the negation of a literal. Only used with literals

complement : Literal -> Literal
complement f=
    case f of
        Atom x -> Neg (Atom x)
            
        Neg (Atom x) -> Atom x

        other -> Atom ""         -- option never chosen

-- Definition of literalsNNF that return a set with all literals in a NNF

literalsNNF : Prop -> List Literal
literalsNNF f =
    case f of
        Conj x y ->  unionLs (literalsNNF x) (literalsNNF y)

        Disj x y ->  unionLs (literalsNNF x) (literalsNNF y)
            
        Atom x -> [Atom x]

        Neg (Atom x) -> [Neg (Atom x)]

        other -> []

-- Definition of interiorizeDisj that returns an equivalent formula where disjunctions are aplicated only to disjunctions or literals. Only aplicated to NNF formulas

interiorizeDisj : Prop -> Prop
interiorizeDisj f =
    case f of
        (Disj (Conj f1 f2) g) -> interiorizeDisj (Conj (Disj (interiorizeDisj f1) (interiorizeDisj g)) (Disj (interiorizeDisj f2) (interiorizeDisj g)))

        (Disj g (Conj f1 f2)) -> interiorizeDisj (Conj (Disj (interiorizeDisj g) (interiorizeDisj f1)) (Disj (interiorizeDisj g) (interiorizeDisj f2)))

        (Conj f1 f2)-> Conj (interiorizeDisj f1)  (interiorizeDisj f2) 

        other -> f

-- Definition of toCNF that returns an equivalent formula that is in CNF (Conjuntive Normal Form)

toCNF : Prop -> Prop 
toCNF f = interiorizeDisj (toNNF f)

-- Definition of interiorizeConj that returns an equivalent formula where conjunctions are aplicated only to conjunctions or literals. Only aplicated to NNF formulas

interiorizeConj : Prop -> Prop   
interiorizeConj f= 
    case f of
        (Conj (Disj f1 f2) g) ->  interiorizeConj (Disj (Conj (interiorizeConj f1) (interiorizeConj g)) (Conj (interiorizeConj f2) (interiorizeConj g)))

        (Conj g (Disj f1 f2)) ->   interiorizeConj (Disj (Conj (interiorizeConj g) (interiorizeConj f1)) (Conj (interiorizeConj g) (interiorizeConj f2)))
    
        (Disj f1 f2)-> Disj (interiorizeConj f1)  (interiorizeConj f2) 

        other -> f

-- Definition of toDNF that returns an equivalent formula that is in DNF (Disjunctive Normal form)
toDNF : Prop -> Prop 
toDNF f = interiorizeConj (toNNF f)                 
        