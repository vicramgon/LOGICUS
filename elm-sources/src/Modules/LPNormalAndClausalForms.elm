module Modules.LPNormalAndClausalForms exposing (..)

import List exposing (member, concat, any, all)
import Modules.SintaxSemanticsLP exposing (PSymb, Prop(..), validity, setSymbols, Interpretation)
import Modules.AuxiliarFunctions exposing (unionLs, uniqueBy, unique, powerset)
import Set exposing (toList)

{-import Html exposing (text)
import Modules.LP_Parser exposing (parserPropSet)
import Maybe exposing (withDefault)-}


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

        Insat -> Insat

-- Definition of containsEquiv that returns if there are equivalencies inside a formula:
containsEquiv : Prop -> Bool
containsEquiv f =
    case f of
        Atom _-> False
            
        Neg x -> containsEquiv x

        Conj x y -> containsEquiv x || containsEquiv y

        Disj x y -> containsEquiv x || containsEquiv y

        Impl x y -> containsEquiv x || containsEquiv y

        Equi _ _ -> True

        Insat -> False



-- Definition of remImpl that returns an equivalent formula that not have implications. This function will not be applicated to equivalencies

remImpl : Prop -> Prop
remImpl f =
    case f of
        Atom x-> Atom x
            
        Neg x -> Neg (remImpl x)

        Conj x y -> Conj (remImpl x) (remImpl y)

        Disj x y -> Disj (remImpl x) (remImpl y)

        Impl x y-> Disj (Neg (remImpl x)) (remImpl y)

        _ -> Insat                           


-- Definition of containsImpl that returns if there are some implication inside a formula:
containsImpl : Prop -> Bool
containsImpl f =
    case f of
        Atom _-> False
            
        Neg x -> containsImpl x

        Conj x y -> containsImpl x || containsImpl y

        Disj x y -> containsImpl x || containsImpl y

        Impl _ _ -> True

        Equi x y -> containsImpl x || containsImpl y

        Insat -> False

-- Definition interiorizeNeg that returns an equivalent formula that negations are aplicated only to atomical formulas. This function will not be applicated to equivalencies neither implications

interiorizeNeg : Prop -> Prop
interiorizeNeg f=
    case f of
        Atom x -> Atom x

        Neg x ->  interiorizeNegAux x

        Conj x y -> Conj (interiorizeNeg x) (interiorizeNeg y)

        Disj x y -> Disj (interiorizeNeg x) (interiorizeNeg y)

        _ -> Insat                                 

interiorizeNegAux : Prop -> Prop
interiorizeNegAux f=
    case f of
        Atom x -> Neg (Atom x)

        Neg x -> x

        Conj x y -> Disj (interiorizeNegAux x) (interiorizeNegAux  y)

        Disj x y -> Conj (interiorizeNegAux x) (interiorizeNegAux y)

        _ -> Insat                                   

-- Definition of deMorganIsAplicable that returns if there are some negation of a conjuctive or disjuntive formula:
deMorganIsApplicable : Prop -> Bool
deMorganIsApplicable f =
    case f of
        Atom _ -> False
            
        Neg (Atom _) -> False 

        Neg (Conj _ _) -> True

        Neg (Disj _ _) -> True

        Neg x -> deMorganIsApplicable x

        Conj x y -> deMorganIsApplicable x || deMorganIsApplicable y

        Disj x y -> deMorganIsApplicable x || deMorganIsApplicable y

        _ -> False 


-- Definition of negativeNF that returns an equivalent formula that is in negative normal formula (NNF)

toNNF : Prop -> Prop
toNNF f = interiorizeNeg <| remImpl <| remEquiv f

-- Definition of literal that checks if a formula is a literal
literal : Prop -> Bool
literal f =
    case f of
        Atom _ -> True

        Neg (Atom _) -> True
            
        _ -> False

-- Definition of Literal type as synonim of Prop

type alias Literal = Prop

-- Definition of complement that returns the negation of a literal. Only used with literals

complement : Literal -> Literal
complement f=
    case f of
        Atom x -> Neg (Atom x)
            
        Neg (Atom x) -> Atom x

        _ -> Insat        

-- Definition of literalsNNF that return a set with all literals in a NNF

literalsNNF : Prop -> List Literal
literalsNNF f =
    case f of
        Conj x y ->  unionLs (literalsNNF x) (literalsNNF y)

        Disj x y ->  unionLs (literalsNNF x) (literalsNNF y)
            
        Atom x -> [Atom x]

        Neg (Atom x) -> [Neg (Atom x)]

        _ -> []

-- Definition of interiorizeDisj that returns an equivalent formula where disjunctions are aplicated only to disjunctions or literals. Only aplicated to NNF formulas

interiorizeDisj : Prop -> Prop
interiorizeDisj f =
    case f of
        (Disj (Conj f1 f2) g) -> interiorizeDisj (Conj (Disj (interiorizeDisj f1) (interiorizeDisj g)) (Disj (interiorizeDisj f2) (interiorizeDisj g)))

        (Disj g (Conj f1 f2)) -> interiorizeDisj (Conj (Disj (interiorizeDisj g) (interiorizeDisj f1)) (Disj (interiorizeDisj g) (interiorizeDisj f2)))

        (Conj f1 f2)-> Conj (interiorizeDisj f1)  (interiorizeDisj f2) 

        _ -> f

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

        _ -> f

-- Definition of toDNF that returns an equivalent formula that is in DNF (Disjunctive Normal form)
toDNF : Prop -> Prop 
toDNF f = interiorizeConj (toNNF f)     


------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                     CLAUSAL FORMS                                                                      --                                                             
------------------------------------------------------------------------------------------------------------------------------------------------------------
type alias Clause = List Literal

-- Definition of toClause that represents the clausal form of a literals disjunction formula.

toClause : Prop -> Clause
toClause x =
    if literal x then
        [ x ]

    else
        case x of
            Disj f g ->
                unionLs (toClause f) (toClause g)

            _ -> 
                []



-- Definition of toClauseCNF that represents the clausal form of a formula in CNF.


toClauseCNF : Prop -> List Clause
toClauseCNF x =
    case x of
        Conj f g ->
            unionLs (toClauseCNF f) (toClauseCNF g)

        _ ->
            [ toClause x ]



-- Definition of propClause that returns the clausal form of any formula

propClause : Prop -> List Clause
propClause x =
    toClauseCNF <| toCNF x



-- Definition of setClauses that represents a set of clauses equivalent to the set
-- filter equal clauses


compareClauses : Clause -> Clause -> Bool
compareClauses a b =
    (List.length a == List.length b) && List.all (\x -> member x b) a


filterEqClauses : List Clause -> List Clause
filterEqClauses xs =
    uniqueBy compareClauses xs



-- remove clauses that have contrary atoms


haveContraryLiteral : Clause -> Bool
haveContraryLiteral x =
    List.any (\y -> member (Neg y) x) x


filterTautClauses : List Clause -> List Clause
filterTautClauses xs =
    List.filter (\x -> not (haveContraryLiteral x)) xs


setClauses : List Prop -> List Clause
setClauses xs =
    filterTautClauses <| filterEqClauses <| concat <| List.map propClause xs



-- Definition of symbolsClause that represents de distinct propositional symbols in a clause


symbolsClause : Clause -> List PSymb
symbolsClause c =
    toList <| setSymbols c



-- Definition of symbolsClauseSet that represents de distinct propositional symbols in a set of clauses


symbolsClauseSet : List Clause -> List PSymb
symbolsClauseSet xs =
    unique <| concat <| List.map symbolsClause xs



-- Definition of clauseInterpretations that represents all posible interpretations of a clause


clauseInterpretations : Clause -> List Interpretation
clauseInterpretations x =
    powerset <| symbolsClause x



-- Definition of clauseSetInterpretations that represents all posible interpretations og a clause


clauseSetInterpretations : List Clause -> List Interpretation
clauseSetInterpretations xs =
    powerset <| symbolsClauseSet xs



-- Definition of isLiteralModel that represents if an interpretation is model of a literal


isLiteralModel : Interpretation -> Literal -> Bool
isLiteralModel i l =
    case l of
        Atom x ->
            member x i

        Neg (Atom x) ->
            not <| member x i

        _ ->
            False



-- option never chosen
-- Definition of isClausalModel that represents if an interpretation is model of a clause


isClausalModel : Interpretation -> Clause -> Bool
isClausalModel i c =
    any (\y -> y) <| List.map (\l -> isLiteralModel i l) c



-- Definition of clausalModels that represents all models of a clause.


clausalModels : Clause -> List Interpretation
clausalModels c =
    List.filter (\x -> isClausalModel x c) <| clauseInterpretations c



-- Definition of isClausalSetModel that represent if an interpretation is model of a set of clauses


isClausalSetModel : Interpretation -> List Clause -> Bool
isClausalSetModel i cs =
    all (\y -> y) <| List.map (\c -> isClausalModel i c) cs



-- Definition of clausalSetModels that represents all models of a set of clauses


clausalSetModels : List Clause -> List Interpretation
clausalSetModels cs =
    List.filter (\x -> isClausalSetModel x cs) <| clauseSetInterpretations cs



-- Definition of validClause that represents if a clause is valid


validClause : Clause -> Bool
validClause =
    haveContraryLiteral



-- Definition of insatisfactibleClause that represents if a clause is insatisfactible


insatisfactibleClause : Clause -> Bool
insatisfactibleClause =
    List.isEmpty



-- Definition of satisfactibleClauser that represents if a clause is satsifactible


satisfactibleClause : Clause -> Bool
satisfactibleClause c =
    not <| insatisfactibleClause c



-- Definition of validClauseSet


validClauseSet : List Clause -> Bool
validClauseSet cs =
    List.all (\x -> x) <| List.map validClause cs



-- Definition of consistenceClauseSet that represents if a clause set is consistence


consistenceClauseSet : List Clause -> Bool
consistenceClauseSet cs =
    not <| List.isEmpty <| clausalSetModels cs



-- Definition of inconsistenceClauseSet


inconsistenceClauseSet : List Clause -> Bool
inconsistenceClauseSet cs =
    List.isEmpty <| clausalSetModels cs



-- Definition of propValidwidthClauses that represents if the clause set of a formula is valid


propValidwidthClauses : Prop -> Bool
propValidwidthClauses p =
    validClauseSet <| propClause <| p



-- Definition of isConsecuenceBetweenClauses that represents if all models of s1 are models of s2


isConsecuenceBetweenClauses : List Clause -> List Clause -> Bool
isConsecuenceBetweenClauses s1 s2 =
    List.isEmpty <|
        List.filter
            (\x ->
                not (isClausalSetModel x s2)
                    && isClausalSetModel x s1
            )
        <|
            clauseSetInterpretations (s1 ++ s2)



-- Definition of propConsecuenceWithClauses


propConsecuenceWithClauses : List Prop -> Prop -> Bool
propConsecuenceWithClauses xs x =
    isConsecuenceBetweenClauses (setClauses xs) (propClause x)


-- main = text <| Debug.toString <| setClauses <| List.map (\(x,_) -> withDefault Insat x) <| parserPropSet "p -> q & q ->p ; t; s;"
        