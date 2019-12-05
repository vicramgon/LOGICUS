module Modules.LPClausalForms exposing (..)

import List exposing (..)
import Html exposing (..)

import Modules.SintaxSemanticsLP exposing (..)
import Modules.LPNormalForms exposing (..)
import Modules.AuxiliarFunctions exposing (..)
import Modules.LPNormalForms exposing (..)


type alias Clause = List Literal

-- Definition of toClause that represents the clausal form of a formula.
toClause : Prop -> Clause
toClause x = 
    if literal x then [x] 
    else
        case x of
            Disj f g -> unionLs (toClause f) (toClause g)

            other -> []
                 

-- Definition of toClauseCNF that represents the clausal form of a formula in CNF.

toClauseCNF : Prop -> List Clause
toClauseCNF x =
    case x of
        Conj f g -> unionLs (toClauseCNF f) (toClauseCNF g)
        
        other -> [ toClause x]

-- Definition of propClause that returns the clausal form of any formula

propClause : Prop -> List Clause
propClause x = toClauseCNF <| toCNF x

-- Definition of setClauses that represents a set of clauses equivalent to the set

-- filter equal clauses

compareClauses : Clause -> Clause -> Bool
compareClauses a b = (List.length a == List.length b) && (List.all (\x -> member x b) a)

filterEqClauses : List Clause -> List Clause
filterEqClauses xs = uniqueBy compareClauses xs

-- remove clauses that have contrary atoms
haveContraryLiteral : Clause -> Bool
haveContraryLiteral x = List.any (\y -> member (Neg y) x) x

filterTautClauses : List Clause -> List Clause
filterTautClauses xs = List.filter (\x -> not (haveContraryLiteral x)) xs

setClauses : List Prop -> List Clause
setClauses xs = filterTautClauses <| filterEqClauses <| concat <| List.map (propClause) xs

-- Definition of symbolsClause that represents de distinct propositional symbols in a clause

symbolsClause : Clause -> List PSymb
symbolsClause = setSymbols

-- Definition of symbolsClauseSet that represents de distinct propositional symbols in a set of clauses

symbolsClauseSet : List Clause -> List PSymb
symbolsClauseSet xs = unique <| concat <| List.map (symbolsClause) xs

-- Definition of clauseInterpretations that represents all posible interpretations of a clause

clauseInterpretations : Clause -> List Interpretation
clauseInterpretations x = subsets <| symbolsClause x

-- Definition of clauseSetInterpretations that represents all posible interpretations og a clause

clauseSetInterpretations : List Clause -> List Interpretation
clauseSetInterpretations xs = subsets <| symbolsClauseSet xs

-- Definition of isLiteralModel that represents if an interpretation is model of a literal

isLiteralModel : Interpretation -> Literal -> Bool
isLiteralModel i l =
    case l of
        Atom x -> member x i
        Neg(Atom x) -> not <| member x i
        other -> False                      -- option never chosen

-- Definition of isClausalModel that represents if an interpretation is model of a clause

isClausalModel : Interpretation -> Clause -> Bool
isClausalModel  i c = any (\y -> y) <| List.map (\l -> isLiteralModel i l) c 

-- Definition of clausalModels that represents all models of a clause.

clausalModels : Clause -> List Interpretation
clausalModels c = List.filter (\x -> isClausalModel x c) <| clauseInterpretations c

-- Definition of isClausalSetModel that represent if an interpretation is model of a set of clauses

isClausalSetModel : Interpretation -> List Clause -> Bool
isClausalSetModel i cs = all (\y -> y) <| List.map (\c -> isClausalModel i c) cs

-- Definition of clausalSetModels that represents all models of a set of clauses

clausalSetModels : List Clause -> List Interpretation
clausalSetModels cs = List.filter (\x -> isClausalSetModel x cs) <| clauseSetInterpretations cs

-- Definition of validClause that represents if a clause is valid

validClause : Clause -> Bool
validClause = haveContraryLiteral

-- Definition of insatisfactibleClause that represents if a clause is insatisfactible

insatisfactibleClause : Clause -> Bool
insatisfactibleClause = List.isEmpty

-- Definition of satisfactibleClauser that represents if a clause is satsifactible
satisfactibleClause : Clause -> Bool
satisfactibleClause c = not <| insatisfactibleClause c

-- Definition of validClauseSet
validClauseSet : List Clause -> Bool
validClauseSet cs = List.all (\x -> x) <| List.map (validClause) cs

-- Definition of consistenceClauseSet that represents if a clause set is consistence

consistenceClauseSet : List Clause -> Bool
consistenceClauseSet cs = not <| List.isEmpty <| clausalSetModels cs

-- Definition of inconsistenceClauseSet

inconsistenceClauseSet : List Clause -> Bool
inconsistenceClauseSet cs = List.isEmpty <| clausalSetModels cs

-- Definition of propValidwidthClauses that represents if the clause set of a formula is valid

propValidwidthClauses : Prop -> Bool
propValidwidthClauses p = validClauseSet <| propClause <| p

-- Definition of isConsecuenceBetweenClauses that represents if all models of s1 are models of s2

isConsecuenceBetweenClauses : List Clause -> List Clause -> Bool
isConsecuenceBetweenClauses s1 s2 = List.isEmpty <| List.filter (\x -> (not (isClausalSetModel x s2)) 
    &&  (isClausalSetModel x s1) ) <| clauseSetInterpretations (s1 ++ s2) 

-- Definition of propConsecuenceWithClauses

propConsecuenceWithClauses : List Prop -> Prop -> Bool
propConsecuenceWithClauses xs x = isConsecuenceBetweenClauses (setClauses xs) (propClause x)
