module Logicus.ResolutionLP exposing (..)

import Logicus.NormalFormsDPLL exposing (Literal, Clause,  literalComplement)
import Logicus.AuxiliarFunctions exposing (unionLs)

resolute : Clause -> Clause -> Literal-> Clause
resolute c1 c2 l =
    unionLs (List.filter (\x -> x /= l) c1) (List.filter (\x -> x /= literalComplement l) c2)
clausesResolutes : Clause -> Clause -> List Clause
clausesResolutes c1 c2 =
    List.foldl (\l ac ->  if List.member (literalComplement l) c2 then ac ++ [resolute c1 c2 l] else ac) [] c1

