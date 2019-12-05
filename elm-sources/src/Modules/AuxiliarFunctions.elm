module Modules.AuxiliarFunctions exposing (..)

import List exposing (..)
import String exposing (..)
import Char exposing (..)
import Html exposing (..)

unique : List a -> List a
unique xs = List.foldr (\x ac -> if List.member x ac then ac else x::ac) [] xs

uniqueBy : (a -> a -> Bool) -> List a -> List a
uniqueBy p xs= List.foldr (\x ac -> if (List.any (\y -> p x y) ac) then ac else x::ac) [] xs

unionLs : List a -> List a -> List a
unionLs xs ys = unique <| xs ++ ys

deleteLs : List a -> a -> List a
deleteLs xs e= List.filter (\x -> x /= e) xs

deleteFirstLs : List a -> List a
deleteFirstLs xs = case List.tail xs of
    Nothing -> []

    Just ys -> ys


init : List a -> List a
init xs =
    case xs of
        [] -> []
        other -> List.reverse <| List.drop 1 <| List.reverse xs

equalList : List a -> List a -> Bool
equalList a b = (List.length a == List.length b) && (List.all (\x -> member x b) a)

isSubSet : List a -> List a -> Bool
isSubSet xs ys = List.all (\x -> List.member x ys) xs

boolToString : Bool -> String
boolToString x = case x of
    True -> "True"
    False -> "False"
        
