module Logicus.AuxiliarFunctions exposing (powerset, deleteFirstLs, unionLs, unique, uniqueBy, uncurry, cleanSpaces, uniqueConcatList, isSubSetList)


import Char.Extra exposing (isSpace)
import List exposing (member, foldl, foldr)

powerset : List a -> List (List a)
powerset = 
  foldr (\x acc -> acc ++ List.map ((::) x) acc) [[]]

deleteFirstLs : List a -> List a
deleteFirstLs xs =
    case List.tail xs of
        Nothing ->
            []

        Just ys ->
            ys

unique : List a -> List a
unique xs =
    foldl
        (\x ac ->
            if List.member x ac then
                ac

            else
                ac ++ [x]
        )
        []
        xs


uniqueBy : (a -> a -> Bool) -> List a -> List a
uniqueBy p xs =
    foldr
        (\x ac ->
            if List.any (\y -> p x y) ac then
                ac

            else
                x :: ac
        )
        []
        xs

uniqueConcatList : List a -> List a -> List a
uniqueConcatList xs ys = foldl (\x ac -> if member x ac then ac else ac ++ [x]) xs ys

unionLs : List a -> List a -> List a
unionLs xs ys =
    unique <| xs ++ ys

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

cleanSpaces : String -> String
cleanSpaces x = 
    String.fromList <| List.filter (\c -> not(isSpace c))  <| String.toList x

isSubSetList : List a -> List a -> Bool
isSubSetList xs ys =
    List.all (\x -> List.member x ys) xs

