module Modules.AuxiliarFunctions exposing (powerset, deleteFirstLs, unionLs, unique, uniqueBy, uncurry, cleanSpaces)

import List
import Char.Extra exposing (isSpace)

powerset : List a -> List (List a)
powerset = 
  List.foldr (\x acc -> acc ++ List.map ((::) x) acc) [[]]

deleteFirstLs : List a -> List a
deleteFirstLs xs =
    case List.tail xs of
        Nothing ->
            []

        Just ys ->
            ys

unique : List a -> List a
unique xs =
    List.foldl
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
    List.foldr
        (\x ac ->
            if List.any (\y -> p x y) ac then
                ac

            else
                x :: ac
        )
        []
        xs


unionLs : List a -> List a -> List a
unionLs xs ys =
    unique <| xs ++ ys

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

cleanSpaces : String -> String
cleanSpaces x = 
    String.fromList <| List.filter (\c -> not(isSpace c))  <| String.toList x

