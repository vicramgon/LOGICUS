module Modules.AuxiliarFunctions exposing (powerset, deleteFirstLs)

import List

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

