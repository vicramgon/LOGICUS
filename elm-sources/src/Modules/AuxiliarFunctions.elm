module Modules.AuxiliarFunctions exposing (powerset, init)

import List

powerset : List a -> List (List a)
powerset = 
  List.foldr (\x acc -> acc ++ List.map ((::) x) acc) [[]]

init : List a -> List a
init xs =
    case xs of
        [] ->
            []

        _ ->
            List.reverse <| List.drop 1 <| List.reverse xs