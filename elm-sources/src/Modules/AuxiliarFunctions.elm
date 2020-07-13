module Modules.AuxiliarFunctions exposing (powerset)

import List

powerset : List a -> List (List a)
powerset = 
  List.foldr (\x acc -> acc ++ List.map ((::) x) acc) [[]]