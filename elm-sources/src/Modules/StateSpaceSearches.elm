module Modules.StateSpaceSearches exposing (StateSpaceSearch, backtrackingSearch, bestFirstSearch)

import Maybe
import List

type alias StateSpaceSearch a =
    {init : a, 
    actions : a -> List (a -> a) , 
    apply : a -> (a -> a) -> a,
    heuristic : a -> Int,
    isGoal : a -> Bool
    }


bestFirstSearch : StateSpaceSearch a -> Maybe a
bestFirstSearch sssP =  bestFirstSearchAux sssP [sssP.init] []

bestFirstSearchAux : StateSpaceSearch a -> List a -> List a -> Maybe a
bestFirstSearchAux sss oS cS = 
    case oS of
        [] -> Nothing

        s::rest ->  if sss.isGoal s then
                        Just s
                    else 
                        let suc = List.filter (\x -> not <| List.member x cS) <| List.map (\a -> sss.apply s a) <| sss.actions s in
                            bestFirstSearchAux sss (List.sortBy (\x -> sss.heuristic x) <| suc ++ rest) (cS ++ [s])
                                
                            
                            
backtrackingSearch : StateSpaceSearch a -> Maybe a
backtrackingSearch sssP =   let backtrackingSearchAux sss curr =  if sss.isGoal curr then
                                                                    Just curr
                                                                else
                                                                    let aplActions = sss.actions curr in
                                                                    let aux actList s = case actList of
                                                                                            [] -> Nothing
                                                                        
                                                                                            action::rest -> case backtrackingSearchAux sss (sss.apply s action) of
                                                                                                                Just g -> Just g
                                                                                
                                                                                                                Nothing -> aux rest s
                                                                    in
                                                                        aux aplActions curr
                            in
                                backtrackingSearchAux sssP sssP.init


        