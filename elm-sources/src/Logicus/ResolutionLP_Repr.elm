module Logicus.ResolutionLP_Repr exposing (..)

import Logicus.ResolutionLP exposing (..)
import Logicus.NormalFormsDPLL exposing (Clause)
import Graph.DOT exposing (outputWithStyles, defaultStyles)
import Logicus.NormalFormsDPLL exposing (Clause, toStringClause)
import Logicus.IO_LP exposing (toStringFLP)

graphvizResolutionGraph :  List Clause -> String 
graphvizResolutionGraph ls = 
    let 
        initialNodes = String.join ";" <| List.map (String.fromInt) <|  List.range 0 <| (List.length ls)-1
        resGraph = clauseSetResolutionGraph ls
        myStyles ={ defaultStyles | node = "shape=box, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
        printNodes = \ x -> Maybe.Just <| toStringClause x
        printEdges = \ x -> Maybe.Just <| toStringFLP x
    in
        String.replace "\n\n" "\n" <| String.replace "\n}" ("\n{rank=same; " ++ initialNodes ++ ";}\n}") <| outputWithStyles myStyles printNodes printEdges resGraph


graphvizLinearResolutionGraph :  List Clause -> String 
graphvizLinearResolutionGraph ls = 
    let 
        initialNodes = String.join ";" <| List.map (String.fromInt) <|  List.range 0 <| (List.length ls)-1
        resGraph = clauseSetLinearResolutionGraph ls
        myStyles ={ defaultStyles | node = "shape=box, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
        printNodes = \ x -> Maybe.Just <| toStringClause x
        printEdges = \ x -> Maybe.Just <| toStringFLP x
    in
        String.replace "\n\n" "\n" <| String.replace "\n}" ("\n{rank=same; " ++ initialNodes ++ ";}\n}") <| outputWithStyles myStyles printNodes printEdges resGraph

graphvizPositiveResolutionGraph :  List Clause -> String 
graphvizPositiveResolutionGraph ls = 
    let 
        initialNodes = String.join ";" <| List.map (String.fromInt) <|  List.range 0 <| (List.length ls)-1
        resGraph = clauseSetPositiveResolutionGraph ls
        myStyles ={ defaultStyles | node = "shape=box, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
        printNodes = \ x -> Maybe.Just <| toStringClause x
        printEdges = \ x -> Maybe.Just <| toStringFLP x
    in
        String.replace "\n\n" "\n" <| String.replace "\n}" ("\n{rank=same; " ++ initialNodes ++ ";}\n}") <| outputWithStyles myStyles printNodes printEdges resGraph

graphvizNegativeResolutionGraph :  List Clause -> String 
graphvizNegativeResolutionGraph ls = 
    let 
        initialNodes = String.join ";" <| List.map (String.fromInt) <|  List.range 0 <| (List.length ls)-1
        resGraph = clauseSetNegativeResolutionGraph ls
        myStyles ={ defaultStyles | node = "shape=box, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
        printNodes = \ x -> Maybe.Just <| toStringClause x
        printEdges = \ x -> Maybe.Just <| toStringFLP x
    in
        String.replace "\n\n" "\n" <| String.replace "\n}" ("\n{rank=same; " ++ initialNodes ++ ";}\n}") <| outputWithStyles myStyles printNodes printEdges resGraph


graphvizEntryResolutionGraph :  List Clause -> String 
graphvizEntryResolutionGraph ls = 
    let 
        initialNodes = String.join ";" <| List.map (String.fromInt) <|  List.range 0 <| (List.length ls)-1
        resGraph = clauseSetEntryResolutionGraph ls
        myStyles ={ defaultStyles | node = "shape=box, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
        printNodes = \ x -> Maybe.Just <| toStringClause x
        printEdges = \ x -> Maybe.Just <| toStringFLP x
    in
        String.replace "\n\n" "\n" <| String.replace "\n}" ("\n{rank=same; " ++ initialNodes ++ ";}\n}") <| outputWithStyles myStyles printNodes printEdges resGraph


