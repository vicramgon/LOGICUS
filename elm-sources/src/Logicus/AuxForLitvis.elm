module Logicus.AuxForLitvis exposing (showGraphViz, showTable)
showGraphViz : String -> String -> String
showGraphViz gid g=
    "<div id=\"" ++ gid ++ "\">\n<div id=\"inputGraph" ++ gid ++ "\" style=\"width:100%; display: inline-block;\">\n<textarea name=\"textarea\" style=\"width:100%; height:300pt;\" id=\"inDOT" ++ gid ++ "\"> " ++ g ++ " </textarea>\n<button style=\"width:100%; height: 5%\" onclick='plotDot(\"" ++ gid ++ "\");'> RENDER GRAPH </button>\n</div>\n\n<div id=\"outputGraph" ++ gid ++ "\" style=\"width:100%; display: none\">\n<div id=\"svgContainer" ++ gid ++ "\" style=\"width:100%; height:95; border: 1px solid black; display: flex; justify-content: center;\"> </div>\n<button style=\"width:100%; height: 5%; \" onclick='generatePng(\"" ++ gid ++ "\");'> DOWNLOAD AS PNG </button>\n</div>\n\n\n<canvas id=\"canvas" ++ gid ++ "\" style=\"display: none;\" ></canvas>\n<div id=\"pngContainer" ++ gid ++ "\" style=\"width:20%; height: 5%; float: left; align-content: center; display: none;\"></div>\n\n</div>"

showTable : List String -> List (List String) -> String
showTable header body =
  let thead = "| " ++ String.join " | " header  ++ " | "
      tbody = String.join " \n " <| List.map (\xs -> "| " ++ String.join " | " xs  ++ " | ") body
  in
    thead ++ " \n| " ++ String.repeat (List.length header) ":-- |" ++ "\n" ++ tbody
