module Modules.AuxForLitvis exposing (showGraphViz, showTable)
showGraphViz : String -> String -> String
showGraphViz gid g=
    "<div style=\"display:flex; align-items: center; width:100%; height:auto;\"> \n" ++ 
    "<textarea style=\"display:none; resize: none; float:left\" id=\"inDOT" ++ gid ++ "\">" ++ g ++ "</textarea> \n" 
    ++"<div id=\"graph" ++ gid ++ "\" style=\" display: table; margin: 0 auto; width:auto; height:auto; float: left; float:left\"></div> \n"
    ++ "</div> \n " ++ 
    "<script type=\"text/javascript\"> plotDot(\"#graph" ++ gid ++ "\", document.getElementById(\"inDOT" ++ gid ++ "\").value); </script>"

showTable : List String -> List (List String) -> String
showTable header body =
  let thead = "| " ++ String.join " | " header  ++ " | "
      tbody = String.join " \n " <| List.map (\xs -> "| " ++ String.join " | " xs  ++ " | ") body
  in
    thead ++ " \n| " ++ String.repeat (List.length header) ":-- |" ++ "\n" ++ tbody
