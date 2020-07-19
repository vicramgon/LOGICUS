module Modules.Prueba exposing (..)

import Random
import Html exposing (..)
point : Random.Generator (Int, Int)
point =
  Random.pair (Random.int -100 100) (Random.int -100 100)

type Msg = NewPoint (Int, Int)

newPoint : Cmd Msg
newPoint =
  Random.generate NewPoint point

main = newPoint 