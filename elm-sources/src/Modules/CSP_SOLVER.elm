module Modules.CSP_SOLVER exposing (..)

import Random exposing (step, initialSeed, float)
import Time exposing (utc, toMillis, now)
import Task exposing (Task)

type alias Prop_CSP =
    { vars : List PSymb
    ,  : ValueType2
    }

genetic_algorithm_csp :