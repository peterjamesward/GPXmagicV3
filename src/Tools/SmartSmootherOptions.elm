module Tools.SmartSmootherOptions exposing (..)

-- A separate module means we can use with Action, without an import loop.

import Angle exposing (Angle)
import Length exposing (Meters)
import PreviewData exposing (PreviewPoint)
import Quantity exposing (Quantity)


type alias Options =
    -- User adjustable in tool.
    { minRadius : Quantity Float Meters -- can turn no tighter
    , minTransition : Quantity Float Meters -- length needed to go from straight to min radius
    , maxGradient : Float -- tan of maxPhi
    , newPoints : List PreviewPoint
    , blend : Float -- [0,1] relative proportions of forward and reverse deltas.
    }

