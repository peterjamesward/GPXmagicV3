module Tools.InterpolateOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Length exposing (Meters)
import Quantity exposing (Quantity)


type alias Options =
    { minimumSpacing : Quantity Float Meters
    , extent : ExtentOption
    }


type ExtentOption
    = ExtentIsRange
    | ExtentIsTrack
