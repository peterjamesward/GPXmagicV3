module Tools.NudgeOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Length exposing (Meters)
import Quantity exposing (Quantity)



type alias Options =
    { horizontal : Quantity Float Meters
    , vertical : Quantity Float Meters
    , fadeExtent : Quantity Float Meters
    }
