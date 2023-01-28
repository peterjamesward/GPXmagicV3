module Tools.NudgeOptions exposing (Options)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Length exposing (Meters)
import Quantity exposing (Quantity)


type alias Options =
    { horizontal : Length.Length
    , vertical : Length.Length
    , fadeExtent : Length.Length
    , cosineEasing : Bool
    , easingSpacing : Length.Length
    }
