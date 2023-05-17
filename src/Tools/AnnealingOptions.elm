module Tools.AnnealingOptions exposing (Options)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Length exposing (Meters)
import Quantity exposing (Quantity)


type alias Options =
    -- Some combination of these will determine score for SA.
    { weightSamePosition : Float
    , weightSameAltitude : Float
    , weightSameGradient : Float
    , weightSameDirection : Float
    , weightDirectionDelta : Float
    , weightMinRadius : Float
    , weightMaxGradientDelta : Float
    , weightMaxGradient : Float
    , minRadius : Quantity Float Meters
    , maxGradient : Float
    , maxDeltaGradient : Float
    }
