module Tools.AnnealingOptions exposing (Options)

import Length exposing (Meters)
import Quantity exposing (Quantity)
import TrackLoaded exposing (TrackLoaded)


type alias Options msg =
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
    , saTrack : Maybe (TrackLoaded msg)
    , iterationsToRun : Int
    , scoreHistory : List Float
    , currentIndex : Int
    , searching : Bool
    }
