module Tools.AnnealingOptions exposing (MinimalTrack, Options, Perturbation)

import Direction2d exposing (Direction2d)
import DomainModel exposing (GPXSource, PeteTree)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Quantity exposing (Quantity)


type alias MinimalTrack =
    -- Import loops making copy of TrackLoaded a pain.
    { tree : PeteTree
    , reference : GPXSource
    }


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
    , saTrack : Maybe MinimalTrack
    , iterationsToRun : Int
    , scoreHistory : List Float
    , currentIndex : Int
    , searching : Bool
    , lastPerturbation : Maybe Perturbation
    }


type alias Perturbation =
    { pointIndex : Int -- proportion of track distance
    , direction : Direction2d LocalCoords
    , distance : Float -- meters horizontal displacement
    , altitude : Float
    , p : Float -- Chance of accepting a "worse" option
    }
