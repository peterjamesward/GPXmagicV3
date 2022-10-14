module Tools.NamedSegmentOptions exposing (..)

import Length exposing (Meters)
import Quantity exposing (Quantity)


type alias Options =
    { selectedSegment : Maybe Int
    , namedSegments : List NamedSegment
    , landUseProximity : Maybe Length.Length
    , landUsePreferCloser : Bool
    }


type alias NamedSegment =
    -- Used for RGT timed segments.
    -- Using length not index gives some robustness against edits (e.g. 1CQF!)
    { startDistance : Quantity Float Meters
    , endDistance : Quantity Float Meters
    , name : String
    , createMode : CreateMode
    }


type CreateMode
    = ManualSegment
    | AutoSegment
