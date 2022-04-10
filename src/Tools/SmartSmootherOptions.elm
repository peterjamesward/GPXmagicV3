module Tools.SmartSmootherOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import PreviewData exposing (PreviewPoint)
import Quantity exposing (Quantity)
import Vector3d exposing (Vector3d)


type alias Options =
    { -- User adjustable
      minRadius : Quantity Float Meters
    , maxDeltaGradient : Angle -- Per metre of worm length
    , steeringSharpness : Float -- affects clothoid profile
    , tolerance : Quantity Float Meters
    , state : Activity
    }


type Activity
    = Idle
    | Parsing AnalyserState
    | Ready TrackAnalysis


type alias AnalyserState =
    { -- Internal state
      atIndex : Int
    , points : List (Point3d Meters LocalCoords)
    , estimatedRadii : List (Quantity Float Meters)
    , headIndex : Int
    , tailIndex : Int
    , assessment : TrackShape
    , assessments : List TrackShape
    }


type alias TrackAnalysis =
    List TrackShape


type TrackShape
    = Straight TrackStraight
    | Intermediate TrackIntermediate
    | Bend TrackBend
    | Undecided


type alias TrackBend =
    -- This for "significant" bends, that require an arc, not just a point-wise fix.
    { startIndex : Int
    , endIndex : Int
    , directionChange : Angle -- +ve is left
    , estimatedRadius : Quantity Float Meters
    , approximateCentre : Point3d Meters LocalCoords
    }


type alias TrackIntermediate =
    -- A region without significant bends, where we smooth with Bezier approximation
    { startIndex : Int
    , endIndex : Int
    , inflections : List Int
    }


type alias TrackStraight =
    -- A region without significant bends, smoothed, if at all, with small arcs.
    { startIndex : Int
    , endIndex : Int
    , inflections : List Int
    }
