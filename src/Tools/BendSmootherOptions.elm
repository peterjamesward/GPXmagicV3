module Tools.BendSmootherOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import DomainModel exposing (EarthPoint)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)


type Msg
    = SmoothBend
    | SetBendTrackPointSpacing Float
    | SetSegments Int
    | SoftenBend


type alias Options =
    { bendTrackPointSpacing : Float
    , smoothedBend : Maybe SmoothedBend
    , segments : Int
    }


type alias SmoothedBend =
    { nodes : List EarthPoint
    , centre : Point2d Meters LocalCoords
    , radius : Float
    , startIndex : Int -- Lead-in node that is NOT to be replaced
    , endIndex : Int -- ... and lead-out, not to be replaced.
    }


type alias DrawingRoad =
    { startsAt : Point3d Meters LocalCoords
    , endsAt : Point3d Meters LocalCoords
    , index : Int
    }
