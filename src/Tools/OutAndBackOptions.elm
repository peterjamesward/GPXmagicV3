module Tools.OutAndBackOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import DomainModel exposing (EarthPoint)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)


type alias Options =
    { offset : Float }


type alias DrawingRoad =
    { startsAt : Point3d Meters LocalCoords
    , endsAt : Point3d Meters LocalCoords
    , index : Int
    }
