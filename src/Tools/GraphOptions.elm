module Tools.GraphOptions exposing
    ( Cluster
    , Edge
    , Graph
    , PointNearbyLeaf
    , PointNearbyPoint
    , ProjectedPointOnLeaf
    )

--This module now about the graph structure, not the UI.

import Angle
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import TrackLoaded exposing (TrackLoaded)


type alias Edge msg =
    -- (low node index, high node index, point 1 XY)
    --( ( Int, Int, XY ), TrackLoaded msg )
    --The choice of lowNode and highNode instead of start and end is so that we
    --find edges in the dictionary regardless of the direction of travel.
    --We can use via to disambiguate but it may not be part of the key.
    { lowNode : String
    , highNode : String
    , via : String
    , track : TrackLoaded msg
    }


type alias Graph msg =
    { nodes : Dict String EarthPoint
    , edges : Dict String (Edge msg)
    , referenceLonLat : GPXSource
    }


emptyGraph : Graph msg
emptyGraph =
    { nodes = Dict.empty
    , edges = Dict.empty
    , referenceLonLat =
        { latitude = Angle.degrees 0
        , longitude = Direction2d.positiveX
        , altitude = Quantity.zero
        , timestamp = Nothing
        }
    }



-- Following structures reflect a more SQL-like declarative formulation for finding the clusters.
-- Efficiency is secondary, though not mutually exclusive from clarity.


type alias PointNearbyLeaf =
    { fromTrack : String
    , fromPoint : Int
    , toTrack : String
    , toLeaf : Int
    }


type alias ProjectedPointOnLeaf =
    -- for expressing that we need a new point inserted in a leaf, it being the
    -- point on the leaf closest to a "nearby" point.
    { fromTrack : String
    , fromPoint : Int
    , toTrack : String
    , toLeaf : Int
    , distanceAlong : Quantity Float Meters
    , projectedPoint : Point3d.Point3d Meters LocalCoords
    }


type alias PointNearbyPoint =
    -- Preliminary to clustering, from index queries.
    { aTrack : String
    , aPointIndex : Int
    , aPoint : Point3d.Point3d Meters LocalCoords
    , bTrack : String
    , bPointIndex : Int
    , bPoint : Point3d.Point3d Meters LocalCoords
    , separation : Quantity Float Meters
    }


type alias Cluster =
    { centroid : Point3d Meters LocalCoords
    , pointsToAdjust : List ( String, Int, Point3d.Point3d Meters LocalCoords ) -- (track name, point index)
    }
