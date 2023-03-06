module Tools.GraphOptions exposing
    ( ClickDetect(..)
    , Cluster
    , Direction(..)
    , Edge
    , Graph
    , InsertedPointOnLeaf
    , Traversal
    , TraversalDisplay
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


type Direction
    = Natural -- from low (sort order) node to high
    | Reverse -- from high (sort order) to low


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
    , originalDirection : Direction
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


type ClickDetect
    = ClickNode Int
    | ClickEdge Int
    | ClickNone


type alias Traversal =
    { edge : Int -- Canonical index of edge
    , direction : Direction
    }


type alias TraversalDisplay =
    { startPlace : Int
    , road : Int
    , endPlace : Int
    , length : Quantity Float Meters
    }


type alias InsertedPointOnLeaf =
    -- for expressing that we need a new point inserted in a leaf, it being the
    -- point closest to a "nearby" point.
    { sourcePointNumber : Int
    , leafNumber : Int
    , distanceAlong : Quantity Float Meters
    , earthPoint : EarthPoint
    }


type alias Cluster =
    { centroid : Point3d Meters LocalCoords
    , pointsToAdjust : List Int
    }
