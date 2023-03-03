module Tools.GraphOptions exposing (ClickDetect(..), Cluster, Direction(..), Edge, Graph, InsertedPointOnLeaf, Options, Traversal, TraversalDisplay, XY)

import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import TrackLoaded exposing (TrackLoaded)


type Direction
    = Natural -- from low node to high
    | Reverse -- from high to low


type alias XY =
    -- First pass is about detecting revisited points and working out where the edges are.
    -- For that, we build a dictionary indexed by track point position (meters from reference).
    ( Float, Float )


type alias Options msg =
    --TODO: Moving out things that are not specific to one Route
    { matchingTolerance : Length.Length -- When to treat a nearby point as on the same road section.
    , centreLineOffset : Length.Length
    , minimumRadiusAtPlaces : Length.Length
    , clustersForPreview : List Cluster
    , perpsForPreview : List InsertedPointOnLeaf
    }


type alias Edge msg =
    -- (low node index, high node index, point 1 XY)
    --( ( Int, Int, XY ), TrackLoaded msg )
    { lowNode : Int
    , highNode : Int
    , via : XY
    , track : TrackLoaded msg
    , originalDirection : Direction
    }


type alias Graph msg =
    { nodes : Dict Int XY
    , edges : Dict Int (Edge msg)
    , userRoute : List Traversal
    , referenceLonLat : GPXSource
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
