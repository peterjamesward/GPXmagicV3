module Tools.GraphOptions exposing (..)

import Dict exposing (Dict)
import DomainModel exposing (PeteTree)
import Length exposing (Meters)
import Quantity exposing (Quantity)
import TrackLoaded exposing (TrackLoaded)


type Direction
    = Forwards
    | Backwards


type alias XY =
    -- First pass is about detecting revisited points and working out where the edges are.
    -- For that, we build a dictionary indexed by track point position (metric units).
    ( Float, Float )


type alias EdgeKey =
    -- In v3, an edge is uniquely defined by a pair of tree indices.
    -- So not sure if I want (Int, Int) here or (XY, XY).
    -- Probably will use Ints for node, so Int, Int is OK.
    ( Int, Int )


type alias Options =
    { graph : Maybe Graph
    , pointTolerance : Quantity Float Meters -- How close in metres to consider points equal.
    , minimumEdgeLength : Quantity Float Meters -- So we can ignore short self-loops
    , centreLineOffset : Length.Length
    }


type alias Graph =
    { nodes : Dict Int ()
    , edges : Dict EdgeKey PeteTree
    , userRoute : List Traversal
    , canonicalRoute : List Traversal
    , selectedTraversal : Maybe Int
    }


type alias Traversal =
    { edge : EdgeKey -- Canonical index of edge
    , direction : Direction
    }


type alias Route =
    { route : List Traversal }


type
    PointType
    -- We shall use this to build an index back from Trackpoint land to Graph land.
    = NodePoint XY -- Canonical index of node
    | EdgePoint EdgeKey -- Canonical index of edge, canonical index of node
