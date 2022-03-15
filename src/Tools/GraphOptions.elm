module Tools.GraphOptions exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import DomainModel exposing (GPXSource, PeteTree)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)


type Direction
    = Natural -- from low node to high
    | Reverse -- from high to low


type alias XY =
    -- First pass is about detecting revisited points and working out where the edges are.
    -- For that, we build a dictionary indexed by track point position (metric units).
    ( Float, Float )


type alias Options =
    { graph : Maybe Graph
    , centreLineOffset : Length.Length
    , boundingBox : BoundingBox3d Length.Meters LocalCoords
    , selectedTraversal : Int
    }


type alias Graph =
    { nodes : Dict Int XY
    , edges : Dict Int ( ( Int, Int, XY ), PeteTree ) -- key == (low node index, high node index, point 1 XY)
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
    { startPlace : String
    , road : String
    , endPlace : String
    , length : String
    }


type alias Route =
    List Traversal
