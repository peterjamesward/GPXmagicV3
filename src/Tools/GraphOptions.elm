module Tools.GraphOptions exposing (..)

import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Quantity exposing (Quantity)
import TrackLoaded exposing (TrackLoaded)


type Direction
    = Natural -- from low node to high
    | Reverse -- from high to low


type alias XY =
    -- First pass is about detecting revisited points and working out where the edges are.
    -- For that, we build a dictionary indexed by track point position (metric units).
    ( Float, Float )


type alias Options msg =
    { graph : Graph msg
    , matchingTolerance : Length.Length -- When to treat a nearby point as on the same road section.
    , centreLineOffset : Length.Length
    , minimumRadiusAtPlaces : Length.Length
    , boundingBox : BoundingBox3d Length.Meters LocalCoords
    , selectedTraversal : Int
    , analyzed : Bool
    , originalTrack : Maybe (TrackLoaded msg)
    , editingTrack : Int
    , undoGraph : Maybe (Graph msg) -- our private undo stack (of one).
    , undoOriginalTrack : Maybe (TrackLoaded msg)
    , straightenedPoints : List NearbyPointMapping
    }


type alias Graph msg =
    { nodes : Dict Int XY
    , edges : Dict Int ( ( Int, Int, XY ), TrackLoaded msg ) -- key == (low node index, high node index, point 1 XY)
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


type alias Route =
    List Traversal


type NearbyPointMapping
    = MapPoint Int Int -- map point $1 to prior point $2
    | MapNewPoint Int Int Length.Length -- map point $1 to distance $3 along prior segment $2
