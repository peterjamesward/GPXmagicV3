module Tools.TracksOptions exposing (..)

import DomainModel exposing (GPXSource, PeteTree)
import Length exposing (Meters)
import Quantity exposing (Quantity)
import Tools.GraphOptions exposing (Cluster, Graph)
import TrackLoaded exposing (TrackLoaded)


type alias Options msg =
    { nextTrackNumber : Int
    , activeTrackName : Maybe String
    , commonReferenceGPX : Maybe GPXSource -- from where we derive (X,Y) by map projection.
    , graph : Graph msg
    , graphState : GraphState msg
    , roadListCollapsed : Bool
    , selectedTraversal : Int
    , userRoute : List Traversal
    , matchingTolerance : Length.Length -- When to treat a nearby point as on the same road section.
    , centreLineOffset : Length.Length
    , minimumRadiusAtPlaces : Length.Length
    , clustersForPreview : List Cluster
    }


type GraphState msg
    = GraphNoTracks
    | GraphOriginalTracks
    | GraphSnapped (Graph msg)
    | GraphWithNodes (Graph msg) (Graph msg)
    | GraphWithEdges (Graph msg) (Graph msg) (Graph msg)


type ClickDetect
    = ClickNode String
    | ClickEdge String
    | ClickNone


type alias Traversal =
    { edge : String
    , direction : Direction
    }


type Direction
    = Natural -- from low (sort order) node to high
    | Reverse -- from high (sort order) to low


type alias TraversalDisplay =
    { startPlace : String
    , road : String
    , endPlace : String
    , length : Quantity Float Meters
    }
