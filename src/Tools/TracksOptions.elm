module Tools.TracksOptions exposing (..)

import DomainModel exposing (GPXSource, PeteTree)
import Length
import Tools.GraphOptions as Graph
import TrackLoaded exposing (TrackLoaded)


type alias Options msg =
    --TODO: This will develop, taking track stuff from Main, and perhaps subsume Graph.
    { nextTrackNumber : Int
    , tracks : List (TrackLoaded msg)
    , activeTrackIndex : Maybe Int
    , commonReferenceGPX : Maybe GPXSource -- from where we derive (X,Y) by map projection.
    , graph : Graph.Graph msg
    , graphOptions : GraphOptions msg
    , graphState : GraphState msg
    , roadListCollapsed : Bool
    }


type GraphState msg
    = GraphNoTracks
    | GraphOriginalTracks
    | GraphSnapped (Graph.Graph msg)
    | GraphWithNodes (Graph.Graph msg) (Graph.Graph msg)
    | GraphWithEdges (Graph.Graph msg) (Graph.Graph msg) (Graph.Graph msg)


type alias GraphOptions msg =
    { matchingTolerance : Length.Length -- When to treat a nearby point as on the same road section.
    , centreLineOffset : Length.Length
    , minimumRadiusAtPlaces : Length.Length

    --, selectedTraversal : Int
    , analyzed : Bool
    , clustersForPreview : List Graph.Cluster
    , graphUndos : List (Graph.Graph msg)
    , userRoute : List Graph.Traversal
    }
