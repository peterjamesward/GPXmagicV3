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
    }


type GraphState msg
    = GraphOriginalTracks
    | GraphSnapped (Graph.Graph msg)
    | GraphAnalyzed (Graph.Graph msg)


type alias GraphOptions msg =
    { matchingTolerance : Length.Length -- When to treat a nearby point as on the same road section.
    , centreLineOffset : Length.Length
    , minimumRadiusAtPlaces : Length.Length

    --, boundingBox : BoundingBox3d Length.Meters LocalCoords
    --, selectedTraversal : Int
    , analyzed : Bool

    --, originalTrack : Maybe (TrackLoaded msg)
    --, editingTrack : Int
    --, undoGraph : Maybe (Graph.Graph msg) -- our private undo stack (of one).
    --, undoOriginalTrack : Maybe (TrackLoaded msg)
    , clustersForPreview : List Graph.Cluster

    --, perpsForPreview : List Graph.InsertedPointOnLeaf
    --, suggestedNewTree : Maybe PeteTree
    --, suggestedNewGraph : Maybe (Graph.Graph msg)
    , graphUndos : List (Graph.Graph msg)
    , userRoute : List Graph.Traversal
    }
