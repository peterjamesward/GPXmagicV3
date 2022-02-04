module Actions exposing (..)

-- This wee DSL allows any tool to ask Main to update the model and display stuff
-- (including on the Map) without the tools needing knowledge of the model or ports.

import DomainModel exposing (EarthPoint, GPXSource)
import Element
import Json.Decode as E
import Tools.BezierOptions
import Tools.MemoryUsage


type ToolAction msg
    = NoAction
    | SetCurrent Int
    | SetCurrentFromMapClick Int -- to avoid re-centering the map!
    | ShowPreview PreviewData
    | HidePreview String
    | DelayMessage Int msg
    | MapCenterOnCurrent
    | MapRefresh -- generally because layout has changed.
    | StoreLocally String E.Value
    | StoredValueRetrieved String E.Value
    | DeletePointsBetween Int Int -- fromStart, fromEnd
    | DeleteSinglePoint Int Int -- fromStart, fromEnd
    | TrackHasChanged -- Must follow an action that changes the track.
    | SetMarker (Maybe Int)
    | UndoLastAction
    | RedoUndoneAction
    | HeapStatusUpdate Tools.MemoryUsage.HeapStatus
    | RenderProfile
    | BezierSplineThroughCurrentPoints Tools.BezierOptions.Options


type PreviewShape
    = PreviewCircle
    | PreviewLine


type alias PreviewData =
    { tag : String
    , shape : PreviewShape
    , colour : Element.Color
    , points : List ( EarthPoint, GPXSource )
    }


interpretAction : ToolAction msg -> String
interpretAction action =
    -- Only needed for track modifying actions that go in the undo stack.
    case action of
        DeletePointsBetween fromStart fromEnd ->
            "deletion of points"

        DeleteSinglePoint fromStart fromEnd ->
            "delete single point"

        BezierSplineThroughCurrentPoints options ->
            "Bezier spline through current points"

        _ -> "ask Pete to fix this message"