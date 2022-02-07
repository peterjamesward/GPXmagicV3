module Actions exposing (..)

-- This wee DSL allows any tool to ask Main to update the model and display stuff
-- (including on the Map) without the tools needing knowledge of the model or ports.

import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Element
import Json.Decode as E
import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)
import Tools.BendSmootherOptions
import Tools.BezierOptions
import Tools.CentroidAverageOptions
import Tools.CurveFormerOptions
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
    | BezierApplyWithOptions Tools.BezierOptions.Options
    | CentroidAverageApplyWithOptions Tools.CentroidAverageOptions.Options
    | CurveFormerApplyWithOptions Tools.CurveFormerOptions.Options
    | BendSmootherApplyWithOptions Tools.BendSmootherOptions.Options
    | MakeMapPointsDraggable Bool
    | PointMovedOnMap Float Float Float Float


type PreviewShape
    = PreviewCircle
    | PreviewLine
    | PreviewToolSupplied (List (Entity LocalCoords))


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

        BezierApplyWithOptions options ->
            "Bezier spline"

        CentroidAverageApplyWithOptions options ->
            "centroid average"

        CurveFormerApplyWithOptions options ->
            "curve former"

        BendSmootherApplyWithOptions options ->
            "bend smoother"

        PointMovedOnMap _ _ _ _ ->
            "move on map"

        _ ->
            "ask Pete to fix this message"
