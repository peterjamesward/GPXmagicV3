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
import Tools.InterpolateOptions
import Tools.MemoryUsage
import Tools.NudgeOptions
import Tools.OutAndBackOptions


type ToolAction msg
    = NoAction
    | SetCurrent Int -- move the orange pointer
    | SetCurrentFromMapClick Int -- to avoid re-centering the map!
    | ShowPreview PreviewData -- add a tool's preview to the collection
    | HidePreview String -- remove a tools' preview
    | DelayMessage Int msg -- set a timer, useful for debouncing
    | MapCenterOnCurrent -- as it says
    | MapRefresh -- generally because layout has changed.
    | StoreLocally String E.Value -- save something in local storage
    | StoredValueRetrieved String E.Value -- retrieve from local storage
    | DeletePointsBetween Int Int -- fromStart, fromEnd
    | DeleteSinglePoint Int Int -- fromStart, fromEnd
    | TrackHasChanged -- Tools need to update to reflect any change in track
    | SetMarker (Maybe Int) -- position the purple marker
    | UndoLastAction
    | RedoUndoneAction
    | HeapStatusUpdate Tools.MemoryUsage.HeapStatus
    | RenderProfile -- rebuild the altitude and gradient charts
    | BezierApplyWithOptions Tools.BezierOptions.Options
    | CentroidAverageApplyWithOptions Tools.CentroidAverageOptions.Options
    | CurveFormerApplyWithOptions Tools.CurveFormerOptions.Options
    | BendSmootherApplyWithOptions Tools.BendSmootherOptions.Options
    | MakeMapPointsDraggable Bool
    | PointMovedOnMap Float Float Float Float
    | NudgeApplyWithOptions Tools.NudgeOptions.Options
    | OutAndBackApplyWithOptions Tools.OutAndBackOptions.Options
    | ApplySimplify
    | ApplyInterpolateWithOptions Tools.InterpolateOptions.Options


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

        NudgeApplyWithOptions options ->
            "nudge"

        OutAndBackApplyWithOptions options ->
            "out and back"

        ApplySimplify ->
            "simplify"

        ApplyInterpolateWithOptions options ->
            "interpolate"

        _ ->
            "ask Pete to fix this message"
