module Actions exposing (..)

-- This wee DSL allows any tool to ask Main to update the model and display stuff
-- (including on the Map) without the tools needing knowledge of the model or ports.

import DomainModel exposing (EarthPoint, GPXSource, PeteTree)
import Element
import File exposing (File)
import Http
import Json.Decode as E
import Length
import LocalCoords exposing (LocalCoords)
import OAuth
import PreviewData exposing (PreviewData)
import Scene3d exposing (Entity)
import Tools.BendSmootherOptions
import Tools.BezierOptions
import Tools.CentroidAverageOptions
import Tools.CurveFormerOptions
import Tools.InterpolateOptions
import Tools.LimitGradientOptions
import Tools.MemoryUsage
import Tools.MoveAndStretchOptions
import Tools.MoveScaleRotateOptions
import Tools.NudgeOptions
import Tools.OutAndBackOptions
import Tools.StartFinishTypes
import Tools.StravaOptions
import Tools.StravaTypes exposing (StravaRoute, StravaSegment, StravaSegmentStreams)


type ToolAction msg
    = NoAction
    | SetCurrent Int -- move the orange pointer
    | SetCurrentFromMapClick Int -- to avoid re-centering the map!
    | SaveLastMapClick Float Float
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
    | SetMapStyle String
    | PointMovedOnMap Float Float Float Float
    | NudgeApplyWithOptions Tools.NudgeOptions.Options
    | OutAndBackApplyWithOptions Tools.OutAndBackOptions.Options
    | ApplySimplify
    | ApplyInterpolateWithOptions Tools.InterpolateOptions.Options
    | OneClickQuickFix
    | LimitGradientWithOptions Tools.LimitGradientOptions.Options
    | ApplyRotateAndScale Tools.MoveScaleRotateOptions.Options
    | ApplyRecentre ( Float, Float )
    | AddFullTrackToMap
    | FetchMapElevations
    | ApplyMapElevations (List Float)
    | SelectSvgFile (File -> msg)
    | LoadSvgFile (String -> msg) File
    | TrackFromSvg String
    | StartFlythoughTicks
    | StopFlythroughTicks
    | RequestStravaRouteHeader (Result Http.Error StravaRoute -> msg) String OAuth.Token
    | RequestStravaRoute (Result Http.Error String -> msg) String OAuth.Token
    | LoadGpxFromStrava String
    | RequestStravaSegment (Result Http.Error StravaSegment -> msg) String OAuth.Token
    | RequestStravaSegmentStreams (Result Http.Error StravaSegmentStreams -> msg) String OAuth.Token
    | PasteStravaSegment Tools.StravaOptions.Options
    | MoveAndStretchWithOptions Tools.MoveAndStretchOptions.Options
    | CloseLoopWithOptions Tools.StartFinishTypes.Options
    | ReverseTrack
    | MoveStartPoint Int
    | AddRiderPens


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

        OneClickQuickFix ->
            "one-click quick0fix"

        LimitGradientWithOptions options ->
            "limit gradients"

        ApplyRotateAndScale _ ->
            "rotate and scale"

        ApplyRecentre _ ->
            "move"

        ApplyMapElevations _ ->
            "use map elevations"

        PasteStravaSegment _ ->
            "Strava segment"

        MoveAndStretchWithOptions _ ->
            "move / stretch"

        CloseLoopWithOptions _ ->
            "close loop"

        ReverseTrack ->
            "reverse route"

        MoveStartPoint _ ->
            "move start"

        AddRiderPens ->
            "add rider pens"

        _ ->
            "tell Pete this needs a message"
