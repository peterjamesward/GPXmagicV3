module Actions exposing (..)

-- This wee DSL allows any tool to ask Main to update the model and display stuff
-- (including on the Map) without the tools needing knowledge of the model or ports.

import File exposing (File)
import Http
import Json.Decode as E
import LandUseDataTypes
import Length exposing (Meters)
import OAuth
import PreviewData exposing (PreviewData)
import Quantity exposing (Quantity)
import Tools.BendSmootherOptions
import Tools.BezierOptions
import Tools.CentroidAverageOptions
import Tools.CurveFormerOptions
import Tools.InterpolateOptions
import Tools.MemoryUsage
import Tools.MoveAndStretchOptions
import Tools.MoveScaleRotateOptions
import Tools.NudgeOptions
import Tools.OutAndBackOptions
import Tools.ProfileSmoothOptions
import Tools.StartFinishTypes
import Tools.StravaOptions
import Tools.StravaTypes exposing (StravaRoute, StravaSegment, StravaSegmentStreams)
import Tools.SmartSmootherOptions


type ToolAction msg
    = NoAction
    | ReRender
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
    | SmartSmootherApplyWithOptions Tools.SmartSmootherOptions.Options
    | MakeMapPointsDraggable Bool
    | SetMapStyle String
    | PointMovedOnMap Float Float Float Float
    | NudgeApplyWithOptions Tools.NudgeOptions.Options
    | OutAndBackApplyWithOptions Tools.OutAndBackOptions.Options
    | ApplySimplify
    | ApplyInterpolateWithOptions Tools.InterpolateOptions.Options
    | OneClickQuickFix
    | LimitGradientWithOptions Tools.ProfileSmoothOptions.Options
    | SmoothAltitudes Tools.ProfileSmoothOptions.Options
    | SmoothGradients Tools.ProfileSmoothOptions.Options
    | ApplyRotateAndScale Tools.MoveScaleRotateOptions.Options
    | ApplyRecentre ( Float, Float )
    | AddFullTrackToMap
    | FetchMapElevations
    | ApplyMapElevations (List (Maybe Float))
    | ApplyLandUseAltitudes (List (Maybe Float))
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
    | SelectGpxFile (File -> msg)
    | LoadGpxFile (String -> msg) File
    | TrackFromGpx String
    | ParseAndAppend String
    | WriteTrackSections (List ( Int, Float, Float ))
    | Straighten
    | DisplayInfo String String
    | Autofix (List Int)
    | AddTraversal Int
    | AddSelfLoop Int
    | LockToolOpen Bool String
    | ChangeActiveTrack Int
    | MakeRouteFromGraph
    | WidenBend (List Int) (Quantity Float Meters)


interpretAction : ToolAction msg -> String
interpretAction action =
    -- Only needed for track modifying actions that go in the undo stack.
    case action of
        DeletePointsBetween fromStart fromEnd ->
            "deletion of points"

        DeleteSinglePoint fromStart fromEnd ->
            "delete single point"

        BezierApplyWithOptions options ->
            "smooth with spline"

        CentroidAverageApplyWithOptions options ->
            "smooth with 3d average"

        CurveFormerApplyWithOptions options ->
            "radiused bend"

        BendSmootherApplyWithOptions options ->
            "circuler arc"

        PointMovedOnMap _ _ _ _ ->
            "move on map"

        NudgeApplyWithOptions options ->
            "nudge"

        OutAndBackApplyWithOptions options ->
            "out and back"

        ApplySimplify ->
            "simplify"

        ApplyInterpolateWithOptions options ->
            "insert points"

        OneClickQuickFix ->
            "one-click quick0fix"

        LimitGradientWithOptions options ->
            "limit gradients"

        SmoothAltitudes options ->
            "smooth altitudes"

        SmoothGradients options ->
            "smooth gradients"

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

        ParseAndAppend _ ->
            "append gpx"

        Straighten ->
            "straighten"

        Autofix _ ->
            "smooth points"

        MakeRouteFromGraph ->
            "route maker"

        WidenBend _ _ ->
            "widen bend"

        SmartSmootherApplyWithOptions _ ->
            "smart smoother"

        _ ->
            "tell Pete this needs a message"
