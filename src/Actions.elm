module Actions exposing (..)

-- This wee DSL allows any tool to ask Main to update the model and display stuff
-- (including on the Map) without the tools needing knowledge of the model or ports.

import File exposing (File)
import Http
import Json.Decode as E
import Length exposing (Meters)
import OAuth
import PreviewData exposing (PreviewData)
import Quantity exposing (Quantity)
import Tools.BendSmootherOptions
import Tools.BezierOptions
import Tools.CentroidAverageOptions
import Tools.CurveFormerOptions
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.InterpolateOptions
import Tools.MemoryUsage
import Tools.MoveAndStretchOptions
import Tools.MoveScaleRotateOptions
import Tools.NudgeOptions
import Tools.OutAndBackOptions
import Tools.ProfileSmoothOptions
import Tools.SmartSmootherOptions
import Tools.StartFinishTypes
import Tools.StravaOptions
import Tools.StravaTypes exposing (StravaRoute, StravaSegment, StravaSegmentStreams)


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
    | PointerChange -- Need to refresh views, but not replace the track on map
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
    | FlushUndo
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
    | DeleteEdge Int
    | StartRoutePlanning
    | ExitRoutePlanning
    | ChangeActiveTrack Int
    | MakeRouteFromGraph
    | CombineNearbyPoints
    | WidenBend (List Int) (Quantity Float Meters)


actionTextForUndo : I18NOptions.Location -> ToolAction msg -> String
actionTextForUndo location action =
    -- Only needed for track modifying actions that go in the undo stack.
    I18N.localisedString location "action" <|
        case action of
            DeletePointsBetween fromStart fromEnd ->
                "deleteN"

            DeleteSinglePoint fromStart fromEnd ->
                "delete1"

            BezierApplyWithOptions options ->
                "spline"

            CentroidAverageApplyWithOptions options ->
                "centroid"

            CurveFormerApplyWithOptions options ->
                "radius"

            BendSmootherApplyWithOptions options ->
                "arc"

            PointMovedOnMap _ _ _ _ ->
                "map"

            NudgeApplyWithOptions options ->
                "nudge"

            OutAndBackApplyWithOptions options ->
                "outback"

            ApplySimplify ->
                "simplify"

            ApplyInterpolateWithOptions options ->
                "insert"

            OneClickQuickFix ->
                "1CQF"

            LimitGradientWithOptions options ->
                "limit"

            SmoothAltitudes options ->
                "altitudes"

            SmoothGradients options ->
                "gradients"

            ApplyRotateAndScale _ ->
                "scale"

            ApplyRecentre _ ->
                "move"

            ApplyMapElevations _ ->
                "elevations"

            PasteStravaSegment _ ->
                "segment"

            MoveAndStretchWithOptions _ ->
                "stretch"

            CloseLoopWithOptions _ ->
                "close"

            ReverseTrack ->
                "reverse"

            MoveStartPoint _ ->
                "start"

            AddRiderPens ->
                "pens"

            ParseAndAppend _ ->
                "append"

            Straighten ->
                "straighten"

            Autofix _ ->
                "autofix"

            MakeRouteFromGraph ->
                "route"

            WidenBend _ _ ->
                "widen"

            SmartSmootherApplyWithOptions _ ->
                "smart"

            CombineNearbyPoints ->
                "combine"

            _ ->
                "unknown"
