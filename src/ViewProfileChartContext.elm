module ViewProfileChartContext exposing (..)

import Html.Events.Extra.Mouse as Mouse
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)


type Msg
    = ImageMouseWheel Float
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | ImageNoOp
    | ImageClick Mouse.Event
    | ImageDoubleClick Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ClickDelayExpired
    | ToggleFollowOrange
    | ToggleColours
    | SetEmphasis Int
    | MouseMove Mouse.Event


type DragAction
    = DragNone
    | DragPan Float


type alias ProfileContext =
    { contextSuffix : String -- needed to issue paint directives to canvas chart.
    , dragAction : DragAction
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float -- 0 = whole track, 1 = half, etc.
    , defaultZoomLevel : Float
    , focalPoint : Quantity Float Meters  -- NB route extrema clamped to chart edges.
    , focalPoint3d : Point3d.Point3d Meters LocalCoords
    , followSelectedPoint : Bool
    , metresPerPixel : Float -- Helps with dragging accurately.
    , waitingForClickDelay : Bool
    , emphasis : Float
    , mouseEvent : Maybe Mouse.Event
    , colouredChart : Bool
    }
