module ViewProfileChartContext exposing (..)

import DomainModel exposing (EarthPoint)
import Html.Events.Extra.Mouse as Mouse
import Length exposing (Meters)
import Quantity exposing (Quantity)


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
    | SetEmphasis Int
    | MouseMove Mouse.Event


type DragAction
    = DragNone
    | DragPan


type alias ProfileContext =
    { dragAction : DragAction
    , zoomLevel : Float -- 0 = whole track, 1 = half, etc.
    , defaultZoomLevel : Float
    , focalPoint : Quantity Float Meters  -- NB route extrema clamped to chart edges.
    , followSelectedPoint : Bool
    , metresPerPixel : Float -- Helps with dragging accurately.
    , waitingForClickDelay : Bool
    , emphasis : Float
    , mouseEvent : Maybe Mouse.Event
    }
