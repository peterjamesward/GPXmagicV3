module ViewContext exposing (..)

import DomainModel exposing (EarthPoint)
import Html.Events.Extra.Mouse as Mouse
import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)


type DragAction
    = DragNone
    | DragPan


type alias ViewContext =
    { dragAction : DragAction
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float -- 0 = whole track, 1 = half, etc.
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , followSelectedPoint : Bool
    , metresPerPixel : Float -- Helps with dragging accurately.
    , waitingForClickDelay : Bool
    , profileScene : List (Entity LocalCoords)
    , emphasis : Float
    , mouseEvent : Maybe Mouse.Event
    }

