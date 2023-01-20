module ViewPlanContext exposing (..)

import Angle exposing (Angle)
import DomainModel exposing (EarthPoint)


type DragAction
    = DragNone
    | DragPan


type alias PlanContext =
    { fieldOfView : Angle
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    }
