module ViewPlanContext exposing (..)

import Angle exposing (Angle)
import DomainModel exposing (EarthPoint)
import MapViewer


type DragAction
    = DragNone
    | DragPan


type alias PlanContext =
    --TODO: Share mapData across views (i.e. the tiles).
    { fieldOfView : Angle
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    , map : MapViewer.Model
    }
