module ViewContextThirdPerson exposing (..)

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import DomainModel exposing (EarthPoint)
import Length
import LocalCoords exposing (LocalCoords)
import Quantity exposing (Quantity)


type DragAction
    = DragNone
    | DragRotate
    | DragPan
    | DragProfile
    | DragPlan


type alias ContextThirdPerson =
    { cameraAzimuth : Direction2d LocalCoords --Camera relative to plane normal at focus point
    , cameraElevation : Angle -- Above local horizon plane
    , cameraDistance : Quantity Float Length.Meters
    , fieldOfView : Angle
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    }
