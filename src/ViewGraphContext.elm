module ViewGraphContext exposing (..)

import Angle exposing (Angle)
import DomainModel exposing (EarthPoint)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Tools.TracksOptions exposing (ClickDetect)


type EdgeMode
    = EdgeArc
    | EdgeSketch


type DragAction
    = DragNone
    | DragPan


type alias GraphContext =
    { fieldOfView : Angle
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    , clickPoint : Maybe ( Float, Float )
    , clickFeature : ClickDetect
    , edgeMode : EdgeMode
    , haveDisplayedEditingReminder : Bool
    , mouseHere : Point2d Pixels LocalCoords
    }
