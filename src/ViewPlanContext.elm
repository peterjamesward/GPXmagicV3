module ViewPlanContext exposing (..)

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Direction2d exposing (Direction2d)
import DomainModel exposing (EarthPoint)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import MapViewer
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)


type DragAction
    = DragNone
    | DragPan
    | DragPaint PaintInfo
    | DragPush PushInfo


type ScreenCoords
    = ScreenCoords


type alias PushInfo =
    -- Whatever we need to draw the Pusher and its impact.
    -- Need direction, origin, pusher width, distance from origin, track points affected, new point locations.
    {}


type alias PaintInfo =
    -- Whatever we need to draw the Painter and its trail.
    -- Start leaf index (?), touch point (XY), all drag locations (in LocalCoords or screen points?).
    { path : List (Point2d Pixels ScreenCoords)
    , screenRectangle : Rectangle2d Pixels ScreenCoords
    , camera : Camera3d Meters LocalCoords
    }


type alias PlanContext =
    { fieldOfView : Angle
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    , map : MapViewer.Model
    , showMap : Bool
    , fingerPainting : Bool
    }
