module Drag3dCommonStructures exposing (..)

import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)


type DragAction
    = DragNone
    | DragRotate Float Float
    | DragPan Float Float
    | DragPaint PaintInfo -- freehand drawing, use entire path
    | DragTool String PointLeafProximity PointLeafProximity -- applying tool, only keep start and end.


type ScreenCoords
    = ScreenCoords


type alias PointLeafProximity =
    --Encapsulates the finger painting as we go so apply should be simple.
    { leafIndex : Int
    , distanceAlong : Quantity Float Meters
    , distanceFrom : Quantity Float Meters
    , proportionAlong : Float -- These distances are planar XY, but proportional along leaf should work.
    , screenPoint : Point2d Pixels ScreenCoords
    , worldPoint : Point3d Meters LocalCoords
    }


type alias PaintInfo =
    -- Whatever we need to draw the Painter and its trail.
    { path : List PointLeafProximity }
