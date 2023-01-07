module Tools.MoveAndStretchOptions exposing (Mode(..), Options, Point)

import Length
import LocalCoords exposing (LocalCoords)
import Point2d
import PreviewData exposing (PreviewPoint)
import Quantity exposing (Quantity)
import Vector2d


type Mode
    = Translate
    | Stretch Int


type alias Point =
    Point2d.Point2d Length.Meters LocalCoords


type alias Options =
    { vector : Vector2d.Vector2d Length.Meters LocalCoords
    , lastVector : Vector2d.Vector2d Length.Meters LocalCoords
    , dragging : Maybe Point
    , preview : List PreviewPoint
    , mode : Mode
    , heightSliderSetting : Quantity Float Length.Meters
    }
