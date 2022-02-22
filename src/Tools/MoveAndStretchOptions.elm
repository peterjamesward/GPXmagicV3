module Tools.MoveAndStretchOptions exposing (..)

import DomainModel exposing (EarthPoint)
import Length
import LocalCoords exposing (LocalCoords)
import Point2d
import Vector2d


type Mode
    = Translate
    | Stretch


type alias Point =
    Point2d.Point2d Length.Meters LocalCoords


type alias Options =
    { vector : Vector2d.Vector2d Length.Meters LocalCoords
    , lastVector : Vector2d.Vector2d Length.Meters LocalCoords
    , dragging : Maybe Point
    , preview : List EarthPoint
    , mode : Mode
    , stretchPointer : Maybe Int
    , heightSliderSetting : Float
    }
