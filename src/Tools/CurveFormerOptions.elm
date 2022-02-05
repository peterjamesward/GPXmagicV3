module Tools.CurveFormerOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Circle3d exposing (Circle3d)
import DomainModel exposing (EarthPoint)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


type GradientSmoothing
    = Piecewise
    | Holistic


type alias Options =
    -- Circle centre is Orange marker xy translated by the vector.
    { vector : Vector2d.Vector2d Meters LocalCoords
    , referencePoint : Maybe EarthPoint
    , dragging : Maybe (Float, Float)
    , smoothGradient : GradientSmoothing
    , pushRadius : Quantity Float Meters
    , pullRadius : Quantity Float Meters
    , spacing : Quantity Float Meters
    , usePullRadius : Bool
    , pointsWithinCircle : List Int
    , pointsWithinDisc : List Int
    , circle : Maybe (Circle3d Meters LocalCoords)
    , pointsAreContiguous : Bool
    , newTrackPoints : List EarthPoint
    , fixedAttachmentPoints : Maybe ( Int, Int )
    , transitionRadius : Quantity Float Meters
    , lastVector : Vector2d Meters LocalCoords
    }
