module Tools.CurveFormerOptions exposing (GradientSmoothing(..), Options, Point)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, RoadSection)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import PreviewData exposing (PreviewPoint)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


type GradientSmoothing
    = Piecewise
    | Holistic


type alias Point =
    Point2d Meters LocalCoords


type alias Options =
    -- Circle centre is Orange marker xy translated by the vector.
    { vector : Vector2d.Vector2d Meters LocalCoords
    , referencePoint : Maybe EarthPoint
    , dragging : Maybe Point
    , smoothGradient : GradientSmoothing
    , pushRadius : Quantity Float Meters
    , pullRadius : Quantity Float Meters
    , spacing : Quantity Float Meters
    , usePullRadius : Bool
    , pointsWithinCircle : Dict Int RoadSection
    , pointsWithinDisc : Dict Int RoadSection

    --, circle : Maybe (Circle3d Meters LocalCoords)
    , pointsAreContiguous : Bool
    , newTrackPoints : List PreviewPoint
    , fixedAttachmentPoints : Maybe ( Int, Int )
    , transitionRadius : Quantity Float Meters
    , lastVector : Vector2d Meters LocalCoords
    }
