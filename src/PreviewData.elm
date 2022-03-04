module PreviewData exposing (..)

import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, distanceFromIndex, earthPointFromIndex, foldOverRouteRL, gpxPointFromIndex, gradientFromNode, leafFromIndex)
import Element
import Length
import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)


type PreviewShape
    = PreviewCircle
    | PreviewLine
    | PreviewToolSupplied (List (Entity LocalCoords))
    | PreviewProfile PeteTree -- experimental for filter previews on Profile only.


type alias PreviewData =
    -- We need distance and gradient for Profile ...
    { tag : String
    , shape : PreviewShape
    , colour : Element.Color
    , points : List PreviewPoint
    }


type alias PreviewPoint =
    { earthPoint : EarthPoint
    , gpx : GPXSource
    }
