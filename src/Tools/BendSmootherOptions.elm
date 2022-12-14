module Tools.BendSmootherOptions exposing (Options, SmoothMode(..), SmoothedBend)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import PreviewData exposing (PreviewPoint)


type alias Options =
    { bendTrackPointSpacing : Float
    , smoothedBend : Maybe SmoothedBend
    , segments : Int
    , mode : SmoothMode
    }


type SmoothMode
    = SmoothPoint
    | SmoothBend


type alias SmoothedBend =
    { nodes : List PreviewPoint
    , centre : Point2d Meters LocalCoords
    , radius : Float
    , startIndex : Int -- Lead-in node that is NOT to be replaced
    , endIndex : Int -- ... and lead-out, not to be replaced.
    }
