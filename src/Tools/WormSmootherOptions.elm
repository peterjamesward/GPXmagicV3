module Tools.WormSmootherOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import PreviewData exposing (PreviewPoint)
import Quantity exposing (Quantity)
import Vector3d exposing (Vector3d)



-- Note TNB see https://en.wikipedia.org/wiki/Frenet–Serret_formulas


type alias Options =
    { -- User adjustable
      wormLength : Quantity Float Meters
    , minRadius : Quantity Float Meters
    , maxDeltaGradient : Angle -- Per metre of worm length
    , outputSpacing : Quantity Float Meters
    }


type alias WormState =
    { -- Internal state
      pointsSwallowed : List (Point3d Meters LocalCoords)
    , displacements : List (Vector3d Meters LocalCoords) -- how far we have moved these points
    , headDistanceAlongTrack : Quantity Float Meters
    , totalAbsoluteDisplacement : Quantity Float Meters
    , tangentNormalBinormal : List ( Direction2d LocalCoords, Angle, Float )
    , unspentDeltaTheta : Angle
    , unspentDeltaPhi : Angle
    }
