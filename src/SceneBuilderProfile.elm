module SceneProfile exposing (..)

-- Combines altitude and gradient. Clever.
-- TODO: Add additional planes to show result of filters.

import Actions exposing (PreviewData, PreviewShape(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, black, darkGreen, green, lightOrange)
import ColourPalette exposing (gradientHue, gradientHue2)
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (..)
import Element
import FlatColors.AussiePalette
import Json.Encode as E
import Length exposing (Meters)
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Pixels
import Plane3d exposing (Plane3d)
import Point3d
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (fullDepthRenderingBoxSize)
import Vector3d


gradientColourPastel : Float -> Color.Color
gradientColourPastel slope =
    Color.hsl (gradientHue slope) 0.6 0.7


render : TrackLoaded msg -> List (Entity LocalCoords)
render track =
    --TODO: Use new traversal to provide better depth function.
    --Write this for Profile, don't mash the 3d one.
    []
