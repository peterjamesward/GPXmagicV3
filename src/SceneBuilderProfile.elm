module SceneBuilderProfile exposing (..)

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
    let
        floorPlane =
            Plane3d.xy
                |> Plane3d.offsetBy
                    (BoundingBox3d.minZ <| boundingBox track.trackTree)

        highDetailBox =
            DomainModel.earthPointFromIndex track.currentPosition track.trackTree
                |> BoundingBox3d.singleton
                |> BoundingBox3d.expandBy (Length.kilometers 4)

        mediumDetailBox =
            highDetailBox
                |> BoundingBox3d.expandBy (Length.kilometers 4)

        makeVisibleSegment : Length.Length -> RoadSection -> List (Entity LocalCoords)
        makeVisibleSegment distance road =
            let
                profileStart =
                    Point3d.xyz
                        distance
                        Quantity.zero
                        (Point3d.zCoordinate road.startPoint)

                profileEnd =
                    Point3d.xyz
                        (distance |> Quantity.plus road.trueLength)
                        Quantity.zero
                        (Point3d.zCoordinate road.endPoint)

                gradient =
                    DomainModel.gradientFromNode <| Leaf road

                roadAsSegment =
                    LineSegment3d.from profileStart profileEnd

                curtainHem =
                    LineSegment3d.projectOnto floorPlane roadAsSegment
            in
            [ Scene3d.point { radius = Pixels.pixels 1 }
                (Material.color black)
                profileStart
            , Scene3d.lineSegment (Material.color black) <|
                LineSegment3d.from profileStart profileEnd
            , Scene3d.quad (Material.color <| gradientColourPastel gradient)
                (LineSegment3d.startPoint roadAsSegment)
                (LineSegment3d.endPoint roadAsSegment)
                (LineSegment3d.endPoint curtainHem)
                (LineSegment3d.startPoint curtainHem)
            ]

        foldFn :
            RoadSection
            -> ( Length.Length, List (Entity LocalCoords) )
            -> ( Length.Length, List (Entity LocalCoords) )
        foldFn road ( distance, collectedEntities ) =
            ( distance |> Quantity.plus road.trueLength
            , makeVisibleSegment distance road ++ collectedEntities
            )

        depthFn road =
            -- Try fading of detail in rendering.
            if road.boundingBox |> BoundingBox3d.intersects highDetailBox then
                Nothing

            else if road.boundingBox |> BoundingBox3d.intersects mediumDetailBox then
                Just 15

            else
                Just 10

        ( _, entities ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount track.trackTree)
                depthFn
                0
                track.trackTree
                foldFn
                ( Quantity.zero, [] )
    in
    entities
