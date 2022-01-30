module SceneBuilderProfile exposing (..)

-- Combines altitude and gradient. Clever.
-- TODO: Add additional planes to show result of filters.

import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, black, darkGreen, green, lightOrange)
import ColourPalette exposing (gradientHue, gradientHue2)
import DomainModel exposing (..)
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


gradientColourPastel : Float -> Color.Color
gradientColourPastel slope =
    Color.hsl (gradientHue slope) 0.6 0.7


renderBoth : TrackLoaded msg -> ( List (Entity LocalCoords), List (Entity LocalCoords) )
renderBoth track =
    let
        floorPlane =
            Plane3d.xy
                |> Plane3d.offsetBy
                    (minZ |> Quantity.minus centreZ |> Quantity.multiplyBy 5.0)

        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema <| boundingBox track.trackTree

        centreZ =
            Point3d.zCoordinate <| BoundingBox3d.centerPoint <| boundingBox track.trackTree

        highDetailBox =
            DomainModel.earthPointFromIndex track.currentPosition track.trackTree
                |> BoundingBox3d.singleton
                |> BoundingBox3d.expandBy (Length.kilometers 4)

        mediumDetailBox =
            highDetailBox
                |> BoundingBox3d.expandBy (Length.kilometers 4)

        makeAltitudeSegment : Length.Length -> RoadSection -> List (Entity LocalCoords)
        makeAltitudeSegment distance road =
            let
                profileStart =
                    -- Try exaggerating scale here and compensating in the view.
                    Point3d.xyz
                        distance
                        Quantity.zero
                        (Point3d.zCoordinate road.startPoint
                            |> Quantity.minus centreZ
                            |> Quantity.multiplyBy 5.0
                        )

                profileEnd =
                    Point3d.xyz
                        (distance |> Quantity.plus road.trueLength)
                        Quantity.zero
                        (Point3d.zCoordinate road.endPoint
                            |> Quantity.minus centreZ
                            |> Quantity.multiplyBy 5.0
                        )

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

        makeGradientSegment : Length.Length -> RoadSection -> List (Entity LocalCoords)
        makeGradientSegment distance road =
            let
                gradient =
                    clamp -30.0 30.0 <|
                        DomainModel.gradientFromNode <|
                            Leaf road

                yValue =
                    -- Exaggerate here, reduce to fit in view depending on zoom level.
                    gradient * 1000.0

                segmentStart =
                    Point3d.xyz
                        distance
                        Quantity.zero
                        (Length.meters yValue)

                segmentEnd =
                    Point3d.xyz
                        (distance |> Quantity.plus road.trueLength)
                        Quantity.zero
                        (Length.meters yValue)
            in
            [ Scene3d.point { radius = Pixels.pixels 1 }
                (Material.color black)
                segmentStart
            , Scene3d.lineSegment (Material.color black) <|
                LineSegment3d.from segmentStart segmentEnd
            ]

        foldFn :
            RoadSection
            -> ( Length.Length, List (Entity LocalCoords), List (Entity LocalCoords) )
            -> ( Length.Length, List (Entity LocalCoords), List (Entity LocalCoords) )
        foldFn road ( distance, altitude, gradient ) =
            -- Ambitiously, do gradient in the same traversal.
            ( distance |> Quantity.plus road.trueLength
            , makeAltitudeSegment distance road ++ altitude
            , makeGradientSegment distance road ++ gradient
            )

        depthFn road =
            -- Try fading of detail in rendering.
            if road.boundingBox |> BoundingBox3d.intersects highDetailBox then
                Nothing

            else if road.boundingBox |> BoundingBox3d.intersects mediumDetailBox then
                Just 15

            else
                Just 10

        ( _, altitudeScene, gradientScene ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                0
                (skipCount track.trackTree)
                depthFn
                0
                track.trackTree
                foldFn
                ( Quantity.zero, [], [] )

        currentDistance =
            distanceFromIndex track.currentPosition track.trackTree

        currentPosLine =
            Scene3d.lineSegment (Material.color lightOrange) <|
                LineSegment3d.from
                    (Point3d.xyz currentDistance Quantity.zero (Length.kilometers -1))
                    (Point3d.xyz currentDistance Quantity.zero (Length.kilometers 3))
    in
    ( currentPosLine :: altitudeScene
    , currentPosLine :: gradientScene
    )
