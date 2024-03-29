module FingerPainting exposing (..)

import Circle2d
import Direction2d
import DomainModel exposing (..)
import Drag3dCommonStructures exposing (..)
import Element exposing (Element, alignTop, centerX, el, html, inFront, none)
import Geometry.Svg as Svg
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Spherical
import Svg
import Svg.Attributes
import SystemSettings exposing (SystemSettings)
import Tools.CentroidAverage
import Tools.ProfileSmooth
import Tools.Simplify
import ToolsController
import TrackLoaded exposing (TrackLoaded)
import Utils
import View3dCommonElements exposing (Context)


fingerPaintingPreview :
    SystemSettings
    -> msg
    -> ToolsController.Options msg
    -> Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Maybe String
    -> Element msg
fingerPaintingPreview settings stopMsg tools context ( givenWidth, givenHeight ) paintTool =
    let
        ( svgWidth, svgHeight ) =
            ( String.fromInt <| Pixels.inPixels givenWidth
            , String.fromInt <| Pixels.inPixels givenHeight
            )

        circleFromProximity proximity =
            Svg.circle2d
                [ Svg.Attributes.stroke "red"
                , Svg.Attributes.strokeWidth "1"
                , Svg.Attributes.fill "white"
                ]
                (Circle2d.withRadius (Pixels.float 5) proximity.screenPoint)

        onOrOffTrack proximity =
            if proximity.distanceFrom |> Quantity.greaterThan (Length.meters 2) then
                Svg.circle2d
                    [ Svg.Attributes.stroke "blue"
                    , Svg.Attributes.strokeWidth "1"
                    , Svg.Attributes.fill "none"
                    ]
                    (Circle2d.withRadius (Pixels.float 5) proximity.screenPoint)

            else
                Svg.circle2d
                    [ Svg.Attributes.stroke "red"
                    , Svg.Attributes.strokeWidth "1"
                    , Svg.Attributes.fill "white"
                    ]
                    (Circle2d.withRadius (Pixels.float 5) proximity.screenPoint)
    in
    el
        [ centerX
        , alignTop
        , inFront <| ToolsController.viewToolForPainting settings tools paintTool stopMsg
        ]
    <|
        case context.dragAction of
            DragPaint paintInfo ->
                html <|
                    Svg.svg
                        [ Svg.Attributes.width svgWidth
                        , Svg.Attributes.height svgHeight
                        ]
                    <|
                        List.map circleFromProximity paintInfo.path

            DragTool tool startInfo endInfo ->
                --TODO: Add a line between these two points.
                html <|
                    Svg.svg
                        [ Svg.Attributes.width svgWidth
                        , Svg.Attributes.height svgHeight
                        ]
                        [ onOrOffTrack startInfo
                        , onOrOffTrack endInfo
                        ]

            _ ->
                none


applyFingerPaint : PaintInfo -> TrackLoaded msg -> TrackLoaded msg
applyFingerPaint paintInfo track =
    -- Wrapper so we can also apply post-paint smoothing.
    track
        |> applyFingerPaintInternal paintInfo
        |> Tools.ProfileSmooth.fingerpaintingHelper
        |> Tools.Simplify.fingerpaintHelper
        |> Tools.Simplify.fingerpaintHelper
        |> Tools.Simplify.fingerpaintHelper
        |> Tools.Simplify.fingerpaintHelper
        |> Tools.CentroidAverage.applyUsingOptions Tools.CentroidAverage.defaultOptions
        |> Tools.CentroidAverage.applyUsingOptions Tools.CentroidAverage.defaultOptions
        |> Tools.CentroidAverage.applyUsingOptions Tools.CentroidAverage.defaultOptions
        |> Tools.CentroidAverage.applyUsingOptions Tools.CentroidAverage.defaultOptions


applyFingerPaintInternal : PaintInfo -> TrackLoaded msg -> TrackLoaded msg
applyFingerPaintInternal paintInfo track =
    case paintInfo.path of
        pathHead :: pathMore ->
            -- At least two points makes it worthwhile.
            case List.reverse pathMore of
                pathLast :: pathMiddle ->
                    let
                        --_ =
                        --    Debug.log "applying" ( pathHead, pathLast )
                        {-
                           1. Use first and last points to work out where we splice the new track section.
                           (remember path could be drawn in either direction!)
                           (cater for case when painting is entirely in one section!)
                           2. Make new 2D points from the path.
                           3. Apply altitudes by interpolation from base track.
                           4. Splice the new section into the track.
                           5. Return with adjusted pointers and new leaf index.
                        -}
                        locationsAreInCorrectOrder =
                            -- I find the prefix notation clearer here, parentheses important.
                            (||) (pathHead.leafIndex < pathLast.leafIndex)
                                ((&&) (pathHead.leafIndex == pathLast.leafIndex)
                                    (pathHead.distanceAlong |> Quantity.lessThan pathLast.distanceAlong)
                                )

                        ( preTrackPoint, postTrackPoint, locations ) =
                            -- Unchanged points that we connect the new track to.
                            if locationsAreInCorrectOrder then
                                ( pathHead.leafIndex, pathLast.leafIndex + 2, paintInfo.path )

                            else
                                ( pathLast.leafIndex, pathHead.leafIndex + 2, List.reverse paintInfo.path )

                        within10cm : GPXSource -> GPXSource -> Bool
                        within10cm gpx1 gpx2 =
                            -- Used as deduper to remove points closer than 10cm.
                            Spherical.range
                                ( gpx1.longitude |> Direction2d.toAngle
                                , gpx1.latitude
                                )
                                ( gpx2.longitude |> Direction2d.toAngle
                                , gpx2.latitude
                                )
                                |> Length.meters
                                |> Quantity.lessThan (Length.centimeters 10)

                        newGpxPoints =
                            -- Splicing is more stable if we preserve the extremities?
                            --Utils.deDupe (==) <|
                            Utils.deDupe within10cm <|
                                gpxPointFromIndex preTrackPoint track.trackTree
                                    :: List.map makeNewGpxPointFromProximity locations
                                    ++ [ gpxPointFromIndex postTrackPoint track.trackTree ]

                        makeNewGpxPointFromProximity : PointLeafProximity -> GPXSource
                        makeNewGpxPointFromProximity proximity =
                            let
                                leaf : RoadSection
                                leaf =
                                    asRecord <| leafFromIndex proximity.leafIndex track.trackTree

                                pointOnTrack =
                                    Point3d.interpolateFrom
                                        leaf.startPoint.space
                                        leaf.endPoint.space
                                        proximity.proportionAlong

                                ( x, y, z ) =
                                    Point3d.coordinates proximity.worldPoint

                                earthPoint =
                                    { space = Point3d.xyz x y (Point3d.zCoordinate pointOnTrack)
                                    , time = Nothing
                                    }
                            in
                            gpxFromPointWithReference track.referenceLonLat earthPoint

                        newTree =
                            DomainModel.replaceRange
                                preTrackPoint
                                (skipCount track.trackTree - postTrackPoint)
                                track.referenceLonLat
                                newGpxPoints
                                track.trackTree
                    in
                    case newTree of
                        Just isTree ->
                            { track
                                | trackTree = Maybe.withDefault track.trackTree newTree
                                , currentPosition = preTrackPoint + 1
                                , markerPosition =
                                    Just <|
                                        (postTrackPoint + skipCount isTree - skipCount track.trackTree - 1)
                                , leafIndex = TrackLoaded.indexLeaves isTree
                            }

                        Nothing ->
                            track

                _ ->
                    track

        _ ->
            -- Without two points, do nothing.
            track
