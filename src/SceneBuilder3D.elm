module SceneBuilder3D exposing (..)

-- In V3 there is only one 3d model, used for first, third, and Plan views.
-- Profile is 2d drawing (or chart).

import Actions exposing (PreviewData, PreviewShape(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox2d
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, black, darkGreen, green, lightOrange)
import ColourPalette exposing (gradientHue, gradientHue2)
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (..)
import Element
import FlatColors.AussiePalette
import FlatColors.FlatUIPalette
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
import Tools.DisplaySettingsOptions exposing (CurtainStyle(..))
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (flatBox, fullDepthRenderingBoxSize)
import Vector3d


gradientColourPastel : Float -> Color.Color
gradientColourPastel slope =
    Color.hsl (gradientHue slope) 0.6 0.7


render3dView : Tools.DisplaySettingsOptions.Options -> TrackLoaded msg -> List (Entity LocalCoords)
render3dView settings track =
    --TODO: Use new traversal to provide better depth function.
    let
        roadWidth =
            Length.meters 4.0

        nominalRenderDepth =
            clamp 1 10 <|
                round <|
                    logBase 2 (toFloat <| skipCount track.trackTree)

        floorPlane =
            Plane3d.xy |> Plane3d.offsetBy (BoundingBox3d.minZ <| boundingBox track.trackTree)

        nearbySpace =
            BoundingBox3d.expandBy Length.kilometer <| boundingBox track.trackTree

        fullRenderingZone =
            BoundingBox3d.withDimensions
                ( fullDepthRenderingBoxSize
                , fullDepthRenderingBoxSize
                , fullDepthRenderingBoxSize
                )
                (startPoint <| leafFromIndex track.currentPosition track.trackTree)

        groundPlane =
            let
                { minX, maxX, minY, maxY, minZ, maxZ } =
                    BoundingBox3d.extrema nearbySpace

                modelMinZ =
                    BoundingBox3d.minZ <| boundingBox track.trackTree
            in
            if settings.groundPlane then
                [ Scene3d.quad (Material.color Color.darkGreen)
                    (Point3d.xyz minX minY modelMinZ)
                    (Point3d.xyz minX maxY modelMinZ)
                    (Point3d.xyz maxX maxY modelMinZ)
                    (Point3d.xyz maxX minY modelMinZ)
                ]

            else
                []

        depthFn : RoadSection -> Maybe Int
        depthFn road =
            if road.boundingBox |> BoundingBox3d.intersects fullRenderingZone then
                Nothing

            else
                Just nominalRenderDepth

        gradientCurtain : RoadSection -> List (Entity LocalCoords)
        gradientCurtain road =
            if settings.curtainStyle == NoCurtain then
                []

            else
                let
                    colourFn =
                        if settings.curtainStyle == PastelCurtain then
                            gradientColourPastel

                        else
                            always <|
                                Color.fromRgba <|
                                    Element.toRgb
                                        FlatColors.FlatUIPalette.greenSea

                    gradient =
                        road.gradientAtStart

                    roadAsSegment =
                        LineSegment3d.from road.startPoint road.endPoint

                    curtainHem =
                        LineSegment3d.projectOnto floorPlane roadAsSegment
                in
                [ Scene3d.quad (Material.color <| colourFn gradient)
                    (LineSegment3d.startPoint roadAsSegment)
                    (LineSegment3d.endPoint roadAsSegment)
                    (LineSegment3d.endPoint curtainHem)
                    (LineSegment3d.startPoint curtainHem)
                ]

        makeVisibleSegment : RoadSection -> List (Entity LocalCoords)
        makeVisibleSegment road =
            (if settings.roadSurface then
                paintSomethingBetween
                    roadWidth
                    (Material.matte Color.grey)
                    road.startPoint
                    road.endPoint

             else
                [ Scene3d.point { radius = Pixels.pixels 1 }
                    (Material.color black)
                    road.startPoint
                , Scene3d.lineSegment (Material.color black) <|
                    LineSegment3d.from road.startPoint road.endPoint
                ]
            )
                ++ gradientCurtain road
                ++ (if settings.centreLine then
                        centreLineBetween gradientColourPastel road

                    else
                        []
                   )

        foldFn : RoadSection -> List (Entity LocalCoords) -> List (Entity LocalCoords)
        foldFn road scene =
            makeVisibleSegment road ++ scene

        renderCurrentMarkers : List (Entity LocalCoords)
        renderCurrentMarkers =
            [ Scene3d.point { radius = Pixels.pixels 10 }
                (Material.color lightOrange)
                (earthPointFromIndex track.currentPosition track.trackTree)
            ]
                ++ (case track.markerPosition of
                        Just marker ->
                            [ Scene3d.point { radius = Pixels.pixels 9 }
                                (Material.color <| Color.fromRgba <| Element.toRgb <| FlatColors.AussiePalette.blurple)
                                (earthPointFromIndex marker track.trackTree)
                            ]

                        Nothing ->
                            []
                   )
    in
    DomainModel.traverseTreeBetweenLimitsToDepth
        0
        (skipCount track.trackTree)
        depthFn
        0
        track.trackTree
        foldFn
        (groundPlane ++ renderCurrentMarkers)


renderPreviews : Dict String PreviewData -> List (Entity LocalCoords)
renderPreviews previews =
    let
        onePreview : PreviewData -> List (Entity LocalCoords)
        onePreview { tag, shape, colour, points } =
            case shape of
                PreviewCircle ->
                    previewAsPoints colour <| List.map Tuple.first points

                PreviewLine ->
                    previewAsLine colour <| List.map Tuple.first points

                PreviewToolSupplied callback ->
                    -- This may be breaking one of those Elmish rules.
                    callback
    in
    previews |> Dict.values |> List.concatMap onePreview


previewAsLine : Element.Color -> List EarthPoint -> List (Entity LocalCoords)
previewAsLine color points =
    let
        material =
            Material.matte <| Color.fromRgba <| Element.toRgb color

        preview p1 p2 =
            paintSomethingBetween
                (Length.meters 0.5)
                material
                p1
                p2
    in
    List.map2 preview points (List.drop 1 points) |> List.concat


previewAsPoints : Element.Color -> List EarthPoint -> List (Entity LocalCoords)
previewAsPoints color points =
    let
        material =
            Material.color <| Color.fromRgba <| Element.toRgb color

        highlightPoint p =
            Scene3d.point { radius = Pixels.pixels 7 } material p
    in
    List.map highlightPoint points


paintSomethingBetween width material pt1 pt2 =
    let
        roadAsSegment =
            LineSegment3d.from pt1 pt2

        halfWidth =
            Vector3d.from pt1 pt2
                |> Vector3d.projectOnto Plane3d.xy
                |> Vector3d.scaleTo width

        ( leftKerbVector, rightKerbVector ) =
            ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidth
            , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidth
            )

        ( leftKerb, rightKerb ) =
            ( LineSegment3d.translateBy leftKerbVector roadAsSegment
            , LineSegment3d.translateBy rightKerbVector roadAsSegment
            )
    in
    [ Scene3d.quad material
        (LineSegment3d.startPoint leftKerb)
        (LineSegment3d.endPoint leftKerb)
        (LineSegment3d.endPoint rightKerb)
        (LineSegment3d.startPoint rightKerb)
    ]


centreLineBetween : (Float -> Color) -> RoadSection -> List (Entity LocalCoords)
centreLineBetween colouringFn road =
    let
        gradient =
            road.gradientAtStart

        smallUpshiftTo pt =
            -- To make line stand slightly proud of the road
            pt |> Point3d.translateBy (Vector3d.meters 0.0 0.0 0.005)
    in
    paintSomethingBetween
        (Length.meters 0.5)
        (Material.color <| colouringFn gradient)
        (smallUpshiftTo road.startPoint)
        (smallUpshiftTo road.endPoint)
