module SceneBuilder3D exposing
    ( Index
    , IndexEntry
    , LocationContext(..)
    , TerrainFoldState
    , render3dView
    , renderGroundPlane
    , renderInactiveView
    , renderKeyPlaces
    , renderPreviews
    )

-- In V3 there is only one 3d model, used for first, third, and Plan views.
-- Profile is 2d drawing (or chart).

import Angle
import Array
import Axis3d
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, lightOrange)
import ColourPalette exposing (gradientColourPastel)
import Cone3d
import Dict exposing (Dict)
import Direction3d
import DomainModel exposing (..)
import Element
import FlatColors.AussiePalette
import FlatColors.FlatUIPalette
import FlatColors.IndianPalette
import Length exposing (Meters)
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Pixels
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d
import PreviewData exposing (PreviewData, PreviewShape(..))
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import SketchPlane3d
import SpatialIndex exposing (SpatialContent)
import Tools.DisplaySettingsOptions exposing (CurtainStyle(..))
import Tools.LandUseColours exposing (landUseColours)
import TrackLoaded exposing (TrackLoaded)
import TriangularMesh
import UtilsForViews exposing (colorFromElmUiColour, flatBox, fullDepthRenderingBoxSize)
import Vector3d


roadWidth =
    Length.meters 4.0


renderGroundPlane :
    Tools.DisplaySettingsOptions.Options
    -> Maybe (BoundingBox3d Meters LocalCoords)
    -> List (Entity LocalCoords)
renderGroundPlane settings box =
    case box of
        Just isBox ->
            let
                minZ =
                    -- Drop to prevent flicker.
                    BoundingBox3d.minZ isBox |> Quantity.minus (Length.inches 6)

                { minX, maxX, minY, maxY } =
                    BoundingBox3d.extrema <|
                        BoundingBox3d.expandBy (Length.meters 50) isBox
            in
            if settings.groundPlane && settings.terrainFineness == 0.0 then
                [ Scene3d.quad (Material.color <| colorFromElmUiColour FlatColors.IndianPalette.keppel)
                    (Point3d.xyz minX minY minZ)
                    (Point3d.xyz minX maxY minZ)
                    (Point3d.xyz maxX maxY minZ)
                    (Point3d.xyz maxX minY minZ)
                ]

            else
                []

        Nothing ->
            []


render3dView :
    Tools.DisplaySettingsOptions.Options
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
render3dView settings track =
    let
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
                (.space <| startPoint <| leafFromIndex track.currentPosition track.trackTree)

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
                        LineSegment3d.from road.startPoint.space road.endPoint.space

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
                    road.startPoint.space
                    road.endPoint.space

             else
                [ Scene3d.point { radius = Pixels.pixels 2 }
                    (Material.color Color.black)
                    road.endPoint.space
                , Scene3d.lineSegment (Material.color Color.lightCharcoal) <|
                    LineSegment3d.from road.startPoint.space road.endPoint.space
                ]
            )
                ++ gradientCurtain road
                ++ (if settings.centreLine then
                        centreLineBetween gradientColourPastel road

                    else
                        []
                   )

        renderPointZero =
            Scene3d.point { radius = Pixels.pixels 2 }
                (Material.color Color.black)
                (DomainModel.getFirstLeaf track.trackTree |> .startPoint |> .space)

        foldFn : RoadSection -> List (Entity LocalCoords) -> List (Entity LocalCoords)
        foldFn road scene =
            makeVisibleSegment road ++ scene

        renderCurrentMarkers : List (Entity LocalCoords)
        renderCurrentMarkers =
            Scene3d.point { radius = Pixels.pixels 15 }
                (Material.color lightOrange)
                (.space <| earthPointFromIndex track.currentPosition track.trackTree)
                :: (case track.markerPosition of
                        Just marker ->
                            [ Scene3d.point { radius = Pixels.pixels 12 }
                                (Material.color <| Color.fromRgba <| Element.toRgb <| FlatColors.AussiePalette.blurple)
                                (.space <| earthPointFromIndex marker track.trackTree)
                            ]

                        Nothing ->
                            []
                   )

        spatialIndex =
            indexTerrain nearbySpace track.trackTree

        terrain =
            -- Using the updated spatial index should make sure terrain does not obscure land use.
            if settings.terrainFineness > 0.0 then
                spatialIndex
                    |> terrainFromIndex
                        (flatBox nearbySpace)
                        (flatBox nearbySpace)
                        NoContext
                        settings
                        (BoundingBox3d.minZ nearbySpace)

            else
                []
    in
    renderPointZero
        :: terrain
        ++ DomainModel.traverseTreeBetweenLimitsToDepth
            0
            (skipCount track.trackTree)
            depthFn
            0
            track.trackTree
            foldFn
            renderCurrentMarkers


renderInactiveView : TrackLoaded msg -> List (Entity LocalCoords)
renderInactiveView track =
    let
        nominalRenderDepth =
            clamp 1 10 <|
                round <|
                    logBase 2 (toFloat <| skipCount track.trackTree)

        depthFn : RoadSection -> Maybe Int
        depthFn _ =
            Just nominalRenderDepth

        makeVisibleSegment : RoadSection -> List (Entity LocalCoords)
        makeVisibleSegment road =
            [ Scene3d.point { radius = Pixels.pixels 2 }
                (Material.color Color.black)
                road.endPoint.space
            , Scene3d.lineSegment (Material.color Color.lightCharcoal) <|
                LineSegment3d.from road.startPoint.space road.endPoint.space
            ]

        renderPointZero =
            Scene3d.point { radius = Pixels.pixels 2 }
                (Material.color Color.black)
                (DomainModel.getFirstLeaf track.trackTree |> .startPoint |> .space)

        foldFn : RoadSection -> List (Entity LocalCoords) -> List (Entity LocalCoords)
        foldFn road scene =
            makeVisibleSegment road ++ scene
    in
    renderPointZero
        :: DomainModel.traverseTreeBetweenLimitsToDepth
            0
            (skipCount track.trackTree)
            depthFn
            0
            track.trackTree
            foldFn
            []


type alias LandUseStuff =
    { scenes : List (Entity LocalCoords)
    , unknownTags : Dict String String
    , updatedIndex : Index
    }


renderPreviews : Tools.DisplaySettingsOptions.Options -> Dict String PreviewData -> List (Entity LocalCoords)
renderPreviews options previews =
    let
        onePreview : PreviewData -> List (Entity LocalCoords)
        onePreview { tag, shape, colour, points } =
            case shape of
                PreviewCircle ->
                    previewAsPoints colour options <| List.map .earthPoint points

                PreviewLine ->
                    previewAsLine colour <| List.map .earthPoint points

                PreviewToolSupplied callback ->
                    -- This may be breaking one of those Elmish rules.
                    callback

                PreviewProfile _ ->
                    -- We don't (yet) do these in 3d.
                    []
    in
    previews |> Dict.values |> List.concatMap onePreview


renderKeyPlaces : Tools.DisplaySettingsOptions.Options -> List EarthPoint -> List (Entity LocalCoords)
renderKeyPlaces options places =
    previewAsPoints FlatColors.FlatUIPalette.wisteria options places


previewAsLine : Element.Color -> List EarthPoint -> List (Entity LocalCoords)
previewAsLine color points =
    let
        material =
            Material.matte <| Color.fromRgba <| Element.toRgb color

        preview p1 p2 =
            paintSomethingBetween
                (Length.meters 0.5)
                material
                p1.space
                p2.space
    in
    List.map2 preview points (List.drop 1 points) |> List.concat


previewAsPoints :
    Element.Color
    -> Tools.DisplaySettingsOptions.Options
    -> List EarthPoint
    -> List (Entity LocalCoords)
previewAsPoints color options points =
    let
        material =
            Material.color <| Color.fromRgba <| Element.toRgb color

        highlightPoint p =
            Scene3d.point
                { radius = Pixels.pixels <| toFloat options.previewSize }
                material
                p.space
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
        (smallUpshiftTo road.startPoint.space)
        (smallUpshiftTo road.endPoint.space)



-- These types only for the spatial index we use to paint the terrain.


type alias IndexEntry =
    { leafIndex : Int
    , altitude : Quantity Float Meters
    }


type alias Index =
    SpatialIndex.SpatialNode IndexEntry Length.Meters LocalCoords


indexTerrain :
    BoundingBox3d Length.Meters LocalCoords
    -> PeteTree
    -> Index
indexTerrain box trackTree =
    let
        emptyIndex =
            -- The last parameter here is not the quality, it
            -- only affects the index efficiency.
            SpatialIndex.empty (flatBox box) (Length.meters 100.0)

        indexRoadSection : RoadSection -> ( Int, Index ) -> ( Int, Index )
        indexRoadSection road ( leafIndex, spaceIndex ) =
            let
                halfWidthVector =
                    Vector3d.from road.startPoint.space road.endPoint.space
                        |> Vector3d.projectOnto Plane3d.xy
                        |> Vector3d.scaleTo roadWidth

                ( leftKerbVector, rightKerbVector ) =
                    ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidthVector
                    , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidthVector
                    )

                ( leftNearKerb, rightNearKerb ) =
                    ( Point3d.translateBy leftKerbVector road.startPoint.space
                    , Point3d.translateBy rightKerbVector road.startPoint.space
                    )

                ( leftFarKerb, rightFarKerb ) =
                    ( Point3d.translateBy leftKerbVector road.endPoint.space
                    , Point3d.translateBy rightKerbVector road.endPoint.space
                    )

                localBounds =
                    BoundingBox3d.hull leftNearKerb [ leftFarKerb, rightFarKerb, rightNearKerb ]
            in
            ( leafIndex + 1
            , SpatialIndex.add
                { content =
                    { leafIndex = leafIndex
                    , altitude = BoundingBox3d.minZ localBounds
                    }
                , box = flatBox localBounds
                }
                spaceIndex
            )

        ( _, populatedIndex ) =
            DomainModel.foldOverRoute indexRoadSection trackTree ( 0, emptyIndex )
    in
    populatedIndex


type LocationContext
    = NoContext
    | NW
    | NE
    | SE
    | SW


type alias TerrainFoldState =
    { minAltitude : Quantity Float Meters
    , resultBox : Maybe (BoundingBox2d Meters LocalCoords)
    , count : Int
    }


queryAltitudeFromIndex : Index -> BoundingBox2d Meters LocalCoords -> TerrainFoldState
queryAltitudeFromIndex index myBox =
    let
        initialFoldState : TerrainFoldState
        initialFoldState =
            { minAltitude = Quantity.positiveInfinity
            , resultBox = Nothing
            , count = 0
            }

        queryFoldFunction :
            SpatialContent IndexEntry Meters LocalCoords
            -> TerrainFoldState
            -> TerrainFoldState
        queryFoldFunction entry accum =
            { minAltitude = Quantity.min accum.minAltitude entry.content.altitude
            , resultBox =
                case accum.resultBox of
                    Just oldBox ->
                        Just <| BoundingBox2d.union oldBox entry.box

                    Nothing ->
                        Just <| entry.box
            , count = accum.count + 1
            }
    in
    SpatialIndex.queryWithFold index myBox queryFoldFunction initialFoldState


terrainFromIndex :
    BoundingBox2d.BoundingBox2d Length.Meters LocalCoords
    -> BoundingBox2d.BoundingBox2d Length.Meters LocalCoords
    -> LocationContext
    -> Tools.DisplaySettingsOptions.Options
    -> Quantity Float Meters
    -> Index
    -> List (Entity LocalCoords)
terrainFromIndex myBox enclosingBox orientation options baseElevation index =
    -- I played with the idea of pushing some of this work into the SpatialIndex but
    -- concluded (rightly, I think) that it would be wrong, other than using the new queryWithFold
    -- function to bring back both the minimum altitude and the aggregate content bounding box
    -- for each query. That's a marginal saving but maybe easier to comprehend.
    -- (Turned out to be 7x speed uplift, so not exactly marginal.)
    let
        centre =
            BoundingBox2d.centerPoint myBox

        { minAltitude, resultBox, count } =
            queryAltitudeFromIndex index myBox

        topBeforeAdjustment =
            if minAltitude |> Quantity.greaterThanOrEqualTo Quantity.positiveInfinity then
                baseElevation

            else
                minAltitude

        top =
            -- Just avoid interference with road surface.
            topBeforeAdjustment |> Quantity.minus Length.foot

        myExtrema =
            BoundingBox2d.extrema myBox

        contentBox =
            -- This box encloses our points. It defines the top of the frustrum and the base for the next level.
            case resultBox of
                Just box ->
                    box
                        |> BoundingBox2d.intersection myBox
                        |> Maybe.withDefault myBox

                Nothing ->
                    myBox

        { nwChildBox, neChildBox, swChildBox, seChildBox } =
            { nwChildBox =
                BoundingBox2d.from centre (Point2d.xy myExtrema.minX myExtrema.maxY)
                    |> BoundingBox2d.intersection contentBox
                    |> Maybe.withDefault contentBox
            , neChildBox =
                BoundingBox2d.from centre (Point2d.xy myExtrema.maxX myExtrema.maxY)
                    |> BoundingBox2d.intersection contentBox
                    |> Maybe.withDefault contentBox
            , swChildBox =
                BoundingBox2d.from centre (Point2d.xy myExtrema.minX myExtrema.minY)
                    |> BoundingBox2d.intersection contentBox
                    |> Maybe.withDefault contentBox
            , seChildBox =
                BoundingBox2d.from centre (Point2d.xy myExtrema.maxX myExtrema.minY)
                    |> BoundingBox2d.intersection contentBox
                    |> Maybe.withDefault contentBox
            }

        isNotTiny bx =
            let
                ( width, height ) =
                    BoundingBox2d.dimensions bx

                splitSize =
                    Length.meters <| 100 / clamp 1.0 10.0 options.terrainFineness
            in
            (width |> Quantity.greaterThan splitSize)
                && (height |> Quantity.greaterThan splitSize)
                && count
                > 1

        thisLevelSceneElements =
            if top |> Quantity.greaterThan baseElevation then
                let
                    elevationIncrease =
                        topBeforeAdjustment |> Quantity.minus baseElevation

                    parentExtrema =
                        BoundingBox2d.extrema enclosingBox

                    contentExtrema =
                        BoundingBox2d.extrema contentBox

                    plateauExtrema =
                        -- How big would the top be if we sloped the sides at 45 degrees?
                        { minX = Quantity.min contentExtrema.minX (myExtrema.minX |> Quantity.plus elevationIncrease)
                        , minY = Quantity.min contentExtrema.minY (myExtrema.minY |> Quantity.plus elevationIncrease)
                        , maxX = Quantity.max contentExtrema.maxX (myExtrema.maxX |> Quantity.minus elevationIncrease)
                        , maxY = Quantity.max contentExtrema.maxY (myExtrema.maxY |> Quantity.minus elevationIncrease)
                        }

                    topColour =
                        terrainColourFromHeight <| Length.inMeters top

                    sideColour =
                        terrainColourFromHeight <| 0.5 * (Length.inMeters top + Length.inMeters baseElevation)

                    northernBottomEdge =
                        if orientation == NW || orientation == NE then
                            parentExtrema.maxY

                        else
                            myExtrema.maxY

                    southernBottomEdge =
                        if orientation == SW || orientation == SE then
                            parentExtrema.minY

                        else
                            myExtrema.minY

                    easternBottomEdge =
                        if orientation == NE || orientation == SE then
                            parentExtrema.maxX

                        else
                            myExtrema.maxX

                    westernBottomEdge =
                        if orientation == NW || orientation == SW then
                            parentExtrema.minX

                        else
                            myExtrema.minX

                    northernSlope =
                        -- Better to write this slowly. Use inner minX, maxX at top
                        Scene3d.quad (Material.matte sideColour)
                            (Point3d.xyz plateauExtrema.minX plateauExtrema.maxY top)
                            (Point3d.xyz plateauExtrema.maxX plateauExtrema.maxY top)
                            (Point3d.xyz easternBottomEdge northernBottomEdge baseElevation)
                            (Point3d.xyz westernBottomEdge northernBottomEdge baseElevation)

                    southernSlope =
                        -- Better to write this slowly. Use inner minX, maxX at top
                        Scene3d.quad (Material.matte sideColour)
                            (Point3d.xyz plateauExtrema.minX plateauExtrema.minY top)
                            (Point3d.xyz plateauExtrema.maxX plateauExtrema.minY top)
                            (Point3d.xyz easternBottomEdge southernBottomEdge baseElevation)
                            (Point3d.xyz westernBottomEdge southernBottomEdge baseElevation)

                    westernSlope =
                        -- Better to write this slowly. Use inner minX, maxX at top
                        Scene3d.quad (Material.matte sideColour)
                            (Point3d.xyz plateauExtrema.minX plateauExtrema.minY top)
                            (Point3d.xyz plateauExtrema.minX plateauExtrema.maxY top)
                            (Point3d.xyz westernBottomEdge northernBottomEdge baseElevation)
                            (Point3d.xyz westernBottomEdge southernBottomEdge baseElevation)

                    easternSlope =
                        -- Better to write this slowly. Use inner minX, maxX at top
                        Scene3d.quad (Material.matte sideColour)
                            (Point3d.xyz plateauExtrema.maxX plateauExtrema.minY top)
                            (Point3d.xyz plateauExtrema.maxX plateauExtrema.maxY top)
                            (Point3d.xyz easternBottomEdge northernBottomEdge baseElevation)
                            (Point3d.xyz easternBottomEdge southernBottomEdge baseElevation)
                in
                [ Scene3d.quad (Material.matte topColour)
                    (Point3d.xyz plateauExtrema.maxX plateauExtrema.maxY top)
                    (Point3d.xyz plateauExtrema.maxX plateauExtrema.minY top)
                    (Point3d.xyz plateauExtrema.minX plateauExtrema.minY top)
                    (Point3d.xyz plateauExtrema.minX plateauExtrema.maxY top)
                , northernSlope
                , southernSlope
                , westernSlope
                , easternSlope
                ]

            else
                []
    in
    thisLevelSceneElements
        ++ -- No point recursing if one element only.
           (if isNotTiny myBox then
                List.concat
                    [ terrainFromIndex nwChildBox contentBox NW options top index
                    , terrainFromIndex neChildBox contentBox NE options top index
                    , terrainFromIndex seChildBox contentBox SE options top index
                    , terrainFromIndex swChildBox contentBox SW options top index
                    ]

            else
                []
           )


terrainColourFromHeight : Float -> Color.Color
terrainColourFromHeight height =
    let
        lightness =
            clamp 0.1 0.9 <| sqrt <| sqrt <| abs <| height / 3000.0
    in
    Color.hsl
        ((80.0 + 50.0 * sin height) / 255)
        (133 / 255)
        lightness
