module SceneBuilder3D exposing (..)

-- In V3 there is only one 3d model, used for first, third, and Plan views.
-- Profile is 2d drawing (or chart).

import Angle exposing (Angle)
import Axis3d
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color, black, darkGreen, green, lightOrange)
import ColourPalette exposing (gradientColourPastel, gradientHue, gradientHue2)
import Cone3d
import Dict exposing (Dict)
import Direction3d
import DomainModel exposing (..)
import Element
import FlatColors.AmericanPalette
import FlatColors.AussiePalette
import FlatColors.CanadianPalette
import FlatColors.FlatUIPalette
import LandUseDataOSM
import LandUseDataTypes
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
import TrackLoaded exposing (TrackLoaded)
import TriangularMesh
import Utils exposing (reversingCons)
import UtilsForViews exposing (colorFromElmUiColour, flatBox, fullDepthRenderingBoxSize)
import Vector3d


roadWidth =
    Length.meters 4.0


render3dView :
    Tools.DisplaySettingsOptions.Options
    -> TrackLoaded msg
    -> LandUseDataTypes.LandUseData
    -> List (Entity LocalCoords)
render3dView settings track landUse =
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
                (startPoint <| leafFromIndex track.currentPosition track.trackTree)

        groundPlane =
            let
                { minX, maxX, minY, maxY, minZ, maxZ } =
                    BoundingBox3d.extrema nearbySpace

                modelMinZ =
                    -- Drop an inch to prevent flicker.
                    boundingBox track.trackTree
                        |> BoundingBox3d.minZ
                        |> Quantity.minus (Length.inches 6)
            in
            if settings.groundPlane && settings.terrainFineness == 0.0 then
                [ Scene3d.quad (Material.color Color.grey)
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
                [ Scene3d.point { radius = Pixels.pixels 2 }
                    (Material.color Color.black)
                    road.startPoint
                , Scene3d.lineSegment (Material.color Color.lightCharcoal) <|
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
            [ Scene3d.point { radius = Pixels.pixels 15 }
                (Material.color lightOrange)
                (earthPointFromIndex track.currentPosition track.trackTree)
            ]
                ++ (case track.markerPosition of
                        Just marker ->
                            [ Scene3d.point { radius = Pixels.pixels 12 }
                                (Material.color <| Color.fromRgba <| Element.toRgb <| FlatColors.AussiePalette.blurple)
                                (earthPointFromIndex marker track.trackTree)
                            ]

                        Nothing ->
                            []
                   )

        spatialIndex =
            indexTerrain nearbySpace track.trackTree

        ( landUseElements, indexWithLandUse ) =
            if settings.landUse then
                makeLandUse landUse spatialIndex track.trackTree floorPlane

            else
                ( [], spatialIndex )

        terrain =
            if settings.terrainFineness > 0.0 then
                indexWithLandUse
                    |> terrainFromIndex
                        (flatBox nearbySpace)
                        (flatBox nearbySpace)
                        NoContext
                        settings
                        (BoundingBox3d.minZ nearbySpace)

            else
                []
    in
    terrain
        ++ landUseElements
        ++ DomainModel.traverseTreeBetweenLimitsToDepth
            0
            (skipCount track.trackTree)
            depthFn
            0
            track.trackTree
            foldFn
            (groundPlane ++ renderCurrentMarkers)


type alias LandUseStuff =
    { scenes : List (Entity LocalCoords)
    , unknownTags : Dict String String
    , updatedIndex : Index
    }


nodeColourMap =
    Dict.fromList


landUseColours =
    Dict.fromList
        [ ( "tree", Color.darkGreen )
        , ( "rock", Color.lightBrown )
        , ( "peak", Color.white )
        , ( "water", Color.lightBlue )
        , ( "wood", Color.darkGreen )
        , ( "recreation_ground", Color.lightGreen )
        , ( "grass", Color.lightGreen )
        , ( "meadow", Color.lightYellow )
        , ( "farmland", colorFromElmUiColour FlatColors.AmericanPalette.lightGreenishBlue )
        , ( "grassland", colorFromElmUiColour FlatColors.AmericanPalette.mintLeaf )
        , ( "forest", Color.darkGreen )
        , ( "industrial", Color.darkGray )
        , ( "residential", colorFromElmUiColour FlatColors.AmericanPalette.firstDate )
        , ( "retail", colorFromElmUiColour FlatColors.FlatUIPalette.carrot )
        , ( "railway", colorFromElmUiColour FlatColors.FlatUIPalette.silver )
        , ( "brownfield", Color.brown )
        ]


makeLandUse :
    LandUseDataTypes.LandUseData
    -> Index
    -> PeteTree
    -> Plane3d Meters LocalCoords
    -> ( List (Entity LocalCoords), Index )
makeLandUse landUse index tree groundPlane =
    --TODO: Collect the bounding boxes and altitudes as we go, then add these to
    --the Index, returning the new Index as well, which we then use to render Terrain!
    --Means changing the fold state to include index but well worthwhile.
    --Will have to pretend that there are roads, or change the index entry.
    let
        drawCone :
            Color
            -> EarthPoint
            -> Maybe ( Entity LocalCoords, SpatialContent IndexEntry Meters LocalCoords )
        drawCone colour at =
            let
                tip =
                    Point3d.translateBy
                        (Vector3d.withLength (Length.meters 10) Direction3d.positiveZ)
                        at
            in
            case Cone3d.from at tip (Length.meters 4) of
                Just cone ->
                    Just
                        ( Scene3d.cone (Material.color colour) cone
                        , { content = { altitude = Point3d.zCoordinate at }
                          , box =
                                BoundingBox2d.withDimensions
                                    ( Length.meters 8, Length.meters 8 )
                                    (at |> Point3d.projectInto SketchPlane3d.xy)
                          }
                        )

                Nothing ->
                    Nothing

        xyNodeIndex : Dict ( Float, Float ) (Quantity Float Meters)
        xyNodeIndex =
            --Ghastly hack
            landUse.nodes
                |> List.map
                    (\node ->
                        ( ( Length.inMeters <| Point3d.xCoordinate node.at
                          , Length.inMeters <| Point3d.yCoordinate node.at
                          )
                        , Point3d.zCoordinate node.at
                        )
                    )
                |> Dict.fromList

        drawPolygon :
            Color
            -> List LandUseDataTypes.LandUseNode
            -> ( Entity LocalCoords, SpatialContent IndexEntry Meters LocalCoords )
        drawPolygon colour nodes =
            -- Must draw a 2D polygon so we can triangulate it.
            -- Ian MacKenzie makes this a pleasure.
            let
                polygon =
                    nodes
                        |> List.map (.at >> Point3d.projectInto SketchPlane3d.xy)
                        |> Polygon2d.singleLoop

                restoreAltitude : Point2d Meters LocalCoords -> Point3d Meters LocalCoords
                restoreAltitude point =
                    case
                        Dict.get
                            ( Length.inMeters <| Point2d.xCoordinate point
                            , Length.inMeters <| Point2d.yCoordinate point
                            )
                            xyNodeIndex
                    of
                        Just altitude ->
                            Point3d.xyz
                                (Point2d.xCoordinate point)
                                (Point2d.yCoordinate point)
                                altitude

                        Nothing ->
                            point |> Point3d.on SketchPlane3d.xy

                mesh =
                    polygon
                        |> Polygon2d.triangulate
                        |> TriangularMesh.mapVertices restoreAltitude
            in
            ( Scene3d.mesh (Material.color colour) (Mesh.indexedTriangles mesh)
            , { content = { altitude = Quantity.zero }
              , box =
                    Polygon2d.boundingBox polygon
                        |> Maybe.withDefault (BoundingBox2d.singleton Point2d.origin)
              }
            )

        drawNode : LandUseDataTypes.LandUseNode -> LandUseStuff -> LandUseStuff
        drawNode node stuff =
            case node.tags of
                Nothing ->
                    stuff

                Just tags ->
                    case Dict.get "natural" tags of
                        Nothing ->
                            stuff

                        Just natural ->
                            case Dict.get natural landUseColours of
                                Just colour ->
                                    -- Careful, the drawing could (compiler-wise) fail.
                                    case drawCone colour node.at of
                                        Just ( gotCone, indexEntry ) ->
                                            { scenes = gotCone :: stuff.scenes
                                            , unknownTags = stuff.unknownTags
                                            , updatedIndex = SpatialIndex.add indexEntry stuff.updatedIndex
                                            }

                                        Nothing ->
                                            stuff

                                Nothing ->
                                    { stuff
                                        | unknownTags = Dict.insert "natural" natural stuff.unknownTags
                                    }

        drawWay : LandUseDataTypes.LandUseWay -> LandUseStuff -> LandUseStuff
        drawWay way stuff =
            case way.tags of
                Nothing ->
                    stuff

                Just tags ->
                    let
                        useTag =
                            Maybe.withDefault "" <|
                                case
                                    ( Dict.get "natural" tags
                                    , Dict.get "landuse" tags
                                    )
                                of
                                    ( _, Just usage ) ->
                                        Just usage

                                    ( Just natural, Nothing ) ->
                                        Just natural

                                    _ ->
                                        Nothing
                    in
                    case Dict.get useTag landUseColours of
                        Just colour ->
                            let
                                ( polygon, indexEntry ) =
                                    drawPolygon colour way.nodes
                            in
                            { scenes = polygon :: stuff.scenes
                            , unknownTags = stuff.unknownTags
                            , updatedIndex = SpatialIndex.add indexEntry stuff.updatedIndex
                            }

                        Nothing ->
                            stuff

        nodeStuff =
            landUse.nodes
                |> List.foldl drawNode
                    { scenes = [], unknownTags = Dict.empty, updatedIndex = index }

        wayStuff =
            landUse.ways
                |> List.foldl drawWay nodeStuff

        --_ =
        --    Debug.log "NODES" nodeStuff.unknownTags
        --
        --_ =
        --    Debug.log "WAYS" wayStuff.unknownTags
    in
    ( wayStuff.scenes, wayStuff.updatedIndex )


renderPreviews : Dict String PreviewData -> List (Entity LocalCoords)
renderPreviews previews =
    let
        onePreview : PreviewData -> List (Entity LocalCoords)
        onePreview { tag, shape, colour, points } =
            case shape of
                PreviewCircle ->
                    previewAsPoints colour <| List.map .earthPoint points

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



-- These types only for the spatial index we use to paint the terrain.


type alias IndexEntry =
    { altitude : Quantity Float Meters }


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

        indexRoadSection : RoadSection -> Index -> Index
        indexRoadSection road index =
            let
                halfWidthVector =
                    Vector3d.from road.startPoint road.endPoint
                        |> Vector3d.projectOnto Plane3d.xy
                        |> Vector3d.scaleTo roadWidth

                ( leftKerbVector, rightKerbVector ) =
                    ( Vector3d.rotateAround Axis3d.z (Angle.degrees 90) halfWidthVector
                    , Vector3d.rotateAround Axis3d.z (Angle.degrees -90) halfWidthVector
                    )

                ( leftNearKerb, rightNearKerb ) =
                    ( Point3d.translateBy leftKerbVector road.startPoint
                    , Point3d.translateBy rightKerbVector road.startPoint
                    )

                ( leftFarKerb, rightFarKerb ) =
                    ( Point3d.translateBy leftKerbVector road.endPoint
                    , Point3d.translateBy rightKerbVector road.endPoint
                    )

                localBounds =
                    BoundingBox3d.hull leftNearKerb [ leftFarKerb, rightFarKerb, rightNearKerb ]
            in
            SpatialIndex.add
                { content = { altitude = BoundingBox3d.minZ localBounds }
                , box = flatBox localBounds
                }
                index
    in
    DomainModel.foldOverRoute indexRoadSection trackTree emptyIndex


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

        elevationIncrease =
            topBeforeAdjustment |> Quantity.minus baseElevation

        myExtrema =
            BoundingBox2d.extrema myBox

        parentExtrema =
            BoundingBox2d.extrema enclosingBox

        contentBox =
            -- This box encloses our points. It defines the top of the frustrum and the base for the next level.
            case resultBox of
                Just box ->
                    box
                        |> BoundingBox2d.intersection myBox
                        |> Maybe.withDefault myBox

                Nothing ->
                    myBox

        contentExtrema =
            BoundingBox2d.extrema contentBox

        plateauExtrema =
            -- How big would the top be if we sloped the sides at 45 degrees?
            { minX = Quantity.min contentExtrema.minX (myExtrema.minX |> Quantity.plus elevationIncrease)
            , minY = Quantity.min contentExtrema.minY (myExtrema.minY |> Quantity.plus elevationIncrease)
            , maxX = Quantity.max contentExtrema.maxX (myExtrema.maxX |> Quantity.minus elevationIncrease)
            , maxY = Quantity.max contentExtrema.maxY (myExtrema.maxY |> Quantity.minus elevationIncrease)
            }

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

        thisLevelSceneElements =
            if top |> Quantity.greaterThan baseElevation then
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
