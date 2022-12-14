module SceneBuilder3D exposing (Index, IndexEntry, LocationContext(..), TerrainFoldState, render3dView, renderPreviews)

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
import Tools.LandUseColours exposing (landUseColours)
import TrackLoaded exposing (TrackLoaded)
import TriangularMesh
import UtilsForViews exposing (colorFromElmUiColour, flatBox, fullDepthRenderingBoxSize)
import Vector3d


roadWidth =
    Length.meters 4.0


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

        groundPlane =
            let
                { minX, maxX, minY, maxY } =
                    BoundingBox3d.extrema nearbySpace
            in
            if settings.groundPlane && settings.terrainFineness == 0.0 then
                let
                    modelMinZ =
                        -- Drop an inch to prevent flicker.
                        boundingBox track.trackTree
                            |> BoundingBox3d.minZ
                            |> Quantity.minus (Length.inches 6)
                in
                [ Scene3d.quad (Material.color <| colorFromElmUiColour FlatColors.IndianPalette.keppel)
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

        ( landUseElements, indexWithLandUse ) =
            case settings.landUse of
                LandUseDataTypes.LandUseHidden ->
                    ( [], spatialIndex )

                LandUseDataTypes.LandUseSloped ->
                    makeLandUseSloped track.landUseData spatialIndex track.trackTree floorPlane

                LandUseDataTypes.LandUsePlanar ->
                    makeLandUsePlanar track.landUseData spatialIndex track floorPlane

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
    renderPointZero
        :: terrain
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


makeLandUsePlanar :
    LandUseDataTypes.LandUseData
    -> Index
    -> TrackLoaded msg
    -> Plane3d Meters LocalCoords
    -> ( List (Entity LocalCoords), Index )
makeLandUsePlanar landUse index track groundPlane =
    let
        drawCone :
            Color
            -> EarthPoint
            -> Maybe ( Entity LocalCoords, SpatialContent IndexEntry Meters LocalCoords )
        drawCone colour at =
            let
                nearestPoint =
                    DomainModel.earthPointFromIndex
                        (DomainModel.nearestToRay
                            (Axis3d.withDirection Direction3d.positiveZ at.space)
                            track.trackTree
                            track.leafIndex
                            track.currentPosition
                        )
                        track.trackTree

                altitudeAdjusted =
                    Point3d.xyz
                        (Point3d.xCoordinate at.space)
                        (Point3d.yCoordinate at.space)
                        (Point3d.zCoordinate nearestPoint.space)

                tip =
                    Point3d.translateBy
                        (Vector3d.withLength (Length.meters 10) Direction3d.positiveZ)
                        altitudeAdjusted
            in
            case Cone3d.from altitudeAdjusted tip (Length.meters 4) of
                Just cone ->
                    Just
                        ( Scene3d.cone (Material.color colour) cone
                        , { content = { leafIndex = -1, altitude = Point3d.zCoordinate nearestPoint.space }
                          , box =
                                BoundingBox2d.withDimensions
                                    ( Length.meters 8, Length.meters 8 )
                                    (at.space |> Point3d.projectInto SketchPlane3d.xy)
                          }
                        )

                Nothing ->
                    Nothing

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
                        |> List.map (.at >> .space >> Point3d.projectInto SketchPlane3d.xy)
                        |> Polygon2d.singleLoop

                { minAltitude } =
                    queryAltitudeFromIndex index
                        (Maybe.withDefault (BoundingBox2d.singleton Point2d.origin) <|
                            Polygon2d.boundingBox polygon
                        )

                useAltitude =
                    if minAltitude |> Quantity.lessThan Quantity.positiveInfinity then
                        minAltitude

                    else
                        Point3d.zCoordinate <| Plane3d.originPoint groundPlane

                restoreAltitude : Point2d Meters LocalCoords -> Point3d Meters LocalCoords
                restoreAltitude point =
                    Point3d.xyz
                        (Point2d.xCoordinate point)
                        (Point2d.yCoordinate point)
                        (useAltitude |> Quantity.minus Length.centimeter)

                mesh =
                    polygon
                        |> Polygon2d.triangulate
                        |> TriangularMesh.mapVertices restoreAltitude
            in
            ( Scene3d.mesh (Material.color colour) (Mesh.indexedTriangles mesh)
            , { content = { leafIndex = -1, altitude = useAltitude }
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
    in
    ( wayStuff.scenes, wayStuff.updatedIndex )


makeLandUseSloped :
    LandUseDataTypes.LandUseData
    -> Index
    -> PeteTree
    -> Plane3d Meters LocalCoords
    -> ( List (Entity LocalCoords), Index )
makeLandUseSloped landUse index tree groundPlane =
    --TODO: Appears to be unused.
    let
        drawCone :
            Color
            -> Point3d Meters LocalCoords
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
                        , { content = { leafIndex = -1, altitude = Point3d.zCoordinate at }
                          , box =
                                BoundingBox2d.withDimensions
                                    ( Length.meters 8, Length.meters 8 )
                                    (at |> Point3d.projectInto SketchPlane3d.xy)
                          }
                        )

                Nothing ->
                    Nothing

        drawPolygon :
            Color
            -> List (Point3d Meters LocalCoords)
            -> ( Entity LocalCoords, List (SpatialContent IndexEntry Meters LocalCoords) )
        drawPolygon colour nodes =
            -- TODO: See if any road(s) traverse the polygon.
            -- 1. Bounding box test, in many cases will return no leafs.
            -- 2. If returns leafs, try to find contiguous set of leaves that
            --      a) Starts outside,
            --      b) Passes inside,
            --      c) Ends outside.
            -- 3. Find the crossing points of the road at the polygon edges
            -- 4. Create two new polygons, one each side, using offset road edges.
            -- 5. Recurse with the smaller polygons in case multiple raods.
            -- Will be a bit trickier because the left and right polygon road
            -- edges take altitude from road, not from the land use.
            let
                -- If there is an entry road, we need to find which polygon edge it crosses,
                -- and where, so that we can create the new vertices along the left of the road.
                -- (We don't know yet which way round the nodes are organised.)
                {-
                   We should now know, if the road intersects the polygon, enough to construct
                   two separate polygons, one each side of the road. Polygon2d are always constructed
                   anti-clockwise but we can't rely on OSM for that.
                   This operation should start with the node left of the incoming road.
                -}
                -- Hmm. Not efficient but now we have reliably ordered nodes, we should find the
                -- "correct" places where the road crosses the boundary.
                -- Now it's decision time. If there is a transiting road, we must make two
                -- polygon and recurse. If not, we can just make a mesh for this polygon.
                -- (Now's as good a time as any to apologise for this odd coding style;
                -- I've been deferring this inevitable crux.)
                mesh3d =
                    -- Make a 3D mesh using the original polygon vertices with altitudes.
                    let
                        polygon =
                            -- Triangulation works in 2D, so we briefly lose altitude info.
                            -- But that works better for road intersection as well!
                            nodes
                                |> List.map (Point3d.projectInto SketchPlane3d.xy)
                                |> Polygon2d.singleLoop

                        mesh2d =
                            Polygon2d.triangulate polygon

                        faceIndices =
                            TriangularMesh.faceIndices mesh2d

                        vertices3d =
                            Array.fromList nodes
                    in
                    TriangularMesh.indexed vertices3d faceIndices

                vertexIndexEntry vertex =
                    { content = { leafIndex = -1, altitude = Point3d.zCoordinate vertex }
                    , box =
                        BoundingBox2d.withDimensions
                            ( Length.meters 10, Length.meters 10 )
                            (vertex |> Point3d.projectInto SketchPlane3d.xy)
                    }
            in
            if List.length nodes < 3 then
                -- Ignore trivial polygons to avoid infinite recursion.
                ( Scene3d.nothing
                , []
                )

            else
                --case ( leftSubPolygon, rightSubPolygon ) of
                --    ( [], [] ) ->
                ( Scene3d.mesh (Material.color colour) (Mesh.indexedTriangles mesh3d)
                , List.map vertexIndexEntry nodes
                )

        --( _, _ ) ->
        --    ( Scene3d.group <|
        --        List.concat <|
        --            [ previewAsLine FlatColors.RussianPalette.cornflower leftSubPolygon
        --            , previewAsLine FlatColors.RussianPalette.rosyHighlight rightSubPolygon
        --            ]
        --    , []
        --    )
        -- Recurse in case there are more road transits!
        -- It would have made more sense to do this at the `foldl` but here we are.
        --let
        --    ( leftPolygonArea, rightPolygonArea ) =
        --        -- Only recurse if area reduced.
        --        ( leftSubPolygon
        --            |> List.map (Point3d.projectInto SketchPlane3d.xy)
        --            |> Polygon2d.singleLoop
        --            |> Polygon2d.area
        --        , rightSubPolygon
        --            |> List.map (Point3d.projectInto SketchPlane3d.xy)
        --            |> Polygon2d.singleLoop
        --            |> Polygon2d.area
        --        )
        --
        --    ( leftMesh, leftIndexEntries ) =
        --        if leftPolygonArea |> Quantity.lessThan (Polygon2d.area polygon) then
        --            drawPolygon colour leftSubPolygon
        --
        --        else
        --            ( Scene3d.nothing
        --            , []
        --            )
        --
        --    ( rightMesh, rightIndexEntries ) =
        --        if rightPolygonArea |> Quantity.lessThan (Polygon2d.area polygon) then
        --            drawPolygon colour rightSubPolygon
        --
        --        else
        --            ( Scene3d.nothing
        --            , []
        --            )
        --in
        --( Scene3d.group [ leftMesh, rightMesh ]
        --, leftIndexEntries ++ rightIndexEntries
        --)
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
                                    case drawCone colour node.at.space of
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

                        addToIndex : SpatialContent IndexEntry Meters LocalCoords -> Index -> Index
                        addToIndex content indexing =
                            SpatialIndex.add content indexing
                    in
                    case Dict.get useTag landUseColours of
                        Just colour ->
                            -- Only draw polygons we know how to colour!
                            let
                                ( polygon, indexEntries ) =
                                    drawPolygon colour <| List.map (.at >> .space) way.nodes
                            in
                            { scenes = polygon :: stuff.scenes
                            , unknownTags = stuff.unknownTags
                            , updatedIndex = List.foldl addToIndex stuff.updatedIndex indexEntries
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
                p1.space
                p2.space
    in
    List.map2 preview points (List.drop 1 points) |> List.concat


previewAsPoints : Element.Color -> List EarthPoint -> List (Entity LocalCoords)
previewAsPoints color points =
    let
        material =
            Material.color <| Color.fromRgba <| Element.toRgb color

        highlightPoint p =
            Scene3d.point { radius = Pixels.pixels 7 } material p.space
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
