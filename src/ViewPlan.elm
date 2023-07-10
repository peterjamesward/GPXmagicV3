module ViewPlan exposing
    ( applyFingerPaint
    , initialiseView
    , resizeOccured
    , subscriptions
    , trackChanged
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Circle2d
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Drag3dCommonStructures exposing (DragAction(..), PaintInfo, PointLeafProximity, ScreenCoords)
import Element exposing (..)
import Element.Border as Border
import Element.Cursor as Cursor
import FlatColors.ChinesePalette exposing (white)
import FlatColors.IndianPalette
import Geometry.Svg as Svg
import Html
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Length exposing (Meters)
import LngLat
import LocalCoords exposing (LocalCoords)
import MapViewer
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d exposing (Rectangle2d)
import Scene3d exposing (Entity)
import SketchPlane3d
import Spherical exposing (metresPerPixel)
import Svg
import Svg.Attributes
import SystemSettings exposing (SystemSettings)
import Tools.CentroidAverage
import Tools.DisplaySettingsOptions
import Tools.ProfileSmooth
import Tools.Simplify
import TrackLoaded exposing (TrackLoaded)
import Utils
import UtilsForViews exposing (colorFromElmUiColour)
import Vector3d
import View3dCommonElements exposing (Context, Msg(..), lngLatFromXY, mapBoundsFromScene, onViewControls, placesOverlay, pointLeafProximity)
import ViewMode exposing (ViewMode(..))
import Viewpoint3d
import ZoomLevel


subscriptions : MapViewer.MapData -> Context -> Sub Msg
subscriptions mapData context =
    MapViewer.subscriptions mapData context.map |> Sub.map MapMsg


initialiseView :
    Int
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> Maybe Context
    -> Context
initialiseView current contentArea track currentContext =
    let
        treeNode =
            track.trackTree

        box =
            DomainModel.boundingBox treeNode

        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema box

        noPadding =
            { left = 0, right = 0, top = 0, bottom = 0 }

        initialMap =
            MapViewer.init
                { lng = 0, lat = 0 }
                (ZoomLevel.fromLogZoom 12)
                1
                contentArea

        newContext : Context
        newContext =
            { fieldOfView = Angle.degrees 45
            , dragAction = DragNone
            , zoomLevel = 12
            , defaultZoomLevel = 12
            , focalPoint = treeNode |> leafFromIndex current |> startPoint
            , waitingForClickDelay = False
            , followSelectedPoint = True
            , map = initialMap
            , showMap = False
            , fingerPainting = False
            , viewMode = ViewPlan
            , cameraAzimuth = Direction2d.y
            , cameraElevation = Angle.degrees 90
            , cameraDistance = Length.kilometer -- irrelevant for orthographic projection
            }

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 0 0 1000
                        , upDirection = positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 90
                }

        ( lngLat1, lngLat2 ) =
            mapBoundsFromScene camera contentArea track
    in
    { newContext | map = MapViewer.withViewBounds noPadding lngLat1 lngLat2 newContext.map }


view :
    Context
    -> MapViewer.MapData
    -> SystemSettings
    -> Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view context mapData settings display contentArea track scene msgWrapper =
    let
        dragging =
            context.dragAction

        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        plan3dView =
            el
                [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
                , if dragging /= DragNone then
                    htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)

                  else
                    pointer
                , htmlAttribute <| Mouse.onUp (ImageRelease >> msgWrapper)
                , htmlAttribute <| Mouse.onClick (ImageClick >> msgWrapper)
                , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> msgWrapper)
                , htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
                , width fill
                , height fill
                , pointer
                , Border.width 0
                , Border.color FlatColors.ChinesePalette.peace
                , inFront <| placesOverlay display contentArea track camera
                , inFront <| fingerPaintingPreview context contentArea track camera
                , inFront <| onViewControls settings msgWrapper context
                , if context.followSelectedPoint then
                    Cursor.default

                  else
                    Cursor.pointer
                ]
            <|
                html <|
                    Scene3d.sunny
                        { camera = camera
                        , dimensions = contentArea
                        , background =
                            if context.showMap then
                                Scene3d.transparentBackground

                            else
                                Scene3d.backgroundColor <| colorFromElmUiColour FlatColors.IndianPalette.keppel
                        , clipDepth = Length.meters 1
                        , entities = scene
                        , upDirection = positiveZ
                        , sunlightDirection = negativeZ
                        , shadows = False
                        }

        mapUnderlay =
            el
                [ inFront <|
                    el [ alignLeft, alignBottom ] <|
                        html MapViewer.attribution
                ]
            <|
                html <|
                    Html.map (msgWrapper << MapMsg) <|
                        MapViewer.view
                            (Just mapCamera)
                            mapData
                            context.map
    in
    el
        [ behindContent <|
            if context.showMap then
                mapUnderlay

            else
                none
        ]
        plan3dView


fingerPaintingPreview :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> Camera3d Meters LocalCoords
    -> Element msg
fingerPaintingPreview context ( givenWidth, givenHeight ) track camera =
    let
        ( svgWidth, svgHeight ) =
            ( String.fromInt <| Pixels.inPixels givenWidth
            , String.fromInt <| Pixels.inPixels givenHeight
            )
    in
    case context.dragAction of
        DragPaint paintInfo ->
            let
                paintNodes =
                    paintInfo.path
                        |> List.map
                            (\proximity ->
                                Svg.circle2d
                                    [ Svg.Attributes.stroke "red"
                                    , Svg.Attributes.strokeWidth "1"
                                    , Svg.Attributes.fill "white"
                                    ]
                                    (Circle2d.withRadius (Pixels.float 5) proximity.screenPoint)
                            )
            in
            html <|
                Svg.svg
                    [ Svg.Attributes.width svgWidth
                    , Svg.Attributes.height svgHeight
                    ]
                    paintNodes

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

                        newGpxPoints =
                            -- Splicing is more stable if we preserve the extremities?
                            Utils.deDupe (==) <|
                                gpxPointFromIndex preTrackPoint track.trackTree
                                    :: List.map makeNewGpxPointFromProximity locations
                                    ++ [ gpxPointFromIndex postTrackPoint track.trackTree ]

                        --++ [ gpxPointFromIndex postTrackPoint track.trackTree ]
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


resizeOccured : ( Quantity Int Pixels, Quantity Int Pixels ) -> Context -> Context
resizeOccured paneArea context =
    { context | map = MapViewer.resizeCanvas 1.0 paneArea context.map }


deriveCamera : GPXSource -> PeteTree -> Context -> Int -> Camera3d Meters LocalCoords
deriveCamera refPoint treeNode context currentPosition =
    let
        latitude =
            effectiveLatitude <| leafFromIndex currentPosition treeNode

        lookingAt =
            if context.followSelectedPoint then
                startPoint <| leafFromIndex currentPosition treeNode

            else
                context.focalPoint

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 5000.0)
                lookingAt.space

        viewpoint =
            -- Fixing "up is North" so that 2-way drag works well.
            Viewpoint3d.lookAt
                { focalPoint = lookingAt.space
                , eyePoint = eyePoint
                , upDirection = Direction3d.positiveY
                }
    in
    Camera3d.orthographic
        { viewpoint = viewpoint
        , viewportHeight = Length.meters <| 1200.0 * metresPerPixel context.zoomLevel latitude
        }


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> MapViewer.MapData
    -> ( Context, List (ToolAction msg), MapViewer.MapData )
update msg msgWrapper track ( width, height ) context mapData =
    let
        -- Let us have some information about the view, making dragging more precise.
        ( wFloat, hFloat ) =
            ( toFloatQuantity width, toFloatQuantity height )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        mapUpdater ctxt =
            --TODO: This is only difference here between Plan and Third but zoom levels are off.
            let
                updatedCamera =
                    deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

                ( lngLat1, lngLat2 ) =
                    mapBoundsFromScene updatedCamera ( width, height ) track
            in
            MapViewer.withViewBounds UtilsForViews.noPadding lngLat1 lngLat2 ctxt.map
    in
    case msg of
        ImageReset ->
            let
                newContext =
                    initialiseView track.currentPosition ( width, height ) track (Just context)
            in
            ( { newContext | map = mapUpdater newContext }, [], mapData )

        ImageGrab event ->
            {-
               Plan view only has pan, no rotation. Obviously no tilt.
               For fingerpainting, we do a click detect to see whether to paint or push track.
               Painting if within one meter of track.
               Apologies for near duplication of detectHit here.
            -}
            let
                ( x, y ) =
                    event.offsetPos

                screenPoint =
                    Point2d.fromTuple Pixels.pixels event.offsetPos

                newState =
                    if context.fingerPainting then
                        case pointLeafProximity camera track screenRectangle screenPoint of
                            Just proximity ->
                                if proximity.distanceFrom |> Quantity.lessThanOrEqualTo (Length.meters 2) then
                                    DragPaint <| PaintInfo [ proximity ]

                                else
                                    DragPan x y

                            _ ->
                                DragPan x y

                    else
                        DragPan x y
            in
            ( { context
                | dragAction = newState
                , waitingForClickDelay = True
              }
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            , mapData
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case context.dragAction of
                DragPan startX startY ->
                    --TODO: It would be slightly cleaner to work out the viewPlan once at grab time
                    --TODO: and use that until released. But it's a small optimisation.
                    let
                        viewPlane =
                            SketchPlane3d.withNormalDirection
                                (Viewpoint3d.viewDirection <| Camera3d.viewpoint camera)
                                context.focalPoint.space

                        grabPointOnScreen =
                            Point2d.pixels startX startY

                        movePointOnScreen =
                            Point2d.pixels dx dy

                        grabPointInModel =
                            Camera3d.ray camera screenRectangle grabPointOnScreen
                                |> Axis3d.intersectionWithPlane (SketchPlane3d.toPlane viewPlane)

                        movePointInModel =
                            Camera3d.ray camera screenRectangle movePointOnScreen
                                |> Axis3d.intersectionWithPlane (SketchPlane3d.toPlane viewPlane)

                        newContext =
                            case ( grabPointInModel, movePointInModel ) of
                                ( Just pick, Just drop ) ->
                                    let
                                        shift =
                                            Vector3d.from drop pick
                                                |> Vector3d.projectInto viewPlane

                                        newFocus =
                                            Point2d.origin
                                                |> Point2d.translateBy shift
                                                |> Point3d.on viewPlane
                                    in
                                    { context
                                        | focalPoint = withoutTime newFocus
                                        , dragAction = DragPan dx dy
                                    }

                                _ ->
                                    context
                    in
                    ( { newContext | map = mapUpdater newContext }
                    , []
                    , mapData
                    )

                DragPaint paintInfo ->
                    let
                        screenPoint =
                            Point2d.pixels dx dy

                        path =
                            case pointLeafProximity camera track screenRectangle screenPoint of
                                Just proximity ->
                                    proximity :: paintInfo.path

                                Nothing ->
                                    paintInfo.path
                    in
                    ( { context | dragAction = DragPaint <| PaintInfo path }
                    , []
                    , mapData
                    )

                _ ->
                    ( context
                    , []
                    , mapData
                    )

        ImageRelease _ ->
            let
                actions =
                    case context.dragAction of
                        DragPaint paintInfo ->
                            -- One of those occasions where I'm pleased I have Actions, avoiding much plumbing.
                            if List.length paintInfo.path > 2 then
                                [ Actions.WithUndo <| Actions.FingerPaint paintInfo
                                , Actions.FingerPaint paintInfo
                                , Actions.TrackHasChanged
                                ]

                            else
                                []

                        _ ->
                            []
            in
            ( { context | dragAction = DragNone }
            , actions
            , mapData
            )

        _ ->
            View3dCommonElements.update
                msg
                msgWrapper
                track
                ( width, height )
                mapData
                context
                mapUpdater
                camera


trackChanged :
    TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> Context
trackChanged newTrack ( width, height ) context =
    -- Only interest is Orange pointer move.
    let
        camera =
            deriveCamera
                newTrack.referenceLonLat
                newTrack.trackTree
                context
                newTrack.currentPosition

        ( lngLat1, lngLat2 ) =
            mapBoundsFromScene camera ( width, height ) newTrack

        noPadding =
            { left = 0, right = 0, top = 0, bottom = 0 }
    in
    { context
        | map = MapViewer.withViewBounds noPadding lngLat1 lngLat2 context.map
    }
