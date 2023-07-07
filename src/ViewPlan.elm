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
import Frame2d
import Geometry.Svg as Svg
import Html
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel
import Length exposing (Meters)
import LineSegment3d
import LngLat
import LocalCoords exposing (LocalCoords)
import MapViewer
import MapboxKey
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d exposing (Rectangle2d)
import Scene3d exposing (Entity)
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
import View3dCommonElements exposing (Context, Msg(..), onViewControls, placesOverlay)
import ViewMode exposing (ViewMode(..))
import Viewpoint3d
import ZoomLevel


subscriptions : MapViewer.MapData -> Context -> Sub Msg
subscriptions mapData context =
    MapViewer.subscriptions mapData context.map |> Sub.map MapMsg


initialiseView :
    Int
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Maybe Context
    -> Context
initialiseView current track contentArea currentContext =
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
            , orbiting = Nothing
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

        ( lngLat1, lngLat2 ) =
            mapBoundsFromScene newContext contentArea track
    in
    { newContext | map = MapViewer.withViewBounds noPadding lngLat1 lngLat2 newContext.map }


stopProp =
    { stopPropagation = True, preventDefault = False }


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
            Html.map (msgWrapper << MapMsg) <|
                MapViewer.view
                    Nothing
                    mapData
                    context.map
    in
    el
        [ behindContent <|
            if context.showMap then
                el
                    [ inFront <|
                        el [ alignLeft, alignBottom ] <|
                            html MapViewer.attribution
                    ]
                <|
                    html mapUnderlay

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

        screenRectangle =
            Rectangle2d.from
                Point2d.origin
                (Point2d.xy
                    (Quantity.toFloatQuantity givenWidth)
                    (Quantity.toFloatQuantity givenHeight)
                )

        topLeftFrame =
            Frame2d.atPoint
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity givenHeight))

        --|> Frame2d.reverseY
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


mapBoundsFromScene :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> ( LngLat.LngLat, LngLat.LngLat )
mapBoundsFromScene updatedContext ( width, height ) track =
    -- Call this after updating context after any update changing the view/
    let
        ( wFloat, hFloat ) =
            ( toFloatQuantity width, toFloatQuantity height )

        oopsLngLat =
            { lng = 0, lat = 0 }

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            deriveCamera track.referenceLonLat track.trackTree updatedContext track.currentPosition

        ( rayOrigin, rayMax ) =
            ( Camera3d.ray camera screenRectangle Point2d.origin
            , Camera3d.ray camera screenRectangle (Point2d.xy wFloat hFloat)
            )

        ( topLeftModel, bottomRightModel ) =
            ( rayOrigin |> Axis3d.intersectionWithPlane Plane3d.xy
            , rayMax |> Axis3d.intersectionWithPlane Plane3d.xy
            )

        lngLatFromXY : Point3d.Point3d Meters LocalCoords -> LngLat.LngLat
        lngLatFromXY point =
            let
                gps : GPXSource
                gps =
                    DomainModel.gpxFromPointWithReference track.referenceLonLat <| DomainModel.withoutTime point
            in
            { lng = gps.longitude |> Direction2d.toAngle |> Angle.inDegrees
            , lat = gps.latitude |> Angle.inDegrees
            }
    in
    case ( topLeftModel, bottomRightModel ) of
        ( Just topLeft, Just bottomRight ) ->
            ( lngLatFromXY topLeft
            , lngLatFromXY bottomRight
            )

        _ ->
            -- We hope never to see this.
            ( oopsLngLat, oopsLngLat )


pointLeafProximity :
    Context
    -> TrackLoaded msg
    -> Rectangle2d Pixels ScreenCoords
    -> Point2d Pixels ScreenCoords
    -> Maybe PointLeafProximity
pointLeafProximity context track screenRectangle screenPoint =
    let
        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        ray =
            Camera3d.ray camera screenRectangle screenPoint

        nearestPointIndex =
            nearestPointToRay ray track.trackTree track.leafIndex track.currentPosition

        sharedPoint =
            earthPointFromIndex nearestPointIndex track.trackTree

        projectionPlane =
            -- Will use this to measure separation between leaf axes and the ray.
            Plane3d.through
                sharedPoint.space
                (Axis3d.direction ray)

        touchPointInWorld =
            Axis3d.intersectionWithPlane projectionPlane ray
                |> Maybe.map (Point3d.projectOnto projectionPlane)

        proximityFrom index =
            let
                {-
                   I find the closest pass between two axes (ray and leaf) by:
                   1. create a plane normal to the ray (origin on the ray, containing nearest point?)
                   2. project each leaf onto that plane
                   3. compare projected leafs:
                       a: is projection of origin to projected leaf within the [start,end]
                       b: which is closest (distanceFrom).
                   The Plan View falls out as a special case.
                -}
                leaf =
                    asRecord <| leafFromIndex index track.trackTree

                leafSegment =
                    LineSegment3d.from leaf.startPoint.space leaf.endPoint.space
                        |> LineSegment3d.projectOnto projectionPlane
            in
            case ( touchPointInWorld, LineSegment3d.axis leafSegment ) of
                ( Just touchPoint, Just leafAxis ) ->
                    let
                        proportion =
                            Quantity.ratio
                                (Point3d.signedDistanceAlong leafAxis touchPoint)
                                (LineSegment3d.length leafSegment)
                    in
                    Just
                        { leafIndex = index
                        , distanceAlong = Point3d.signedDistanceAlong leafAxis touchPoint
                        , distanceFrom = Point3d.distanceFromAxis leafAxis touchPoint
                        , proportionAlong = proportion
                        , screenPoint = screenPoint
                        , worldPoint = touchPoint
                        }

                _ ->
                    Nothing
    in
    -- So, is the click before or after the point?
    case
        ( proximityFrom <| nearestPointIndex - 1
        , proximityFrom nearestPointIndex
        )
    of
        ( Just before, Just after ) ->
            Just before

        --let
        --    internal =
        --        Interval.from 0.0 1.0
        --in
        --case
        --    ( internal |> Interval.contains before.proportionAlong
        --    , internal |> Interval.contains after.proportionAlong
        --    )
        --of
        --    ( True, False ) ->
        --        Just before
        --
        --    ( False, True ) ->
        --        Just after
        --
        --    _ ->
        --        -- No clear winner, closest wins.
        --        if before.distanceFrom |> Quantity.lessThanOrEqualTo after.distanceFrom then
        --            Just before
        --
        --        else
        --            Just after
        ( Just before, Nothing ) ->
            -- Probably better to choose a non-zero side.
            Just before

        ( Nothing, Just after ) ->
            Just after

        _ ->
            -- Really bad luck, who cares?
            Nothing


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

        ( rayOrigin, rayMax ) =
            ( Camera3d.ray camera screenRectangle Point2d.origin
            , Camera3d.ray camera screenRectangle (Point2d.xy wFloat hFloat)
            )

        ( topLeftModel, bottomRightModel ) =
            ( rayOrigin |> Axis3d.intersectionWithPlane Plane3d.xy
            , rayMax |> Axis3d.intersectionWithPlane Plane3d.xy
            )

        metersPerPixel =
            case ( topLeftModel, bottomRightModel ) of
                ( Just topLeft, Just bottomRight ) ->
                    (Length.inMeters <| Vector3d.xComponent <| Vector3d.from topLeft bottomRight)
                        / Pixels.toFloat wFloat

                _ ->
                    -- We hope never to see this.
                    1

        updatedMap ctxt =
            let
                ( lngLat1, lngLat2 ) =
                    mapBoundsFromScene ctxt ( width, height ) track
            in
            MapViewer.withViewBounds UtilsForViews.noPadding lngLat1 lngLat2 ctxt.map
    in
    -- Second return value indicates whether selection needs to change.
    case msg of
        MapMsg mapMsg ->
            let
                { newModel, newMapData, outMsg, cmd } =
                    MapViewer.update
                        (MapViewer.mapboxAccessToken MapboxKey.mapboxKey)
                        mapData
                        mapMsg
                        context.map
            in
            ( { context | map = newModel }
            , [ ExternalCommand <| Cmd.map (msgWrapper << MapMsg) cmd ]
            , newMapData
            )

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
                    Point2d.pixels x y

                newState =
                    if context.fingerPainting then
                        case pointLeafProximity context track screenRectangle screenPoint of
                            Just proximity ->
                                if proximity.distanceFrom |> Quantity.lessThanOrEqualTo (Length.meters 2) then
                                    DragPaint <| PaintInfo [ proximity ]

                                else
                                    --TODO: DragPush <| ViewContext.PushInfo
                                    DragPan

                            _ ->
                                DragPan

                    else
                        DragPan
            in
            ( { context
                | orbiting = Just event.offsetPos
                , dragAction = newState
                , waitingForClickDelay = True
              }
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            , mapData
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }
            , []
            , mapData
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case ( context.dragAction, context.orbiting ) of
                ( DragPan, Just ( startX, startY ) ) ->
                    let
                        shiftVector =
                            Vector3d.meters
                                ((startX - dx) * metersPerPixel)
                                ((dy - startY) * metersPerPixel)
                                0.0

                        newFocus =
                            context.focalPoint
                                |> .space
                                |> Point3d.translateBy shiftVector
                                |> DomainModel.withoutTime

                        newContext =
                            { context
                                | focalPoint = newFocus
                                , orbiting = Just ( dx, dy )
                            }
                    in
                    ( { newContext | map = updatedMap newContext }
                    , []
                    , mapData
                    )

                ( DragPaint paintInfo, _ ) ->
                    let
                        screenPoint =
                            Point2d.pixels dx dy

                        path =
                            case pointLeafProximity context track screenRectangle screenPoint of
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
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , actions
            , mapData
            )

        ImageMouseWheel deltaY ->
            let
                newZoom =
                    clamp 0.0 22.0 <| context.zoomLevel - (0.001 * deltaY)

                newContext =
                    { context | zoomLevel = newZoom }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ImageClick event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            if context.waitingForClickDelay then
                ( context
                , [ SetCurrent <| detectHit event track ( width, height ) context
                  , TrackHasChanged
                  ]
                , mapData
                )

            else
                ( context
                , []
                , mapData
                )

        ImageDoubleClick event ->
            let
                nearestPoint =
                    detectHit event track ( width, height ) context
            in
            ( { context
                | focalPoint = earthPointFromIndex nearestPoint track.trackTree
              }
            , [ SetCurrent nearestPoint
              , TrackHasChanged
              ]
            , mapData
            )

        ImageZoomIn ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ImageZoomOut ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ImageReset ->
            let
                newContext =
                    { context | zoomLevel = context.defaultZoomLevel }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ToggleFollowOrange ->
            let
                newContext =
                    { context
                        | followSelectedPoint = not context.followSelectedPoint
                        , focalPoint = earthPointFromIndex track.currentPosition track.trackTree
                    }
            in
            ( { newContext | map = updatedMap newContext }
            , []
            , mapData
            )

        ToggleShowMap ->
            ( { context | showMap = not context.showMap }
            , []
            , mapData
            )

        ToggleFingerpainting ->
            ( { context | fingerPainting = not context.fingerPainting }
            , []
            , mapData
            )

        _ ->
            ( context
            , []
            , mapData
            )


trackChanged :
    TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> Context
trackChanged newTrack ( width, height ) context =
    -- Only interest is Orange pointer move.
    let
        ( lngLat1, lngLat2 ) =
            mapBoundsFromScene context ( width, height ) newTrack

        noPadding =
            { left = 0, right = 0, top = 0, bottom = 0 }
    in
    { context
        | map = MapViewer.withViewBounds noPadding lngLat1 lngLat2 context.map
    }


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> Int
detectHit event track ( w, h ) context =
    let
        ( x, y ) =
            event.offsetPos

        screenPoint =
            Point2d.pixels x y

        ( wFloat, hFloat ) =
            ( toFloatQuantity w, toFloatQuantity h )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)

        camera =
            -- Must use same camera derivation as for the 3D model, else pointless!
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    nearestPointToRay
        ray
        track.trackTree
        track.leafIndex
        track.currentPosition
