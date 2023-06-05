module ViewThirdPerson exposing (initialiseView, resizeOccured, subscriptions, update, view)

import Actions exposing (ToolAction(..))
import Angle
import Axis2d
import Axis3d
import BoundingBox2d
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Color
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Html
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length exposing (Meters)
import LngLat
import LocalCoords exposing (LocalCoords)
import MapViewer
import MapboxKey
import Math.Matrix4 exposing (Mat4)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d
import Quantity exposing (Quantity, Unitless, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import SketchPlane3d
import Spherical
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews
import Vector3d
import View3dCommonElements exposing (..)
import Viewpoint3d
import WebGL.Matrices
import ZoomLevel


subscriptions : MapViewer.MapData -> Context -> Sub Msg
subscriptions mapData context =
    MapViewer.subscriptions mapData context.map |> Sub.map MapMsg


view :
    SystemSettings
    -> MapViewer.MapData
    -> Context
    -> Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view settings mapData context display contentArea track scene msgWrapper =
    let
        dragging =
            context.dragAction

        lngLatFromGps gps =
            { lng = gps.longitude |> Direction2d.toAngle |> Angle.inDegrees
            , lat = gps.latitude |> Angle.inDegrees
            }

        worldFromGps =
            lngLatFromGps >> MapViewer.lngLatToWorld

        groundHeight =
            DomainModel.boundingBox track.trackTree
                |> BoundingBox3d.minZ

        lookingAt =
            --TODO: Remove repeated code here, quickly added to test 3d map stuff.
            if context.followSelectedPoint then
                startPoint <| leafFromIndex track.currentPosition track.trackTree

            else
                context.focalPoint

        latitude =
            effectiveLatitude <| leafFromIndex track.currentPosition track.trackTree

        {-
           I want a consistent conversion of meters to World Coordinates across the globe.
           I'll do that by numerically differentiating the Web Mercator formula.
           If it works, I'll do the proper maths.
           For now, I'll just take the first leaf and use those two points.
        -}
        firstLeaf =
            DomainModel.getFirstLeaf track.trackTree

        ( worldStart, worldEnd ) =
            Tuple.mapBoth worldFromGps worldFromGps firstLeaf.sourceData

        leafLengthInWorld : Quantity Float Unitless
        leafLengthInWorld =
            Point2d.distanceFrom worldStart worldEnd

        viewDistance : Quantity Float Meters
        viewDistance =
            --TODO: Some fudging going on here that should not be needed. See ViewPlan; maybe better.
            --Length.meters <| 100.0 * Spherical.metresPerPixel context.zoomLevel latitude
            Length.meters <| 2 ^ (21 - context.zoomLevel)

        viewDistanceInWorld : Quantity Float Unitless
        viewDistanceInWorld =
            leafLengthInWorld |> Quantity.multiplyBy (Quantity.ratio viewDistance firstLeaf.trueLength)

        lookingAtPosition =
            withHeight heightInWorld <|
                worldFromGps <|
                    DomainModel.gpxFromPointWithReference track.referenceLonLat lookingAt

        lookingAtHeight =
            Point3d.zCoordinate lookingAt.space |> Quantity.minus groundHeight

        heightInWorld =
            leafLengthInWorld |> Quantity.multiplyBy (Quantity.ratio lookingAtHeight firstLeaf.trueLength)

        withHeight h pt =
            let
                { x, y } =
                    Point2d.toUnitless pt
            in
            Point3d.fromUnitless { x = x, y = y, z = Quantity.toFloat h }

        mapCamera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbit
                        { focalPoint = lookingAtPosition
                        , groundPlane = SketchPlane3d.yx
                        , azimuth = Direction2d.toAngle <| Direction2d.rotateCounterclockwise context.cameraAzimuth
                        , elevation = context.cameraElevation
                        , distance = viewDistanceInWorld
                        }
                , verticalFieldOfView = Angle.degrees 45
                }

        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        overlay =
            placesOverlay display contentArea track camera

        mapUnderlay =
            Html.map (msgWrapper << MapMsg) <|
                MapViewer.view
                    (Just mapCamera)
                    mapData
                    context.map

        view3d =
            el
                ((if dragging /= DragNone then
                    htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)

                  else
                    pointer
                 )
                    :: (inFront <| overlay)
                    :: (inFront <| zoomButtons settings msgWrapper context)
                    :: common3dSceneAttributes msgWrapper context
                )
            <|
                html <|
                    Scene3d.sunny
                        { camera = deriveCamera track.referenceLonLat track.trackTree context track.currentPosition
                        , dimensions = contentArea
                        , background =
                            if context.showMap then
                                Scene3d.transparentBackground

                            else
                                backgroundColor Color.lightBlue
                        , clipDepth = Length.meters 1
                        , entities = scene
                        , upDirection = positiveZ
                        , sunlightDirection = negativeZ
                        , shadows = False
                        }
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
        view3d


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

        cameraViewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = lookingAt.space
                , azimuth = Direction2d.toAngle context.cameraAzimuth
                , elevation = context.cameraElevation
                , distance =
                    --TODO: Some fudging going on here that should not be needed. See ViewPlan; maybe better.
                    --Length.meters <| 100.0 * Spherical.metresPerPixel context.zoomLevel latitude
                    Length.meters <| 2 ^ (21 - context.zoomLevel)
                }
    in
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = context.fieldOfView
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
    nearestToRay
        ray
        track.trackTree
        track.leafIndex
        track.currentPosition


mapBoundsFromScene :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> ( LngLat.LngLat, LngLat.LngLat )
mapBoundsFromScene updatedContext ( width, height ) track =
    -- Call this after updating context after any update changing the view/
    -- We restrict the bounds here to the track enclosing box.
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

        trackBox =
            -- Restrict the bounds to the track enclosing box.
            track.trackTree
                |> DomainModel.boundingBox
                |> BoundingBox3d.expandBy Length.kilometer

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
    --Debug.log "BOUNDS" <|
    case ( topLeftModel, bottomRightModel ) of
        ( Just topLeft, Just bottomRight ) ->
            let
                visibleBox =
                    BoundingBox3d.from
                        topLeft
                        bottomRight

                { minX, minY, maxX, maxY, minZ, maxZ } =
                    visibleBox
                        |> BoundingBox3d.intersection trackBox
                        |> Maybe.withDefault trackBox
                        |> BoundingBox3d.extrema
            in
            ( lngLatFromXY <| Point3d.xyz minX maxY Quantity.zero
            , lngLatFromXY <| Point3d.xyz maxX minY Quantity.zero
            )

        _ ->
            -- We hope never to see this.
            ( oopsLngLat, oopsLngLat )


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> MapViewer.MapData
    -> Context
    -> ( Context, List (ToolAction msg), MapViewer.MapData )
update msg msgWrapper track ( width, height ) mapData context =
    let
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

        updatedMap ctxt =
            let
                lookingAt =
                    MapViewer.lngLatToWorld <|
                        lngLatFromXY <|
                            if context.followSelectedPoint then
                                DomainModel.earthPointFromIndex track.currentPosition track.trackTree
                                    |> .space

                            else
                                context.focalPoint.space
            in
            MapViewer.withPositionAndZoom
                lookingAt
                (ZoomLevel.fromLogZoom context.zoomLevel)
                context.map
    in
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

        ImageZoomIn ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
            in
            ( { newContext | map = updatedMap newContext }, [], mapData )

        ImageZoomOut ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
            in
            ( { newContext | map = updatedMap newContext }, [], mapData )

        ImageReset ->
            let
                newContext =
                    initialiseView track.currentPosition ( width, height ) track (Just context)
            in
            ( { newContext | map = updatedMap newContext }, [], mapData )

        ImageNoOp ->
            ( context, [], mapData )

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
                ( context, [], mapData )

        ImageDoubleClick event ->
            let
                nearestPoint =
                    detectHit event track ( width, height ) context

                newContext =
                    { context | focalPoint = earthPointFromIndex nearestPoint track.trackTree }
            in
            ( { newContext | map = updatedMap newContext }
            , [ SetCurrent nearestPoint
              , TrackHasChanged
              ]
            , mapData
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }, [], mapData )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + increment }
            in
            ( { newContext | map = updatedMap newContext }, [], mapData )

        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                alternate =
                    event.keys.ctrl || event.button == SecondButton

                newContext =
                    { context
                        | orbiting = Just event.offsetPos
                        , dragAction =
                            if alternate then
                                DragRotate

                            else
                                DragPan
                        , waitingForClickDelay = True
                    }
            in
            ( newContext
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            , mapData
            )

        ImageDrag event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case ( context.dragAction, context.orbiting ) of
                ( DragRotate, Just ( startX, startY ) ) ->
                    -- Change the camera azimuth and elevation
                    let
                        newAzimuth =
                            Angle.degrees <|
                                (Angle.inDegrees <| Direction2d.toAngle context.cameraAzimuth)
                                    - (dx - startX)

                        newElevation =
                            Angle.degrees <|
                                Angle.inDegrees context.cameraElevation
                                    + (dy - startY)

                        newContext =
                            { context
                                | cameraAzimuth = Direction2d.fromAngle newAzimuth
                                , cameraElevation = newElevation
                                , orbiting = Just ( dx, dy )
                            }
                    in
                    ( { newContext | map = updatedMap newContext }
                    , []
                    , mapData
                    )

                ( DragPan, Just ( startX, startY ) ) ->
                    let
                        shiftVector =
                            Vector3d.meters
                                ((startY - dy) * Angle.sin context.cameraElevation)
                                (startX - dx)
                                ((dy - startY) * Angle.cos context.cameraElevation)
                                |> Vector3d.rotateAround
                                    Axis3d.z
                                    (Direction2d.toAngle context.cameraAzimuth)
                                |> Vector3d.scaleBy
                                    (0.1
                                        -- Empirical
                                        * Spherical.metresPerPixel
                                            context.zoomLevel
                                            (Angle.degrees 30)
                                    )

                        newContext =
                            { context
                                | focalPoint =
                                    context.focalPoint.space
                                        |> Point3d.translateBy shiftVector
                                        |> DomainModel.withoutTime
                                , orbiting = Just ( dx, dy )
                            }
                    in
                    ( { newContext | map = updatedMap newContext }, [], mapData )

                _ ->
                    ( context, [], mapData )

        ImageRelease _ ->
            let
                newContext =
                    { context
                        | orbiting = Nothing
                        , dragAction = DragNone
                    }
            in
            ( newContext, [], mapData )

        ToggleFollowOrange ->
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
                , focalPoint = earthPointFromIndex track.currentPosition track.trackTree
              }
            , []
            , mapData
            )

        ToggleShowMap ->
            ( { context | showMap = not context.showMap }
            , []
            , mapData
            )

        SetEmphasis _ ->
            ( context, [], mapData )

        MouseMove _ ->
            -- Only interested if dragging.
            ( context, [], mapData )


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

        initialMap =
            MapViewer.init
                { lng = 0, lat = 0 }
                (ZoomLevel.fromLogZoom 12)
                1
                contentArea

        newContext =
            case currentContext of
                Just context ->
                    { context
                        | cameraAzimuth = Direction2d.negativeY
                        , cameraElevation = Angle.degrees 30
                        , cameraDistance = Length.kilometers 10
                        , fieldOfView = Angle.degrees 45
                        , orbiting = Nothing
                        , dragAction = DragNone
                        , zoomLevel = 14.0
                        , defaultZoomLevel = 14.0
                        , focalPoint =
                            treeNode |> leafFromIndex current |> startPoint
                        , waitingForClickDelay = False
                    }

                Nothing ->
                    { cameraAzimuth = Direction2d.negativeY
                    , cameraElevation = Angle.degrees 30
                    , cameraDistance = Length.kilometers 10
                    , fieldOfView = Angle.degrees 45
                    , orbiting = Nothing
                    , dragAction = DragNone
                    , zoomLevel = 14.0
                    , defaultZoomLevel = 14.0
                    , focalPoint =
                        treeNode |> leafFromIndex current |> startPoint
                    , waitingForClickDelay = False
                    , followSelectedPoint = True
                    , map = initialMap
                    , showMap = False
                    }

        ( lngLat1, lngLat2 ) =
            mapBoundsFromScene newContext contentArea track
    in
    { newContext
        | map =
            MapViewer.withViewBounds
                UtilsForViews.noPadding
                lngLat1
                lngLat2
                newContext.map
    }
