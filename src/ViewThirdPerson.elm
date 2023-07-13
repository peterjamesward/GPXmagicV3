module ViewThirdPerson exposing
    ( initialiseView
    , resizeOccured
    , subscriptions
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import Angle
import Axis3d
import Camera3d exposing (Camera3d)
import Color
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Drag3dCommonStructures exposing (DragAction(..))
import Element exposing (..)
import FingerPainting exposing (fingerPaintingPreview)
import Html
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length exposing (Meters)
import LngLat
import LocalCoords exposing (LocalCoords)
import MapViewer
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Quantity exposing (Quantity, Unitless, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder3D
import SketchPlane3d
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions
import ToolsController
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import View3dCommonElements exposing (..)
import ViewMode exposing (ViewMode(..))
import Viewpoint3d
import ZoomLevel


subscriptions : MapViewer.MapData -> Context -> Sub Msg
subscriptions mapData context =
    MapViewer.subscriptions mapData context.map |> Sub.map MapMsg


view :
    SystemSettings
    -> MapViewer.MapData
    -> Context
    -> Tools.DisplaySettingsOptions.Options
    -> ToolsController.Options msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Maybe String
    -> Element msg
view settings mapData context display tools contentArea track scene msgWrapper paintTool =
    let
        dragging =
            context.dragAction

        lookingAt =
            --TODO: Remove repeated code here, quickly added to test 3d map stuff.
            if context.followSelectedPoint then
                DomainModel.earthPointFromIndex track.currentPosition track.trackTree

            else
                context.focalPoint

        viewDistance : Quantity Float Meters
        viewDistance =
            Length.meters <| 2 ^ (21 - context.zoomLevel)

        mapCamera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbit
                        { focalPoint = mapPositionFromTrack lookingAt track
                        , groundPlane = SketchPlane3d.yx
                        , azimuth = Direction2d.toAngle <| Direction2d.rotateCounterclockwise context.cameraAzimuth
                        , elevation = context.cameraElevation
                        , distance = scaleToMapWorld track viewDistance
                        }
                , verticalFieldOfView = Angle.degrees 45
                }

        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

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

        sceneWithOptionalGround =
            if display.groundPlane && not context.showMap then
                (SceneBuilder3D.renderGroundPlane display <|
                    Just <|
                        DomainModel.boundingBox track.trackTree
                )
                    ++ scene

            else
                scene

        view3d =
            el
                ((if dragging /= DragNone then
                    htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)

                  else
                    pointer
                 )
                    :: (inFront <| placesOverlay display contentArea track camera)
                    :: (inFront <| fingerPaintingPreview settings tools context contentArea paintTool)
                    :: (inFront <| onViewControls settings msgWrapper context)
                    :: common3dSceneAttributes msgWrapper context
                )
            <|
                html <|
                    Scene3d.sunny
                        { camera = camera
                        , dimensions = contentArea
                        , background =
                            if context.showMap then
                                Scene3d.transparentBackground

                            else
                                backgroundColor Color.lightBlue
                        , clipDepth = Length.meters 1
                        , entities = sceneWithOptionalGround
                        , upDirection = positiveZ
                        , sunlightDirection = negativeZ
                        , shadows = False
                        }
    in
    el
        [ behindContent <|
            if context.showMap then
                mapUnderlay

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
                , distance = Length.meters <| 2 ^ (21 - context.zoomLevel)
                }
    in
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = context.fieldOfView
        }


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> MapViewer.MapData
    -> Context
    -> Maybe String
    -> ( Context, List (ToolAction msg), MapViewer.MapData )
update msg msgWrapper track ( width, height ) mapData context paintTool =
    let
        camera =
            deriveCamera track.referenceLonLat track.trackTree context track.currentPosition

        mapUpdater ctxt =
            --TODO: This is only difference here between Plan and Third
            let
                lookingAt =
                    MapViewer.lngLatToWorld <|
                        lngLatFromXY track <|
                            if ctxt.followSelectedPoint then
                                DomainModel.earthPointFromIndex track.currentPosition track.trackTree
                                    |> .space

                            else
                                ctxt.focalPoint.space
            in
            MapViewer.withPositionAndZoom
                lookingAt
                (ZoomLevel.fromLogZoom <| 3 + ctxt.zoomLevel)
                ctxt.map
    in
    case msg of
        ImageReset ->
            let
                newContext =
                    initialiseView track.currentPosition ( width, height ) track (Just context)
            in
            ( { newContext | map = mapUpdater newContext }, [], mapData )

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
                paintTool


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
                        , dragAction = DragNone
                        , zoomLevel = 14.0
                        , defaultZoomLevel = 14.0
                        , focalPoint = treeNode |> leafFromIndex current |> startPoint
                        , waitingForClickDelay = False
                    }

                Nothing ->
                    { cameraAzimuth = Direction2d.negativeY
                    , cameraElevation = Angle.degrees 30
                    , cameraDistance = Length.kilometers 10
                    , fieldOfView = Angle.degrees 45
                    , dragAction = DragNone
                    , zoomLevel = 14.0
                    , defaultZoomLevel = 14.0
                    , focalPoint = treeNode |> leafFromIndex current |> startPoint
                    , waitingForClickDelay = False
                    , followSelectedPoint = True
                    , map = initialMap
                    , showMap = False
                    , fingerPainting = False
                    , viewMode = ViewThird
                    }

        initialMapForTrack =
            let
                lookingAt =
                    MapViewer.lngLatToWorld <|
                        lngLatFromXY track <|
                            newContext.focalPoint.space
            in
            MapViewer.withPositionAndZoom
                lookingAt
                (ZoomLevel.fromLogZoom <| newContext.zoomLevel)
                newContext.map
    in
    { newContext | map = initialMapForTrack }
