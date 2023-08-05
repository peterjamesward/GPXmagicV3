module ViewPlan exposing
    ( initialiseView
    , resizeOccured
    , subscriptions
    , trackChanged
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Drag3dCommonStructures exposing (DragAction(..), PaintInfo, PointLeafProximity, ScreenCoords)
import Element exposing (..)
import Element.Border as Border
import Element.Cursor as Cursor
import FingerPainting exposing (fingerPaintingPreview)
import FlatColors.ChinesePalette exposing (white)
import FlatColors.IndianPalette
import Html
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Length exposing (Meters)
import LngLat
import LocalCoords exposing (LocalCoords)
import MapViewer
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Scene3d exposing (Entity)
import Spherical exposing (metresPerPixel)
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions
import ToolsController
import TrackLoaded exposing (TrackLoaded)
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
    -> ToolsController.Options msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Maybe String
    -> Element msg
view context mapData settings display tools contentArea track scene msgWrapper paintTool =
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
                , inFront <| fingerPaintingPreview settings (msgWrapper StopInkMode) tools context contentArea paintTool
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
                            Nothing
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
    -> Maybe String
    -> ( Context, List (ToolAction msg), MapViewer.MapData )
update msg msgWrapper track ( width, height ) context mapData paintTool =
    let
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
