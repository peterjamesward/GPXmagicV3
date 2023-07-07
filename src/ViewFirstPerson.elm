module ViewFirstPerson exposing
    ( initialiseView
    , resizeOccured
    , subscriptions
    , view
    )

import Angle
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Color
import ColourPalette exposing (gradientColourPastel)
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (asRecord)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FlatColors.ChinesePalette exposing (white)
import Html
import Length
import LocalCoords exposing (LocalCoords)
import MapViewer
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder3D
import SketchPlane3d
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions
import Tools.Flythrough
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (elmuiColour, showDecimal1)
import View3dCommonElements exposing (..)
import ViewMode exposing (ViewMode(..))
import ViewThirdPerson
import Viewpoint3d


subscriptions : MapViewer.MapData -> Context -> Sub Msg
subscriptions mapData context =
    MapViewer.subscriptions mapData context.map |> Sub.map MapMsg


resizeOccured : ( Quantity Int Pixels, Quantity Int Pixels ) -> Context -> Context
resizeOccured paneArea context =
    { context | map = MapViewer.resizeCanvas 1.0 paneArea context.map }


initialiseView :
    Int
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> Maybe Context
    -> Context
initialiseView current contentArea track currentContext =
    let
        sameAsThird =
            ViewThirdPerson.initialiseView current contentArea track currentContext
    in
    { sameAsThird | viewMode = ViewFirst }


view :
    SystemSettings
    -> Context
    -> Tools.DisplaySettingsOptions.Options
    -> MapViewer.MapData
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Maybe Tools.Flythrough.Flythrough
    -> Element msg
view settings context display mapData contentArea track scene msgWrapper mFlythrough =
    let
        flythroughHUD =
            case mFlythrough of
                Just flythrough ->
                    inFront <| headUpDisplay flythrough.gradient

                Nothing ->
                    inFront none

        ( camera3d, cameraMap ) =
            deriveViewPointsAndCameras context track mFlythrough

        sceneWithOptionalGround =
            if display.groundPlane && not context.showMap then
                (SceneBuilder3D.renderGroundPlane display <| Just <| DomainModel.boundingBox track.trackTree)
                    ++ scene

            else
                scene

        view3d =
            el
                (flythroughHUD
                    :: (inFront <| onViewControls settings msgWrapper context)
                    :: common3dSceneAttributes msgWrapper context
                )
            <|
                html <|
                    Scene3d.sunny
                        { camera = camera3d
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
                            (Just cameraMap)
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
        view3d


deriveViewPointsAndCameras :
    Context
    -> TrackLoaded msg
    -> Maybe Tools.Flythrough.Flythrough
    -> ( Camera3d Length.Meters LocalCoords, Camera3d Quantity.Unitless MapViewer.WorldCoordinates )
deriveViewPointsAndCameras context track mFlythrough =
    let
        groundHeight =
            DomainModel.boundingBox track.trackTree
                |> BoundingBox3d.minZ

        localRoad =
            DomainModel.leafFromIndex track.currentPosition track.trackTree
                |> asRecord

        gradientAsAngle =
            Angle.atan <| localRoad.gradientAtStart / 100.0

        azimuth =
            localRoad.directionAtStart
                |> Direction2d.reverse
                |> Direction2d.toAngle

        elevation =
            Angle.degrees 20.0 |> Quantity.minus gradientAsAngle

        riderPosition =
            case mFlythrough of
                Nothing ->
                    localRoad.startPoint.space

                Just flying ->
                    flying.cameraPosition

        cameraViewpoint =
            case mFlythrough of
                Nothing ->
                    Viewpoint3d.orbitZ
                        { focalPoint = riderPosition
                        , azimuth = azimuth
                        , elevation = elevation
                        , distance = Length.meters 10
                        }

                Just flying ->
                    Viewpoint3d.lookAt
                        { eyePoint = riderPosition
                        , focalPoint = flying.focusPoint
                        , upDirection = Direction3d.positiveZ
                        }

        viewpointForMap =
            case mFlythrough of
                Nothing ->
                    Viewpoint3d.orbit
                        { focalPoint = mapPositionFromTrack (DomainModel.withoutTime riderPosition) track
                        , groundPlane = SketchPlane3d.yx
                        , azimuth =
                            Direction2d.toAngle <|
                                Direction2d.rotateCounterclockwise <|
                                    Direction2d.fromAngle
                                        azimuth
                        , elevation = elevation
                        , distance = scaleToMapWorld track (Length.meters 10)
                        }

                Just flying ->
                    Viewpoint3d.lookAt
                        { eyePoint = mapPositionFromTrack (DomainModel.withoutTime riderPosition) track
                        , focalPoint =
                            mapPositionFromTrack
                                (DomainModel.withoutTime <| flying.focusPoint)
                                track
                        , upDirection = Direction3d.negativeZ
                        }

        camera3d =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees 45
                }

        cameraMap =
            Camera3d.perspective
                { viewpoint = viewpointForMap
                , verticalFieldOfView = Angle.degrees 45
                }
    in
    ( camera3d, cameraMap )


headUpDisplay gradient =
    el
        [ alignTop
        , alignLeft
        , moveDown 10
        , moveRight 10
        , Background.color <| elmuiColour <| gradientColourPastel gradient
        , Font.size 30
        , Font.color white
        , padding 6
        , width <| px 100
        , height <| px 100
        , Border.rounded 100
        , Border.width 2
        , Border.color white
        ]
    <|
        el [ centerX, centerY ] <|
            text (showDecimal1 gradient)
