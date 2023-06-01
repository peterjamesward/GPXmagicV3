module ViewFirstPerson exposing (subscriptions, view)

import Angle
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
import Length
import LocalCoords exposing (LocalCoords)
import MapViewer
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity, backgroundColor)
import Tools.Flythrough
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (elmuiColour, showDecimal1)
import View3dCommonElements exposing (..)
import Viewpoint3d


subscriptions : MapViewer.MapData -> Context -> Sub Msg
subscriptions mapData context =
    MapViewer.subscriptions mapData context.map |> Sub.map MapMsg


view :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Maybe Tools.Flythrough.Flythrough
    -> Element msg
view context contentArea track scene msgWrapper mFlythrough =
    let
        flythroughHUD =
            case mFlythrough of
                Just flythrough ->
                    inFront <| headUpDisplay flythrough.gradient

                Nothing ->
                    inFront none
    in
    el
        (flythroughHUD :: common3dSceneAttributes msgWrapper context)
    <|
        html <|
            Scene3d.sunny
                { camera = deriveViewPointAndCamera context track mFlythrough
                , dimensions = contentArea
                , background = backgroundColor Color.lightBlue
                , clipDepth = Length.meters 1
                , entities = scene
                , upDirection = positiveZ
                , sunlightDirection = negativeZ
                , shadows = False
                }


deriveViewPointAndCamera :
    Context
    -> TrackLoaded msg
    -> Maybe Tools.Flythrough.Flythrough
    -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera context track mFlythrough =
    let
        cameraViewpoint =
            case mFlythrough of
                Nothing ->
                    let
                        localRoad =
                            DomainModel.leafFromIndex track.currentPosition track.trackTree
                                |> asRecord

                        gradientAsAngle =
                            Angle.atan <| localRoad.gradientAtStart / 100.0
                    in
                    Viewpoint3d.orbitZ
                        { focalPoint = localRoad.startPoint.space
                        , azimuth =
                            localRoad.directionAtStart
                                |> Direction2d.reverse
                                |> Direction2d.toAngle
                        , elevation = Angle.degrees 20.0 |> Quantity.minus gradientAsAngle
                        , distance = Length.meters 10
                        }

                Just flying ->
                    Viewpoint3d.lookAt
                        { eyePoint = flying.cameraPosition
                        , focalPoint = flying.focusPoint
                        , upDirection = Direction3d.positiveZ
                        }
    in
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = Angle.degrees <| 120.0 - context.zoomLevel * 2.0
        }


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
