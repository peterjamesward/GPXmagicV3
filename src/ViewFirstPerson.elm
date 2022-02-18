module ViewFirstPerson exposing (..)

import Angle
import Camera3d exposing (Camera3d)
import Color
import Direction2d
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (asRecord)
import Element exposing (..)
import Length exposing (meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity, backgroundColor)
import TrackLoaded exposing (TrackLoaded)
import View3dCommonElements exposing (..)
import Viewpoint3d


view :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view context contentArea track scene msgWrapper =
    --let
    --    flythroughHUD =
    --        case context.flythrough of
    --            Just flythrough ->
    --                inFront <| headUpDisplay flythrough.gradient
    --
    --            Nothing ->
    --                inFront none
    --in
    el
        (common3dSceneAttributes msgWrapper context)
    <|
        html <|
            Scene3d.sunny
                { camera = deriveViewPointAndCamera context track
                , dimensions = contentArea
                , background = backgroundColor Color.lightBlue
                , clipDepth = Length.meters 1
                , entities = scene
                , upDirection = positiveZ
                , sunlightDirection = negativeZ
                , shadows = False
                }


deriveViewPointAndCamera : Context -> TrackLoaded msg -> Camera3d Length.Meters LocalCoords
deriveViewPointAndCamera context track =
    let
        localRoad =
            DomainModel.leafFromIndex track.currentPosition track.trackTree
                |> asRecord

        gradientAsAngle =
            Angle.atan <| localRoad.gradientAtStart / 100.0

        cameraViewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = localRoad.startPoint
                , azimuth =
                    localRoad.directionAtStart
                        |> Direction2d.reverse
                        |> Direction2d.toAngle
                , elevation = Angle.degrees 20.0 |> Quantity.minus gradientAsAngle
                , distance = Length.meters 10
                }
    in
    Camera3d.perspective
        { viewpoint = cameraViewpoint
        , verticalFieldOfView = Angle.degrees <| 120.0 - context.zoomLevel * 2.0
        }
