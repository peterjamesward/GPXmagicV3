module ViewThirdPerson exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import Camera3d exposing (Camera3d)
import Color
import Direction2d exposing (Direction2d)
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Spherical
import Tools.DisplaySettingsOptions
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import View3dCommonElements exposing (..)
import Viewpoint3d exposing (Viewpoint3d)


view :
    I18NOptions.Location
    -> Context
    -> Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view location context display contentArea track scene msgWrapper =
    let
        dragging =
            context.dragAction

        camera =
            deriveCamera track.trackTree context track.currentPosition

        overlay =
            placesOverlay display contentArea track camera
    in
    el
        ((if dragging /= DragNone then
            htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)

          else
            pointer
         )
            :: (inFront <| overlay)
            :: (inFront <| zoomButtons location msgWrapper context)
            :: common3dSceneAttributes msgWrapper context
        )
    <|
        html <|
            Scene3d.sunny
                { camera = deriveCamera track.trackTree context track.currentPosition
                , dimensions = contentArea
                , background = backgroundColor Color.lightBlue
                , clipDepth = Length.meters 1
                , entities = scene
                , upDirection = positiveZ
                , sunlightDirection = negativeZ
                , shadows = False
                }


deriveCamera : PeteTree -> Context -> Int -> Camera3d Meters LocalCoords
deriveCamera treeNode context currentPosition =
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
                    --TODO: Some fudging going on here that should not be needed.
                    Length.meters <| 100.0 * Spherical.metresPerPixel context.zoomLevel latitude
                }

        perspectiveCamera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = context.fieldOfView
                }
    in
    perspectiveCamera


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
            deriveCamera track.trackTree context track.currentPosition

        ray =
            Camera3d.ray camera screenRectangle screenPoint
    in
    nearestToRay
        ray
        track.trackTree
        track.leafIndex
        track.currentPosition


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper track area context =
    case msg of
        ImageZoomIn ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
            in
            ( newContext, [] )

        ImageZoomOut ->
            let
                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
            in
            ( newContext, [] )

        ImageReset ->
            ( initialiseView track.currentPosition track.trackTree (Just context)
            , []
            )

        ImageNoOp ->
            ( context, [] )

        ImageClick event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            if context.waitingForClickDelay then
                ( context
                , [ SetCurrent <| detectHit event track area context
                  , TrackHasChanged
                  ]
                )

            else
                ( context, [] )

        ImageDoubleClick event ->
            let
                nearestPoint =
                    detectHit event track area context
            in
            ( { context | focalPoint = earthPointFromIndex nearestPoint track.trackTree }
            , [ SetCurrent nearestPoint
              , TrackHasChanged
              ]
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }, [] )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                newContext =
                    { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + increment }
            in
            ( newContext, [] )

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
                    ( newContext
                    , []
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
                    ( newContext, [] )

                _ ->
                    ( context, [] )

        ImageRelease event ->
            let
                newContext =
                    { context
                        | orbiting = Nothing
                        , dragAction = DragNone
                    }
            in
            ( newContext, [] )

        ToggleFollowOrange ->
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
                , focalPoint = earthPointFromIndex track.currentPosition track.trackTree
              }
            , []
            )

        SetEmphasis int ->
            ( context, [] )

        MouseMove _ ->
            -- Only interested if dragging.
            ( context, [] )


initialiseView :
    Int
    -> PeteTree
    -> Maybe Context
    -> Context
initialiseView current treeNode currentContext =
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
            }
