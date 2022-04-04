module ViewThirdPerson exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis2d
import Axis3d
import Camera3d exposing (Camera3d)
import Circle2d
import Color
import Dict
import Direction2d exposing (Direction2d)
import Direction3d exposing (negativeZ, positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Frame2d
import Geometry.Svg as Svg
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import LandUseDataTypes
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d
import Point3d.Projection as Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Spherical
import Svg
import Svg.Attributes
import Tools.DisplaySettingsOptions
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import View3dCommonElements exposing (..)
import Viewpoint3d exposing (Viewpoint3d)


view :
    Context
    -> Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view context display ( givenWidth, givenHeight ) track scene msgWrapper =
    let
        dragging =
            context.dragAction

        camera =
            deriveCamera track.trackTree context track.currentPosition

        topLeftFrame =
            Frame2d.atPoint
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity givenHeight))
                |> Frame2d.reverseY

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

        nodes2d =
            --case display.landUse of
            --    LandUseDataTypes.LandUseSloped ->
                    track.landUseData.places
                        |> Dict.map
                            (\name place ->
                                place |> Point3d.toScreenSpace camera screenRectangle
                            )

                --_ ->
                --    track.landUseData.places
                --        |> Dict.map
                --            (\name place ->
                --                place
                --                    |> Point3d.projectOnto Plane3d.xy
                --                    |> Point3d.toScreenSpace camera screenRectangle
                --            )

        textAttributes atPoint =
            [ Svg.Attributes.fill "white"
            , Svg.Attributes.fontFamily "sans-serif"
            , Svg.Attributes.fontSize "12px"
            , Svg.Attributes.stroke "none"
            , Svg.Attributes.x (String.fromFloat (Pixels.toFloat (Point2d.xCoordinate atPoint) + 10))
            , Svg.Attributes.y (String.fromFloat (Pixels.toFloat (Point2d.yCoordinate atPoint)))
            ]

        -- Create an SVG label at each place
        placeNames =
            nodes2d
                |> Dict.map
                    (\name vertex ->
                        Svg.g []
                            [ Svg.circle2d
                                [ Svg.Attributes.stroke "white"
                                , Svg.Attributes.strokeWidth "1"
                                , Svg.Attributes.fill "none"
                                ]
                                (Circle2d.withRadius (Pixels.float 3) vertex)
                            , Svg.text_
                                (textAttributes vertex)
                                [ Svg.text name ]
                                -- Hack: flip the text upside down since our later
                                -- 'Svg.relativeTo topLeftFrame' call will flip it
                                -- back right side up
                                |> Svg.mirrorAcross (Axis2d.through vertex Direction2d.x)
                            ]
                    )
                |> Dict.values
                |> Svg.g []

        placesOverlay =
            if
                Dict.isEmpty track.landUseData.places
                    || not display.placeNames
            then
                none

            else
                html <|
                    Svg.svg
                        [ Svg.Attributes.width svgWidth
                        , Svg.Attributes.height svgHeight
                        ]
                        [ Svg.relativeTo topLeftFrame placeNames ]
    in
    el
        ((if dragging /= DragNone then
            htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)

          else
            pointer
         )
            :: (inFront <| placesOverlay)
            :: (inFront <| zoomButtons msgWrapper context)
            :: common3dSceneAttributes msgWrapper context
        )
    <|
        html <|
            Scene3d.sunny
                { camera = deriveCamera track.trackTree context track.currentPosition
                , dimensions = ( givenWidth, givenHeight )
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
                { focalPoint = lookingAt
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
    nearestToRay ray track.trackTree


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
                                    context.focalPoint |> Point3d.translateBy shiftVector
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
