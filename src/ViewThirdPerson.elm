module ViewThirdPerson exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import Camera3d exposing (Camera3d)
import Color
import Direction2d exposing (Direction2d)
import Direction3d exposing (positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.ChinesePalette exposing (white)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Spherical
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import ViewContextThirdPerson exposing (DragAction(..), ThirdPersonContext)
import ViewPureStyles exposing (useIcon)
import Viewpoint3d exposing (Viewpoint3d)


type Msg
    = ImageMouseWheel Float
    | ImageGrab Mouse.Event
    | ImageDrag Mouse.Event
    | ImageRelease Mouse.Event
    | ImageNoOp
    | ImageClick Mouse.Event
    | ImageDoubleClick Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ClickDelayExpired


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons : (Msg -> msg) -> Element msg
zoomButtons msgWrapper =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 5
        , Background.color white
        , Font.size 40
        , padding 6
        , spacing 8
        , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ImageNoOp >> msgWrapper)
        ]
        [ Input.button []
            { onPress = Just <| msgWrapper ImageZoomIn
            , label = useIcon FeatherIcons.plus
            }
        , Input.button []
            { onPress = Just <| msgWrapper ImageZoomOut
            , label = useIcon FeatherIcons.minus
            }
        , Input.button []
            { onPress = Just <| msgWrapper ImageReset
            , label = useIcon FeatherIcons.maximize
            }
        ]


view :
    ThirdPersonContext
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view context contentArea track scene msgWrapper =
    let
        dragging =
            context.dragAction
    in
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
        , onContextMenu (msgWrapper ImageNoOp)
        , width fill
        , height fill
        , pointer
        , Border.width 0
        , Border.color FlatColors.ChinesePalette.peace
        , inFront <| zoomButtons msgWrapper
        ]
    <|
        html <|
            Scene3d.cloudy
                { camera = deriveCamera track.trackTree context track.currentPosition
                , dimensions = contentArea
                , background = backgroundColor Color.lightBlue
                , clipDepth = Length.meters 1
                , entities = scene
                , upDirection = positiveZ
                }


onContextMenu : a -> Element.Attribute a
onContextMenu msg =
    HE.custom "contextmenu"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        |> htmlAttribute


deriveCamera : PeteTree -> ThirdPersonContext -> Int -> Camera3d Meters LocalCoords
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
    -> ThirdPersonContext
    -> Int
detectHit event track context =
    --TODO: Move into view/pane/whatever it will be.
    let
        ( x, y ) =
            event.offsetPos

        ( w, h ) =
            context.contentArea

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
    -> ThirdPersonContext
    -> ( ThirdPersonContext, List (ToolAction msg) )
update msg msgWrapper track context =
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
            ( initialiseView track.currentPosition track.trackTree context.contentArea
            , []
            )

        ImageNoOp ->
            ( context, [] )

        ImageClick event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            if context.waitingForClickDelay then
                ( context
                , [ SetCurrent <| detectHit event track context
                  , TrackHasChanged
                  ]
                )

            else
                ( context, [] )

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

                        --, followSelectedPoint = False
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

        ImageDoubleClick event ->
            ( context, [] )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }, [] )


initialiseView :
    Int
    -> PeteTree
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> ThirdPersonContext
initialiseView current treeNode viewSize =
    { cameraAzimuth = Direction2d.x
    , cameraElevation = Angle.degrees 30
    , cameraDistance = Length.kilometers 10
    , fieldOfView = Angle.degrees 45
    , orbiting = Nothing
    , dragAction = DragNone
    , zoomLevel = 10.0
    , defaultZoomLevel = 10.0
    , focalPoint =
        treeNode |> leafFromIndex current |> startPoint
    , waitingForClickDelay = False
    , followSelectedPoint = True
    , contentArea = viewSize
    }
