module ViewProfileCharts exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
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
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import Html.Attributes exposing (id)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Spherical
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import ViewPureStyles exposing (useIcon)
import Viewpoint3d exposing (Viewpoint3d)



--TODO: Link into PaneLayoutManager.
--TODO: Make two view areas.
--TODO: Render altitude and gradient scene(s).
--TODO: Cameras, orthographic, centering, zoom.
--TODO: Tilt to compensate for zoom level.
--TODO: SVG scales overlay.


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
    | ToggleFollowOrange


type DragAction
    = DragNone
    | DragPan


type alias Context =
    { altitudeCameraElevation : Angle -- Tilt to compensate for zoom, making for constant z-scale.
    , gradientCameraElevation : Angle -- Ditto
    , cameraDistance : Quantity Float Length.Meters -- Shared.
    , fieldOfView : Angle
    , dragAction : DragAction
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    }


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons : (Msg -> msg) -> Context -> Element msg
zoomButtons msgWrapper context =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 5
        , Background.color white
        , Font.size 40
        , padding 6
        , spacing 8
        , Border.width 1
        , Border.rounded 4
        , Border.color FlatColors.AussiePalette.blurple
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
        , Input.button []
            { onPress = Just <| msgWrapper ToggleFollowOrange
            , label =
                if context.followSelectedPoint then
                    useIcon FeatherIcons.lock

                else
                    useIcon FeatherIcons.unlock
            }
        ]


view :
    Context
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> List (Entity LocalCoords)
    -> List (Entity LocalCoords)
    -> (Msg -> msg)
    -> Element msg
view context ( givenWidth, givenHeight ) track sceneAltitude sceneGradient msgWrapper =
    let
        dragging =
            context.dragAction

        splitProportion = 0.5

        altitudePortion =
            ( givenWidth
            , givenHeight
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy splitProportion
                |> Quantity.truncate
            )

        gradientPortion =
            ( givenWidth
            , givenHeight
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy (1.0 - splitProportion)
                |> Quantity.truncate
            )
    in
    column
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
        , inFront <| zoomButtons msgWrapper context
        ]
        [ html <|
            Scene3d.unlit
                { camera = deriveAltitudeCamera track.trackTree context track.currentPosition
                , dimensions = altitudePortion
                , background = backgroundColor Color.white
                , clipDepth = Length.meters 1
                , entities = sceneAltitude
                }
        , html <|
            Scene3d.unlit
                { camera = deriveGradientCamera track.trackTree context track.currentPosition
                , dimensions = gradientPortion
                , background = backgroundColor Color.white
                , clipDepth = Length.meters 1
                , entities = sceneGradient
                }
        ]


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


deriveAltitudeCamera : PeteTree -> Context -> Int -> Camera3d Meters LocalCoords
deriveAltitudeCamera treeNode context currentPosition =
    let
        centre =
            BoundingBox3d.centerPoint <| boundingBox treeNode

        latitude =
            effectiveLatitude <| leafFromIndex currentPosition treeNode

        altitudeLookingAt =
            if context.followSelectedPoint then
                Point3d.xyz
                    (distanceFromIndex currentPosition treeNode)
                    Quantity.zero
                    (Point3d.zCoordinate centre)

            else
                context.focalPoint

        altitudeViewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = altitudeLookingAt
                , azimuth = Direction2d.toAngle Direction2d.negativeY
                , elevation = context.altitudeCameraElevation
                , distance =
                    --TODO: Some fudging going on here that should not be needed.
                    Length.meters <| 20.0 * Spherical.metresPerPixel context.zoomLevel latitude
                }
    in
    Camera3d.orthographic
        { viewpoint = altitudeViewpoint
        , viewportHeight =
            --TODO: Work this out properly
            Length.meters 1000.0
        }


deriveGradientCamera : PeteTree -> Context -> Int -> Camera3d Meters LocalCoords
deriveGradientCamera treeNode context currentPosition =
    let
        centre =
            BoundingBox3d.centerPoint <| boundingBox treeNode

        latitude =
            effectiveLatitude <| leafFromIndex currentPosition treeNode

        gradientLookingAt =
            if context.followSelectedPoint then
                Point3d.xyz
                    (distanceFromIndex currentPosition treeNode)
                    Quantity.zero
                    Quantity.zero

            else
                context.focalPoint

        gradientViewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = gradientLookingAt
                , azimuth = Direction2d.toAngle Direction2d.negativeY
                , elevation = context.gradientCameraElevation
                , distance =
                    --TODO: Some fudging going on here that should not be needed.
                    Length.meters <| 20.0 * Spherical.metresPerPixel context.zoomLevel latitude
                }
    in
    Camera3d.orthographic
        { viewpoint = gradientViewpoint
        , viewportHeight =
            --TODO: Work this out properly
            Length.meters 1000.0
        }


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> Int
detectHit event track ( w, h ) context =
    --TODO: Simplify, using x position to find distance.
    --TODO: Depends which pane we are in? No.
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
            deriveAltitudeCamera track.trackTree context track.currentPosition

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
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }, [] )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }, [] )

        ImageReset ->
            ( initialiseView track.currentPosition track.trackTree
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
            in
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + increment }, [] )

        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                newContext =
                    { context
                        | orbiting = Just event.offsetPos
                        , dragAction = DragPan
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
                ( DragPan, Just ( startX, startY ) ) ->
                    let
                        shiftVector =
                            Vector3d.meters
                                ((startY - dy) * Angle.sin context.altitudeCameraElevation)
                                0
                                0

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
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , []
            )

        ToggleFollowOrange ->
            ( { context | followSelectedPoint = not context.followSelectedPoint }
            , []
            )


initialiseView :
    Int
    -> PeteTree
    -> Context
initialiseView current treeNode =
    { altitudeCameraElevation = Angle.degrees 0
    , gradientCameraElevation = Angle.degrees 0
    , cameraDistance = Length.kilometers 10
    , fieldOfView = Angle.degrees 45
    , orbiting = Nothing
    , dragAction = DragNone
    , zoomLevel = 10.0
    , defaultZoomLevel = 10.0
    , focalPoint = treeNode |> leafFromIndex current |> startPoint
    , waitingForClickDelay = False
    , followSelectedPoint = False
    }
