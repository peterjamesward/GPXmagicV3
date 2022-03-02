module ViewPlan exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (negativeZ, positiveY, positiveZ)
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
import Pixels exposing (Pixels, inPixels)
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Spherical exposing (metresPerPixel)
import TrackLoaded exposing (TrackLoaded)
import Vector3d
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
    | ToggleFollowOrange


type DragAction
    = DragNone
    | DragPan


type alias Context =
    { fieldOfView : Angle
    , orbiting : Maybe ( Float, Float )
    , dragAction : DragAction
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , waitingForClickDelay : Bool
    , followSelectedPoint : Bool
    }


initialiseView :
    Int
    -> PeteTree
    -> Maybe Context
    -> Context
initialiseView current treeNode currentContext =
    case currentContext of
        Just context ->
            { context
                | fieldOfView = Angle.degrees 45
                , orbiting = Nothing
                , dragAction = DragNone
                , zoomLevel = 14.0
                , defaultZoomLevel = 14.0
                , focalPoint =
                    treeNode |> leafFromIndex current |> startPoint
                , waitingForClickDelay = False
            }

        Nothing ->
            { fieldOfView = Angle.degrees 45
            , orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = 14.0
            , defaultZoomLevel = 14.0
            , focalPoint =
                treeNode |> leafFromIndex current |> startPoint
            , waitingForClickDelay = False
            , followSelectedPoint = True
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


view :
    Context
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
        , inFront <| zoomButtons msgWrapper context
        ]
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

        eyePoint =
            Point3d.translateBy
                (Vector3d.meters 0.0 0.0 5000.0)
                lookingAt

        viewpoint =
            -- Fixing "up is North" so that 2-way drag works well.
            Viewpoint3d.lookAt
                { focalPoint = lookingAt
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
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper track area context =
    -- Second return value indicates whether selection needs to change.
    case msg of
        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            ( { context
                | orbiting = Just event.offsetPos
                , dragAction = DragPan
                , waitingForClickDelay = True
              }
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            )

        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }
            , []
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
                                (startX - dx)
                                (dy - startY)
                                0.0
                                |> Vector3d.scaleBy
                                    (1.0
                                        -- Empirical
                                        * Spherical.metresPerPixel
                                            context.zoomLevel
                                            (Angle.degrees 30)
                                    )
                    in
                    ( { context
                        | focalPoint =
                            context.focalPoint |> Point3d.translateBy shiftVector
                        , orbiting = Just ( dx, dy )
                      }
                    , []
                    )

                _ ->
                    ( context, [] )

        ImageRelease _ ->
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
              }
            , []
            )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY
            in
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + increment }
            , []
            )

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

        ImageZoomIn ->
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
            , []
            )

        ImageReset ->
            ( { context | zoomLevel = context.defaultZoomLevel }, [] )

        ToggleFollowOrange ->
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
                , focalPoint = earthPointFromIndex track.currentPosition track.trackTree
              }
            , []
            )

        _ ->
            ( context, [] )


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
