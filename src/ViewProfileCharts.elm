module ViewProfileCharts exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Axis3d
import BoundingBox3d
import Camera3d exposing (Camera3d)
import Chart as C
import Chart.Attributes as CA
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilderProfile exposing (ProfileDatum)
import TrackLoaded exposing (TrackLoaded)
import Vector3d
import ViewPureStyles exposing (useIcon)


type ClickZone
    = ZoneAltitude
    | ZoneGradient


type Msg
    = ImageMouseWheel Float
    | ImageGrab ClickZone Mouse.Event
    | ImageDrag ClickZone Mouse.Event
    | ImageRelease ClickZone Mouse.Event
    | ImageNoOp
    | ImageClick ClickZone Mouse.Event
    | ImageDoubleClick ClickZone Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ClickDelayExpired
    | ToggleFollowOrange


type DragAction
    = DragNone
    | DragPan


type alias Context =
    { dragAction : DragAction
    , orbiting : Maybe ( Float, Float )
    , zoomLevel : Float
    , defaultZoomLevel : Float
    , focalPoint : EarthPoint
    , followSelectedPoint : Bool
    , metresPerPixel : Float -- Helps with dragging accurately.
    , waitingForClickDelay : Bool
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
    -> List ProfileDatum
    -> (Msg -> msg)
    -> Element msg
view context ( givenWidth, givenHeight ) track profileData msgWrapper =
    let
        dragging =
            context.dragAction

        splitProportion =
            0.5

        altitudePortion =
            -- Subtract pixels we use for padding around the scene view.
            ( givenWidth |> Quantity.minus (Pixels.pixels 20)
            , givenHeight
                |> Quantity.minus (Pixels.pixels 20)
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy splitProportion
                |> Quantity.truncate
            )

        gradientPortion =
            ( givenWidth |> Quantity.minus (Pixels.pixels 20)
            , givenHeight
                |> Quantity.minus (Pixels.pixels 20)
                |> Quantity.toFloatQuantity
                |> Quantity.multiplyBy (1.0 - splitProportion)
                |> Quantity.truncate
            )
    in
    column
        [ htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
        , onContextMenu (msgWrapper ImageNoOp)
        , width fill
        , height fill
        , pointer
        , Border.width 0
        , Border.color FlatColors.ChinesePalette.peace
        , inFront <| zoomButtons msgWrapper context
        ]
        []


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


modelPointFromClick :
    Mouse.Event
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> TrackLoaded msg
    -> Maybe EarthPoint
modelPointFromClick event ( w, h ) context track =
    let
        ( x, y ) =
            event.offsetPos
    in
    Nothing


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> Int
detectHit event track ( w, h ) context =
    case modelPointFromClick event ( w, h ) context track of
        Just pointOnZX ->
            DomainModel.indexFromDistance (Point3d.xCoordinate pointOnZX) track.trackTree

        Nothing ->
            -- Leave position unchanged; should not occur.
            track.currentPosition


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Context
    -> ( Context, List (ToolAction msg) )
update msg msgWrapper track ( givenWidth, givenHeight ) context =
    let
        splitProportion =
            --TODO: Remove duplicate with `view`
            0.5

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

        areaForZone zone =
            case zone of
                ZoneAltitude ->
                    altitudePortion

                ZoneGradient ->
                    gradientPortion

        centre =
            BoundingBox3d.centerPoint <| boundingBox track.trackTree
    in
    case msg of
        ImageZoomIn ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel + 0.5 }
            , []
            )

        ImageZoomOut ->
            ( { context | zoomLevel = clamp 0 10 <| context.zoomLevel - 0.5 }
            , []
            )

        ImageReset ->
            ( initialiseView track.currentPosition track.trackTree (Just context), [] )

        ImageNoOp ->
            ( context, [] )

        ImageClick zone event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            if context.waitingForClickDelay then
                ( context
                , [ SetCurrent <| detectHit event track (areaForZone zone) context
                  , TrackHasChanged
                  ]
                )

            else
                ( context, [] )

        ImageDoubleClick zone event ->
            let
                nearestPoint =
                    detectHit event track (areaForZone zone) context
            in
            ( { context | focalPoint = earthPointFromIndex nearestPoint track.trackTree }
            , [ SetCurrent nearestPoint
              , TrackHasChanged
              ]
            )

        ClickDelayExpired ->
            --TODO: Replace with logic that looks for mouse movement.
            ( { context | waitingForClickDelay = False }, [] )

        ImageMouseWheel deltaY ->
            let
                increment =
                    -0.001 * deltaY

                zoomLevel =
                    clamp 0 10 <| context.zoomLevel + increment
            in
            ( { context | zoomLevel = zoomLevel }, [] )

        ImageGrab zone event ->
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

        ImageDrag zone event ->
            let
                ( dx, dy ) =
                    event.offsetPos
            in
            case ( context.dragAction, context.orbiting ) of
                ( DragPan, Just ( startX, startY ) ) ->
                    let
                        shiftVector =
                            --TODO: Find out how to do the pixel calculation. See examples?
                            Vector3d.meters
                                --((startX - dx) * 1.15 ^ (22 - context.zoomLevel))
                                ((startX - dx) * context.metresPerPixel)
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

        ImageRelease zone event ->
            ( { context
                | orbiting = Nothing
                , dragAction = DragNone
                , waitingForClickDelay = False
              }
            , []
            )

        ToggleFollowOrange ->
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
                , focalPoint =
                    Point3d.xyz
                        (distanceFromIndex track.currentPosition track.trackTree)
                        Quantity.zero
                        (Point3d.zCoordinate centre)
              }
            , []
            )


initialiseView :
    Int
    -> PeteTree
    -> Maybe Context
    -> Context
initialiseView current treeNode currentContext =
    case currentContext of
        Just context ->
            { context
                | orbiting = Nothing
                , dragAction = DragNone
                , zoomLevel = 10.0
                , defaultZoomLevel = 10.0
                , focalPoint = treeNode |> leafFromIndex current |> startPoint
                , metresPerPixel = 10.0
                , waitingForClickDelay = False
            }

        Nothing ->
            { orbiting = Nothing
            , dragAction = DragNone
            , zoomLevel = 10.0
            , defaultZoomLevel = 10.0
            , focalPoint = treeNode |> leafFromIndex current |> startPoint
            , followSelectedPoint = False
            , metresPerPixel = 10.0
            , waitingForClickDelay = False
            }
