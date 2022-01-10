module ViewThirdPerson exposing (..)

import Actions
import Angle exposing (Angle)
import Axis3d
import Browser.Dom
import Camera3d exposing (Camera3d)
import Color
import Delay
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
import Html.Attributes exposing (id)
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import MapPortsController exposing (MapMsg)
import ModelRecord exposing (ModelRecord)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Spherical
import Vector3d
import ViewContextThirdPerson exposing (ContextThirdPerson, DragAction(..))
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
    { model
        | scene : List (Entity LocalCoords)
        , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
        , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
        , trackTree : Maybe PeteTree
        , currentPosition : Int
        , viewContext : Maybe ContextThirdPerson
    }
    -> (Msg -> msg)
    -> Element msg
view model msgWrapper =
    el
        [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
        , htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)
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
        case ( model.trackTree, model.viewContext ) of
            ( Just treeNode, Just context ) ->
                html <|
                    Scene3d.cloudy
                        { camera = deriveCamera treeNode context model.currentPosition
                        , dimensions = model.contentArea
                        , background = backgroundColor Color.lightBlue
                        , clipDepth = Length.meters 1
                        , entities = model.scene
                        , upDirection = positiveZ
                        }

            _ ->
                text "No track to show"


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


deriveCamera : PeteTree -> ContextThirdPerson -> Int -> Camera3d Meters LocalCoords
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
    ->
        { m
            | trackTree : Maybe PeteTree
            , currentPosition : Int
            , contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
            , viewContext : Maybe ContextThirdPerson
        }
    -> Int
detectHit event model =
    --TODO: Move into view/pane/whatever it will be.
    case ( model.trackTree, model.viewContext ) of
        ( Just topNode, Just context ) ->
            let
                ( x, y ) =
                    event.offsetPos

                screenPoint =
                    Point2d.pixels x y

                ( w, h ) =
                    model.contentArea

                ( wFloat, hFloat ) =
                    ( toFloatQuantity w, toFloatQuantity h )

                screenRectangle =
                    Rectangle2d.from
                        (Point2d.xy Quantity.zero hFloat)
                        (Point2d.xy wFloat Quantity.zero)

                camera =
                    -- Must use same camera derivation as for the 3D model, else pointless!
                    deriveCamera topNode context model.currentPosition

                ray =
                    Camera3d.ray camera screenRectangle screenPoint
            in
            nearestToRay ray topNode

        _ ->
            0


update :
    Msg
    -> ModelRecord
    -> (Msg -> msg)
    -> ( ModelRecord, Cmd msg )
update msg model msgWrapper =
    case ( model.trackTree, model.viewContext ) of
        ( Just treeNode, Just context ) ->
            case msg of
                ImageZoomIn ->
                    let
                        newContext =
                            { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + 0.5 }
                    in
                    ( { model | viewContext = Just newContext }
                    , Cmd.none
                    )

                ImageZoomOut ->
                    let
                        newContext =
                            { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel - 0.5 }
                    in
                    ( { model | viewContext = Just newContext }
                    , Cmd.none
                    )

                ImageReset ->
                    ( { model | viewContext = Just <| initialiseView model.currentPosition treeNode }
                    , Cmd.none
                    )

                ImageNoOp ->
                    ( model, Cmd.none )

                ImageClick event ->
                    -- Click moves pointer but does not re-centre view. (Double click will.)
                    if context.waitingForClickDelay then
                        { model | currentPosition = detectHit event model }
                            |> Actions.updateAllDisplays

                    else
                        ( model, Cmd.none )

                ImageMouseWheel deltaY ->
                    let
                        increment =
                            -0.001 * deltaY

                        newContext =
                            { context | zoomLevel = clamp 0.0 22.0 <| context.zoomLevel + increment }
                    in
                    ( { model | viewContext = Just newContext }
                    , Cmd.none
                    )

                ImageGrab event ->
                    -- Mouse behaviour depends which view is in use...
                    -- Right-click or ctrl-click to mean rotate; otherwise pan.
                    let
                        alternate =
                            event.keys.ctrl || event.button == SecondButton

                        newContext =
                            Just
                                { context
                                    | orbiting = Just event.offsetPos
                                    , dragAction =
                                        if alternate then
                                            DragRotate

                                        else
                                            DragPan
                                    , waitingForClickDelay = True
                                    , followSelectedPoint = False
                                }
                    in
                    ( { model | viewContext = newContext }
                    , Delay.after 250 (msgWrapper ClickDelayExpired)
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
                            ( { model | viewContext = Just newContext }
                            , Cmd.none
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
                            ( { model | viewContext = Just newContext }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                ImageRelease event ->
                    let
                        newContext =
                            Just
                                { context
                                    | orbiting = Nothing
                                    , dragAction = DragNone
                                }
                    in
                    ( { model | viewContext = newContext }
                    , Cmd.none
                    )

                ImageDoubleClick event ->
                    ( model, Cmd.none )

                ClickDelayExpired ->
                    ( { model | viewContext = Just { context | waitingForClickDelay = False } }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


multiplyDistanceBy : Float -> ContextThirdPerson -> Maybe ContextThirdPerson
multiplyDistanceBy factor context =
    Just { context | cameraDistance = context.cameraDistance |> Quantity.multiplyBy factor }


initialiseView : Int -> PeteTree -> ContextThirdPerson
initialiseView current treeNode =
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
    }
