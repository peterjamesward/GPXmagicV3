module ViewThirdPerson exposing (..)

import Angle exposing (Angle)
import Axis3d
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
import Html.Events as HE
import Html.Events.Extra.Mouse as Mouse exposing (Button(..))
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as D
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
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
        , trackTree : Maybe PeteTree
        , currentPosition : Int
        , viewContext : Maybe ContextThirdPerson
    }
    -> (Msg -> msg)
    -> Element msg
view model msgWrapper =
    case ( model.trackTree, model.viewContext ) of
        ( Just treeNode, Just context ) ->
            el
                [ htmlAttribute <| Mouse.onDown (ImageGrab >> msgWrapper)
                , htmlAttribute <| Mouse.onMove (ImageDrag >> msgWrapper)
                , htmlAttribute <| Mouse.onUp (ImageRelease >> msgWrapper)
                , htmlAttribute <| Mouse.onClick (ImageClick >> msgWrapper)
                , htmlAttribute <| Mouse.onDoubleClick (ImageDoubleClick >> msgWrapper)
                , htmlAttribute <| Wheel.onWheel (\event -> msgWrapper (ImageMouseWheel event.deltaY))
                , onContextMenu (msgWrapper ImageNoOp)
                , width fill
                , pointer
                , Border.width 2
                , Border.color FlatColors.ChinesePalette.peace
                , inFront <| zoomButtons msgWrapper
                ]
            <|
                html <|
                    Scene3d.cloudy
                        { camera = deriveCamera treeNode context
                        , dimensions = model.viewDimensions
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


viewpoint : Viewpoint3d Meters LocalCoords
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.centimeters 20 10 10
        , upDirection = Direction3d.positiveZ
        }


deriveCamera : PeteTree -> ContextThirdPerson -> Camera3d Meters LocalCoords
deriveCamera treeNode context =
    let
        directionToEye =
            Direction3d.xyZ
                (context.cameraAzimuth |> Direction2d.toAngle)
                context.cameraElevation

        eyePoint =
            context.focalPoint
                |> Point3d.translateBy (Vector3d.withLength context.cameraDistance directionToEye)

        cameraViewpoint =
            Viewpoint3d.lookAt
                { eyePoint = eyePoint
                , focalPoint = context.focalPoint
                , upDirection = Direction3d.positiveZ
                }

        perspectiveCamera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    perspectiveCamera


detectHit :
    Mouse.Event
    ->
        { m
            | trackTree : Maybe PeteTree
            , currentPosition : Int
            , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
            , viewContext : Maybe ContextThirdPerson
        }
    -> Int
detectHit event model =
    --TODO: Move into view/pane/whatever it will be.
    case ( model.trackTree, model.viewContext ) of
        ( Just topNode, Just context ) ->
            let
                leaf =
                    leafFromIndex model.currentPosition topNode

                ( x, y ) =
                    event.offsetPos

                screenPoint =
                    Point2d.pixels x y

                ( w, h ) =
                    model.viewDimensions

                ( wFloat, hFloat ) =
                    ( toFloatQuantity w, toFloatQuantity h )

                screenRectangle =
                    Rectangle2d.from
                        (Point2d.xy Quantity.zero hFloat)
                        (Point2d.xy wFloat Quantity.zero)

                camera =
                    -- Must use same camera derivation as for the 3D model, else pointless!
                    deriveCamera leaf context

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
                    ( { model | viewContext = multiplyDistanceBy 0.7 context }
                    , Cmd.none
                    )

                ImageZoomOut ->
                    ( { model | viewContext = multiplyDistanceBy (1 / 0.7) context }
                    , Cmd.none
                    )

                ImageReset ->
                    ( { model | viewContext = Just <| initialiseView model.currentPosition treeNode }
                    , Cmd.none
                    )

                ImageNoOp ->
                    ( model, Cmd.none )

                ImageClick event ->
                    -- Click moves pointer but does not recentre view. (Double click will.)
                    ( { model | currentPosition = detectHit event model }
                    , Cmd.none
                    )

                ImageMouseWheel deltaY ->
                    ( { model | viewContext = multiplyDistanceBy (1.001 ^ deltaY) context }
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
                            ( model, Cmd.none )

                        ( DragPan, Just ( startX, startY ) ) ->
                            -- Change the camera azimuth and elevation
                            let
                                rotationRate =
                                    Angle.degrees 1 |> Quantity.per Pixels.pixel

                                azimuthChange =
                                    (startX - dx) |> Pixels.pixels |> Quantity.at rotationRate

                                elevationChange =
                                    (dy - startY) |> Pixels.pixels |> Quantity.at rotationRate

                                newContext =
                                    Just
                                        { context
                                            | orbiting = Just ( dx, dy )
                                            , cameraAzimuth =
                                                context.cameraAzimuth
                                                    |> Direction2d.rotateBy azimuthChange
                                            , cameraElevation =
                                                context.cameraElevation
                                                    |> Quantity.plus elevationChange
                                        }
                            in
                            ( { model | viewContext = newContext }
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
                                    , waitingForClickDelay = False
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
    , cameraElevation = Angle.degrees 0
    , cameraDistance = Length.kilometers 1000
    , fieldOfView = Angle.degrees 45
    , orbiting = Nothing
    , dragAction = DragNone
    , zoomLevel = 10.0
    , defaultZoomLevel = 10.0
    , focalPoint =
        treeNode |> leafFromIndex current |> startPoint
    , waitingForClickDelay = False
    }
