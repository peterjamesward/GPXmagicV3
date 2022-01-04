module ViewThirdPerson exposing (..)

import Angle exposing (Angle)
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
import Html.Events.Extra.Mouse as Mouse
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import ModelRecord exposing (ModelRecord)
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Spherical
import ViewContextThirdPerson exposing (ContextThirdPerson, DragAction(..))
import ViewPureStyles exposing (useIcon)
import Viewpoint3d


type Msg
    = ImageClick Mouse.Event
    | ImageZoomIn
    | ImageZoomOut
    | ImageReset
    | ImageNoOp


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
                [ htmlAttribute <| Mouse.onClick (msgWrapper << ImageClick)
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


deriveCamera : PeteTree -> ContextThirdPerson -> Camera3d Meters LocalCoords
deriveCamera treeNode context =
    let
        eyePoint =
            pointFromVector <|
                makeEarthVector
                    context.earthAzimuth
                    context.earthElevation
                    (context.cameraDistance |> Quantity.plus (Length.meters Spherical.meanRadius))

        cameraViewpoint =
            -- Fixed for now.
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
                    let
                        newContext =
                            { context
                                | cameraDistance = context.cameraDistance |> Quantity.multiplyBy 0.7
                                , focalPoint =
                                    treeNode
                                        |> leafFromIndex model.currentPosition
                                        |> startVector
                                        |> pointFromVector
                            }
                    in
                    ( { model
                        | viewContext = Just newContext
                      }
                    , Cmd.none
                    )

                ImageZoomOut ->
                    ( model, Cmd.none )

                ImageReset ->
                    ( model, Cmd.none )

                ImageNoOp ->
                    ( model, Cmd.none )

                ImageClick event ->
                    -- Click moves pointer but does not recentre view. (Double click will.)
                    ( { model | currentPosition = detectHit event model }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.map msgWrapper Cmd.none )


initialiseView : PeteTree -> ContextThirdPerson
initialiseView treeNode =
    { earthAzimuth = Direction2d.x
    , earthElevation = Angle.degrees 0
    , cameraAzimuth = Direction2d.x
    , cameraElevation = Angle.degrees 0
    , cameraDistance = Length.kilometers 1000
    , fieldOfView = Angle.degrees 45
    , orbiting = Nothing
    , dragAction = DragNone
    , zoomLevel = 10.0
    , defaultZoomLevel = 10.0
    , focalPoint = treeNode |> startVector |> pointFromVector
    , waitingForClickDelay = False
    }
