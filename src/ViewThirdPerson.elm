module ViewThirdPerson exposing (..)

import Angle
import Camera3d exposing (Camera3d)
import Color
import Direction2d
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
import Msg exposing (Msg(..))
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import SceneBuilder exposing (render3dView)
import Spherical
import Vector3d
import ViewingContext exposing (ViewingContext)
import Viewpoint3d


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 5
        , Background.color white
        , Font.size 40
        , padding 6
        , spacing 8
        , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ImageNoOp)
        , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ImageNoOp)
        , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ImageNoOp)
        , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ImageNoOp)
        ]
        [ Input.button []
            { onPress = Just ImageZoomIn
            , label = useIcon FeatherIcons.plus
            }
        , Input.button []
            { onPress = Just ImageZoomOut
            , label = useIcon FeatherIcons.minus
            }
        , Input.button []
            { onPress = Just ImageReset
            , label = useIcon FeatherIcons.maximize
            }
        ]


useIcon =
    html << FeatherIcons.toHtml [] << FeatherIcons.withSize 20


view :
    { model
        | scene : List (Entity LocalCoords)
        , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
        , trackTree : Maybe PeteTree
        , currentPosition : Int
    }
    -> ViewingContext
    -> Element Msg
view model context =
    case model.trackTree of
        Just treeNode ->
            el
                [ htmlAttribute <| Mouse.onClick ImageClick
                , Border.width 2
                , Border.color FlatColors.ChinesePalette.peace
                , inFront zoomButtons
                ]
            <|
                html <|
                    Scene3d.cloudy
                        { camera = context.camera
                        , dimensions = model.viewDimensions
                        , background = backgroundColor Color.lightBlue
                        , clipDepth = Length.meters 1
                        , entities = model.scene
                        , upDirection = positiveZ
                        }

        _ ->
            text "No track to show"


deriveCamera : PeteTree -> ViewingContext -> Camera3d Meters LocalCoords
deriveCamera treeNode context =
    let
        eyePoint =
            pointFromVector <|
                makeEarthVector
                    (Direction2d.fromAngle context.azimuth)
                    context.elevation
                    (context.distance |> Quantity.plus (Length.meters Spherical.meanRadius))

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
        }
    -> ViewingContext
    -> Int
detectHit event model context =
    --TODO: Move into view/pane/whatever it will be.
    case model.trackTree of
        Just topNode ->
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
    -> ( ModelRecord, Cmd Msg )
update msg model =
    case model.trackTree of
        Nothing ->
            ( model, Cmd.none )

        Just treeNode ->
            case msg of
                ImageZoomIn ->
                    let
                        oldContext =
                            model.viewContext

                        newDistance =
                            oldContext.distance |> Quantity.multiplyBy 0.7

                        newFocalPoint =
                            treeNode
                                |> leafFromIndex model.currentPosition
                                |> startVector
                                |> pointFromVector

                        newContext =
                            { oldContext
                                | distance = newDistance
                                , focalPoint = newFocalPoint
                            }

                        newCamera =
                            deriveCamera treeNode newContext

                        finalContext =
                            { newContext | camera = newCamera }
                    in
                    ( { model
                        | viewContext = finalContext
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
                    ( { model
                        | currentPosition = detectHit event model model.viewContext
                        , scene = SceneBuilder.render3dView model
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
