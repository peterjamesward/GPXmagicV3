module ViewThirdPerson exposing (..)

import Angle
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Border as Border
import FlatColors.ChinesePalette
import Html.Events.Extra.Mouse as Mouse
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Msg exposing (Msg(..))
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import Scene3d exposing (Entity, backgroundColor)
import Vector3d
import Viewpoint3d


view :
    { model
        | scene : List (Entity LocalCoords)
        , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
        , trackTree : Maybe PeteTree
        , currentPosition : Int
        , focusPoint : EarthPoint
    }
    -> Element Msg
view model =
    case model.trackTree of
        Just treeNode ->
            el
                [ htmlAttribute <| Mouse.onClick ImageClick
                , Border.width 2
                , Border.color FlatColors.ChinesePalette.peace
                ]
            <|
                html <|
                    Scene3d.cloudy
                        { camera = deriveCamera (leafFromIndex model.currentPosition treeNode) model.focusPoint
                        , dimensions = model.viewDimensions
                        , background = backgroundColor Color.lightBlue
                        , clipDepth = Length.meters 1
                        , entities = model.scene
                        , upDirection = positiveZ
                        }

        _ ->
            text "No track to show"


deriveCamera : PeteTree -> EarthPoint -> Camera3d Meters LocalCoords
deriveCamera treeNode focusPoint =
    let
        eyePoint =
            -- Interesting scale factor
            focusPoint |> Point3d.scaleAbout Point3d.origin 1.015

        cameraViewpoint =
            -- Fixed for now.
            Viewpoint3d.lookAt
                { eyePoint = eyePoint
                , focalPoint = focusPoint
                , upDirection = Direction3d.positiveZ
                }

        perspectiveCamera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees 20
                }
    in
    perspectiveCamera


detectHit :
    Mouse.Event
    ->
        { m
            | trackTree : Maybe PeteTree
            , currentPosition : Int
            , focusPoint : EarthPoint
            , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
        }
    -> Int
detectHit event model =
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
                    deriveCamera leaf model.focusPoint

                ray =
                    Camera3d.ray camera screenRectangle screenPoint
            in
            nearestToRay ray topNode

        _ ->
            0
