module ViewThirdPerson exposing (..)

import Angle
import BoundingBox3d exposing (BoundingBox3d)
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
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity, backgroundColor)
import Spherical
import Vector3d
import Viewpoint3d


view :
    { model
        | scene : List (Entity LocalCoords)
        , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
        , trackTree : Maybe PeteTree
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
                        { camera = deriveCamera treeNode
                        , dimensions = model.viewDimensions
                        , background = backgroundColor Color.lightBlue
                        , clipDepth = Length.meters 1
                        , entities = model.scene
                        , upDirection = positiveZ
                        }

        _ ->
            text "No track to show"


deriveCamera : PeteTree -> Camera3d Meters LocalCoords
deriveCamera treeNode =
    let
        eyePoint =
            -- Interesting scale factor
            Point3d.origin
                |> Point3d.translateBy
                    (treeNode |> startVector |> Vector3d.scaleBy 1.01)

        cameraViewpoint =
            -- Fixed for now.
            Viewpoint3d.lookAt
                { eyePoint = eyePoint
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }

        perspectiveCamera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees 45
                }
    in
    perspectiveCamera
