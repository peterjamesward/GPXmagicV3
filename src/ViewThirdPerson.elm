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
            let
                box =
                    DomainModel.boundingBox treeNode
            in
            el
                [ htmlAttribute <| Mouse.onClick ImageClick
                , Border.width 2
                , Border.color FlatColors.ChinesePalette.peace
                ]
            <|
                html <|
                    Scene3d.cloudy
                        { camera = deriveCamera box
                        , dimensions = model.viewDimensions
                        , background = backgroundColor Color.lightBlue
                        , clipDepth = Length.meters 1
                        , entities = model.scene
                        , upDirection = positiveZ
                        }

        _ ->
            text "No track to show"


deriveCamera : BoundingBox3d Meters LocalCoords -> Camera3d Meters LocalCoords
deriveCamera box =
    let
        ( xSize, ySize, zSize ) =
            BoundingBox3d.dimensions box

        largestEdge =
            xSize |> Quantity.max ySize |> Quantity.max zSize

        eyePoint =
            Point3d.xyz largestEdge (Quantity.negate largestEdge) largestEdge

        cameraViewpoint =
            -- Fixed for now.
            Viewpoint3d.lookAt
                { eyePoint = eyePoint
                , focalPoint = BoundingBox3d.centerPoint box
                , upDirection = Direction3d.positiveZ
                }

        perspectiveCamera =
            Camera3d.perspective
                { viewpoint = cameraViewpoint
                , verticalFieldOfView = Angle.degrees 45
                }
    in
    perspectiveCamera
