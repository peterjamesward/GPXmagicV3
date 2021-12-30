module ViewMap exposing (..)

import Angle
import BoundingBox3d exposing (BoundingBox3d)
import Camera3d exposing (Camera3d)
import Color exposing (white)
import Direction3d exposing (positiveZ)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FlatColors.ChinesePalette
import Html.Attributes exposing (id)
import Html.Events.Extra.Mouse as Mouse
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Msg exposing (Msg(..))
import Pixels exposing (Pixels, inPixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity, backgroundColor)
import Viewpoint3d


view :
    { model
        | viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
        , trackTree : Maybe PeteTree
    }
    -> Element Msg
view model =
    let
        ( viewWidth, viewHeight ) =
            model.viewDimensions

        handyMapControls =
            column
                [ alignTop
                , alignRight
                , moveDown 100
                , moveLeft 10
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                , Font.size 40
                , padding 6
                , spacing 8
                ]
                [ button []
                    { onPress = Nothing
                    , label = text "x"
                    }
                ]
    in
    row
        [ spacing 0
        , padding 0
        --, inFront handyMapControls
        ]
        [ el
            [ width <| px <| inPixels viewWidth
            , height <| px <| inPixels viewHeight
            , alignLeft
            , alignTop
            , htmlAttribute (id "map")
            ]
            none
        ]
