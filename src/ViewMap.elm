module ViewMap exposing (..)

import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import FlatColors.ChinesePalette
import Html.Attributes exposing (id)
import MapPortsController
import Pixels exposing (Pixels, inPixels)
import Quantity exposing (Quantity)
import TrackLoaded exposing (TrackLoaded)


type alias MapContext =
    { mapClickDebounce : Bool
    , lastMapClick : ( Float, Float )
    }


view :
    { model
        | contentArea : ( Quantity Int Pixels, Quantity Int Pixels )
        , track : Maybe TrackLoaded
    }
    -> (MapPortsController.MapMsg -> msg)
    -> Element msg
view model msgWrapper =
    let
        ( viewWidth, viewHeight ) =
            model.contentArea

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
            , Border.width 2
            , Border.color FlatColors.ChinesePalette.peace
            , htmlAttribute (id "map")
            ]
            none
        ]
