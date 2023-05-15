module ViewDerivatives exposing
    ( Msg(..)
    , initialiseView
    , update
    , view
    )

import Chart as C
import Chart.Attributes as CA
import DomainModel exposing (..)
import Element exposing (..)
import Element.Border as Border
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, toFloatQuantity)
import Svg
import SystemSettings exposing (SystemSettings)
import Tools.DisplaySettingsOptions
import TrackLoaded exposing (TrackLoaded)
import ViewDerivativesContext exposing (DerivativesContext)


type Msg
    = WhatMessages


initialiseView : DerivativesContext
initialiseView =
    { dummy = 0 }


stopProp =
    { stopPropagation = True, preventDefault = False }


type alias GradientDatum =
    { startDistance : Float
    , endDistance : Float
    , gradient : Float
    }


view :
    DerivativesContext
    -> SystemSettings
    -> Tools.DisplaySettingsOptions.Options
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> TrackLoaded msg
    -> (Msg -> msg)
    -> Element msg
view context settings display ( contentAreaWidth, contentAreaHeight ) track msgWrapper =
    let
        datumFromRoad road ( distance, inputs ) =
            ( distance |> Quantity.plus road.trueLength
            , { startDistance = Length.inKilometers distance
              , endDistance = Length.inKilometers (distance |> Quantity.plus road.trueLength)
              , gradient = road.gradientAtStart
              }
                :: inputs
            )

        gradients =
            List.reverse <|
                Tuple.second <|
                    DomainModel.foldOverRoute
                        datumFromRoad
                        track.trackTree
                        ( Quantity.zero, [] )
    in
    el [ width fill, height fill, padding 20 ] <|
        html <|
            C.chart
                [ CA.height <| toFloat <| (Pixels.inPixels contentAreaHeight - 40)
                , CA.width <| toFloat <| (Pixels.inPixels contentAreaWidth - 40)
                , CA.domain
                    [ CA.lowest -5 CA.orLower
                    , CA.highest 5 CA.orHigher
                    ]
                ]
                [ C.xLabels []
                , C.yLabels [ CA.withGrid ]
                , C.bars
                    [ CA.x1 .startDistance
                    , CA.x2 .endDistance
                    , CA.margin 1
                    ]
                    [ C.bar .gradient [ CA.borderWidth 0.3, CA.opacity 0.5 ] ]
                    gradients
                ]


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> DerivativesContext
    -> DerivativesContext
update msg msgWrapper track area context =
    -- Second return value indicates whether selection needs to change.
    case msg of
        WhatMessages ->
            context
