module TrackInfoBox exposing (..)

import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette
import FlatColors.SpanishPalette
import Length
import UtilsForViews exposing (showDecimal0, showDecimal2, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (useIcon)


trackInfoList : List ( Element msg, PeteTree -> Element msg )
trackInfoList =
    [ ( text "Points", asRecord >> .skipCount >> (+) 1 >> String.fromInt >> text )
    , ( text "Length", asRecord >> .trueLength >> showLongMeasure False >> text )
    , ( text "Ascent", asRecord >> .altitudeGained >> showLongMeasure False >> text )
    , ( text "Descent", asRecord >> .altitudeLost >> showLongMeasure False >> text )
    , ( text "Climb distance", asRecord >> .distanceClimbing >> showLongMeasure False >> text )
    , ( text "Descent distance", asRecord >> .distanceDescending >> showLongMeasure False >> text )
    , ( text "Steepest", asRecord >> .steepestClimb >> String.fromFloat >> text )
    ]


trackInfoBox : Maybe PeteTree -> Element msg
trackInfoBox maybeTree =
    column [ width fill ]
        [ row
            [ width fill
            , padding 4
            , spacing 4
            , Font.color FlatColors.AussiePalette.beekeeper
            , Background.color FlatColors.ChinesePalette.bayWharf
            ]
            [ useIcon FeatherIcons.settings
            , text "The Route"
            ]
        , case maybeTree of
            Just trackTree ->
                row
                    [ Background.color FlatColors.AussiePalette.beekeeper
                    , Border.rounded 10
                    , padding 10
                    , spacing 5
                    ]
                    [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> txt) trackInfoList
                    , column [ spacing 5 ] <| List.map (\( _, fn ) -> fn trackTree) trackInfoList
                    ]

            Nothing ->
                el [ centerX, centerY ] <| text "No track loaded"
        ]
