module TrackInfoBox exposing (..)

import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import FlatColors.ChinesePalette
import UtilsForViews exposing (showDecimal0, showDecimal2, showLongMeasure, showShortMeasure)


trackInfoList : List ( Element msg, PeteTree -> Element msg )
trackInfoList =
    [ ( text "Points", asRecord >> .skipCount >> (+) 1 >> String.fromInt >> text )
    , ( text "Length", asRecord >> .trueLength >> showLongMeasure False >> text )
    , ( text "Ascent", asRecord >> .altitudeGained >> showLongMeasure False >> text )
    , ( text "Descent", asRecord >> .altitudeLost >> showLongMeasure False >> text )
    , ( text "Climbing", asRecord >> .distanceClimbing >> showLongMeasure False >> text )
    , ( text "Descending", asRecord >> .distanceDescending >> showLongMeasure False >> text )
    , ( text "Steepest", asRecord >> .steepestClimb >> showDecimal2 >> text )
    ]


trackInfoBox : Maybe PeteTree -> Element msg
trackInfoBox maybeTree =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case maybeTree of
            Just trackTree ->
                row
                    [ padding 10
                    , spacing 5
                    ]
                    [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> txt) trackInfoList
                    , column [ spacing 5 ] <| List.map (\( _, fn ) -> fn trackTree) trackInfoList
                    ]

            Nothing ->
                row
                    [ padding 10
                    , spacing 5
                    ]
                    [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> txt) trackInfoList
                    , column [ spacing 5 ] <| List.map (always <| text "- no data -") trackInfoList
                    ]
