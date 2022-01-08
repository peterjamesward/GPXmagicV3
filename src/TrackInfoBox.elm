module TrackInfoBox exposing (..)

import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import FlatColors.AussiePalette
import FlatColors.SpanishPalette
import Length
import UtilsForViews exposing (showDecimal2, showLongMeasure)


trackInfoList : List ( Element msg, PeteTree -> Element msg)
trackInfoList =
    [ ( text "Points", \tree -> text <| String.fromInt <| skipCount tree + 1 )
    , ( text "Length", \tree -> text <| showLongMeasure False <| trueLength tree )
    ]


trackInfoBox : Maybe PeteTree -> Element msg
trackInfoBox maybeTree =
    case maybeTree of
        Just trackTree ->
            row
                [ Background.color FlatColors.AussiePalette.beekeeper
                , Border.rounded 10
                , padding 10
                , spacing 5
                ]
                [ column [ spacing 5 ] <| List.map (\(txt, _) -> txt) trackInfoList
                , column [ spacing 5 ] <| List.map (\(_, fn) -> fn trackTree ) trackInfoList
                ]

        Nothing ->
            el [ centerX, centerY ] <| text "No track loaded"
