module Tools.TrackInfoBox exposing (..)

import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import FlatColors.ChinesePalette
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showDecimal2, showLongMeasure, showShortMeasure)


trackInfoList : List ( Element msg, Bool -> RoadSection -> Element msg )
trackInfoList =
    [ ( text "Points"
      , \imperial info -> info.skipCount |> (+) 1 |> String.fromInt |> text
      )
    , ( text "Length"
      , \imperial info -> info.trueLength |> showLongMeasure imperial |> text
      )
    , ( text "Ascent"
      , \imperial info -> info.altitudeGained |> showLongMeasure imperial |> text
      )
    , ( text "Descent"
      , \imperial info -> info.altitudeLost |> showLongMeasure imperial |> text
      )
    , ( text "Climbing"
      , \imperial info -> info.distanceClimbing |> showLongMeasure imperial |> text
      )
    , ( text "Descending"
      , \imperial info -> info.distanceDescending |> showLongMeasure imperial |> text
      )
    , ( text "Steepest"
      , \imperial info -> info.steepestClimb |> showDecimal2 |> text
      )
    ]


trackInfoBox : Maybe (TrackLoaded msg) -> Bool -> Element msg
trackInfoBox maybeTrack imperial =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case maybeTrack of
            Just track ->
                let
                    info =
                        asRecord <| track.trackTree
                in
                row
                    [ padding 10
                    , spacing 5
                    ]
                    [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> txt) trackInfoList
                    , column [ spacing 5 ] <| List.map (\( _, fn ) -> fn imperial info) trackInfoList
                    ]

            Nothing ->
                row
                    [ padding 10
                    , spacing 5
                    ]
                    [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> txt) trackInfoList
                    , column [ spacing 5 ] <| List.map (always <| text "- no data -") trackInfoList
                    ]
