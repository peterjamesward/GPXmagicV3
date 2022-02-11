module Tools.TrackInfoBox exposing (..)

import Angle
import Axis2d
import Direction2d
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (labelHidden)
import FlatColors.ChinesePalette
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showDecimal2, showLongMeasure, showShortMeasure)


type alias Options =
    { displayMode : InformationContext
    , memoryInfo : Maybe MemoryInfo
    }


type alias MemoryInfo =
    { jsHeapSizeLimit : Int
    , totalJSHeapSize : Int
    , usedJSHeapSize : Int
    }


type InformationContext
    = InfoForTrack
    | InfoForPoint
    | InfoForSystem


defaultOptions : Options
defaultOptions =
    { displayMode = InfoForTrack
    , memoryInfo = Nothing
    }


type Msg
    = ChooseDisplayMode InformationContext


update : Msg -> Options -> Options
update msg options =
    case msg of
        ChooseDisplayMode mode ->
            { options | displayMode = mode }


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


displayInfoForPoint : Bool -> TrackLoaded msg -> Element msg
displayInfoForPoint imperial track =
    let
        index =
            track.currentPosition

        leaf =
            leafFromIndex index track.trackTree |> asRecord

        distance =
            distanceFromIndex index track.trackTree

        { longitude, latitude, altitude } =
            Tuple.first leaf.sourceData

        bearing =
            leaf.directionAtStart
                |> Direction2d.angleFrom Direction2d.positiveY
                |> Angle.inDegrees
                |> negate

        labels =
            [ "Number"
            , "Distance"
            , "Longitude"
            , "Latitude"
            , "Altitude"
            , "Bearing"
            , "Gradient"
            ]
    in
    row
        [ padding 10
        , spacing 5
        ]
        [ column [ spacing 5 ] <| List.map text labels
        , column [ spacing 5 ]
            [ text <| String.fromInt index
            , text <| showLongMeasure imperial distance
            , text <| showDecimal2 <| Angle.inDegrees <| Direction2d.toAngle longitude
            , text <| showDecimal2 <| Angle.inDegrees <| latitude
            , text <| showShortMeasure imperial altitude
            , text <| showDecimal2 <| bearing
            , text <| showDecimal2 leaf.gradientAtStart
            ]
        ]


displayValuesWithTrack :
    Bool
    -> List ( Element msg, Bool -> RoadSection -> Element msg )
    -> TrackLoaded msg
    -> Element msg
displayValuesWithTrack imperial infoList track =
    let
        info =
            asRecord <| track.trackTree
    in
    row
        [ padding 10
        , spacing 5
        ]
        [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> txt) infoList
        , column [ spacing 5 ] <| List.map (\( _, fn ) -> fn imperial info) infoList
        ]


view : (Msg -> msg) -> Bool -> Maybe (TrackLoaded msg) -> Options -> Element msg
view wrapper imperial ifTrack options =
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case ifTrack of
            Just track ->
                column [ padding 5 ]
                    [ Input.radioRow [ centerX, spacing 5 ]
                        { onChange = wrapper << ChooseDisplayMode
                        , options =
                            [ Input.option InfoForTrack (text "Track")
                            , Input.option InfoForPoint (text "Point")
                            , Input.option InfoForSystem (text "Memory")
                            ]
                        , selected = Just options.displayMode
                        , label = labelHidden "Oj"
                        }
                    , case options.displayMode of
                        InfoForTrack ->
                            displayValuesWithTrack imperial trackInfoList track

                        InfoForPoint ->
                            displayInfoForPoint imperial track

                        InfoForSystem ->
                            displayMemoryDetails options
                    ]

            Nothing ->
                paragraph [ padding 10 ]
                    [ text "Information will show here when a track is loaded." ]


updateMemory : MemoryInfo -> Options -> Options
updateMemory memory options =
    { options | memoryInfo = Just memory }


displayMemoryDetails : Options -> Element msg
displayMemoryDetails options =
    let
        labels =
            [ "Heap limit"
            , "Heap size"
            , "Used heap"
            ]

        asMB value =
            (toFloat value
                / 1024
                / 1024
                |> showDecimal2
            )
                ++ "MB"
    in
    case options.memoryInfo of
        Nothing ->
            text "Not available"

        Just memory ->
            row
                [ padding 10
                , spacing 5
                ]
                [ column [ spacing 5 ] <| List.map text labels
                , column [ spacing 5 ]
                    [ text <| asMB memory.jsHeapSizeLimit
                    , text <| asMB memory.totalJSHeapSize
                    , text <| asMB memory.usedJSHeapSize
                    ]
                ]
