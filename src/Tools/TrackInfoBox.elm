module Tools.TrackInfoBox exposing (..)

import Actions exposing (ToolAction)
import Angle
import Axis2d
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (labelHidden)
import FlatColors.ChinesePalette
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal0, showDecimal2, showDecimal6, showLongMeasure, showShortMeasure)


toolId =
    "info"


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
    | DisplayInfo String String


update : Msg -> Options -> Options
update msg options =
    case msg of
        ChooseDisplayMode mode ->
            { options | displayMode = mode }

        DisplayInfo _ _ ->
            options


trackInfoList : List ( String, Bool -> RoadSection -> Element msg )
trackInfoList =
    [ ( "points"
      , \imperial info -> info.skipCount |> (+) 1 |> String.fromInt |> text
      )
    , ( "length"
      , \imperial info -> info.trueLength |> showLongMeasure imperial |> text
      )
    , ( "ascent"
      , \imperial info -> info.altitudeGained |> showLongMeasure imperial |> text
      )
    , ( "descent"
      , \imperial info -> info.altitudeLost |> showLongMeasure imperial |> text
      )
    , ( "climbing"
      , \imperial info -> info.distanceClimbing |> showLongMeasure imperial |> text
      )
    , ( "descending"
      , \imperial info -> info.distanceDescending |> showLongMeasure imperial |> text
      )
    , ( "steepest"
      , \imperial info -> info.steepestClimb |> showDecimal2 |> text
      )
    ]


displayInfoForPoint : I18NOptions.Location -> Bool -> TrackLoaded msg -> Element msg
displayInfoForPoint location imperial track =
    let
        index =
            track.currentPosition

        ( leaf, gpxPoint ) =
            ( leafFromIndex index track.trackTree |> asRecord
            , gpxPointFromIndex index track.trackTree
            )

        distance =
            distanceFromIndex index track.trackTree

        { longitude, latitude, altitude } =
            gpxPoint

        bearing =
            leaf.directionAtStart
                |> Direction2d.angleFrom Direction2d.positiveY
                |> Angle.inDegrees
                |> negate

        labels =
            [ "number"
            , "distance"
            , "longitude"
            , "latitude"
            , "altitude"
            , "bearing"
            , "gradient"
            ]
    in
    row
        [ padding 10
        , spacing 5
        ]
        [ column [ spacing 5 ] <| List.map (I18N.text location "info") labels
        , column [ spacing 5 ]
            [ text <| String.fromInt index
            , text <| showLongMeasure imperial distance
            , text <| UtilsForViews.longitudeString <| Direction2d.toAngle longitude
            , text <| UtilsForViews.latitudeString <| latitude
            , text <| showShortMeasure imperial altitude
            , text <| showDecimal2 <| bearing
            , text <| showDecimal2 leaf.gradientAtStart
            ]
        ]


displayValuesWithTrack :
    I18NOptions.Location
    -> Bool
    -> List ( String, Bool -> RoadSection -> Element msg )
    -> TrackLoaded msg
    -> Element msg
displayValuesWithTrack location imperial infoList track =
    let
        info =
            asRecord <| track.trackTree
    in
    row
        [ padding 10
        , spacing 5
        ]
        [ column [ spacing 5 ] <| List.map (\( txt, _ ) -> I18N.text location "info" txt) infoList
        , column [ spacing 5 ] <| List.map (\( _, fn ) -> fn imperial info) infoList
        ]


view : I18NOptions.Location -> (Msg -> msg) -> Bool -> Maybe (TrackLoaded msg) -> Options -> Element msg
view location wrapper imperial ifTrack options =
    let
        helper =
            I18N.text location "info"
    in
    el [ width fill, Background.color FlatColors.ChinesePalette.antiFlashWhite ] <|
        case ifTrack of
            Just track ->
                column [ padding 5 ]
                    [ Input.radioRow [ centerX, spacing 5 ]
                        { onChange = wrapper << ChooseDisplayMode
                        , options =
                            [ Input.option InfoForTrack (helper "track")
                            , Input.option InfoForPoint (helper "point")
                            , Input.option InfoForSystem (helper "memory")
                            ]
                        , selected = Just options.displayMode
                        , label = labelHidden "mode"
                        }
                    , case options.displayMode of
                        InfoForTrack ->
                            displayValuesWithTrack location imperial trackInfoList track

                        InfoForPoint ->
                            displayInfoForPoint location imperial track

                        InfoForSystem ->
                            displayMemoryDetails location options
                    ]

            Nothing ->
                paragraph [ padding 10 ]
                    [ helper "notrack" ]


updateMemory : MemoryInfo -> Options -> Options
updateMemory memory options =
    { options | memoryInfo = Just memory }


displayMemoryDetails : I18NOptions.Location -> Options -> Element msg
displayMemoryDetails location options =
    let
        labels =
            [ "limit"
            , "size"
            , "heap"
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
            I18N.text location "info" "none"

        Just memory ->
            row
                [ padding 10
                , spacing 5
                ]
                [ column [ spacing 5 ] <| List.map (I18N.text location "info") labels
                , column [ spacing 5 ]
                    [ text <| asMB memory.jsHeapSizeLimit
                    , text <| asMB memory.totalJSHeapSize
                    , text <| asMB memory.usedJSHeapSize
                    ]
                ]
