module Tools.Timestamp exposing (..)

import Actions exposing (ToolAction(..))
import ColourPalette
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import Time
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.TimestampOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Utils
import UtilsForViews exposing (showShortMeasure)
import ViewPureStyles exposing (..)


toolId =
    "timestamps"


defaultOptions : Options
defaultOptions =
    { extent = ExtentOrangeToEnd
    , desiredRangeStartOffsetSeconds = 0
    , desiredRangeEndOffsetSeconds = 0
    , startMilliseconds = 0
    , endMilliseconds = 0
    , desiredTickIntervalMillis = 0
    , stretchTimes = False
    , precision = SliderOneSecond
    }


type Msg
    = Apply
    | SetExtent ExtentOption
    | DisplayInfo String String
    | SetRangeStartOffset Int
    | SetRangeEndOffset Int
    | SetStartMilliseconds Int
    | SetEndMilliseconds Int
    | SetTickInterval Int
    | SetSliderPrecision SliderPrecision


computeNewPoints : Bool -> Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints excludeExisting options track =
    let
        newPoints =
            []

        previews =
            -- But these are based on distance from first mark, need to
            -- be based on first point.
            TrackLoaded.asPreviewPoints
                track
                Quantity.zero
                newPoints
    in
    previews


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentMarkers ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentOrangeToEnd ->
                    ( TrackLoaded.getRangeFromMarkers track |> Tuple.first
                    , 0
                    )

        newCourse =
            computeNewPoints False options track
                |> List.map .gpx

        newTree =
            DomainModel.replaceRange
                fromStart
                (fromEnd + 1)
                track.referenceLonLat
                newCourse
                track.trackTree

        oldPoints =
            DomainModel.extractPointsInRange
                fromStart
                fromEnd
                track.trackTree
    in
    ( newTree
    , oldPoints |> List.map Tuple.second
    )


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            let
                newOptions =
                    { options
                        | extent =
                            case theTrack.markerPosition of
                                Just purple ->
                                    ExtentMarkers

                                Nothing ->
                                    ExtentOrangeToEnd
                    }
            in
            ( newOptions, actions newOptions colour theTrack )

        _ ->
            ( options, [ HidePreview "timestamps" ] )


actions : Options -> Color -> TrackLoaded msg -> List (ToolAction a)
actions newOptions previewColour track =
    --[ ShowPreview
    --    { tag = "timestamps"
    --    , shape = PreviewCircle
    --    , colour = previewColour
    --    , points = computeNewPoints True newOptions track
    --    }
    --]
    []


timeModuloBy : Int -> Time.Posix -> Time.Posix
timeModuloBy modulus time =
    time |> Time.posixToMillis |> modBy modulus |> Time.millisToPosix


timeRemainderBy : Int -> Time.Posix -> Time.Posix
timeRemainderBy modulus time =
    time |> Time.posixToMillis |> (//) modulus |> (*) modulus |> Time.millisToPosix


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetRangeStartOffset offset ) ->
            let
                newOptions =
                    { options
                        | desiredRangeStartOffsetSeconds = 0
                        , startMilliseconds = 0
                    }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetRangeEndOffset offset ) ->
            let
                newOptions =
                    { options
                        | desiredRangeEndOffsetSeconds = 0
                        , endMilliseconds = 0
                    }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetStartMilliseconds millis ) ->
            let
                newOptions =
                    { options | startMilliseconds = millis }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetEndMilliseconds millis ) ->
            let
                newOptions =
                    { options | endMilliseconds = millis }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetTickInterval interval ) ->
            let
                newOptions =
                    { options | desiredTickIntervalMillis = interval }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetSliderPrecision precision ) ->
            let
                newOptions =
                    { options | precision = precision }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, Apply ) ->
            let
                newOptions =
                    { options
                        | extent =
                            case track.markerPosition of
                                Just purple ->
                                    ExtentMarkers

                                Nothing ->
                                    ExtentOrangeToEnd
                    }
            in
            ( newOptions
            , [ Actions.AdjustTimes
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


sliderStyles =
    [ height <| px 20
    , width <| px 300
    , centerY
    , centerX
    , behindContent <|
        -- Slider track
        el
            [ width <| px 300
            , height <| px 2
            , centerY
            , centerX
            , Border.rounded 6
            , Background.color ColourPalette.scrollbarBackground
            ]
            Element.none
    ]


effectiveTime precision seconds milliseconds =
    case precision of
        SliderOneSecond ->
            Time.millisToPosix <| seconds * 1000

        _ ->
            Time.millisToPosix <| seconds * 1000 + milliseconds


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial wrapper options mTrack =
    let
        i18n =
            I18N.text location toolId

        trackStartTime : TrackLoaded msg -> Maybe Time.Posix
        trackStartTime track =
            DomainModel.getFirstLeaf track.trackTree
                |> .startPoint
                |> .time

        markedRegion track =
            case options.extent of
                ExtentMarkers ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentOrangeToEnd ->
                    ( TrackLoaded.getRangeFromMarkers track |> Tuple.first
                    , 0
                    )

        extent track =
            el [ centerX, width fill ] <|
                paragraph [ centerX ] <|
                    if track.markerPosition == Nothing then
                        [ i18n "ExtentOrangeToEnd" ]

                    else
                        [ i18n "ExtentMarkers" ]

        selectPrecision =
            Input.radioRow [ centerX, spacing 5 ]
                { onChange = wrapper << SetSliderPrecision
                , options =
                    [ Input.option SliderOneSecond (i18n "second")
                    , Input.option SliderHalfSecond (i18n "half")
                    , Input.option SliderMillisecond (i18n "millis")
                    ]
                , selected = Just options.precision
                , label = Input.labelHidden "precision"
                }

        timeSlider sliderMsg value =
            Input.slider sliderStyles
                { onChange = wrapper << sliderMsg << floor
                , label = Input.labelHidden "no label"
                , min = 10
                , max = 60 * 60 * 1000
                , step =
                    Just <|
                        case options.precision of
                            SliderOneSecond ->
                                1000

                            SliderHalfSecond ->
                                500

                            SliderMillisecond ->
                                -- Use secondary slider for this
                                1000
                , value = toFloat value
                , thumb = Input.defaultThumb
                }

        millisecondSlider sliderMsg value =
            Input.slider sliderStyles
                { onChange = floor >> sliderMsg >> wrapper
                , label = Input.labelHidden "milliseconds"
                , min = 0.0
                , max = 1000
                , step = Just 1
                , value = toFloat value
                , thumb = Input.defaultThumb
                }

        startTimeAdjustments track =
            column [ centerX, width fill, spacing 4, Border.width 1 ]
                [ wrappedRow [ spacing 6, padding 3 ]
                    [ i18n "start"

                    -- Show current start of range absolute time.
                    -- Show current start of range relative to start.
                    -- Show selected new start relative time.
                    ]
                , el [ centerX ] <| timeSlider SetRangeStartOffset options.desiredRangeStartOffsetSeconds
                , if options.precision == SliderMillisecond then
                    el [ centerX ] <| millisecondSlider SetStartMilliseconds options.startMilliseconds

                  else
                    none
                ]

        endTimeAdjustments track =
            column [ centerX, width fill, spacing 4, Border.width 1 ]
                [ wrappedRow [ spacing 6, padding 3 ]
                    [ i18n "end"

                    -- Show current end of range absolute time.
                    -- Show current end of range relative to start.
                    -- Show selected new end relative time.
                    ]
                , el [ centerX ] <| timeSlider SetRangeEndOffset options.desiredRangeEndOffsetSeconds
                , if options.precision == SliderMillisecond then
                    el [ centerX ] <| millisecondSlider SetEndMilliseconds options.endMilliseconds

                  else
                    none
                ]

        equiSpacing track =
            none

        doubleTimes track =
            none

        removeTimes track =
            none
    in
    case mTrack of
        Just isTrack ->
            column
                [ padding 5
                , spacing 5
                , width fill
                , centerX
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ none
                , el [ centerX ] <| extent isTrack
                , selectPrecision
                , startTimeAdjustments isTrack

                --TODO: "Stretch mode"
                --, if options.extent == ExtentMarkers then
                --    endTimeAdjustments isTrack
                --
                --  else
                --    none
                , equiSpacing isTrack
                , doubleTimes isTrack
                , removeTimes isTrack
                ]

        Nothing ->
            noTrackMessage location
