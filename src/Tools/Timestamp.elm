module Tools.Timestamp exposing (..)

import Acceleration exposing (Acceleration)
import Actions exposing (ToolAction(..))
import Angle
import ColourPalette exposing (warningColor)
import DomainModel exposing (..)
import Duration exposing (Duration)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (button, labelHidden)
import FeatherIcons
import FlatColors.AmericanPalette
import FlatColors.BritishPalette
import FlatColors.ChinesePalette
import Mass exposing (Mass)
import Point3d
import Power exposing (Power)
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import Speed exposing (Speed)
import Time
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.TimestampOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Utils
import UtilsForViews exposing (showDecimal0, showShortMeasure)
import ViewPureStyles exposing (..)


toolId =
    "timestamps"


defaultOptions : Options
defaultOptions =
    { extent = ExtentOrangeToEnd
    , desiredStartMillis = 0
    , desiredTickIntervalMillis = 1000
    , endLockedToStart = True
    , steadyPower = Power.watts 200
    , maxDownhill = Speed.kilometersPerHour 80
    , mass = Mass.kilograms 80
    , estimatedDuration = Quantity.zero
    , mode = Actual
    }


type Msg
    = ApplyNewTimes
      --| DisplayInfo String String
    | SetTickInterval Int
    | TimeChange Int
    | ClearMilliseconds
    | DoubleRelativeTimes
    | ApplyTickInterval Int
    | SetPower Power
    | SetMass Mass
    | SetTerminal Speed
    | ComputeTimes
    | SetMode TimestampMode


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


applyTimeShift : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyTimeShift options track =
    let
        orangeOffsetMillis =
            relativeMillisToPoint track.currentPosition track

        requiredAdjustment =
            Just <| Time.millisToPosix <| options.desiredStartMillis - orangeOffsetMillis

        adjustedPoint gpx =
            { gpx | timestamp = Utils.addTimes gpx.timestamp requiredAdjustment }

        newCourse =
            List.map adjustedPoint oldPoints

        newTree =
            DomainModel.replaceRange
                track.currentPosition
                0
                track.referenceLonLat
                newCourse
                track.trackTree

        oldPoints =
            List.map Tuple.second <|
                DomainModel.extractPointsInRange
                    track.currentPosition
                    0
                    track.trackTree
    in
    ( newTree
    , DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    )


applyDoubling : TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyDoubling track =
    let
        startTimeAbsolute =
            DomainModel.earthPointFromIndex 0 track.trackTree
                |> .time

        adjustedPoint gpx =
            let
                relative =
                    Utils.subtractTimes startTimeAbsolute gpx.timestamp
            in
            { gpx | timestamp = Utils.addTimes gpx.timestamp relative }

        newCourse =
            List.map adjustedPoint oldPoints

        newTree =
            DomainModel.treeFromSourcePoints newCourse

        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    in
    case startTimeAbsolute of
        Just baseline ->
            ( newTree
            , oldPoints
            )

        Nothing ->
            ( Nothing, [] )


applyTicks : Int -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyTicks tickSpacing track =
    let
        routeStartTime =
            DomainModel.earthPointFromIndex 0 track.trackTree
                |> .time
                |> Maybe.map (\t -> (Time.posixToMillis t // 1000) * 1000)
                |> Maybe.withDefault 0

        ( _, newCourse ) =
            DomainModel.foldOverRoute
                emitTicks
                track.trackTree
                ( routeStartTime
                , []
                )

        emitTicks : RoadSection -> ( Int, List GPXSource ) -> ( Int, List GPXSource )
        emitTicks road ( nextTick, reversedOutputs ) =
            case ( road.startPoint.time, road.endPoint.time ) of
                ( Just start, Just end ) ->
                    if nextTick < Time.posixToMillis end then
                        -- At least one (more) tick in this road section
                        emitTicks
                            road
                            ( nextTick + tickSpacing
                            , interpolateAtTime nextTick road :: reversedOutputs
                            )

                    else
                        -- No (more) ticks in this road section
                        ( nextTick, reversedOutputs )

                _ ->
                    ( 0, [] )

        interpolateAtTime : Int -> RoadSection -> GPXSource
        interpolateAtTime tick road =
            case ( road.transitTime, road.startPoint.time ) of
                ( Just transit, Just start ) ->
                    let
                        numerator =
                            tick - Time.posixToMillis start

                        denominator =
                            Time.posixToMillis transit

                        proportion =
                            toFloat numerator / toFloat denominator
                    in
                    DomainModel.gpxFromPointWithReference
                        track.referenceLonLat
                        { time = Just <| Time.millisToPosix tick
                        , space =
                            Point3d.interpolateFrom
                                road.startPoint.space
                                road.endPoint.space
                                proportion
                        }

                _ ->
                    Tuple.first road.sourceData

        newTree =
            DomainModel.treeFromSourcePoints <| List.reverse newCourse

        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    in
    ( newTree, oldPoints )


applyPhysics : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyPhysics options track =
    -- Returns a track with new timestamps.
    let
        initialPoint =
            DomainModel.gpxPointFromIndex 0 track.trackTree

        ( _, newCourse ) =
            DomainModel.foldOverRoute
                computeSpeedAndTimes
                track.trackTree
                ( Quantity.zero
                , [ { initialPoint | timestamp = Just <| Time.millisToPosix 0 } ]
                )

        computeSpeedAndTimes :
            RoadSection
            -> ( Duration, List GPXSource )
            -> ( Duration, List GPXSource )
        computeSpeedAndTimes road ( inputTime, reversedOutputs ) =
            let
                untimedNextPoint =
                    Tuple.second road.sourceData

                thisSection =
                    durationForSection options.steadyPower road

                cumulative =
                    inputTime |> Quantity.plus thisSection
            in
            ( cumulative
            , { untimedNextPoint
                | timestamp =
                    Just <|
                        Time.millisToPosix <|
                            floor <|
                                Duration.inMilliseconds cumulative
              }
                :: reversedOutputs
            )

        newTree =
            DomainModel.treeFromSourcePoints <| List.reverse newCourse

        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    in
    ( newTree, oldPoints )


durationForSection : Power -> RoadSection -> Duration
durationForSection power section =
    {-
       Adopting Dan Connelly's idea to not use power but just derive speed from slope
       e.g. y = 30 - 2x + 0.002x^3 , x clamped to [-20, + 20] gives max 55kph, min 5kph.
       But modify that with (say) uplift speed by sqrt(power/200).
    -}
    let
        effectiveGradient =
            clamp -20 20 section.gradientAtStart

        baselineSpeed =
            -- Yes, this is our "physics model". Derive kph from gradient.
            30 - (2 * effectiveGradient) + 0.002 * effectiveGradient ^ 3

        modifiedSpeed =
            -- TODO: Factor out to just modify the overall time!
            Quantity.ratio power defaultOptions.steadyPower
                |> sqrt
                |> (*) baselineSpeed
                |> Speed.kilometersPerHour

        duration =
            section.trueLength |> Quantity.at_ modifiedSpeed
    in
    duration


estimatedTime : Power -> TrackLoaded msg -> Duration
estimatedTime power track =
    -- Returns only the total new duration.
    let
        initialPoint =
            DomainModel.gpxPointFromIndex 0 track.trackTree

        newDuration =
            DomainModel.foldOverRoute
                computeSpeedAndTimes
                track.trackTree
                Quantity.zero

        computeSpeedAndTimes : RoadSection -> Duration -> Duration
        computeSpeedAndTimes road inputTime =
            let
                thisSectionDuration =
                    durationForSection power road
            in
            inputTime |> Quantity.plus thisSectionDuration
    in
    newDuration


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
                        | extent = ExtentOrangeToEnd
                        , desiredStartMillis = relativeMillisToPoint theTrack.currentPosition theTrack
                        , estimatedDuration = estimatedTime options.steadyPower theTrack
                        , mode =
                            if trackHasTimestamps theTrack then
                                options.mode

                            else
                                Estimated
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
    case hasTrack of
        Just track ->
            updateWithTrack msg options previewColour track

        Nothing ->
            ( options, [] )


updateWithTrack :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
updateWithTrack msg options previewColour track =
    case msg of
        SetMode mode ->
            let
                newOptions =
                    { options | mode = mode }
            in
            ( newOptions, actions newOptions previewColour track )

        SetTickInterval interval ->
            let
                newOptions =
                    { options | desiredTickIntervalMillis = interval }
            in
            ( newOptions, actions newOptions previewColour track )

        SetPower watts ->
            let
                newOptions =
                    { options
                        | steadyPower = watts
                        , estimatedDuration = estimatedTime watts track
                    }
            in
            ( newOptions, actions newOptions previewColour track )

        SetMass kg ->
            let
                newOptions =
                    { options | mass = kg }
            in
            ( newOptions, actions newOptions previewColour track )

        SetTerminal kph ->
            let
                newOptions =
                    { options | maxDownhill = kph }
            in
            ( newOptions, actions newOptions previewColour track )

        TimeChange change ->
            let
                previousPointOffsetMillis =
                    relativeMillisToPoint (track.currentPosition - 1) track

                newOptions =
                    { options
                        | desiredStartMillis =
                            options.desiredStartMillis
                                + change
                                |> max previousPointOffsetMillis
                    }
            in
            ( newOptions, actions newOptions previewColour track )

        ClearMilliseconds ->
            let
                newOptions =
                    { options
                        | desiredStartMillis =
                            1000 * (options.desiredStartMillis // 1000)
                    }
            in
            ( newOptions, actions newOptions previewColour track )

        ApplyNewTimes ->
            ( options
            , [ Actions.AdjustTimes options
              , TrackHasChanged
              ]
            )

        ApplyTickInterval tick ->
            ( options
            , [ Actions.SetTimeTicks tick
              , TrackHasChanged
              ]
            )

        DoubleRelativeTimes ->
            ( options
            , [ Actions.TimeDoubling
              , TrackHasChanged
              ]
            )

        ComputeTimes ->
            ( options
            , [ Actions.UsePhysicsModel
              , TrackHasChanged
              ]
            )


absoluteMillisToPoint : Int -> TrackLoaded msg -> Int
absoluteMillisToPoint pointIndex track =
    DomainModel.earthPointFromIndex pointIndex track.trackTree
        |> .time
        |> Maybe.map Time.posixToMillis
        |> Maybe.withDefault 0


relativeMillisToPoint : Int -> TrackLoaded msg -> Int
relativeMillisToPoint pointIndex track =
    absoluteMillisToPoint pointIndex track - trackStartTime track


trackStartTime : TrackLoaded msg -> Int
trackStartTime track =
    absoluteMillisToPoint 0 track


trackHasTimestamps track =
    DomainModel.earthPointFromIndex 0 track.trackTree
        |> .time
        |> (/=) Nothing


viewWithTrack :
    I18NOptions.Location
    -> Bool
    -> (Msg -> msg)
    -> Options
    -> TrackLoaded msg
    -> Element msg
viewWithTrack location imperial wrapper options track =
    let
        i18n =
            I18N.text location toolId

        orangeMillis =
            Just <|
                Time.millisToPosix <|
                    absoluteMillisToPoint track.currentPosition track

        orangeOffsetMillis =
            Just <|
                Time.millisToPosix <|
                    relativeMillisToPoint track.currentPosition track

        previousPointOffsetMillis =
            relativeMillisToPoint (track.currentPosition - 1) track

        startTimeAdjustments =
            column [ centerX, width fill, spacing 4, padding 4, Border.width 1 ]
                [ row [ alignRight, spacing 10 ]
                    [ i18n "start absolute"
                    , UtilsForViews.formattedTime orangeMillis
                    ]
                , row [ alignRight, spacing 10 ]
                    [ i18n "start relative"
                    , UtilsForViews.formattedTime orangeOffsetMillis
                    ]

                --DONE: Add time display with up/down widgets for H,M,S,ms.
                , row [ alignRight, spacing 4 ]
                    [ i18n "desired start"
                    , column [ Border.width 1, Border.color FlatColors.AmericanPalette.soothingBreeze ]
                        [ Input.button [ centerX ]
                            { onPress = Just <| wrapper <| TimeChange 3600000
                            , label = useIconWithSize 12 FeatherIcons.chevronUp
                            }
                        , el [ alignRight ] <|
                            text <|
                                String.fromInt <|
                                    options.desiredStartMillis
                                        // 1000
                                        // 60
                                        // 60
                        , Input.button [ centerX ]
                            { onPress = Just <| wrapper <| TimeChange -3600000
                            , label = useIconWithSize 12 FeatherIcons.chevronDown
                            }
                        ]
                    , text ":"
                    , column [ Border.width 1, Border.color FlatColors.AmericanPalette.soothingBreeze ]
                        [ Input.button [ centerX ]
                            { onPress = Just <| wrapper <| TimeChange 60000
                            , label = useIconWithSize 12 FeatherIcons.chevronUp
                            }
                        , text <|
                            UtilsForViews.withLeadingZeros 2 <|
                                String.fromInt <|
                                    modBy 60 <|
                                        options.desiredStartMillis
                                            // 1000
                                            // 60
                        , Input.button [ centerX ]
                            { onPress = Just <| wrapper <| TimeChange -60000
                            , label = useIconWithSize 12 FeatherIcons.chevronDown
                            }
                        ]
                    , text ":"
                    , column [ Border.width 1, Border.color FlatColors.AmericanPalette.soothingBreeze ]
                        [ Input.button [ centerX ]
                            { onPress = Just <| wrapper <| TimeChange 1000
                            , label = useIconWithSize 12 FeatherIcons.chevronUp
                            }
                        , text <|
                            UtilsForViews.withLeadingZeros 2 <|
                                String.fromInt <|
                                    modBy 60 <|
                                        options.desiredStartMillis
                                            // 1000
                        , Input.button [ centerX ]
                            { onPress = Just <| wrapper <| TimeChange -1000
                            , label = useIconWithSize 12 FeatherIcons.chevronDown
                            }
                        ]
                    , text "."
                    , column [ Border.width 1, Border.color FlatColors.AmericanPalette.soothingBreeze ]
                        [ row []
                            [ Input.button [ centerX ]
                                { onPress = Just <| wrapper <| TimeChange 10
                                , label = useIconWithSize 12 FeatherIcons.chevronUp
                                }
                            , Input.button [ centerX ]
                                { onPress = Just <| wrapper <| ClearMilliseconds
                                , label = useIconWithSize 12 FeatherIcons.x
                                }
                            ]
                        , text <|
                            UtilsForViews.withLeadingZeros 3 <|
                                String.fromInt <|
                                    modBy 1000 <|
                                        options.desiredStartMillis
                        , Input.button [ centerX ]
                            { onPress = Just <| wrapper <| TimeChange -10
                            , label = useIconWithSize 12 FeatherIcons.chevronDown
                            }
                        ]
                    ]
                , if track.currentPosition == 0 then
                    paragraph
                        [ Background.color warningColor
                        , width fill
                        ]
                        [ i18n "atStart" ]

                  else if options.desiredStartMillis == previousPointOffsetMillis then
                    paragraph
                        [ Background.color warningColor
                        , width fill
                        ]
                        [ i18n "tooEarly" ]

                  else
                    Input.button (centerX :: neatToolsBorder)
                        { onPress = Just <| wrapper <| ApplyNewTimes
                        , label = paragraph [] [ i18n "apply" ]
                        }
                ]

        equiSpacing =
            column [ centerX, width fill, spacing 4, padding 4, Border.width 1 ]
                [ paragraph [] [ i18n "uniform" ]
                , el [ centerX, width fill ] <|
                    Input.radioRow [ spacing 8, width fill, centerX ]
                        { onChange = wrapper << SetTickInterval
                        , options =
                            [ Input.option 500 (i18n "half")
                            , Input.option 1000 (i18n "second")
                            , Input.option 5000 (i18n "five")
                            ]
                        , selected = Just options.desiredTickIntervalMillis
                        , label = Input.labelHidden "tick"
                        }
                , Input.button (centerX :: neatToolsBorder)
                    { onPress = Just <| wrapper <| ApplyTickInterval options.desiredTickIntervalMillis
                    , label = paragraph [] [ i18n "usetick" ]
                    }
                ]

        doubleTimes =
            column [ centerX, width fill, spacing 4, padding 4, Border.width 1 ]
                [ paragraph [ width fill ] [ i18n "doubling" ]
                , Input.button (centerX :: neatToolsBorder)
                    { onPress = Just (wrapper DoubleRelativeTimes)
                    , label = paragraph [] [ i18n "double" ]
                    }
                ]

        doSomePhysics =
            column [ centerX, width fill, spacing 4, padding 4, Border.width 1 ]
                [ paragraph [ width fill ] [ i18n "physics" ]
                , powerSlider
                , durationEstimate
                , Input.button (centerX :: neatToolsBorder)
                    { onPress = Just (wrapper ComputeTimes)
                    , label = paragraph [] [ i18n "applyPhysics" ]
                    }
                ]

        durationEstimate =
            row [ centerX, spacing 10 ]
                [ i18n "estimate"
                , UtilsForViews.formattedTime <|
                    Just <|
                        Time.millisToPosix <|
                            floor <|
                                Duration.inMilliseconds
                                    options.estimatedDuration
                ]

        powerSlider =
            el [ centerX ] <|
                Input.slider
                    commonShortHorizontalSliderStyles
                    { onChange = Power.watts >> SetPower >> wrapper
                    , label =
                        Input.labelBelow [ centerX ] <|
                            text <|
                                (UtilsForViews.showDecimal0 <|
                                    Power.inWatts options.steadyPower
                                )
                                    ++ "W"
                    , min = 80
                    , max = 400
                    , step = Just 10
                    , value = Power.inWatts options.steadyPower
                    , thumb = Input.defaultThumb
                    }

        modeSelection =
            Input.radio [ centerX, spacing 5 ]
                { onChange = wrapper << SetMode
                , options =
                    [ Input.option Actual (i18n "actual")
                    , Input.option Estimated (i18n "estimated")
                    ]
                , selected = Just options.mode
                , label = labelHidden "mode"
                }
    in
    column
        [ padding 5
        , spacing 5
        , width <| px 300
        , centerX
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
    <|
        if trackHasTimestamps track && options.mode == Actual then
            [ modeSelection
            , startTimeAdjustments
            , equiSpacing
            , doubleTimes
            ]

        else
            [ if trackHasTimestamps track then
                modeSelection

              else
                none
            , doSomePhysics
            ]


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial wrapper options mTrack =
    case mTrack of
        Just isTrack ->
            viewWithTrack location imperial wrapper options isTrack

        Nothing ->
            noTrackMessage location
