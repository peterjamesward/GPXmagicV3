module Tools.Timestamp exposing (..)

import Actions exposing (ToolAction(..))
import ColourPalette exposing (warningColor)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (button)
import FeatherIcons
import FlatColors.AmericanPalette
import FlatColors.BritishPalette
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
    , desiredStartMillis = 0
    , desiredTickIntervalMillis = 1000
    , endLockedToStart = True
    }


type Msg
    = ApplyNewTimes
      --| DisplayInfo String String
    | SetTickInterval Int
    | TimeChange Int
    | ClearMilliseconds
    | DoubleRelativeTimes
    | ApplyTickInterval Int


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
    , oldPoints
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
    --TODO: Map over all the times, interpolating the model as we go. It's that easy.
    let
        startTimeAbsolute =
            DomainModel.earthPointFromIndex 0 track.trackTree
                |> .time

        lastCurrentTime =
            DomainModel.getLastLeaf track.trackTree
                |> .endPoint
                |> .time

        newCourse baselineStart baselineEnd =
            let
                howManyTicks =
                    (baselineEnd - baselineStart) // tickSpacing

                ticks =
                    List.range 0 howManyTicks

                pointByTickNumber tick =
                    DomainModel.interpolateGpxByTime
                        track.trackTree
                        (Time.millisToPosix <| tick * tickSpacing)
            in
            List.map pointByTickNumber ticks

        newTree baselineStart baselineEnd =
            DomainModel.treeFromSourcePoints <|
                newCourse baselineStart baselineEnd

        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    in
    case ( startTimeAbsolute, lastCurrentTime ) of
        ( Just baselineStart, Just baselineEnd ) ->
            ( newTree (Time.posixToMillis baselineStart) (Time.posixToMillis baselineEnd)
            , oldPoints
            )

        _ ->
            ( Nothing, [] )


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
        ( Just track, SetTickInterval interval ) ->
            let
                newOptions =
                    { options | desiredTickIntervalMillis = interval }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, TimeChange change ) ->
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

        ( Just track, ClearMilliseconds ) ->
            let
                newOptions =
                    { options
                        | desiredStartMillis =
                            1000 * (options.desiredStartMillis // 1000)
                    }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, ApplyNewTimes ) ->
            ( options
            , [ Actions.AdjustTimes options
              , TrackHasChanged
              ]
            )

        ( Just track, ApplyTickInterval tick ) ->
            ( options
            , [ Actions.SetTimeTicks tick
              , TrackHasChanged
              ]
            )

        ( Just track, DoubleRelativeTimes ) ->
            ( options
            , [ Actions.TimeDoubling
              , TrackHasChanged
              ]
            )

        ( Nothing, _ ) ->
            ( options, [] )


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

        removeTimes =
            none
    in
    column
        [ padding 5
        , spacing 5
        , width <| px 300
        , centerX
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ none
        , startTimeAdjustments
        , equiSpacing
        , doubleTimes
        , removeTimes
        ]


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial wrapper options mTrack =
    case mTrack of
        Just isTrack ->
            viewWithTrack location imperial wrapper options isTrack

        Nothing ->
            noTrackMessage location