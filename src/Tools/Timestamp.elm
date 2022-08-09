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
    , startOffsetHours = 0
    , startOffsetMinutes = 0
    , startOffsetSeconds = 0
    , startOffsetMilliseconds = 0
    , endOffsetHours = 0
    , endOffsetMinutes = 0
    , endOffsetSeconds = 0
    , endOffsetMilliseconds = 0
    , desiredTickIntervalMillis = 1000
    , endLockedToStart = True
    }


type Msg
    = Apply
    | DisplayInfo String String
    | SetTickInterval Int
    | SetEndMode Bool


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
        ( Just track, SetTickInterval interval ) ->
            let
                newOptions =
                    { options | desiredTickIntervalMillis = interval }
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

        absoluteMillisToPoint : Int -> Int
        absoluteMillisToPoint pointIndex =
            DomainModel.earthPointFromIndex pointIndex track.trackTree
                |> .time
                |> Maybe.map Time.posixToMillis
                |> Maybe.withDefault 0

        trackStartTime : Int
        trackStartTime =
            absoluteMillisToPoint 0

        relativeMillisToPoint : Int -> Int
        relativeMillisToPoint pointIndex =
            absoluteMillisToPoint pointIndex - trackStartTime

        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentMarkers ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentOrangeToEnd ->
                    ( TrackLoaded.getRangeFromMarkers track |> Tuple.first
                    , 0
                    )

        endIndex =
            skipCount track.trackTree - fromEnd

        extent =
            el [ centerX, width fill ] <|
                paragraph [ centerX ] <|
                    case options.extent of
                        ExtentMarkers ->
                            [ i18n "ExtentMarkers" ]

                        ExtentOrangeToEnd ->
                            [ i18n "ExtentOrangeToEnd" ]

        startTimeAdjustments =
            column [ centerX, width fill, spacing 4, Border.width 1 ]
                [ row [ spaceEvenly ]
                    [ text "start absolute "
                    , UtilsForViews.formattedTime <|
                        Just <|
                            Time.millisToPosix <|
                                absoluteMillisToPoint <|
                                    fromStart
                    ]
                , row [ spaceEvenly ]
                    [ text "start relative "
                    , UtilsForViews.formattedTime <|
                        Just <|
                            Time.millisToPosix <|
                                relativeMillisToPoint <|
                                    fromStart
                    ]
                ]

        endTimeAdjustments =
            column [ centerX, width fill, spacing 4, Border.width 1 ]
                [ row [ spaceEvenly ]
                    [ text "end absolute "
                    , UtilsForViews.formattedTime <|
                        Just <|
                            Time.millisToPosix <|
                                absoluteMillisToPoint <|
                                    endIndex
                    ]
                , row [ spaceEvenly ]
                    [ text "end relative "
                    , UtilsForViews.formattedTime <|
                        Just <|
                            Time.millisToPosix <|
                                relativeMillisToPoint <|
                                    endIndex
                    ]
                ]

        equiSpacing =
            none

        doubleTimes =
            none

        removeTimes =
            none
    in
    column
        [ padding 5
        , spacing 5
        , width fill
        , centerX
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ none
        , extent
        , startTimeAdjustments
        , endTimeAdjustments
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
