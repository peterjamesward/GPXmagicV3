module Tools.Nudge exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters)
import LineSegment2d
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d, xCoordinate, yCoordinate, zCoordinate)
import Polyline2d
import Quantity exposing (Quantity)
import Tools.NudgeOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector3d
import ViewPureStyles exposing (..)


type Msg
    = SetHorizontalNudgeFactor (Quantity Float Meters)
    | SetVerticalNudgeFactor (Quantity Float Meters)
    | SetFadeExtent (Quantity Float Meters)
    | ZeroNudgeFactors
    | ApplyWithOptions


defaultOptions : Options
defaultOptions =
    { horizontal = Quantity.zero
    , vertical = Quantity.zero
    , fadeExtent = Quantity.zero
    }


type alias Point =
    Point2d Meters LocalCoords


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        previewPoints points =
            points
                |> List.map
                    (\earth ->
                        ( earth
                        , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                        )
                    )
    in
    []


applyUsingOptions : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyUsingOptions options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                []
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
            ( options, previewActions options colour theTrack )

        _ ->
            ( options, [ HidePreview "nudge" ] )


previewActions newOptions colour track =
    [ ShowPreview
        { tag = "nudge"
        , shape = PreviewCircle
        , colour = colour
        , points = computeNewPoints newOptions track
        }
    ]


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetHorizontalNudgeFactor value ) ->
            let
                newOptions =
                    { options | horizontal = value }
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, SetVerticalNudgeFactor value ) ->
            let
                newOptions =
                    { options | vertical = value }
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, SetFadeExtent value ) ->
            let
                newOptions =
                    { options | fadeExtent = value }
            in
            ( newOptions, previewActions newOptions previewColour track )

        ( Just track, ZeroNudgeFactors ) ->
            ( defaultOptions, [ HidePreview "nudge" ] )

        ( Just track, ApplyWithOptions ) ->
            ( options
            , [ Actions.NudgeApplyWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : Bool -> Options -> (Msg -> msg) -> Maybe (TrackLoaded msg) -> Element msg
view imperial options msgWrapper track =
    case track of
        Nothing ->
            noTrackMessage

        Just isTrack ->
            row
                [ width fill
                , padding 5
                , spacing 5
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ verticalNudgeSlider imperial options.vertical msgWrapper
                , column [ width fill, centerX, padding 5, spacing 5 ]
                    [ horizontalNudgeSlider imperial options.horizontal msgWrapper
                    , row [ padding 5, spacing 5 ]
                        [ nudgeButton options msgWrapper
                        , zeroButton msgWrapper
                        ]
                    , text "Fade in/out"
                    , fadeSlider imperial options.fadeExtent msgWrapper
                    ]
                ]


horizontalNudgeSlider : Bool -> Length.Length -> (Msg -> msg) -> Element msg
horizontalNudgeSlider imperial value wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = Length.meters >> SetHorizontalNudgeFactor >> wrap
        , label = Input.labelBelow [ centerX ] <| text <| showShortMeasure imperial value
        , min =
            Length.inMeters <|
                if imperial then
                    Length.feet -16.0

                else
                    Length.meters -5.0
        , max =
            Length.inMeters <|
                if imperial then
                    Length.feet 16.0

                else
                    Length.meters 5.0
        , step = Nothing
        , value = Length.inMeters value
        , thumb = Input.defaultThumb
        }


fadeSlider : Bool -> Length.Length -> (Msg -> msg) -> Element msg
fadeSlider imperial value wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = Length.meters >> SetFadeExtent >> wrap
        , label = Input.labelBelow [ centerX ] <| text <| showShortMeasure imperial value
        , min = 0.0
        , max =
            Length.inMeters <|
                if imperial then
                    Length.feet 160.0

                else
                    Length.meters 50.0
        , step = Nothing
        , value = Length.inMeters value
        , thumb = Input.defaultThumb
        }


verticalNudgeSlider : Bool -> Quantity Float Meters -> (Msg -> msg) -> Element msg
verticalNudgeSlider imperial value wrap =
    el [ width fill, alignRight, paddingEach { edges | left = 10 } ] <|
        Input.slider
            commonShortVerticalSliderStyles
            { onChange = Length.meters >> SetVerticalNudgeFactor >> wrap
            , label = Input.labelBelow [ centerY ] <| text <| showShortMeasure imperial value
            , min =
                Length.inMeters <|
                    if imperial then
                        Length.feet -16.0

                    else
                        Length.meters -5.0
            , max =
                Length.inMeters <|
                    if imperial then
                        Length.feet 16.0

                    else
                        Length.meters 5.0
            , step = Nothing
            , value = Length.inMeters value
            , thumb = Input.defaultThumb
            }


nudgeButton : Options -> (Msg -> msg) -> Element msg
nudgeButton settings wrap =
    button
        neatToolsBorder
        { onPress = Just <| wrap ApplyWithOptions
        , label = text "Apply"
        }


zeroButton : (Msg -> msg) -> Element msg
zeroButton wrap =
    button
        neatToolsBorder
        { onPress = Just <| wrap ZeroNudgeFactors
        , label = text "Zero sliders"
        }
