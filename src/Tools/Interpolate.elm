module Tools.Interpolate exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import Point3d
import Quantity
import Tools.InterpolateOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import ViewPureStyles exposing (..)


defaultOptions : Options
defaultOptions =
    { minimumSpacing = Length.meters 10.0 }


type Msg
    = Apply
    | SetSpacing Float


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        -- This feels like a fold over the leaves, interpolating each as needed.
        interpolateRoadSection : RoadSection -> List EarthPoint -> List EarthPoint
        interpolateRoadSection road new =
            let
                numNewPointsNeeded =
                    Quantity.ratio road.trueLength options.minimumSpacing
                        |> truncate

                spacingOnThisSegment =
                    road.trueLength |> Quantity.divideBy (toFloat numNewPointsNeeded + 1)

                fractionalIncrement =
                    Quantity.ratio road.trueLength spacingOnThisSegment

                interpolatedPoints =
                    -- Includes start point!
                    List.range 0 numNewPointsNeeded
                        |> List.map
                            (\n ->
                                Point3d.interpolateFrom
                                    road.startPoint
                                    road.endPoint
                                    (fractionalIncrement * toFloat n)
                            )
            in
            List.reverse interpolatedPoints ++ new

        newPoints =
            -- If fold function conses the start points (reversing them),
            -- then we need to reverse back. But we drop the initial start point
            -- so that the splicing works as expected without duplication.
            DomainModel.traverseTreeBetweenLimitsToDepth
                fromStart
                fromEnd
                (always Nothing)
                0
                track.trackTree
                interpolateRoadSection
                []
                |> List.reverse
                |> List.drop 1

        previewPoints =
            newPoints
                |> List.map
                    (\earth ->
                        ( earth
                        , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                        )
                    )
    in
    previewPoints


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        newCourse =
            computeNewPoints options track
                |> List.map Tuple.second

        newTree =
            DomainModel.replaceRange
                fromStart
                fromEnd
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
            ( options, [] )

        _ ->
            ( options, [ HidePreview "interpolate" ] )


actions newOptions previewColour track =
    [ ShowPreview
        { tag = "interpolate"
        , shape = PreviewCircle
        , colour = previewColour
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
        ( Just track, SetSpacing spacing ) ->
            let
                newOptions =
                    { options | minimumSpacing = Length.meters spacing }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, Apply ) ->
            ( options
            , [ Actions.ApplyInterpolateWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view imperial wrapper options track =
    let
        fixButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper Apply
                , label = text "Interpolate"
                }
    in
    case track of
        Just isTrack ->
            column
                [ padding 5
                , spacing 5
                , width fill
                , centerX
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ el [ centerX ] <| spacingSlider imperial options wrapper
                , el [ centerX ] <| fixButton
                ]

        Nothing ->
            noTrackMessage


spacingSlider : Bool -> Options -> (Msg -> msg) -> Element msg
spacingSlider imperial options wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetSpacing
        , label =
            Input.labelBelow [] <|
                text <|
                    "Spacing: "
                        ++ showShortMeasure imperial options.minimumSpacing
        , min = 1.0
        , max = 50.0
        , step = Just 0.5
        , value = Length.inMeters options.minimumSpacing
        , thumb = Input.defaultThumb
        }
