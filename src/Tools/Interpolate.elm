module Tools.Interpolate exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import Tools.InterpolateOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import ViewPureStyles exposing (..)


toolId =
    "insert"


defaultOptions : Options
defaultOptions =
    { minimumSpacing = Length.meters 10.0
    , extent = ExtentIsRange
    }


type Msg
    = Apply
    | SetSpacing Float
    | SetExtent ExtentOption
    | DisplayInfo String String


computeNewPoints : Bool -> Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints excludeExisting options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentIsRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentIsTrack ->
                    ( 0, 0 )

        startPoint =
            earthPointFromIndex fromStart track.trackTree

        interpolateStartIndex =
            -- Sneaky (?) skip existing start points for preview.
            if excludeExisting then
                1

            else
                0

        -- This is a fold over the leaves, interpolating each as needed.
        interpolateRoadSection : RoadSection -> List EarthPoint -> List EarthPoint
        interpolateRoadSection road new =
            let
                intervalsNeeded =
                    Quantity.ratio road.trueLength options.minimumSpacing
                        |> ceiling

                spacingOnThisSegment =
                    road.trueLength |> Quantity.divideBy (toFloat intervalsNeeded)

                fractionalIncrement =
                    Quantity.ratio spacingOnThisSegment road.trueLength

                interpolatedPoints =
                    -- Includes start point!
                    List.range interpolateStartIndex (intervalsNeeded - 1)
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
                (skipCount track.trackTree - fromEnd)
                (always Nothing)
                0
                track.trackTree
                interpolateRoadSection
                []
                |> List.reverse

        previews =
            -- But these are based on distance from first mark, need to
            -- be based on first point.
            TrackLoaded.asPreviewPoints
                track
                (DomainModel.distanceFromIndex fromStart track.trackTree)
                newPoints
    in
    previews


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentIsRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentIsTrack ->
                    ( 0, 0 )

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


interpolateFor1CQF : TrackLoaded msg -> PeteTree
interpolateFor1CQF track =
    let
        ( outputTree, oldPoints ) =
            apply defaultOptions track
    in
    outputTree |> Maybe.withDefault track.trackTree


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
                                    ExtentIsRange

                                Nothing ->
                                    ExtentIsTrack
                    }
            in
            ( newOptions, actions newOptions colour theTrack )

        _ ->
            ( options, [ HidePreview "interpolate" ] )


actions : Options -> Color -> TrackLoaded msg -> List (ToolAction a)
actions newOptions previewColour track =
    --case newOptions.extent of
    --    ExtentIsRange ->
    [ ShowPreview
        { tag = "interpolate"
        , shape = PreviewCircle
        , colour = previewColour
        , points = computeNewPoints True newOptions track
        }
    ]



--ExtentIsTrack ->
--    []


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

        ( Just track, SetExtent extent ) ->
            let
                newOptions =
                    { options | extent = extent }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, Apply ) ->
            let
                newOptions =
                    { options
                        | extent =
                            case track.markerPosition of
                                Just purple ->
                                    ExtentIsRange

                                Nothing ->
                                    ExtentIsTrack
                    }
            in
            ( newOptions
            , [ Actions.ApplyInterpolateWithOptions newOptions
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
                , label = text "Insert points"
                }

        extent =
            el [ centerX, width fill ] <|
                paragraph [ centerX ]
                    [ text """Use both markers to apply to a range, otherwise applies to whole track""" ]
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
                , el [ centerX ] extent
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
