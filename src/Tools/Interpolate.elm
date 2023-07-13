module Tools.Interpolate exposing
    ( Msg(..)
    , apply
    , applyFromPaint
    , defaultOptions
    , toolId
    , toolStateChange
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import CommonToolStyles exposing (noTrackMessage)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import Tools.I18N as I18N
import Tools.InterpolateOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Utils
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


computeNewPoints : Bool -> Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints excludeExisting options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentIsRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentIsTrack ->
                    ( 0, 0 )

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
                                { space =
                                    Point3d.interpolateFrom
                                        road.startPoint.space
                                        road.endPoint.space
                                        (fractionalIncrement * toFloat n)
                                , time =
                                    Utils.interpolateTimes
                                        (fractionalIncrement * toFloat n)
                                        road.startPoint.time
                                        road.endPoint.time
                                }
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
    in
    TrackLoaded.asPreviewPoints track newPoints


applyFromPaint : Options -> TrackLoaded msg -> TrackLoaded msg
applyFromPaint options track =
    case apply { options | extent = ExtentIsRange } track of
        Just newTree ->
            { track | trackTree = newTree }

        Nothing ->
            track


apply : Options -> TrackLoaded msg -> Maybe PeteTree
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
    in
    newTree


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
                                Just _ ->
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

        ( Just track, Apply ) ->
            let
                ensureCorrectExtent =
                    { options
                        | extent =
                            case track.markerPosition of
                                Just _ ->
                                    ExtentIsRange

                                Nothing ->
                                    ExtentIsTrack
                    }
            in
            ( ensureCorrectExtent
            , [ WithUndo (Actions.ApplyInterpolateWithOptions ensureCorrectExtent)
              , Actions.ApplyInterpolateWithOptions ensureCorrectExtent
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : SystemSettings -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view settings wrapper options track =
    case track of
        Just _ ->
            let
                i18n =
                    I18N.text settings.location toolId

                fixButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper Apply
                        , label = text "Insert points"
                        }

                extent =
                    el [ centerX, width fill ] <|
                        paragraph [ centerX ]
                            [ i18n "usage" ]

                spacingSlider =
                    Input.slider
                        commonShortHorizontalSliderStyles
                        { onChange = wrapper << SetSpacing
                        , label =
                            Input.labelBelow [] <|
                                text <|
                                    String.Interpolate.interpolate
                                        (I18N.localisedString settings.location toolId "spacing")
                                        [ showShortMeasure settings.imperial options.minimumSpacing ]
                        , min = 1.0
                        , max = 50.0
                        , step = Just 0.5
                        , value = Length.inMeters options.minimumSpacing
                        , thumb = Input.defaultThumb
                        }
            in
            column
                (CommonToolStyles.toolContentBoxStyle settings)
                [ el [ centerX ] <| spacingSlider
                , el [ centerX ] extent
                , el [ centerX ] <| fixButton
                ]

        Nothing ->
            noTrackMessage settings
