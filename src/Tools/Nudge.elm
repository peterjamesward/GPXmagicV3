module Tools.Nudge exposing
    ( Msg(..)
    , applyUsingOptions
    , defaultOptions
    , nudgeTrackPoint
    , toolId
    , toolStateChange
    , update
    , view
    , widenBendHelper
    )

import Actions exposing (ToolAction(..))
import Direction2d exposing (Direction2d)
import Direction3d
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters)
import LocalCoords exposing (LocalCoords)
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import SketchPlane3d
import Tools.I18N as I18N exposing (localisedString)
import Tools.I18NOptions as I18NOptions
import Tools.NudgeOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector3d
import ViewPureStyles exposing (..)


toolId =
    "nudge"


type Msg
    = SetHorizontalNudgeFactor Length.Length
    | SetVerticalNudgeFactor Length.Length
    | SetFadeExtent Length.Length
    | ZeroNudgeFactors
    | ApplyWithOptions
    | NudgeButton Length.Length
    | SetCosineEasing Bool
    | SetEasingSpacing Length.Length


defaultOptions : Options
defaultOptions =
    { horizontal = Quantity.zero
    , vertical = Quantity.zero
    , fadeExtent = Quantity.zero
    , cosineEasing = False
    , easingSpacing = Length.meters 10
    }


applyUsingOptions :
    Options
    -> TrackLoaded msg
    -> TrackLoaded msg
applyUsingOptions options track =
    let
        ( ( actualStart, actualEnd ), newPoints ) =
            computeNudgedPoints options track

        ( fromStart, fromEnd ) =
            if track.markerPosition == Nothing then
                ( track.currentPosition
                , skipCount track.trackTree - track.currentPosition
                )

            else
                TrackLoaded.getRangeFromMarkers track

        endIndex =
            DomainModel.skipCount track.trackTree - fromEnd

        originalMarkedLength =
            endIndex - fromStart

        pointsInEachFadeZone =
            (List.length newPoints - originalMarkedLength) // 2

        startAdjustment =
            pointsInEachFadeZone - (fromStart - actualStart)

        ( newOrange, newPurple ) =
            case track.markerPosition of
                Just purple ->
                    ( track.currentPosition + startAdjustment
                    , Just <| purple + startAdjustment
                    )

                Nothing ->
                    ( track.currentPosition + startAdjustment
                    , Nothing
                    )
    in
    case
        DomainModel.replaceRange
            actualStart
            (skipCount track.trackTree - actualEnd)
            track.referenceLonLat
            (List.map .gpx newPoints)
            track.trackTree
    of
        Just newTree ->
            { track
                | trackTree = newTree
                , currentPosition = newOrange
                , markerPosition = newPurple
            }

        Nothing ->
            track


widenBendHelper :
    List Int
    -> Quantity Float Meters
    -> TrackLoaded msg
    -> Maybe PeteTree
widenBendHelper points adjustment track =
    -- See DirectionChanges for where this used.
    case ( List.minimum points, List.maximum points ) of
        ( Just statedStart, Just statedEnd ) ->
            let
                useDummyOptions =
                    { defaultOptions | horizontal = adjustment, fadeExtent = Length.meters 10 }

                useDummyTrack =
                    { track
                        | currentPosition = statedStart
                        , markerPosition = Just statedEnd
                    }

                ( ( actualStart, actualEnd ), newPoints ) =
                    computeNudgedPoints useDummyOptions useDummyTrack

                newTree =
                    DomainModel.replaceRange
                        actualStart
                        (skipCount track.trackTree - actualEnd)
                        track.referenceLonLat
                        (List.map .gpx newPoints)
                        track.trackTree
            in
            newTree

        _ ->
            Nothing


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
    let
        nudged =
            Tuple.second <| computeNudgedPoints newOptions track
    in
    [ ShowPreview
        { tag = "nudge"
        , shape =
            if List.length nudged == 1 then
                PreviewCircle

            else
                PreviewLine
        , colour = colour
        , points = nudged
        }
    ]


type alias Nudgeable =
    -- Need location and clue as to how to Nudge it.
    { point : EarthPoint
    , effectiveDirection : Direction2d LocalCoords
    , fade : Float
    }


computeNudgedPoints :
    Options
    -> TrackLoaded msg
    -> ( ( Int, Int ), List PreviewPoint )
computeNudgedPoints settings track =
    let
        interpolatePoints : Length.Length -> Length.Length -> Length.Length -> List Nudgeable
        interpolatePoints interval start end =
            let
                pointCount =
                    floor <| Quantity.ratio (end |> Quantity.minus start) interval

                sampling =
                    List.range 0 pointCount

                interpolateSampleAt sample =
                    let
                        sampleDistance =
                            interval
                                |> Quantity.multiplyBy (toFloat sample)
                                |> Quantity.plus start

                        ( precedingIndex, point ) =
                            DomainModel.interpolateTrack sampleDistance track.trackTree
                    in
                    { point = point
                    , effectiveDirection =
                        DomainModel.leafFromIndex precedingIndex track.trackTree
                            |> asRecord
                            |> .directionAtStart
                    , fade =
                        Quantity.ratio
                            (sampleDistance |> Quantity.minus start)
                            (end |> Quantity.minus start)
                    }
            in
            List.map interpolateSampleAt sampling

        ( fromStart, fromEnd ) =
            if track.markerPosition == Nothing then
                ( track.currentPosition
                , skipCount track.trackTree - track.currentPosition
                )

            else
                TrackLoaded.getRangeFromMarkers track

        ( fromNode, toNode ) =
            -- Shim for legacy code; not worth re-writing what is already quite clear.
            ( fromStart, skipCount track.trackTree - fromEnd )

        ( startDistance, endDistance ) =
            ( DomainModel.distanceFromIndex fromNode track.trackTree
            , DomainModel.distanceFromIndex toNode track.trackTree
            )

        fadeInStartDistance =
            startDistance
                |> Quantity.minus settings.fadeExtent
                |> Quantity.max Quantity.zero

        fadeOutEndDistance =
            endDistance
                |> Quantity.plus settings.fadeExtent
                |> Quantity.min (trueLength track.trackTree)

        ( firstReplacedPoint, lastReplacedPoint ) =
            if settings.fadeExtent |> Quantity.greaterThanZero then
                ( DomainModel.indexFromDistanceRoundedUp fadeInStartDistance track.trackTree
                , DomainModel.indexFromDistanceRoundedDown fadeOutEndDistance track.trackTree
                )

            else
                ( fromNode
                , toNode
                )

        ( fadeInZonePoints, fadeOutZonePoints ) =
            -- Efficiency NOT a concern here, though we should make a fold.
            ( interpolatePoints settings.easingSpacing fadeInStartDistance startDistance
            , List.drop 1 <|
                interpolatePoints settings.easingSpacing endDistance fadeOutEndDistance
            )

        ( fadeInZoneNudged, fadeOutZoneNudged ) =
            if settings.fadeExtent |> Quantity.greaterThanZero then
                ( List.map nudgeFadeInZonePoint fadeInZonePoints
                , List.map nudgeFadeOutZonePoint fadeOutZonePoints
                )

            else
                ( [], [] )

        nudgeFadeInZonePoint : Nudgeable -> EarthPoint
        nudgeFadeInZonePoint nudgeable =
            nudgeEarthPoint
                settings
                (fader nudgeable.fade)
                nudgeable.effectiveDirection
                nudgeable.point

        nudgeFadeOutZonePoint : Nudgeable -> EarthPoint
        nudgeFadeOutZonePoint nudgeable =
            nudgeEarthPoint
                settings
                (fader (1 - nudgeable.fade))
                nudgeable.effectiveDirection
                nudgeable.point

        fader x =
            if settings.cosineEasing then
                (1 - cos (x * pi)) / 2

            else
                x

        nudge index =
            -- Now only using this for in-range points
            nudgeTrackPoint settings 1 index track.trackTree

        fullyNudgedPoints =
            List.map nudge <| List.range fromNode toNode

        newEarthPoints =
            fadeInZoneNudged
                ++ fullyNudgedPoints
                ++ fadeOutZoneNudged

        previewPoints =
            TrackLoaded.asPreviewPoints
                track
                fadeInStartDistance
                newEarthPoints
    in
    ( ( firstReplacedPoint, lastReplacedPoint ), previewPoints )


effectiveDirection : Int -> PeteTree -> Direction2d LocalCoords
effectiveDirection index tree =
    --In v1 and v2, each point had its before and after directions.
    --That's not stored in v3, but not too hard to compute.
    let
        precedingLeaf =
            -- Will be first leaf if index is zero.
            leafFromIndex (index - 1) tree |> asRecord

        thisLeaf =
            leafFromIndex index tree |> asRecord

        deviation =
            Direction2d.angleFrom
                precedingLeaf.directionAtStart
                thisLeaf.directionAtStart

        halfDeviation =
            Quantity.half deviation
    in
    precedingLeaf.directionAtStart
        |> Direction2d.rotateBy halfDeviation


nudgeEarthPoint : Options -> Float -> Direction2d LocalCoords -> EarthPoint -> EarthPoint
nudgeEarthPoint options fade direction rawPoint =
    let
        horizontalVector =
            direction
                |> Direction2d.rotateClockwise
                |> Direction3d.on SketchPlane3d.xy
                |> Vector3d.withLength options.horizontal
                |> Vector3d.scaleBy fade

        verticalVector =
            Vector3d.xyz Quantity.zero Quantity.zero options.vertical
                |> Vector3d.scaleBy fade

        newXYZ =
            rawPoint.space
                |> Point3d.translateBy horizontalVector
                |> Point3d.translateBy verticalVector
    in
    { rawPoint | space = newXYZ }


nudgeTrackPoint : Options -> Float -> Int -> PeteTree -> EarthPoint
nudgeTrackPoint options fade index tree =
    if fade == 0 then
        earthPointFromIndex index tree

    else
        let
            unNudged =
                earthPointFromIndex index tree

            horizontalDirection =
                effectiveDirection index tree
        in
        nudgeEarthPoint options fade horizontalDirection unNudged


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    case msg of
        SetHorizontalNudgeFactor value ->
            let
                newOptions =
                    { options | horizontal = value }
            in
            ( newOptions, previewActions newOptions previewColour track )

        SetEasingSpacing value ->
            let
                newOptions =
                    { options | easingSpacing = value }
            in
            ( newOptions, previewActions newOptions previewColour track )

        SetVerticalNudgeFactor value ->
            let
                newOptions =
                    { options | vertical = value }
            in
            ( newOptions, previewActions newOptions previewColour track )

        NudgeButton value ->
            let
                newOptions =
                    { options | vertical = options.vertical |> Quantity.plus value }
            in
            ( newOptions, previewActions newOptions previewColour track )

        SetFadeExtent value ->
            let
                newOptions =
                    { options | fadeExtent = value }
            in
            ( newOptions, previewActions newOptions previewColour track )

        ZeroNudgeFactors ->
            ( defaultOptions, [ HidePreview "nudge" ] )

        ApplyWithOptions ->
            let
                ( ( actualStart, actualEnd ), _ ) =
                    --TODO: Avoid calling this twice.
                    computeNudgedPoints options track

                oldPoints =
                    DomainModel.extractPointsInRange
                        actualStart
                        (skipCount track.trackTree - actualEnd)
                        track.trackTree

                undoInfo =
                    { action = Actions.NudgeApplyWithOptions options
                    , originalPoints = List.map Tuple.second oldPoints
                    , fromStart = actualStart
                    , fromEnd = skipCount track.trackTree - actualEnd
                    , currentPosition = track.currentPosition
                    , markerPosition = track.markerPosition
                    }
            in
            ( options
            , [ WithUndo undoInfo
              , undoInfo.action
              , TrackHasChanged
              ]
            )

        SetCosineEasing bool ->
            let
                newOptions =
                    { options | cosineEasing = bool }
            in
            ( newOptions, previewActions newOptions previewColour track )


view : I18NOptions.Location -> Bool -> Options -> (Msg -> msg) -> Maybe (TrackLoaded msg) -> Element msg
view location imperial options msgWrapper track =
    let
        i18n =
            I18N.text location toolId

        vertical label increment =
            button
                (width fill :: neatToolsBorder)
                { onPress = Just <| msgWrapper <| NudgeButton increment
                , label = i18n label
                }
    in
    case track of
        Nothing ->
            noTrackMessage location

        Just _ ->
            let
                horizontalNudgeSlider =
                    Input.slider
                        commonShortHorizontalSliderStyles
                        { onChange = Length.meters >> SetHorizontalNudgeFactor >> msgWrapper
                        , label = Input.labelBelow [ centerX ] <| text <| showShortMeasure imperial options.horizontal
                        , min =
                            Length.inMeters <|
                                if imperial then
                                    Length.feet -21.0

                                else
                                    Length.meters -7.0
                        , max =
                            Length.inMeters <|
                                if imperial then
                                    Length.feet 21.0

                                else
                                    Length.meters 7.0
                        , step =
                            Just <|
                                Length.inMeters <|
                                    if imperial then
                                        Length.inches 2

                                    else
                                        Length.centimeters 5
                        , value = Length.inMeters options.horizontal
                        , thumb = Input.defaultThumb
                        }

                fadeSlider =
                    Input.slider
                        commonShortHorizontalSliderStyles
                        { onChange = Length.meters >> SetFadeExtent >> msgWrapper
                        , label =
                            Input.labelBelow [ centerX ] <|
                                text <|
                                    (localisedString location toolId "fade"
                                        ++ showShortMeasure imperial options.fadeExtent
                                    )
                        , min = 0.0
                        , max =
                            Length.inMeters <|
                                if imperial then
                                    Length.feet 330.0

                                else
                                    Length.meters 100.0
                        , step = Nothing
                        , value = Length.inMeters options.fadeExtent
                        , thumb = Input.defaultThumb
                        }

                easingOptions =
                    column [ centerX, spacing 5 ]
                        [ Input.slider
                            commonShortHorizontalSliderStyles
                            { onChange = Length.meters >> SetEasingSpacing >> msgWrapper
                            , label =
                                Input.labelBelow [ centerX ] <|
                                    text
                                        (localisedString location toolId "spacing"
                                            ++ showShortMeasure imperial options.easingSpacing
                                        )
                            , min = 1 -- metres
                            , max = 10 -- metres
                            , step = Nothing
                            , value = Length.inMeters options.easingSpacing
                            , thumb = Input.defaultThumb
                            }
                        , Input.checkbox []
                            { onChange = SetCosineEasing >> msgWrapper
                            , icon = Input.defaultCheckbox
                            , checked = options.cosineEasing
                            , label = Input.labelRight [] (i18n "easing")
                            }
                        ]

                verticalNudgeSlider =
                    el [ width fill, alignRight, paddingEach { edges | left = 10 } ] <|
                        Input.slider
                            commonShortVerticalSliderStyles
                            { onChange = Length.meters >> SetVerticalNudgeFactor >> msgWrapper
                            , label = Input.labelBelow [ centerY ] <| text <| showShortMeasure imperial options.vertical
                            , min =
                                Length.inMeters <|
                                    if imperial then
                                        Length.feet -21.0

                                    else
                                        Length.meters -7.0
                            , max =
                                Length.inMeters <|
                                    if imperial then
                                        Length.feet 21.0

                                    else
                                        Length.meters 7.0
                            , step =
                                Just <|
                                    Length.inMeters <|
                                        if imperial then
                                            Length.inches 2

                                        else
                                            Length.centimeters 5
                            , value = Length.inMeters options.vertical
                            , thumb = Input.defaultThumb
                            }

                nudgeButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| msgWrapper ApplyWithOptions
                        , label = i18n "Apply"
                        }

                zeroButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| msgWrapper ZeroNudgeFactors
                        , label = i18n "Zero"
                        }

                verticalNudgeButtons =
                    column [ alignRight ] <|
                        if imperial then
                            [ vertical "+1yd" <| Length.yard
                            , vertical "+1ft" <| Length.foot
                            , vertical "+1in" <| Length.inch
                            , vertical "-1in" <| Quantity.negate Length.inch
                            , vertical "-1ft" <| Quantity.negate Length.foot
                            , vertical "-1yd" <| Quantity.negate Length.yard
                            ]

                        else
                            [ vertical "+1m" <| Length.meter
                            , vertical "+10cm" <| Length.centimeters 10
                            , vertical "+1cm" <| Length.centimeter
                            , vertical "-1cm" <| Quantity.negate Length.centimeter
                            , vertical "-10cm" <| Quantity.negate <| Length.centimeters 10
                            , vertical "-1m" <| Quantity.negate Length.meter
                            ]
            in
            row
                [ width fill
                , padding 5
                , spacing 5
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ verticalNudgeButtons
                , verticalNudgeSlider
                , column [ width fill, centerX, padding 5, spacing 5 ]
                    [ horizontalNudgeSlider
                    , row [ padding 5, spacing 5 ]
                        [ nudgeButton
                        , zeroButton
                        ]
                    , fadeSlider
                    , easingOptions
                    ]
                ]
