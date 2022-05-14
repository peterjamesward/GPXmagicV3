module Tools.SmartSmoother exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Arc3d
import Axis3d
import BezierSplines
import CubicSpline3d exposing (CubicSpline3d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, endPoint, skipCount, startPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import LocalCoords exposing (LocalCoords)
import Loop
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import PreviewData exposing (PreviewData, PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import SketchPlane3d
import String.Interpolate
import Tools.BendSmoother
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.SmartSmootherOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import Utils
import UtilsForViews exposing (showDecimal2, showShortMeasure)
import Vector3d exposing (Vector3d)
import ViewPureStyles exposing (..)


toolId =
    "smart"


defaultOptions : Options
defaultOptions =
    { minRadius = Length.meters 4
    , minTransition = Length.meters 4.5
    , maxGradient = 20
    , newPoints = []
    , blend = 0.5
    }


type Msg
    = SetMinRadius (Quantity Float Meters)
    | SetMinTransition (Quantity Float Meters)
    | SetMaxGradient Float
    | SetBlend Float
    | Apply
    | DisplayInfo String String


type alias WindowSettings =
    -- Derived from Options and used in calculations
    { maxDeltaTheta : Angle -- from radius, turn permitted per metre
    , maxDeltaDeltaTheta : Angle -- from transition, rate of change of delta theta per metre
    , maxDeltaPhi : Angle -- max permitted gradient change per metre, from transition
    , maxPhi : Angle -- `atan` of max permitted gradient
    }


type alias Window =
    -- Internal state that applies limits in options.
    -- Do this forwards and backwards, average the deltas at each point, see what comes out!
    { nextDistance : Quantity Float Meters -- from start, in one metre increments
    , lastTrackIndex : Int -- grab a new point based on `distance`
    , lastTrackDirection : Direction2d LocalCoords
    , outputDeltaTheta : List Angle
    , outputDeltaPhi : List Angle
    , unspentDeltaTheta : Angle
    , lastDeltaTheta : Angle
    , targetPhi : Angle
    , lastPhi : Angle
    , unspentPhi : Angle
    }


computeNewPoints : Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            if track.markerPosition == Nothing then
                --Default whole track
                ( 0, 0 )

            else
                TrackLoaded.getRangeFromMarkers track

        ( distanceAtStart, distanceAtEnd ) =
            ( DomainModel.distanceFromIndex fromStart track.trackTree
            , DomainModel.distanceFromIndex (skipCount track.trackTree - fromEnd) track.trackTree
            )

        settings : WindowSettings
        settings =
            let
                maxDeltaTheta =
                    Angle.radians (1.0 / Length.inMeters options.minRadius)

                maxPhi =
                    Angle.radians <| atan <| options.maxGradient / 100.0
            in
            { maxDeltaTheta = maxDeltaTheta
            , maxDeltaDeltaTheta = maxDeltaTheta |> Quantity.divideBy (Length.inMeters options.minTransition)
            , maxDeltaPhi = maxPhi |> Quantity.divideBy (Length.inMeters options.minTransition)
            , maxPhi = maxPhi
            }

        forwardWindow : Window
        forwardWindow =
            let
                firstLeaf =
                    DomainModel.asRecord <|
                        DomainModel.leafFromIndex fromStart track.trackTree

                targetPhi =
                    Quantity.clamp
                        (Quantity.negate settings.maxPhi)
                        settings.maxPhi
                        (Angle.atan <| firstLeaf.gradientAtStart / 100.0)
            in
            { nextDistance = distanceAtStart
            , lastTrackIndex = 0
            , lastTrackDirection = firstLeaf.directionAtStart
            , lastPhi = targetPhi
            , targetPhi = targetPhi
            , outputDeltaTheta = []
            , outputDeltaPhi = []
            , unspentDeltaTheta = Quantity.zero
            , unspentPhi = Quantity.zero
            , lastDeltaTheta = Quantity.zero
            }

        withDeltaConstraints window unspentDeltaTheta =
            Quantity.clamp
                (window.lastDeltaTheta |> Quantity.minus settings.maxDeltaDeltaTheta)
                (window.lastDeltaTheta |> Quantity.plus settings.maxDeltaDeltaTheta)
            <|
                Quantity.clamp
                    (Quantity.negate settings.maxDeltaTheta)
                    settings.maxDeltaTheta
                    window.unspentDeltaTheta

        withPhiConstraints window targetPhi =
            Quantity.clamp
                (Quantity.negate settings.maxDeltaPhi)
                settings.maxDeltaPhi
                (targetPhi |> Quantity.minus window.lastPhi)

        filterForwards : Window -> Window
        filterForwards window =
            if window.nextDistance |> Quantity.greaterThanOrEqualTo distanceAtEnd then
                if
                    not (Quantity.equalWithin (Angle.degrees 2) window.unspentDeltaTheta Quantity.zero)
                        || not (Quantity.equalWithin (Angle.degrees 2) window.lastPhi window.targetPhi)
                then
                    -- Probably should drain our unspent??
                    -- Show's over. Note lists are consed and hence reversed.
                    let
                        availableDeltaTheta =
                            withDeltaConstraints window window.unspentDeltaTheta

                        availableDeltaPhi =
                            withPhiConstraints window window.unspentDeltaTheta
                    in
                    { window
                        | outputDeltaTheta = availableDeltaTheta :: window.outputDeltaTheta
                        , outputDeltaPhi = availableDeltaPhi :: window.outputDeltaPhi
                        , unspentDeltaTheta = window.unspentDeltaTheta |> Quantity.minus availableDeltaTheta
                        , lastDeltaTheta = availableDeltaTheta
                        , lastPhi = window.lastPhi |> Quantity.plus availableDeltaPhi
                        , nextDistance = window.nextDistance |> Quantity.plus Length.meter
                    }

                else
                    window

            else
                let
                    lastPassedPoint =
                        DomainModel.indexFromDistanceRoundedDown window.nextDistance track.trackTree

                    newWindow =
                        if lastPassedPoint == window.lastTrackIndex then
                            -- Nothing new, just empty our tank.
                            let
                                availableDeltaTheta =
                                    withDeltaConstraints window window.unspentDeltaTheta

                                availableDeltaPhi =
                                    withPhiConstraints window window.targetPhi
                            in
                            { window
                                | outputDeltaTheta = availableDeltaTheta :: window.outputDeltaTheta
                                , outputDeltaPhi = availableDeltaPhi :: window.outputDeltaPhi
                                , unspentDeltaTheta = window.unspentDeltaTheta |> Quantity.minus availableDeltaTheta
                                , lastDeltaTheta = availableDeltaTheta
                                , lastPhi = window.lastPhi |> Quantity.plus availableDeltaPhi
                            }

                        else
                            -- This will adjust all our levels
                            let
                                newLeaf =
                                    DomainModel.asRecord <|
                                        DomainModel.leafFromIndex lastPassedPoint track.trackTree

                                deltaThetaHere =
                                    newLeaf.directionAtStart
                                        |> Direction2d.angleFrom window.lastTrackDirection

                                targetPhi =
                                    Quantity.clamp
                                        (Quantity.negate settings.maxPhi)
                                        settings.maxPhi
                                        (Angle.atan <| newLeaf.gradientAtStart / 100.0)

                                unspentDeltaTheta =
                                    window.unspentDeltaTheta |> Quantity.plus deltaThetaHere

                                availableDeltaTheta =
                                    withDeltaConstraints window unspentDeltaTheta

                                availableDeltaPhi =
                                    withPhiConstraints window targetPhi
                            in
                            { window
                                | lastTrackDirection = newLeaf.directionAtStart
                                , lastTrackIndex = lastPassedPoint
                                , outputDeltaTheta = availableDeltaTheta :: window.outputDeltaTheta
                                , outputDeltaPhi = availableDeltaPhi :: window.outputDeltaPhi
                                , unspentDeltaTheta = unspentDeltaTheta |> Quantity.minus availableDeltaTheta
                                , lastDeltaTheta = availableDeltaTheta
                                , lastPhi = window.lastPhi |> Quantity.plus availableDeltaPhi
                                , targetPhi = targetPhi
                            }
                in
                -- This I hope is properly tail recursive.
                filterForwards
                    { newWindow
                        | nextDistance = window.nextDistance |> Quantity.plus Length.meter
                        , lastTrackIndex = lastPassedPoint
                    }

        -- Do it all again, but from Finish to Start.
        -- If I didn't have Covid-brain, this might be a doddle.
        -- I shall (endeavour) to negate the quantities as we encounter them, so the core
        -- logic is the same, then we flip all the deltas again at the end.
        reverseWindow : Window
        reverseWindow =
            let
                firstLeaf =
                    DomainModel.asRecord <|
                        DomainModel.leafFromIndex
                            (skipCount track.trackTree - fromEnd)
                            track.trackTree

                targetPhi =
                    Quantity.clamp
                        (Quantity.negate settings.maxPhi)
                        settings.maxPhi
                        (Quantity.negate <| Angle.atan <| firstLeaf.gradientAtEnd / 100.0)
            in
            { nextDistance = distanceAtEnd
            , lastTrackIndex = skipCount track.trackTree - fromEnd
            , lastTrackDirection = Direction2d.reverse firstLeaf.directionAtStart
            , lastPhi = targetPhi
            , targetPhi = targetPhi
            , outputDeltaTheta = []
            , outputDeltaPhi = []
            , unspentDeltaTheta = Quantity.zero
            , unspentPhi = Quantity.zero
            , lastDeltaTheta = Quantity.zero
            }

        filterReverse : Window -> Window
        filterReverse window =
            if window.nextDistance |> Quantity.lessThanOrEqualTo distanceAtStart then
                if
                    not (Quantity.equalWithin (Angle.degrees 2) window.unspentDeltaTheta Quantity.zero)
                        || not (Quantity.equalWithin (Angle.degrees 2) window.lastPhi window.targetPhi)
                then
                    -- Probably should drain our unspent??
                    -- Show's over. Note lists are consed and hence reversed.
                    let
                        availableDeltaTheta =
                            withDeltaConstraints window window.unspentDeltaTheta

                        availableDeltaPhi =
                            withPhiConstraints window window.targetPhi
                    in
                    { window
                        | outputDeltaTheta = availableDeltaTheta :: window.outputDeltaTheta
                        , outputDeltaPhi = availableDeltaPhi :: window.outputDeltaPhi
                        , unspentDeltaTheta = window.unspentDeltaTheta |> Quantity.minus availableDeltaTheta
                        , lastDeltaTheta = availableDeltaTheta
                        , lastPhi = window.lastPhi |> Quantity.plus availableDeltaPhi
                        , nextDistance = window.nextDistance |> Quantity.minus Length.meter
                    }

                else
                    window

            else
                let
                    lastPassedPoint =
                        DomainModel.indexFromDistanceRoundedUp window.nextDistance track.trackTree

                    newWindow =
                        if lastPassedPoint == window.lastTrackIndex then
                            -- Nothing new, just empty our tank.
                            let
                                availableDeltaTheta =
                                    withDeltaConstraints window window.unspentDeltaTheta

                                availableDeltaPhi =
                                    withPhiConstraints window window.targetPhi
                            in
                            { window
                                | outputDeltaTheta = availableDeltaTheta :: window.outputDeltaTheta
                                , outputDeltaPhi = availableDeltaPhi :: window.outputDeltaPhi
                                , unspentDeltaTheta = window.unspentDeltaTheta |> Quantity.minus availableDeltaTheta
                                , lastDeltaTheta = availableDeltaTheta
                                , lastPhi = window.lastPhi |> Quantity.plus availableDeltaPhi
                            }

                        else
                            -- This will adjust all our levels
                            let
                                newLeaf =
                                    DomainModel.asRecord <|
                                        DomainModel.leafFromIndex (lastPassedPoint - 1) track.trackTree

                                deltaThetaHere =
                                    newLeaf.directionAtStart
                                        |> Direction2d.reverse
                                        |> Direction2d.angleFrom window.lastTrackDirection

                                targetPhi =
                                    Quantity.clamp
                                        (Quantity.negate settings.maxPhi)
                                        settings.maxPhi
                                        (Quantity.negate <| Angle.atan <| newLeaf.gradientAtStart / 100.0)

                                unspentDeltaTheta =
                                    window.unspentDeltaTheta |> Quantity.plus deltaThetaHere

                                availableDeltaTheta =
                                    withDeltaConstraints window unspentDeltaTheta

                                availableDeltaPhi =
                                    withPhiConstraints window targetPhi
                            in
                            { window
                                | lastTrackDirection = Direction2d.reverse newLeaf.directionAtStart
                                , lastTrackIndex = lastPassedPoint
                                , outputDeltaTheta = availableDeltaTheta :: window.outputDeltaTheta
                                , outputDeltaPhi = availableDeltaPhi :: window.outputDeltaPhi
                                , unspentDeltaTheta = unspentDeltaTheta |> Quantity.minus availableDeltaTheta
                                , lastDeltaTheta = availableDeltaTheta
                                , lastPhi = window.lastPhi |> Quantity.plus availableDeltaPhi
                                , targetPhi = targetPhi
                            }
                in
                -- This I hope is properly tail recursive.
                filterReverse
                    { newWindow
                        | nextDistance = window.nextDistance |> Quantity.minus Length.meter
                        , lastTrackIndex = lastPassedPoint
                    }

        derivedTrackForwards : List EarthPoint
        derivedTrackForwards =
            let
                firstLeaf =
                    DomainModel.asRecord <|
                        DomainModel.leafFromIndex fromStart track.trackTree

                ( forwardsDeltaThetas, forwardsDeltaPhis ) =
                    let
                        finalWindow =
                            filterForwards forwardWindow
                    in
                    -- These were consed so we reverse them here.
                    ( List.reverse finalWindow.outputDeltaTheta
                    , List.reverse finalWindow.outputDeltaPhi
                    )

                ( reverseDeltaThetas, reverseDeltaPhis ) =
                    let
                        finalWindow =
                            filterReverse reverseWindow
                    in
                    -- Our traversal consed these in the natural order, as it happens.
                    ( finalWindow.outputDeltaTheta
                    , finalWindow.outputDeltaPhi
                    )

                combineDeltas : Angle -> Angle -> Angle -> Angle -> ( Angle, Angle )
                combineDeltas forwardDTheta forwardDPhi reverseDTheta reverseDPhi =
                    ( --Quantity.half <|
                      Quantity.plus
                        (forwardDTheta |> Quantity.multiplyBy options.blend)
                        (Quantity.negate reverseDTheta |> Quantity.multiplyBy (1.0 - options.blend))
                    , forwardDPhi
                    )

                combinedDeltaLists =
                    List.map4
                        combineDeltas
                        forwardsDeltaThetas
                        forwardsDeltaPhis
                        reverseDeltaThetas
                        reverseDeltaPhis

                startDirection =
                    Direction3d.xyZ
                        (Direction2d.toAngle firstLeaf.directionAtStart)
                        (Angle.atan <| firstLeaf.gradientAtStart / 100.0)

                accumulate :
                    Point3d Meters LocalCoords
                    -> Direction3d LocalCoords
                    -> List ( Angle, Angle )
                    -> List EarthPoint
                    -> List EarthPoint
                accumulate point direction deltas outputs =
                    -- Note this will cons into a reversed list.
                    case deltas of
                        ( dTheta, dPhi ) :: moreDeltas ->
                            let
                                vector =
                                    Vector3d.withLength Length.meter direction

                                newPoint =
                                    point |> Point3d.translateBy vector

                                newDirection =
                                    Direction3d.xyZ
                                        (direction
                                            |> Direction3d.azimuthIn SketchPlane3d.xy
                                            |> Quantity.plus dTheta
                                        )
                                        (direction
                                            |> Direction3d.elevationFrom SketchPlane3d.xy
                                            |> Quantity.plus dPhi
                                        )
                            in
                            accumulate
                                newPoint
                                newDirection
                                moreDeltas
                                (newPoint :: outputs)

                        _ ->
                            outputs
            in
            accumulate
                firstLeaf.startPoint
                startDirection
                combinedDeltaLists
                [ firstLeaf.startPoint ]
    in
    derivedTrackForwards
        |> Utils.elide
        |> List.reverse
        |> TrackLoaded.asPreviewPoints track distanceAtStart


applyUsingOptions : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyUsingOptions options track =
    let
        ( fromStart, fromEnd ) =
            case track.markerPosition of
                Just _ ->
                    TrackLoaded.getRangeFromMarkers track

                Nothing ->
                    ( 0, 0 )

        newPoints =
            computeNewPoints options track

        newTree =
            DomainModel.replaceRange
                fromStart
                fromEnd
                track.referenceLonLat
                (List.map .gpx <| newPoints)
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
                    { options | newPoints = computeNewPoints options theTrack }
            in
            ( newOptions, previewActions newOptions colour theTrack )

        _ ->
            ( options, [ HidePreview "smart", HidePreview "smartprofile" ] )


previewActions : Options -> Color -> TrackLoaded msg -> List (ToolAction msg)
previewActions options colour track =
    let
        ( previewTree, _ ) =
            applyUsingOptions options track

        normalPreview =
            ShowPreview
                { tag = "smart"
                , shape = PreviewCircle
                , colour = colour
                , points = computeNewPoints options track
                }

        profilePreview tree =
            ShowPreview
                { tag = "smartprofile"
                , shape = PreviewProfile tree
                , colour = colour
                , points = []
                }
    in
    case previewTree of
        Just pTree ->
            [ normalPreview, profilePreview pTree ]

        _ ->
            [ HidePreview "smart", HidePreview "smartprofile" ]


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    case msg of
        Apply ->
            ( options
            , [ Actions.SmartSmootherApplyWithOptions options
              , TrackHasChanged
              ]
            )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )

        SetMinRadius radius ->
            let
                newOptions =
                    { options | minRadius = radius }
            in
            ( newOptions, previewActions newOptions previewColour track )

        SetMinTransition transition ->
            let
                newOptions =
                    { options | minTransition = transition }
            in
            ( newOptions, previewActions newOptions previewColour track )

        SetMaxGradient gradient ->
            let
                newOptions =
                    { options | maxGradient = gradient }
            in
            ( newOptions, previewActions newOptions previewColour track )

        SetBlend blend ->
            let
                newOptions =
                    { options | blend = blend }
            in
            ( newOptions, previewActions newOptions previewColour track )


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view location imperial wrapper options track =
    let
        i18n =
            I18N.text location toolId

        applyButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper Apply
                , label = i18n "Smooth"
                }

        minRadiusSlider =
            row [ spacing 3 ]
                [ Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetMinRadius << Length.meters
                    , label = Input.labelHidden "minimum radius"
                    , min = 4.0
                    , max = 20.0
                    , step = Just 0.1
                    , value = Length.inMeters options.minRadius
                    , thumb = Input.defaultThumb
                    }
                , infoButton <| wrapper <| DisplayInfo "smart" "radius"
                , text <|
                    String.Interpolate.interpolate
                        (I18N.localisedString location toolId "viewminimum")
                        [ showShortMeasure imperial options.minRadius ]
                ]

        transitionSlider =
            row [ spacing 3 ]
                [ Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetMinTransition << Length.meters
                    , label = Input.labelHidden "transitions"
                    , min = 1.0
                    , max = 10.0
                    , step = Just 0.1
                    , value = Length.inMeters options.minTransition
                    , thumb = Input.defaultThumb
                    }
                , infoButton <| wrapper <| DisplayInfo "smart" "transition"
                , text <|
                    String.Interpolate.interpolate
                        (I18N.localisedString location toolId "viewtransition")
                        [ showShortMeasure imperial options.minTransition ]
                ]

        gradientSlider =
            row [ spacing 3 ]
                [ Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetMaxGradient
                    , label = Input.labelHidden "gradient"
                    , min = 10.0
                    , max = 30.0
                    , step = Just 0.5
                    , value = options.maxGradient
                    , thumb = Input.defaultThumb
                    }
                , infoButton <| wrapper <| DisplayInfo "smart" "gradient"
                , text <|
                    String.Interpolate.interpolate
                        (I18N.localisedString location toolId "viewgradient")
                        [ showDecimal2 options.maxGradient ]
                ]

        blendSlider =
            row [ spacing 3 ]
                [ Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetBlend
                    , label = Input.labelHidden "blend"
                    , min = 0.0
                    , max = 1.0
                    , step = Nothing
                    , value = options.blend
                    , thumb = Input.defaultThumb
                    }
                , infoButton <| wrapper <| DisplayInfo "smart" "blend"
                , text <|
                    String.Interpolate.interpolate
                        (I18N.localisedString location toolId "viewblend")
                        [ showDecimal2 options.blend ]
                ]
    in
    column
        [ padding 10
        , spacing 5
        , width fill
        , centerX
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ none
        , minRadiusSlider
        , transitionSlider
        , blendSlider
        , gradientSlider
        , applyButton
        ]
