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
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import PreviewData exposing (PreviewData, PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import SketchPlane3d
import Tools.BendSmoother
import Tools.SmartSmootherOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import Utils
import UtilsForViews exposing (showDecimal2, showShortMeasure)
import Vector3d exposing (Vector3d)
import ViewPureStyles exposing (..)


defaultOptions : Options
defaultOptions =
    { minRadius = Length.meters 5
    , minTransition = Length.meters 10
    , maxGradient = 20
    , newPoints = []
    }


type Msg
    = SetMinRadius (Quantity Float Meters)
    | SetMinTransition (Quantity Float Meters)
    | SetMaxGradient Float
    | Apply
    | DisplayInfo String String


toolID : String
toolID =
    "smart"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Smart smoother" )
        , ( "info", infoText )
        , ( "radius", "Bends with a radius smaller than this will be replaced by a circular arc." )
        ]
    )


infoText =
    """Smooths the whole track by as yet uncertain means."""


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
    , unspentDeltaPhi : Angle
    , lastDeltaTheta : Angle
    , lastPhi : Angle
    , lastDeltaPhi : Angle
    }


computeNewPoints : Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints options track =
    let
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
                    DomainModel.getFirstLeaf track.trackTree
            in
            { nextDistance = Quantity.zero
            , lastTrackIndex = 0
            , lastTrackDirection = firstLeaf.directionAtStart
            , lastPhi = Angle.atan (firstLeaf.gradientAtStart / 100.0)
            , outputDeltaTheta = []
            , outputDeltaPhi = []
            , unspentDeltaTheta = Quantity.zero
            , unspentDeltaPhi = Quantity.zero
            , lastDeltaTheta = Quantity.zero
            , lastDeltaPhi = Quantity.zero
            }

        filterForwards : Window -> Window
        filterForwards window =
            if
                -- Probably should drain our unspent??
                window.nextDistance
                    |> Quantity.greaterThanOrEqualTo (DomainModel.trueLength track.trackTree)
            then
                -- Show's ever. Note lists are consed and hence reversed.
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
                                    Quantity.clamp
                                        (window.lastDeltaTheta |> Quantity.minus settings.maxDeltaDeltaTheta)
                                        (window.lastDeltaTheta |> Quantity.plus settings.maxDeltaDeltaTheta)
                                    <|
                                        Quantity.clamp
                                            (Quantity.negate settings.maxDeltaTheta)
                                            settings.maxDeltaTheta
                                            window.unspentDeltaTheta

                                availableDeltaPhi =
                                    Quantity.clamp
                                        (Quantity.negate settings.maxPhi |> Quantity.minus window.lastPhi)
                                        (settings.maxPhi |> Quantity.minus window.lastPhi)
                                    <|
                                        Quantity.clamp
                                            (Quantity.negate settings.maxDeltaPhi)
                                            settings.maxDeltaPhi
                                            window.unspentDeltaPhi
                            in
                            { window
                                | outputDeltaTheta = availableDeltaTheta :: window.outputDeltaTheta
                                , outputDeltaPhi = availableDeltaPhi :: window.outputDeltaPhi
                                , unspentDeltaTheta = window.unspentDeltaTheta |> Quantity.minus availableDeltaTheta
                                , unspentDeltaPhi = window.unspentDeltaPhi |> Quantity.minus availableDeltaPhi
                                , lastDeltaTheta = availableDeltaTheta
                                , lastPhi = window.lastPhi |> Quantity.plus availableDeltaPhi
                                , lastDeltaPhi = availableDeltaPhi
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

                                unspentDeltaTheta =
                                    window.unspentDeltaTheta |> Quantity.plus deltaThetaHere

                                availableDeltaTheta =
                                    Quantity.clamp
                                        (window.lastDeltaTheta |> Quantity.minus settings.maxDeltaDeltaTheta)
                                        (window.lastDeltaTheta |> Quantity.plus settings.maxDeltaDeltaTheta)
                                    <|
                                        Quantity.clamp
                                            (Quantity.negate settings.maxDeltaTheta)
                                            settings.maxDeltaTheta
                                            window.unspentDeltaTheta

                                phiHere =
                                    Angle.atan <| newLeaf.gradientAtStart / 100.0

                                deltaPhiHere =
                                    phiHere |> Quantity.minus window.lastPhi

                                unspentDeltaPhi =
                                    window.unspentDeltaPhi |> Quantity.plus deltaPhiHere

                                availableDeltaPhi =
                                    Quantity.clamp
                                        (Quantity.negate settings.maxPhi |> Quantity.minus window.lastPhi)
                                        (settings.maxPhi |> Quantity.minus window.lastPhi)
                                    <|
                                        Quantity.clamp
                                            (Quantity.negate settings.maxDeltaPhi)
                                            settings.maxDeltaPhi
                                            unspentDeltaPhi
                            in
                            { window
                                | lastTrackDirection = newLeaf.directionAtStart
                                , lastTrackIndex = lastPassedPoint
                                , outputDeltaTheta = availableDeltaTheta :: window.outputDeltaTheta
                                , outputDeltaPhi = availableDeltaPhi :: window.outputDeltaPhi
                                , unspentDeltaTheta = unspentDeltaTheta |> Quantity.minus availableDeltaTheta
                                , unspentDeltaPhi = unspentDeltaPhi |> Quantity.minus availableDeltaPhi
                                , lastDeltaTheta = availableDeltaTheta
                                , lastPhi = window.lastPhi |> Quantity.plus availableDeltaPhi
                                , lastDeltaPhi = availableDeltaPhi
                            }
                in
                -- This I hope is properly tail recursive.
                filterForwards
                    { newWindow
                        | nextDistance = window.nextDistance |> Quantity.plus Length.meter
                        , lastTrackIndex = lastPassedPoint
                    }

        derivedTrackForwards : List EarthPoint
        derivedTrackForwards =
            -- This just to look at the preview! Is simply summing the changes!
            let
                firstLeaf =
                    DomainModel.getFirstLeaf track.trackTree

                result =
                    filterForwards forwardWindow

                startDirection =
                    Direction3d.xyZ
                        (Direction2d.toAngle firstLeaf.directionAtStart)
                        (Angle.atan <| firstLeaf.gradientAtStart / 100.0)

                accumulate :
                    Point3d Meters LocalCoords
                    -> Direction3d LocalCoords
                    -> List Angle
                    -> List Angle
                    -> List EarthPoint
                    -> List EarthPoint
                accumulate point direction deltaThetas deltaPhis outputs =
                    case ( deltaThetas, deltaPhis ) of
                        ( dTheta :: moreTheta, dPhi :: morePhi ) ->
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
                                moreTheta
                                morePhi
                                (newPoint :: outputs)

                        _ ->
                            outputs
            in
            accumulate
                firstLeaf.startPoint
                startDirection
                (List.reverse result.outputDeltaTheta)
                (List.reverse result.outputDeltaPhi)
                [ firstLeaf.startPoint ]
    in
    derivedTrackForwards
        |> List.map
            (\earth ->
                { earthPoint = earth
                , gpx = DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                }
            )


applyUsingOptions : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyUsingOptions options track =
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
                    { options | newPoints = computeNewPoints options theTrack }
            in
            ( newOptions, previewActions newOptions colour theTrack )

        _ ->
            ( options, [ HidePreview "smart" ] )


previewActions : Options -> Color -> TrackLoaded msg -> List (ToolAction msg)
previewActions options colour track =
    [ ShowPreview
        { tag = "smart"
        , shape = PreviewCircle
        , colour = colour
        , points = computeNewPoints options track
        }
    ]


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


view : Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view imperial wrapper options track =
    let
        applyButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper Apply
                , label = text "Smooth"
                }

        minRadiusSlider =
            row [ spacing 3 ]
                [ Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrapper << SetMinRadius << Length.meters
                    , label = Input.labelHidden "minimum radius"
                    , min = 4.0
                    , max = 20.0
                    , step = Nothing
                    , value = Length.inMeters options.minRadius
                    , thumb = Input.defaultThumb
                    }
                , infoButton <| wrapper <| DisplayInfo "smart" "radius"
                , text <| "Minimum radius " ++ showShortMeasure imperial options.minRadius
                ]

        transitionSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetMinTransition << Length.meters
                , label =
                    Input.labelRight []
                        (text <| "Meters of turn-in " ++ showShortMeasure imperial options.minTransition)
                , min = 1.0
                , max = 10.0
                , step = Nothing
                , value = Length.inMeters options.minTransition
                , thumb = Input.defaultThumb
                }

        gradientSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetMaxGradient
                , label =
                    Input.labelRight []
                        (text <| "Maximum gradient " ++ showDecimal2 options.maxGradient)
                , min = 10.0
                , max = 30.0
                , step = Nothing
                , value = options.maxGradient
                , thumb = Input.defaultThumb
                }

        analysis =
            -- Will be a data table showing results of analysis
            text "Analysis here..."
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
        , gradientSlider
        , applyButton
        ]
