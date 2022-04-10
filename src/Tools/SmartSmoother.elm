module Tools.SmartSmoother exposing (..)

import Actions exposing (ToolAction(..))
import Angle exposing (Angle)
import Arc3d
import Axis3d
import BezierSplines
import CubicSpline3d exposing (CubicSpline3d)
import Dict exposing (Dict)
import Direction2d
import Direction3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, endPoint, skipCount, startPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d
import Polyline3d exposing (Polyline3d)
import PreviewData exposing (PreviewData, PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import Tools.BendSmoother
import Tools.SmartSmootherOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import Utils
import UtilsForViews exposing (showDecimal2, showShortMeasure)
import ViewPureStyles exposing (..)


defaultOptions : Options
defaultOptions =
    { -- User adjustable
      minRadius = Length.meters 10.0
    , maxDeltaGradient = Angle.degrees 1.0
    , steeringSharpness = 0.5
    , tolerance = Length.meters 1.0
    , state = Idle
    }


type alias Point =
    Point2d Meters LocalCoords


type Msg
    = SetMinRadius (Quantity Float Meters)
    | SetDeltaGradient Angle
    | SetSharpness Float
    | SetTolerance (Quantity Float Meters)
    | Apply
    | Analyze Int
    | Cancel
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
    """Smooths the whole track, replacing sections alternately by circular arcs and Clothoids."""


type alias FoldState =
    { arcStart : Maybe EarthPoint
    , vertex : Maybe EarthPoint
    , previous : Maybe RoadSection
    , outputs : List (List EarthPoint)
    }


analyse : TrackLoaded msg -> Options -> Options
analyse track options =
    options


computeNewPoints : Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints options track =
    []


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
                    analyse theTrack options
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


analyseAtIndex : TrackLoaded msg -> Int -> AnalyserState -> AnalyserState
analyseAtIndex track index state =
    let
        estimatedRadius =
            -- Don't need to test for end-points; it falls out as no circumcentre.
            let
                ( prior, this, next ) =
                    -- This is cheap, inefficiency excused; it means we can become asynchronous.
                    ( DomainModel.earthPointFromIndex (index - 1) track.trackTree
                    , DomainModel.earthPointFromIndex index track.trackTree
                    , DomainModel.earthPointFromIndex (index + 1) track.trackTree
                    )
            in
            case Point3d.circumcenter prior this next of
                Just circumcentre ->
                    Point3d.distanceFrom circumcentre this

                Nothing ->
                    Quantity.positiveInfinity
    in
    -- It all about whether the radius is consistent with our current understanding.
    -- If so, we just accumulate the current region;
    -- If not, we file the old region and start a new one.
    state


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    case msg of
        Analyze index ->
            let
                initialAnalyserState : AnalyserState
                initialAnalyserState =
                    { atIndex = 0
                    , points = []
                    , estimatedRadii = []
                    , headIndex = 0
                    , tailIndex = 0
                    , assessment = Undecided
                    , assessments = []
                    }

                newOptions =
                    { options | state = Parsing initialAnalyserState }

                optionsWithResult =
                    -- Folding over the indices in case we need to make this asynchronous,
                    -- though that's probably more an aesthetic requirement.
                    List.range 0 (skipCount track.trackTree)
                        |> List.foldl
                            (analyseAtIndex track)
                            initialAnalyserState
            in
            ( newOptions
            , if index > 0 then
                []

              else
                []
            )

        Cancel ->
            let
                newOptions =
                    { options | state = Idle }
            in
            ( newOptions, [] )

        Apply ->
            ( options
            , [ Actions.RecursiveSmootherApplyWithOptions options
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

        SetDeltaGradient angle ->
            let
                newOptions =
                    { options | maxDeltaGradient = angle }
            in
            ( newOptions, previewActions newOptions previewColour track )

        SetSharpness sharpness ->
            let
                newOptions =
                    { options | steeringSharpness = sharpness }
            in
            ( newOptions, previewActions newOptions previewColour track )

        SetTolerance tolerance ->
            let
                newOptions =
                    { options | tolerance = tolerance }
            in
            ( newOptions, previewActions newOptions previewColour track )


view : Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view imperial wrapper options track =
    let
        applyButton =
            case options.state of
                Idle ->
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper <| Analyze (skipCount track.trackTree)
                        , label = text "Analyse"
                        }

                Parsing analyserState ->
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper Cancel
                        , label = text "Cancel"
                        }

                Ready trackAnalysis ->
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

        maxDeltaGradientSlider =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrapper << SetDeltaGradient << Angle.degrees
                , label =
                    Input.labelRight []
                        (text <| "Slope change per metre " ++ (showDecimal2 <| Angle.inDegrees options.maxDeltaGradient))
                , min = 1.0
                , max = 10.0
                , step = Nothing
                , value = Angle.inDegrees options.maxDeltaGradient
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

        --, maxDeltaGradientSlider
        , analysis
        , applyButton
        ]
