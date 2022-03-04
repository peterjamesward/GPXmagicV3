module Tools.BezierSplines exposing (..)

import Actions exposing (ToolAction(..))
import BezierSplines
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import Tools.BezierOptions as BezierOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (fullDepthRenderingBoxSize, showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles)


defaultOptions : Options
defaultOptions =
    { bezierTension = 0.5
    , bezierTolerance = 5.0
    , bezierStyle = BezierOptions.Approximated
    , extent = ExtentIsRange
    }


type Msg
    = SetBezierTension Float
    | SetBezierTolerance Float
    | BezierApplyWithOptions
    | SetBezierStyle BezierStyle
    | SetExtent ExtentOption


computeNewPoints : Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentIsRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentIsTrack ->
                    ( 0, 0 )

        distanceToPreview =
            distanceFromIndex fromStart track.trackTree

        splineFunction =
            case options.bezierStyle of
                ThroughExisting ->
                    BezierSplines.bezierSplinesThroughExistingPoints

                Approximated ->
                    BezierSplines.bezierSplineApproximation

        splineEarthPoints =
            splineFunction
                False
                options.bezierTension
                options.bezierTolerance
                fromStart
                (skipCount track.trackTree - fromEnd)
                track.trackTree

        previewPoints =
            TrackLoaded.asPreviewPoints track distanceToPreview splineEarthPoints
    in
    previewPoints


applyUsingOptions :
    Options
    -> TrackLoaded msg
    -> ( Maybe PeteTree, List GPXSource, ( Int, Int ) )
applyUsingOptions options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentIsRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentIsTrack ->
                    ( 0, 0 )

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                (List.map .gpx <| computeNewPoints options track)
                track.trackTree

        oldPoints =
            -- +1s here?
            DomainModel.extractPointsInRange
                fromStart
                fromEnd
                track.trackTree
    in
    ( newTree
    , oldPoints |> List.map Tuple.second
    , ( fromStart, fromEnd )
    )


bezierApproximationFor1CQF : TrackLoaded msg -> PeteTree
bezierApproximationFor1CQF track =
    let
        ( outputTree, oldPoints, _ ) =
            applyUsingOptions defaultOptions track
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
            ( options
            , actions options colour theTrack
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "bezier", HidePreview "bezierprofile" ] )


actions options previewColour track =
    let
        ( previewTree, _, _ ) =
            -- What would the track become if applied?
            applyUsingOptions options track

        normalPreview =
            ShowPreview
                { tag = "bezier"
                , shape = PreviewCircle
                , colour = previewColour
                , points = computeNewPoints options track
                }

        profilePreview ptree =
            ShowPreview
                { tag = "bezierprofile"
                , shape = PreviewProfile ptree
                , colour = previewColour
                , points = []
                }
    in
    case ( options.extent, previewTree ) of
        ( ExtentIsRange, Just ptree ) ->
            [ normalPreview, profilePreview ptree ]

        ( ExtentIsTrack, Just ptree ) ->
            [ profilePreview ptree ]

        _ ->
            [ HidePreview "bezier", HidePreview "bezierprofile" ]


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetBezierTension tension ) ->
            let
                newOptions =
                    { options | bezierTension = tension }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetBezierTolerance tolerance ) ->
            let
                newOptions =
                    { options | bezierTolerance = tolerance }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, BezierApplyWithOptions ) ->
            ( options
            , [ Actions.BezierApplyWithOptions options
              , TrackHasChanged
              ]
            )

        ( Just track, SetBezierStyle style ) ->
            let
                newOptions =
                    { options | bezierStyle = style }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetExtent extent ) ->
            let
                newOptions =
                    { options | extent = extent }
            in
            ( newOptions, actions newOptions previewColour track )

        _ ->
            ( options, [] )


view : (Msg -> msg) -> Options -> Element msg
view wrap options =
    let
        sliders =
            column [ centerX, width fill, spacing 5 ]
                [ Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrap << SetBezierTension
                    , label =
                        Input.labelBelow [] <|
                            text <|
                                "Tension "
                                    ++ showDecimal2 options.bezierTension
                    , min = 0.0
                    , max = 1.0
                    , step = Nothing
                    , value = options.bezierTension
                    , thumb = Input.defaultThumb
                    }
                , Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrap << SetBezierTolerance
                    , label =
                        Input.labelBelow [] <|
                            text <|
                                "Tolerance "
                                    ++ showDecimal2 options.bezierTolerance
                    , min = 1.0
                    , max = 10.0
                    , step = Just 0.5
                    , value = options.bezierTolerance
                    , thumb = Input.defaultThumb
                    }
                ]

        modeChoice =
            Input.radio
                [ padding 10
                , spacing 5
                ]
                { onChange = wrap << SetBezierStyle
                , selected = Just options.bezierStyle
                , label = Input.labelHidden "Style"
                , options =
                    [ Input.option ThroughExisting (text "Through existing points")
                    , Input.option Approximated (text "Approximating existing points")
                    ]
                }

        extent =
            Input.radioRow
                [ padding 10
                , spacing 5
                ]
                { onChange = wrap << SetExtent
                , selected = Just options.extent
                , label = Input.labelHidden "Style"
                , options =
                    [ Input.option ExtentIsRange (text "Selected range\n(preview)")
                    , Input.option ExtentIsTrack (text "Whole track\n(no preview)")
                    ]
                }

        actionButton =
            el [ centerX, width fill, spacing 5 ] <|
                button (width fill :: neatToolsBorder)
                    { onPress = Just <| wrap BezierApplyWithOptions
                    , label = paragraph [] [ text "Apply" ]
                    }
    in
    column
        [ spacing 5
        , padding 5
        , centerX
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ el [ centerX ] sliders
        , el [ centerX ] modeChoice
        , el [ centerX ] extent
        , el [ centerX ] actionButton
        ]
