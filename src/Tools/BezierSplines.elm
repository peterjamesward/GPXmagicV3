module Tools.BezierSplines exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import BezierSplines
import BoundingBox3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, getDualCoords, leafFromIndex, skipCount, startPoint, traverseTreeBetweenLimitsToDepth)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Tools.BezierOptions as BezierOptions exposing (BezierStyle(..), Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (fullDepthRenderingBoxSize, showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles)


defaultOptions : Options
defaultOptions =
    { bezierTension = 0.5
    , bezierTolerance = 5.0
    , bezierStyle = BezierOptions.Approximated
    }


type Msg
    = SetBezierTension Float
    | SetBezierTolerance Float
    | BezierSplines
    | BezierApproximation
    | SetBezierStyle BezierStyle


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        splineEarthPoints =
            BezierSplines.bezierSplinesThroughExistingPoints
                False
                options.bezierTension
                options.bezierTolerance
                fromStart
                (skipCount track.trackTree - fromEnd)
                track.trackTree

        previewPoints =
            splineEarthPoints
                |> List.map
                    (\earth ->
                        ( earth
                        , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                        )
                    )
    in
    previewPoints


applyUsingCurrentPoints : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyUsingCurrentPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                (List.map Tuple.second <| computeNewPoints options track)
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
            ( options
            , [ ShowPreview
                    { tag = "bezier"
                    , shape = PreviewCircle
                    , colour = colour
                    , points = computeNewPoints options theTrack
                    }
              ]
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "bezier" ] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetBezierTension tension ) ->
            ( { options | bezierTension = tension }
            , [ ShowPreview
                    { tag = "bezier"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints options track
                    }
              ]
            )

        ( Just track, SetBezierTolerance tolerance ) ->
            ( { options | bezierTolerance = tolerance }
            , [ ShowPreview
                    { tag = "bezier"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints options track
                    }
              ]
            )

        ( Just track, BezierSplines ) ->
            ( options
            , [ Actions.BezierSplineThroughCurrentPoints options
              , TrackHasChanged
              ]
            )

        ( Just track, SetBezierStyle style ) ->
            ( { options | bezierStyle = style }
            , [ ShowPreview
                    { tag = "bezier"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints options track
                    }
              ]
            )

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

        actionButton =
            el [ centerX, width fill, spacing 5 ] <|
                button (width fill :: neatToolsBorder) <|
                    case options.bezierStyle of
                        ThroughExisting ->
                            { onPress = Just <| wrap BezierSplines
                            , label = paragraph [] [ text "Apply" ]
                            }

                        Approximated ->
                            { onPress = Just <| wrap BezierApproximation
                            , label = paragraph [] [ text "Apply" ]
                            }
    in
    column
        [ spacing 10
        , padding 10
        , centerX
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ sliders
        , modeChoice
        , actionButton
        ]
