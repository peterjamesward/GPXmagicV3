module Tools.BezierSplines exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import BezierSplines
import BoundingBox3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, getDualCoords, leafFromIndex, skipCount, startPoint, traverseTreeBetweenLimitsToDepth)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (fullDepthRenderingBoxSize, showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles)


type alias Options =
    { bezierTension : Float
    , bezierTolerance : Float
    }


defaultOptions : Options
defaultOptions =
    { bezierTension = 0.5
    , bezierTolerance = 5.0
    }


type Msg
    = SetBezierTension Float
    | SetBezierTolerance Float
    | BezierSplines
    | BezierApproximation


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        fullRenderingZone =
            BoundingBox3d.withDimensions
                ( fullDepthRenderingBoxSize
                , fullDepthRenderingBoxSize
                , fullDepthRenderingBoxSize
                )
                (startPoint <| leafFromIndex track.currentPosition track.trackTree)

        ( fromStart, fromEnd ) =
            case track.markerPosition of
                Just marker ->
                    TrackLoaded.getRangeFromMarkers track

                Nothing ->
                    ( 0, 0 )

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
                    , shape = PreviewLine
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
                    , shape = PreviewLine
                    , colour = previewColour
                    , points = computeNewPoints options track
                    }
              ]
            )

        ( Just track, SetBezierTolerance tolerance ) ->
            ( { options | bezierTolerance = tolerance }
            , [ ShowPreview
                    { tag = "bezier"
                    , shape = PreviewLine
                    , colour = previewColour
                    , points = computeNewPoints options track
                    }
              ]
            )

        ( Just track, BezierSplines ) ->
            ( options
            , [ Actions.BezierSplineThroughCurrentPoints
              , TrackHasChanged
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
                    , max = 2.0
                    , step = Just 0.1
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

        buttons =
            row [ centerX, width fill, spacing 5 ]
                [ button
                    (width fill :: neatToolsBorder)
                    { onPress = Just <| wrap BezierSplines
                    , label = paragraph [] [ text "Pass through existing points" ]
                    }
                , button
                    (width fill :: neatToolsBorder)
                    { onPress = Just <| wrap BezierApproximation
                    , label = paragraph [] [ text "Use existing points as a guide" ]
                    }
                ]
    in
    column
        [ spacing 10
        , padding 10
        , centerX
        , width fill
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ sliders
        , buttons
        ]
