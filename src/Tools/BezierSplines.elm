module Tools.BezierSplines exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
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
                fullRenderingZone =
                    BoundingBox3d.withDimensions
                        ( fullDepthRenderingBoxSize
                        , fullDepthRenderingBoxSize
                        , fullDepthRenderingBoxSize
                        )
                        (startPoint <| leafFromIndex theTrack.currentPosition theTrack.trackTree)

                ( fromStart, fromEnd ) =
                    TrackLoaded.getRangeFromMarkers theTrack

                depthFunction : RoadSection -> Maybe Int
                depthFunction road =
                    if road.boundingBox |> BoundingBox3d.intersects fullRenderingZone then
                        Nothing

                    else
                        Just 10

                foldFn : RoadSection -> List ( EarthPoint, GPXSource ) -> List ( EarthPoint, GPXSource )
                foldFn road accum =
                    ( road.startPoint, Tuple.first road.sourceData )
                        :: accum

                previews =
                    case theTrack.markerPosition of
                        Just marker ->
                            []

                        Nothing ->
                            []
            in
            ( options
            , [ ShowPreview
                    { tag = "bezier"
                    , shape = PreviewLine
                    , colour = colour
                    , points = previews
                    }
              ]
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "delete" ] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetBezierTension tension ) ->
            ( { options | bezierTension = tension }, [] )

        ( Just track, SetBezierTolerance tolerance ) ->
            ( { options | bezierTolerance = tolerance }, [] )

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
