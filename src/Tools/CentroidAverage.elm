module Tools.CentroidAverage exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, getDualCoords, leafFromIndex, skipCount, startPoint, traverseTreeBetweenLimitsToDepth)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Point3d
import Tools.CentroidAverageOptions exposing (Options)
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import UtilsForViews exposing (fullDepthRenderingBoxSize, showDecimal2)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles)


defaultOptions : Options
defaultOptions =
    { weighting = 1.0
    , applyToAltitude = True
    , applyToPosition = True
    }


type Msg
    = SetWeighting Float
    | ToggleAltitude Bool
    | TogglePosition Bool
    | ApplyWithOptions


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        earthPoints =
            centroidAverage False options.weighting fromStart fromEnd track.trackTree

        previewPoints =
            earthPoints
                |> List.map
                    (\earth ->
                        ( earth
                        , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                        )
                    )
    in
    previewPoints


applyUsingOptions : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyUsingOptions options track =
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
                    { tag = "centroid"
                    , shape = PreviewCircle
                    , colour = colour
                    , points = computeNewPoints options theTrack
                    }
              ]
            )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "centroid" ] )


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetWeighting weight ) ->
            let
                newOptions =
                    { options | weighting = weight }
            in
            ( newOptions
            , [ ShowPreview
                    { tag = "centroid"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints newOptions track
                    }
              ]
            )

        ( Just track, ToggleAltitude _ ) ->
            let
                newOptions =
                    { options | applyToAltitude = not options.applyToAltitude }
            in
            ( newOptions
            , [ ShowPreview
                    { tag = "centroid"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints newOptions track
                    }
              ]
            )

        ( Just track, TogglePosition _ ) ->
            let
                newOptions =
                    { options | applyToPosition = not options.applyToPosition }
            in
            ( newOptions
            , [ ShowPreview
                    { tag = "centroid"
                    , shape = PreviewCircle
                    , colour = previewColour
                    , points = computeNewPoints newOptions track
                    }
              ]
            )

        ( Just track, ApplyWithOptions ) ->
            ( options
            , [ Actions.CentroidAverageApplyWithOptions options
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
                    { onChange = wrap << SetWeighting
                    , label =
                        Input.labelBelow [] <|
                            text <|
                                "Weighting "
                                    ++ showDecimal2 options.weighting
                    , min = 0.0
                    , max = 1.0
                    , step = Nothing
                    , value = options.weighting
                    , thumb = Input.defaultThumb
                    }
                ]

        modeChoices =
            row []
                [ Input.checkbox
                    [ padding 10
                    , spacing 5
                    ]
                    { onChange = wrap << TogglePosition
                    , checked = options.applyToPosition
                    , label = Input.labelLeft [] <| text "Position"
                    , icon = Input.defaultCheckbox
                    }
                , Input.checkbox
                    [ padding 10
                    , spacing 5
                    ]
                    { onChange = wrap << ToggleAltitude
                    , checked = options.applyToAltitude
                    , label = Input.labelRight [] <| text "Altitude"
                    , icon = Input.defaultCheckbox
                    }
                ]

        actionButton =
            el [ centerX, width fill, spacing 5 ] <|
                button (width fill :: neatToolsBorder)
                    { onPress = Just <| wrap ApplyWithOptions
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
        [ el [ centerX ] sliders
        , el [ centerX ] modeChoices
        , el [ centerX ] actionButton
        ]


type alias FoldState =
    { roadMinusOne : Maybe RoadSection
    , newPoints : List EarthPoint
    }


centroidAverage : Bool -> Float -> Int -> Int -> PeteTree -> List EarthPoint
centroidAverage isLoop weight startIndx endIndex treeNode =
    -- Structurally pretty much same as Bezier approximation.
    let
        foldFn : RoadSection -> FoldState -> FoldState
        foldFn road state =
            case state.roadMinusOne of
                Nothing ->
                    -- Defer action until we have three road pieces.
                    { state | roadMinusOne = Just road }

                Just roadMinusOne ->
                    let
                        triangle =
                            Triangle3d.from
                                roadMinusOne.startPoint
                                road.startPoint
                                road.endPoint

                        centroid =
                            Triangle3d.centroid triangle

                        newPoint =
                            Point3d.interpolateFrom
                                road.startPoint
                                centroid
                                weight
                    in
                    { state
                        | roadMinusOne = Just road
                        , newPoints = newPoint :: state.newPoints
                    }

        foldOutput =
            DomainModel.traverseTreeBetweenLimitsToDepth
                startIndx
                endIndex
                (always Nothing)
                0
                treeNode
                foldFn
                (FoldState Nothing [])
    in
    foldOutput.newPoints |> List.reverse