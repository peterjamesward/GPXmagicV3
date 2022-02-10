module Tools.CentroidAverage exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, getDualCoords, leafFromIndex, skipCount, startPoint, traverseTreeBetweenLimitsToDepth)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Plane3d
import Point3d
import Quantity
import Tools.CentroidAverageOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import UtilsForViews exposing (fullDepthRenderingBoxSize, showDecimal2)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder, prettyButtonStyles)


defaultOptions : Options
defaultOptions =
    { weighting = 1.0
    , applyToAltitude = True
    , applyToPosition = True
    , extent = ExtentRange
    }


type Msg
    = SetWeighting Float
    | ToggleAltitude Bool
    | TogglePosition Bool
    | SetExtent Extent
    | ApplyWithOptions


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            case options.extent of
                ExtentRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentTrack ->
                    ( 0, 0 )

        earthPoints =
            centroidAverage False options fromStart fromEnd track.trackTree

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
            case options.extent of
                ExtentRange ->
                    TrackLoaded.getRangeFromMarkers track

                ExtentTrack ->
                    ( 0, 0 )

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
            ( options, actions options colour theTrack )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "centroid" ] )


actions newOptions previewColour track =
    if newOptions.extent == ExtentRange then
        [ ShowPreview
            { tag = "centroid"
            , shape = PreviewCircle
            , colour = previewColour
            , points = computeNewPoints newOptions track
            }
        ]

    else
        [ HidePreview "centroid" ]


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
            ( newOptions, actions newOptions previewColour track )

        ( Just track, ToggleAltitude _ ) ->
            let
                newOptions =
                    { options | applyToAltitude = not options.applyToAltitude }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, TogglePosition _ ) ->
            let
                newOptions =
                    { options | applyToPosition = not options.applyToPosition }
            in
            ( newOptions, actions newOptions previewColour track )

        ( Just track, SetExtent extent ) ->
            let
                newOptions =
                    { options | extent = extent }
            in
            ( newOptions, actions newOptions previewColour track )

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

        extent =
            Input.radioRow
                [ padding 10
                , spacing 5
                ]
                { onChange = wrap << SetExtent
                , selected = Just options.extent
                , label = Input.labelHidden "Style"
                , options =
                    [ Input.option ExtentRange (text "Selected range")
                    , Input.option ExtentTrack (text "Whole track")
                    ]
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
        , el [ centerX ] modeChoices
        , el [ centerX ] extent
        , el [ centerX ] actionButton
        ]


type alias FoldState =
    { roadMinusOne : Maybe RoadSection
    , newPoints : List EarthPoint
    }


centroidAverage : Bool -> Options -> Int -> Int -> PeteTree -> List EarthPoint
centroidAverage isLoop options fromStart fromEnd treeNode =
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
                        originalPoint =
                            road.startPoint

                        triangle =
                            Triangle3d.from
                                roadMinusOne.startPoint
                                originalPoint
                                road.endPoint

                        centroid =
                            Triangle3d.centroid triangle

                        newPoint =
                            Point3d.interpolateFrom
                                originalPoint
                                centroid
                                options.weighting

                        shiftVector =
                            Vector3d.from originalPoint newPoint

                        shiftWithOptions =
                            if options.applyToAltitude && options.applyToPosition then
                                shiftVector

                            else if options.applyToPosition then
                                shiftVector |> Vector3d.projectOnto Plane3d.xy

                            else if options.applyToAltitude then
                                Vector3d.xyz Quantity.zero Quantity.zero (Vector3d.zComponent shiftVector)

                            else
                                Vector3d.zero

                        adjustedPoint =
                            originalPoint |> Point3d.translateBy shiftWithOptions
                    in
                    { state
                        | roadMinusOne = Just road
                        , newPoints = adjustedPoint :: state.newPoints
                    }

        foldOutput =
            DomainModel.traverseTreeBetweenLimitsToDepth
                fromStart
                (skipCount treeNode - fromEnd)
                (always Nothing)
                0
                treeNode
                foldFn
                (FoldState Nothing [])
    in
    foldOutput.newPoints |> List.reverse
