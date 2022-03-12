module Tools.CentroidAverage exposing (..)

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, getDualCoords, leafFromIndex, skipCount, startPoint, traverseTreeBetweenLimitsToDepth)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Plane3d
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
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
    }


type Msg
    = SetWeighting Float
    | ToggleAltitude Bool
    | TogglePosition Bool
    | SetExtent Extent
    | ApplyWithOptions
    | DisplayInfo String String


toolID : String
toolID =
    "centroid"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Centroid average" )
        , ( "info", infoText )
        ]
    )


infoText =
    """A simple way to remove "noise" from a track is by taking the average of each
point with its neighbours. This tool does that in three dimensions, but lets you
decide whether to apply this to the position or altitude.

You can choose any "weighting" between the original points and the averaged points.

You can use this repeatedly to spread the averaging effect over more points.

If you're interested, the average we use is the centroid of the triangle defined
by a point and its neighbours.
"""


computeNewPoints : Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            case track.markerPosition of
                Just _ ->
                    TrackLoaded.getRangeFromMarkers track

                Nothing ->
                    ( 0, 0 )

        distanceToPreview =
            DomainModel.distanceFromIndex (fromStart + 1) track.trackTree

        earthPoints =
            centroidAverage False options fromStart fromEnd track.trackTree
    in
    TrackLoaded.asPreviewPoints track distanceToPreview earthPoints


applyUsingOptions : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyUsingOptions options track =
    let
        ( fromStart, fromEnd ) =
            case track.markerPosition of
                Just _ ->
                    TrackLoaded.getRangeFromMarkers track

                Nothing ->
                    ( 0, 0 )

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                (List.map .gpx <| computeNewPoints options track)
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


centroidAverageFor1CQF : TrackLoaded msg -> PeteTree
centroidAverageFor1CQF track =
    let
        ( outputTree, oldPoints ) =
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
            ( options, actions options colour theTrack )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "centroid", HidePreview "centroidprofile" ] )


actions newOptions previewColour track =
    let
        ( previewTree, _ ) =
            applyUsingOptions newOptions track

        normalPreview =
            ShowPreview
                { tag = "centroid"
                , shape = PreviewCircle
                , colour = previewColour
                , points = computeNewPoints newOptions track
                }

        profilePreview tree =
            ShowPreview
                { tag = "centroidprofile"
                , shape = PreviewProfile tree
                , colour = previewColour
                , points = []
                }
    in
    case previewTree of
        Just tree ->
            [ normalPreview, profilePreview tree ]

        _ ->
            [ HidePreview "centroid", HidePreview "centroidprofile" ]


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

        ( Just track, ApplyWithOptions ) ->
            ( options
            , [ Actions.CentroidAverageApplyWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view wrap options track =
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
            paragraph [] <|
                if track.markerPosition == Nothing then
                    [ text """Applies to whole track""" ]

                else
                    [ text "Applies between markers" ]
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
