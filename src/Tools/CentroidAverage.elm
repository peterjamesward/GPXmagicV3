module Tools.CentroidAverage exposing (Msg(..), applyUsingOptions, centroidAverageFor1CQF, defaultOptions, toolId, toolStateChange, update, view)

import Actions exposing (ToolAction(..))
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, skipCount)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Plane3d
import Point3d
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import String.Interpolate
import Tools.CentroidAverageOptions exposing (..)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import UtilsForViews exposing (showDecimal2)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, neatToolsBorder)


toolId =
    "centroid"


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

        newPoints =
            computeNewPoints options track

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
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


centroidAverageFor1CQF : TrackLoaded msg -> PeteTree
centroidAverageFor1CQF track =
    let
        ( outputTree, _ ) =
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
            let
                normalPreview =
                    ShowPreview
                        { tag = "centroid"
                        , shape = PreviewCircle
                        , colour = previewColour
                        , points = computeNewPoints newOptions track
                        }
            in
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

        ( Just _, ApplyWithOptions ) ->
            ( options
            , [ Actions.CentroidAverageApplyWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view :
    I18NOptions.Location
    -> (Msg -> msg)
    -> Options
    -> TrackLoaded msg
    -> Element msg
view location wrap options track =
    let
        i18n =
            I18N.text location toolId

        sliders =
            column [ centerX, width fill, spacing 5 ]
                [ Input.slider commonShortHorizontalSliderStyles
                    { onChange = wrap << SetWeighting
                    , label =
                        Input.labelBelow [] <|
                            text <|
                                String.Interpolate.interpolate
                                    (I18N.localisedString location toolId "weight")
                                    [ showDecimal2 options.weighting ]
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
                    , label = Input.labelLeft [] <| i18n "Position"
                    , icon = Input.defaultCheckbox
                    }
                , Input.checkbox
                    [ padding 10
                    , spacing 5
                    ]
                    { onChange = wrap << ToggleAltitude
                    , checked = options.applyToAltitude
                    , label = Input.labelRight [] <| i18n "Altitude"
                    , icon = Input.defaultCheckbox
                    }
                ]

        actionButton =
            el [ centerX, width fill, spacing 5 ] <|
                button (width fill :: neatToolsBorder)
                    { onPress = Just <| wrap ApplyWithOptions
                    , label = paragraph [] [ i18n "Apply" ]
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
                            road.startPoint.space

                        triangle =
                            Triangle3d.from
                                roadMinusOne.startPoint.space
                                originalPoint
                                road.endPoint.space

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
                            { space = originalPoint |> Point3d.translateBy shiftWithOptions
                            , time = road.startPoint.time
                            }
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
