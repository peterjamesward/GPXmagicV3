module Tools.StartFinish exposing (..)

import Actions exposing (ToolAction(..))
import Axis3d
import CubicSpline3d exposing (CubicSpline3d)
import Direction3d
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Plane3d
import Point2d
import Point3d
import Polyline3d exposing (Polyline3d)
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import SketchPlane3d
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.StartFinishTypes exposing (ClosingInfo, Loopiness(..), Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector3d
import ViewPureStyles exposing (neatToolsBorder)


type Msg
    = CloseTheLoop
    | ReverseTrack
    | ChangeLoopStart Int
    | AddRiderPens


defaultOptions : Options
defaultOptions =
    { loopiness = NotALoop Quantity.zero
    , pointsToClose = []
    }


view : Bool -> Options -> TrackLoaded msg -> (Msg -> msg) -> Element msg
view imperial options track wrap =
    let
        loopButton =
            button
                neatToolsBorder
            <|
                case options.loopiness of
                    AlmostLoop _ ->
                        { onPress = Just <| wrap CloseTheLoop
                        , label = paragraph [] [ text "Make the track into a loop" ]
                        }

                    IsALoop ->
                        { onPress = Nothing
                        , label = paragraph [] [ text "Already a loop" ]
                        }

                    NotALoop _ ->
                        { onPress = Nothing
                        , label = paragraph [] [ text "Gap is too big" ]
                        }

        reverseButton =
            button
                neatToolsBorder
                { onPress = Just <| wrap ReverseTrack
                , label = paragraph [] [ text "Reverse the track" ]
                }

        changeStartButton c =
            button
                neatToolsBorder
                { onPress = Just (wrap <| ChangeLoopStart c)
                , label = paragraph [] [ text "Move start/finish to current point" ]
                }

        addRiderPens =
            button
                (buttonStylesWithTooltip below "Add 60m at start, 140m at end")
                { onPress = Just (wrap <| AddRiderPens)
                , label = paragraph [] [ text "Add RGT pens" ]
                }
    in
    column
        [ spacing 10
        , padding 10
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , width fill
        ]
    <|
        case options.loopiness of
            IsALoop ->
                [ paragraph [] [ text "This track is a loop." ]
                , changeStartButton track.currentPosition
                , reverseButton
                ]

            AlmostLoop gap ->
                [ paragraph []
                    [ text <|
                        "This track is "
                            ++ showShortMeasure imperial gap
                            ++ " away from a loop"
                    ]
                , loopButton
                , reverseButton
                , addRiderPens
                ]

            NotALoop gap ->
                [ paragraph []
                    [ text <|
                        "This track is "
                            ++ showShortMeasure imperial gap
                            ++ " away from a loop"
                    ]
                , loopButton
                , reverseButton
                , addRiderPens
                ]


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> ( Options, List (Actions.ToolAction msg) )
update msg options track =
    case msg of
        CloseTheLoop ->
            ( { options | pointsToClose = [], loopiness = IsALoop }
            , [ CloseLoopWithOptions options, TrackHasChanged ]
            )

        ReverseTrack ->
            ( { options | pointsToClose = [] }
            , [ Actions.ReverseTrack, TrackHasChanged ]
            )

        ChangeLoopStart tp ->
            ( { options | pointsToClose = [] }
            , [ Actions.MoveStartPoint track.currentPosition, TrackHasChanged ]
            )

        AddRiderPens ->
            ( { options | pointsToClose = [] }
            , [ Actions.AddRiderPens, TrackHasChanged ]
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
            -- Let's check the status of the track here.
            let
                ( first, last ) =
                    ( DomainModel.earthPointFromIndex 0 theTrack.trackTree
                        |> Point3d.projectInto SketchPlane3d.xy
                    , DomainModel.earthPointFromIndex (skipCount theTrack.trackTree) theTrack.trackTree
                        |> Point3d.projectInto SketchPlane3d.xy
                    )

                separation =
                    Point2d.distanceFrom first last

                ( loopiness, points ) =
                    if separation |> Quantity.lessThanOrEqualTo (Length.meters 5.0) then
                        ( IsALoop, [] )

                    else if separation |> Quantity.lessThanOrEqualTo (Length.meters 100) then
                        ( AlmostLoop separation, closeTheLoop theTrack )

                    else
                        ( NotALoop separation, [] )

                newOptions =
                    { options
                        | loopiness = loopiness
                        , pointsToClose = points
                    }
            in
            ( newOptions, previewActions newOptions colour theTrack )

        _ ->
            ( options, [ HidePreview "loop" ] )


previewActions newOptions colour track =
    -- Subverting this mechanism to show the discs and captured points on the views.
    [ ShowPreview
        { tag = "loop"
        , shape = PreviewCircle
        , colour = colour
        , points = newOptions.pointsToClose
        }
    ]


closeTheLoop : TrackLoaded msg -> List PreviewPoint
closeTheLoop track =
    -- Experiment with splines here as a simple and fairly general method.
    let
        ( lastLeaf, firstLeaf ) =
            ( DomainModel.getLastLeaf track.trackTree
            , DomainModel.getFirstLeaf track.trackTree
            )

        ( midOfLast, midOfFirst ) =
            ( Point3d.midpoint lastLeaf.startPoint lastLeaf.endPoint
            , Point3d.midpoint firstLeaf.startPoint firstLeaf.endPoint
            )

        ( ( b1, c1 ), ( a2, b2 ) ) =
            ( ( midOfLast, lastLeaf.endPoint )
            , ( firstLeaf.startPoint, midOfFirst )
            )

        spline : CubicSpline3d Meters LocalCoords
        spline =
            -- From previous road start to end, using control points
            -- from adjacent edges.
            CubicSpline3d.fromControlPoints b1 c1 a2 b2

        polylineFromSpline : Polyline3d Meters LocalCoords
        polylineFromSpline =
            CubicSpline3d.approximate
                (Length.meters 0.1)
                spline

        vertices : List EarthPoint
        vertices =
            Polyline3d.vertices polylineFromSpline
    in
    TrackLoaded.asPreviewPoints track (trueLength track.trackTree) vertices


applyCloseLoop : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyCloseLoop options track =
    -- Let's deem the new start to be the spline point nearest the origin.
    let
        numberedSplinePoints =
            List.indexedMap Tuple.pair options.pointsToClose

        newGpxPoints =
            List.map .gpx options.pointsToClose

        newStartPoint =
            numberedSplinePoints
                |> List.Extra.minimumBy
                    (\( idx, preview ) ->
                        preview.earthPoint
                            |> Point3d.distanceFrom Point3d.origin
                            |> Length.inMeters
                    )

        ( newEndPoints, newStartPoints ) =
            case newStartPoint of
                Just ( index, _ ) ->
                    List.Extra.splitAt index newGpxPoints

                Nothing ->
                    -- Hmm. Put them at the end.
                    ( newGpxPoints, [] )

        collectStartPointsInReverse : RoadSection -> List GPXSource -> List GPXSource
        collectStartPointsInReverse road outputs =
            -- We don't want the very start or the very end.
            Tuple.first road.sourceData :: outputs

        oldPoints =
            List.reverse <|
                DomainModel.foldOverRoute collectStartPointsInReverse track.trackTree []

        newPoints =
            newStartPoints ++ List.drop 1 oldPoints ++ newEndPoints
    in
    ( DomainModel.treeFromSourcePoints newPoints
    , DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    )


applyReverse : TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyReverse track =
    let
        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    in
    ( DomainModel.treeFromSourcePoints <| List.reverse oldPoints
    , oldPoints
    )


applyMoveStart : Int -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyMoveStart index track =
    let
        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree

        ( beforeNewStart, afterNewStart ) =
            List.Extra.splitAt index oldPoints

        newPoints =
            afterNewStart ++ beforeNewStart
    in
    ( DomainModel.treeFromSourcePoints newPoints
    , oldPoints
    )


addPens : TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
addPens track =
    let
        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree

        ( firstLeaf, lastLeaf ) =
            ( getFirstLeaf track.trackTree, getLastLeaf track.trackTree )

        ( startDirection, endDirection ) =
            ( Direction3d.on SketchPlane3d.xy firstLeaf.directionAtStart
            , Direction3d.on SketchPlane3d.xy lastLeaf.directionAtEnd
            )

        ( startVector, endVector ) =
            ( Vector3d.withLength (Length.meters -60) startDirection
            , Vector3d.withLength (Length.meters 140) endDirection
            )

        ( newStart, newEnd ) =
            ( Point3d.translateBy startVector firstLeaf.startPoint
                |> gpxFromPointWithReference track.referenceLonLat
            , Point3d.translateBy endVector lastLeaf.endPoint
                |> gpxFromPointWithReference track.referenceLonLat
            )

        newPoints =
            newStart :: oldPoints ++ [ newEnd ]
    in
    ( DomainModel.treeFromSourcePoints newPoints
    , oldPoints
    )
