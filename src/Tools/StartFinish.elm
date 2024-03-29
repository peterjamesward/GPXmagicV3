module Tools.StartFinish exposing (Msg(..), addPens, applyCloseLoop, applyMoveStart, applyReverse, defaultOptions, toolId, toolStateChange, update, view)

import Actions exposing (ToolAction(..))
import CommonToolStyles
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
import Point2d
import Point3d
import Polyline3d exposing (Polyline3d)
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import SketchPlane3d
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (buttonStylesWithTooltip)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.StartFinishTypes exposing (Loopiness(..), Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector3d
import ViewPureStyles exposing (neatToolsBorder)


toolId =
    "loop"


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


view : SystemSettings -> Options -> TrackLoaded msg -> (Msg -> msg) -> Element msg
view settings options track wrap =
    let
        i18n =
            I18N.text settings.location toolId

        loopButton =
            button
                neatToolsBorder
            <|
                case options.loopiness of
                    AlmostLoop _ ->
                        { onPress = Just <| wrap CloseTheLoop
                        , label = paragraph [] [ i18n "make" ]
                        }

                    IsALoop ->
                        { onPress = Nothing
                        , label = paragraph [] [ i18n "loop" ]
                        }

                    NotALoop _ ->
                        { onPress = Nothing
                        , label = paragraph [] [ i18n "biggap" ]
                        }

        reverseButton =
            button
                neatToolsBorder
                { onPress = Just <| wrap ReverseTrack
                , label = paragraph [] [ i18n "reverse" ]
                }

        changeStartButton c =
            button
                neatToolsBorder
                { onPress = Just (wrap <| ChangeLoopStart c)
                , label = paragraph [] [ i18n "move" ]
                }

        addRiderPens =
            button
                (buttonStylesWithTooltip below <| I18N.localisedString settings.location toolId "pens")
                { onPress = Just (wrap <| AddRiderPens)
                , label = paragraph [] [ i18n "add" ]
                }
    in
    column
        (CommonToolStyles.toolContentBoxStyle settings)
    <|
        case options.loopiness of
            IsALoop ->
                [ paragraph [] [ i18n "isloop" ]
                , changeStartButton track.currentPosition
                , reverseButton
                ]

            AlmostLoop gap ->
                [ paragraph []
                    [ text <|
                        String.Interpolate.interpolate
                            (I18N.localisedString settings.location toolId "isnear")
                            [ showShortMeasure settings.imperial gap ]
                    ]
                , loopButton
                , reverseButton
                , addRiderPens
                ]

            NotALoop gap ->
                [ paragraph []
                    [ text <|
                        String.Interpolate.interpolate
                            (I18N.localisedString settings.location toolId "isnear")
                            [ showShortMeasure settings.imperial gap ]
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
    let
        makeActions actionCode =
            [ WithUndo actionCode
            , actionCode
            , TrackHasChanged
            ]
    in
    case msg of
        CloseTheLoop ->
            ( { options | pointsToClose = [], loopiness = IsALoop }
            , makeActions <| Actions.CloseLoopWithOptions options
            )

        ReverseTrack ->
            ( { options | pointsToClose = [] }
            , makeActions Actions.ReverseTrack
            )

        ChangeLoopStart _ ->
            ( { options | pointsToClose = [] }
            , makeActions <| Actions.MoveStartPoint track.currentPosition
            )

        AddRiderPens ->
            ( { options | pointsToClose = [] }
            , makeActions Actions.AddRiderPens
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
                        |> .space
                        |> Point3d.projectInto SketchPlane3d.xy
                    , DomainModel.earthPointFromIndex (skipCount theTrack.trackTree) theTrack.trackTree
                        |> .space
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
            ( Point3d.midpoint lastLeaf.startPoint.space lastLeaf.endPoint.space
            , Point3d.midpoint firstLeaf.startPoint.space firstLeaf.endPoint.space
            )

        ( ( b1, c1 ), ( a2, b2 ) ) =
            ( ( midOfLast, lastLeaf.endPoint.space )
            , ( firstLeaf.startPoint.space, midOfFirst )
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
                |> List.map (DomainModel.withTime lastLeaf.endPoint.time)
    in
    --TODO: Proper times for new points.
    TrackLoaded.asPreviewPoints track vertices


applyCloseLoop : Options -> TrackLoaded msg -> TrackLoaded msg
applyCloseLoop options track =
    -- Let's deem the new start to be the spline point nearest the origin.
    let
        numberedSplinePoints =
            List.indexedMap Tuple.pair options.pointsToClose

        newGpxPoints =
            List.map .gpx options.pointsToClose

        newStartPoint =
            -- Take the nearest to the current start as the new start.
            numberedSplinePoints
                |> List.Extra.minimumBy
                    (\( idx, preview ) ->
                        preview.earthPoint
                            |> .space
                            |> Point3d.distanceFrom Point3d.origin
                            |> Length.inMeters
                    )

        ( newEndPoints, newStartPoints ) =
            case newStartPoint of
                -- Note the new S/F appears twice.
                Just ( index, _ ) ->
                    ( List.take (index + 1) newGpxPoints
                    , List.drop index newGpxPoints
                    )

                Nothing ->
                    -- Hmm. Put them at the end.
                    ( newGpxPoints, [] )

        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree

        newPoints =
            newStartPoints
                ++ (List.drop 1 <| List.take (skipCount track.trackTree - 1) oldPoints)
                ++ newEndPoints
    in
    case DomainModel.treeFromSourcePoints newPoints of
        Just isTree ->
            { track
                | trackTree = isTree
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track


applyReverse : TrackLoaded msg -> TrackLoaded msg
applyReverse track =
    let
        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    in
    case DomainModel.treeFromSourcePoints <| List.reverse oldPoints of
        Just isTree ->
            { track
                | trackTree = isTree
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track


applyMoveStart : Int -> TrackLoaded msg -> TrackLoaded msg
applyMoveStart index track =
    -- A littel more care needed.
    -- Where the S/F join we have two points; one must be removed.
    -- At the new S?F we need to have thw same point at start and at end.
    let
        oldPoints =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree

        ( beforeNewStart, afterNewStart ) =
            List.Extra.splitAt index oldPoints

        newPoints =
            afterNewStart ++ List.drop 1 beforeNewStart ++ List.take 1 afterNewStart
    in
    case DomainModel.treeFromSourcePoints newPoints of
        Just isTree ->
            { track
                | trackTree = isTree
                , leafIndex = TrackLoaded.indexLeaves isTree
                , currentPosition = 0
                , markerPosition = Nothing
            }

        Nothing ->
            track


addPens : TrackLoaded msg -> TrackLoaded msg
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
            ( Point3d.translateBy startVector firstLeaf.startPoint.space
                |> DomainModel.withTime firstLeaf.startPoint.time
                |> gpxFromPointWithReference track.referenceLonLat
            , Point3d.translateBy endVector lastLeaf.endPoint.space
                |> DomainModel.withTime lastLeaf.endPoint.time
                |> gpxFromPointWithReference track.referenceLonLat
            )

        newPoints =
            newStart :: oldPoints ++ [ newEnd ]
    in
    case DomainModel.treeFromSourcePoints newPoints of
        Just isTree ->
            { track
                | trackTree = isTree
                , leafIndex = TrackLoaded.indexLeaves isTree
                , currentPosition = track.currentPosition + 1
                , markerPosition = Maybe.map ((+) 1) track.markerPosition
            }

        Nothing ->
            track
