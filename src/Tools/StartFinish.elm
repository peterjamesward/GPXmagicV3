module Tools.StartFinish exposing (..)

import Actions exposing (ToolAction(..))
import CubicSpline3d exposing (CubicSpline3d)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d
import Polyline3d exposing (Polyline3d)
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity exposing (Quantity)
import SketchPlane3d
import Tools.StartFinishTypes exposing (ClosingInfo, Loopiness(..), Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import ViewPureStyles exposing (neatToolsBorder)


type Msg
    = CloseTheLoop
    | ReverseTrack
    | ChangeLoopStart Int


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
                , label =
                    paragraph []
                        [ text <|
                            case track.markerPosition of
                                Just _ ->
                                    "Reverse the track\nbetween the markers"

                                Nothing ->
                                    "Reverse the track"
                        ]
                }

        changeStartButton c =
            button
                neatToolsBorder
                { onPress = Just (wrap <| ChangeLoopStart c)
                , label = paragraph [] [ text "Move start/finish to current point" ]
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
                ]


update :
    Msg
    -> Options
    -> TrackLoaded msg
    -> ( Options, List (Actions.ToolAction msg) )
update msg options track =
    case msg of
        CloseTheLoop ->
            ( options, [] )

        ReverseTrack ->
            ( options, [] )

        ChangeLoopStart tp ->
            ( options, [] )


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
                    if separation |> Quantity.lessThanOrEqualTo Length.meter then
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
                |> List.drop 1
                |> List.reverse
    in
    TrackLoaded.asPreviewPoints track (trueLength track.trackTree) vertices
