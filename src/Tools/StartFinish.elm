module Tools.StartFinish exposing (..)

import Actions exposing (ToolAction(..))
import Direction3d
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import SketchPlane3d
import Tools.StartFinishTypes exposing (ClosingInfo, Loopiness(..), Options)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector3d
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

                loopiness =
                    if separation |> Quantity.lessThanOrEqualTo Length.meter then
                        IsALoop

                    else if separation |> Quantity.lessThanOrEqualTo (Length.meters 100) then
                        AlmostLoop separation

                    else
                        NotALoop separation

                newOptions =
                    { options
                        | loopiness = loopiness
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
