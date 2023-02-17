module Tools.OutAndBack exposing (Msg(..), apply, defaultOptions, toolId, update, view)

import Actions exposing (ToolAction(..))
import Arc3d
import Axis3d
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length
import List.Extra
import Point3d
import Polyline3d
import Quantity
import String.Interpolate
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import Tools.Nudge
import Tools.OutAndBackOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import ViewPureStyles exposing (..)


toolId =
    "bytheway"


defaultOptions : Options
defaultOptions =
    { offset = 0.0 }


type Msg
    = ApplyOutAndBack
    | SetOffset Float


apply : Options -> TrackLoaded msg -> Maybe PeteTree
apply options track =
    let
        noNudge =
            Tools.Nudge.defaultOptions

        useNudgeTool nudgeOption index =
            -- Simple wrapper to use internal operation in Nudge
            Tools.Nudge.nudgeTrackPoint
                nudgeOption
                1.0
                index
                track.trackTree

        outwardLeg =
            -- nudge entire route one way, in natural order
            let
                nudge =
                    { noNudge | horizontal = Length.meters options.offset }
            in
            List.map (useNudgeTool nudge) (List.range 0 (skipCount track.trackTree))

        returnLeg =
            let
                nudge =
                    { noNudge
                        | horizontal = Quantity.negate <| Length.meters options.offset
                        , vertical = Quantity.negate Length.centimeter
                    }
            in
            List.map (useNudgeTool nudge) (List.range 0 (skipCount track.trackTree))
                |> List.reverse

        homeTurnMidpoint =
            -- extend first leaf back to find point on turn
            let
                homeLeaf =
                    getFirstLeaf track.trackTree

                leafAxis =
                    Axis3d.throughPoints homeLeaf.startPoint.space homeLeaf.endPoint.space
            in
            case leafAxis of
                Just axis ->
                    Point3d.along
                        axis
                        (Quantity.negate <| Length.meters <| abs options.offset)

                Nothing ->
                    homeLeaf.startPoint.space

        awayTurnMidpoint =
            -- extend last leaf to find point on turn
            let
                awayLeaf =
                    getLastLeaf track.trackTree

                leafAxis =
                    Axis3d.throughPoints awayLeaf.endPoint.space awayLeaf.startPoint.space
            in
            case leafAxis of
                Just axis ->
                    Point3d.along
                        axis
                        (Quantity.negate <| Length.meters <| abs options.offset)

                Nothing ->
                    awayLeaf.endPoint.space

        awayTurn =
            -- arc through midpoint joining outward and return legs
            let
                finalOutwardPoint =
                    List.Extra.last outwardLeg

                firstInwardPoint =
                    List.head returnLeg
            in
            case ( finalOutwardPoint, firstInwardPoint ) of
                ( Just outEarth, Just backEarth ) ->
                    Arc3d.throughPoints
                        outEarth.space
                        awayTurnMidpoint
                        backEarth.space

                _ ->
                    Nothing

        homeTurn =
            -- arc through midpoint joining return and outward legs
            let
                finalInwardPoint =
                    List.Extra.last returnLeg

                firstOutwardPoint =
                    List.head outwardLeg
            in
            case ( finalInwardPoint, firstOutwardPoint ) of
                ( Just inEarth, Just outEarth ) ->
                    Arc3d.throughPoints
                        inEarth.space
                        homeTurnMidpoint
                        outEarth.space

                _ ->
                    Nothing

        outwardInGpx =
            List.map (gpxFromPointWithReference track.referenceLonLat) outwardLeg

        returnInGpx =
            List.map (gpxFromPointWithReference track.referenceLonLat) returnLeg

        homeTurnInGpx =
            case homeTurn of
                Just arc ->
                    arc
                        |> Arc3d.approximate (Length.meters 0.1)
                        |> Polyline3d.vertices
                        |> List.map (DomainModel.withoutTime >> gpxFromPointWithReference track.referenceLonLat)

                Nothing ->
                    []

        awayTurnInGpx =
            case awayTurn of
                Just arc ->
                    arc
                        |> Arc3d.approximate (Length.meters 0.1)
                        |> Polyline3d.vertices
                        |> List.map (DomainModel.withoutTime >> gpxFromPointWithReference track.referenceLonLat)

                Nothing ->
                    []

        newCourse =
            outwardInGpx ++ awayTurnInGpx ++ returnInGpx ++ homeTurnInGpx

        newTree =
            DomainModel.treeFromSourcePoints newCourse
    in
    newTree


update :
    Msg
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options hasTrack =
    case ( hasTrack, msg ) of
        ( Just _, SetOffset offset ) ->
            let
                newOptions =
                    { options | offset = offset }
            in
            ( newOptions, [] )

        ( Just track, ApplyOutAndBack ) ->
            let
                undoInfo =
                    TrackLoaded.undoInfo
                        (Actions.OutAndBackApplyWithOptions options)
                        track
            in
            ( options
            , [ WithUndo (Actions.OutAndBackApplyWithOptions options)
              , Actions.OutAndBackApplyWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial wrapper options track =
    case track of
        Just _ ->
            let
                i18n =
                    I18N.text location toolId

                offsetSlider =
                    Input.slider
                        commonShortHorizontalSliderStyles
                        { onChange = wrapper << SetOffset
                        , label =
                            Input.labelBelow [] <|
                                text <|
                                    String.Interpolate.interpolate
                                        (I18N.localisedString location toolId "offset")
                                        [ showShortMeasure imperial (Length.meters options.offset) ]
                        , min =
                            Length.inMeters <|
                                if imperial then
                                    Length.feet -16.0

                                else
                                    Length.meters -5.0
                        , max =
                            Length.inMeters <|
                                if imperial then
                                    Length.feet 16.0

                                else
                                    Length.meters 5.0
                        , step = Just 0.5
                        , value = options.offset
                        , thumb = Input.defaultThumb
                        }

                fixButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper ApplyOutAndBack
                        , label = i18n "apply"
                        }
            in
            column
                [ padding 5
                , spacing 5
                , width fill
                , centerX
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ el [ centerX ] <| offsetSlider
                , el [ centerX ] <| fixButton
                ]

        Nothing ->
            noTrackMessage location
