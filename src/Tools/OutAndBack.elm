module Tools.OutAndBack exposing (..)

import Actions exposing (PreviewData, PreviewShape(..), ToolAction(..))
import Angle
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Geometry101 as G exposing (..)
import Length exposing (Meters, inMeters, meters)
import LineSegment2d
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d, xCoordinate, yCoordinate, zCoordinate)
import Polyline2d
import Polyline3d
import Quantity
import SketchPlane3d
import Tools.OutAndBackOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (..)


defaultOptions : Options
defaultOptions =
    { offset = 0.0 }


type alias Point =
    Point2d Meters LocalCoords


type Msg
    = ApplyOutAndBack
    | SetOffset Float


computeNewPoints : Options -> TrackLoaded msg -> List ( EarthPoint, GPXSource )
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        previewPoints points =
            points
                |> List.map
                    (\earth ->
                        ( earth
                        , DomainModel.gpxFromPointWithReference track.referenceLonLat earth
                        )
                    )
    in
    []


apply : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
apply options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        gpxPoints =
            []

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                gpxPoints
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
            ( options, [] )

        _ ->
            ( options, [] )


update :
    Msg
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetOffset offset ) ->
            let
                newOptions =
                    { options | offset = offset }
            in
            ( newOptions, [] )

        ( Just track, ApplyOutAndBack ) ->
            ( options
            , [ Actions.OutAndBackApplyWithOptions options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view : Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view imperial wrapper options track =
    let
        fixButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper ApplyOutAndBack
                , label = text "Make out and back"
                }
    in
    case track of
        Just isTrack ->
            column
                [ padding 5
                , spacing 5
                , width fill
                , centerX
                , Background.color FlatColors.ChinesePalette.antiFlashWhite
                ]
                [ el [ centerX ] <| offsetSlider imperial options wrapper
                , el [ centerX ] <| fixButton
                ]

        Nothing ->
            noTrackMessage


offsetSlider : Bool -> Options -> (Msg -> msg) -> Element msg
offsetSlider imperial options wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetOffset
        , label =
            Input.labelBelow [] <|
                text <|
                    "Offset: "
                        ++ showShortMeasure imperial (Length.meters options.offset)
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
