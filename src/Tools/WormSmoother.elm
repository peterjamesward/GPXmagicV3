module Tools.WormSmoother exposing (..)

import Actions exposing (ToolAction(..))
import Angle
import Arc3d
import Axis3d
import BezierSplines
import CubicSpline3d exposing (CubicSpline3d)
import Dict exposing (Dict)
import Direction2d
import Direction3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, endPoint, skipCount, startPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Length exposing (Meters, inMeters, meters)
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d
import Polyline3d exposing (Polyline3d)
import PreviewData exposing (PreviewData, PreviewPoint, PreviewShape(..))
import Quantity
import Tools.BendSmoother
import Tools.WormSmootherOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Triangle3d
import Utils
import ViewPureStyles exposing (..)


defaultOptions : Options
defaultOptions =
    { -- User adjustable
      wormLength = Quantity.zero
    , minRadius = Quantity.zero
    , maxDeltaGradient = Angle.degrees 0
    , outputSpacing = Quantity.zero
    }


type alias Point =
    Point2d Meters LocalCoords


type Msg
    = SetDepth Int
    | Apply
    | DisplayInfo String String


toolID : String
toolID =
    "recursive"


textDictionary : ( String, Dict String String )
textDictionary =
    -- Introducing the convention of toolID, its use as a text tag, and the "info" tag.
    -- ToolsController can use these for info button and tool label.
    ( toolID
    , Dict.fromList
        [ ( toolID, "Recursive smoother" )
        , ( "info", infoText )
        ]
    )


infoText =
    """Uses successive approximations to the whole track, replacing sections alternately by
 circular arcs and either Bezier splines or Clothoids."""


type alias FoldState =
    { arcStart : Maybe EarthPoint
    , vertex : Maybe EarthPoint
    , previous : Maybe RoadSection
    , outputs : List (List EarthPoint)
    }


computeNewPoints : Int -> TrackLoaded msg -> List PreviewPoint
computeNewPoints depth track =
    -- Fold has to alternate arcs and splines/clothoids.
    -- Maybe do them separately and the interweave.
    -- UPDATE: Simple fixed depth fold is not good enough.
    -- Options?
    -- 1: Use adaptive depth (not a fold) based on distance metric;
    -- 2: Use Bend finder to locate radius bends, treat rest as "straight".
    let
        makeLeadInAndArc : RoadSection -> FoldState -> FoldState
        makeLeadInAndArc road foldState =
            let
                startDirection =
                    Direction3d.xyZ
                        (Direction2d.toAngle road.directionAtStart)
                        (Angle.atan <| road.gradientAtStart / 100.0)

                endDirection =
                    Direction3d.xyZ
                        (Direction2d.toAngle road.directionAtEnd)
                        (Angle.atan <| road.gradientAtEnd / 100.0)

                ( startAxis, endAxis ) =
                    ( Axis3d.through road.startPoint startDirection
                    , Axis3d.through road.endPoint endDirection
                    )

                ( thisArcEnd, nextArcStart ) =
                    ( Point3d.along startAxis <|
                        Quantity.min
                            (Length.meters 10.0)
                            (Maybe.withDefault Quantity.zero <| Maybe.map .trueLength foldState.previous)
                    , Point3d.along endAxis <|
                        Quantity.negate <|
                            Quantity.min
                                (Length.meters 10.0)
                                road.trueLength
                    )

                arc =
                    case ( foldState.arcStart, foldState.vertex ) of
                        ( Just arcStart, Just vertex ) ->
                            Tools.BendSmoother.arc3dFromThreePoints
                                arcStart
                                vertex
                                thisArcEnd

                        _ ->
                            Nothing

                arcApproximation =
                    case arc of
                        Just realArc ->
                            realArc
                                |> Arc3d.approximate (Length.meters 0.5)
                                |> Polyline3d.vertices

                        Nothing ->
                            []

                triangle1 =
                    Triangle3d.from
                        road.startPoint
                        thisArcEnd
                        nextArcStart

                triangle2 =
                    Triangle3d.from
                        thisArcEnd
                        nextArcStart
                        road.endPoint

                ( ( c1, b1, a1 ), ( c2, b2, a2 ) ) =
                    -- Might not be the order you expected.
                    ( BezierSplines.controlPointsFromTriangle 1.0 triangle1
                    , BezierSplines.controlPointsFromTriangle 1.0 triangle2
                    )

                spline : CubicSpline3d Meters LocalCoords
                spline =
                    -- From previous road start to end, using control points
                    -- from adjacent edges.
                    CubicSpline3d.fromControlPoints b1 c1 a2 b2

                polylineFromSpline : Polyline3d Meters LocalCoords
                polylineFromSpline =
                    CubicSpline3d.approximate
                        (Length.meters 1.0)
                        spline

                vertices : List EarthPoint
                vertices =
                    Polyline3d.vertices polylineFromSpline
                        |> List.drop 1
                        |> List.reverse
            in
            { arcStart = Just nextArcStart
            , vertex = Just road.endPoint
            , outputs = vertices :: arcApproximation :: foldState.outputs
            , previous = Just road
            }

        dualFormat : EarthPoint -> PreviewPoint
        dualFormat earth =
            { earthPoint = earth
            , gpx = DomainModel.gpxFromPointWithReference track.referenceLonLat earth
            }
    in
    List.map dualFormat <|
        Utils.combineLists <|
            .outputs <|
                DomainModel.traverseTreeBetweenLimitsToDepth
                    0
                    (skipCount track.trackTree)
                    (always <| Just depth)
                    0
                    track.trackTree
                    makeLeadInAndArc
                    { arcStart = Nothing
                    , vertex = Nothing
                    , previous = Nothing
                    , outputs = []
                    }


applyUsingOptions : Options -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyUsingOptions options track =
    ( Nothing, [] )


applyWithBezier : Int -> TrackLoaded msg -> ( Maybe PeteTree, List GPXSource )
applyWithBezier depth track =
    let
        ( fromStart, fromEnd ) =
            ( 0, 0 )

        gpxPoints =
            List.map .gpx <| computeNewPoints depth track

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
        --( True, Just theTrack ) ->
        --    ( options, previewActions options colour theTrack )
        _ ->
            ( options, [ HidePreview "recursive" ] )


previewActions options colour track =
    [ ShowPreview
        { tag = "recursive"
        , shape = PreviewCircle
        , colour = colour
        , points = computeNewPoints options.depth track
        }
    ]


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    case msg of
        --SetDepth depth ->
        --    let
        --        newOptions =
        --            { options | depth = depth }
        --    in
        --    ( options, previewActions opt previewColour track )
        --
        --Apply ->
        --    ( options
        --    , [ Actions.RecursiveSmootherApplyWithOptions options
        --      , TrackHasChanged
        --      ]
        --    )
        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )

        _ ->
            ( options, [] )


view : Bool -> (Msg -> msg) -> Options -> TrackLoaded msg -> Element msg
view imperial wrapper options track =
    let
        applyButton =
            button
                neatToolsBorder
                { onPress = Just <| wrapper Apply
                , label = text "Smooth"
                }
    in
    column
        [ padding 10
        , spacing 5
        , width fill
        , centerX
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ none
        , applyButton
        ]
