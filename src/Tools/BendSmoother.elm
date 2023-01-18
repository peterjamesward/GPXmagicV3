module Tools.BendSmoother exposing (Msg(..), Point, applyUsingOptions, defaultOptions, softenMultiplePoints, toolId, toolStateChange, update, view)

import Actions exposing (ToolAction(..))
import Angle
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, endPoint, skipCount, startPoint)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input exposing (button)
import FlatColors.ChinesePalette
import Geometry101 as G exposing (distance, findIntercept, interpolateLine, isAfter, isBefore, lineEquationFromTwoPoints, lineIntersection, linePerpendicularTo, pointAlongRoad, pointsToGeometry)
import Length exposing (Meters, inMeters, meters)
import LineSegment2d
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d, xCoordinate, yCoordinate, zCoordinate)
import Polyline2d
import Polyline3d
import PreviewData exposing (PreviewShape(..))
import Quantity
import SketchPlane3d
import String.Interpolate
import Tools.BendSmootherOptions exposing (..)
import Tools.I18N as I18N
import Tools.I18NOptions as I18NOptions
import TrackLoaded exposing (TrackLoaded)
import Utils
import UtilsForViews exposing (showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (..)


toolId =
    "arcs"


defaultOptions : Options
defaultOptions =
    { bendTrackPointSpacing = 5.0
    , smoothedBend = Nothing
    , segments = 1
    , mode = SmoothBend
    }


type alias Point =
    Point2d Meters LocalCoords


type Msg
    = ApplySmoothBend
    | SetBendTrackPointSpacing Float
    | SetMode SmoothMode
    | SetSegments Int


tryBendSmoother : TrackLoaded msg -> Options -> Options
tryBendSmoother track options =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        ( startPoint, endPoint ) =
            -- Legacy code is zero based from start.
            ( fromStart, skipCount track.trackTree - fromEnd )
    in
    { options
        | smoothedBend =
            if endPoint >= startPoint + 2 then
                lookForSmoothBendOption options.bendTrackPointSpacing track startPoint endPoint

            else
                Nothing
    }


applyUsingOptions : Options -> TrackLoaded msg -> Maybe PeteTree
applyUsingOptions options track =
    case options.mode of
        SmoothPoint ->
            softenSinglePoint options.segments track.currentPosition track

        SmoothBend ->
            applyClassicBendSmoother options track


applyClassicBendSmoother : Options -> TrackLoaded msg -> Maybe PeteTree
applyClassicBendSmoother options track =
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        gpxPoints =
            case options.smoothedBend of
                Just bend ->
                    List.map .gpx bend.nodes

                Nothing ->
                    []

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                gpxPoints
                track.trackTree
    in
    newTree


softenMultiplePoints : Options -> List Int -> TrackLoaded msg -> Maybe PeteTree
softenMultiplePoints options indices track =
    -- There may be a more efficient way...
    let
        helper : Int -> PeteTree -> PeteTree
        helper index previousTree =
            case
                softenSinglePoint options.segments index { track | trackTree = previousTree }
            of
                Just newTree ->
                    newTree

                Nothing ->
                    track.trackTree

        finalTree =
            -- Indices must be in descending order or we lose context.
            indices
                |> List.sort
                |> List.reverse
                |> List.foldl helper track.trackTree
    in
    Just finalTree


softenSinglePoint : Int -> Int -> TrackLoaded msg -> Maybe PeteTree
softenSinglePoint numSegments index track =
    -- Apply the new bend smoother to a single point, if possible.
    case singlePoint3dArc track index of
        Just arc ->
            let
                gpxPoints =
                    Arc3d.segments numSegments arc
                        |> Polyline3d.vertices
                        |> List.map
                            (DomainModel.withoutTime
                                >> DomainModel.gpxFromPointWithReference track.referenceLonLat
                            )

                newTree =
                    DomainModel.replaceRange
                        index
                        (skipCount track.trackTree - index)
                        track.referenceLonLat
                        gpxPoints
                        track.trackTree
            in
            newTree

        Nothing ->
            Just track.trackTree


singlePoint3dArc : TrackLoaded msg -> Int -> Maybe (Arc3d Meters LocalCoords)
singlePoint3dArc track index =
    let
        ( pa, pb, pc ) =
            ( DomainModel.earthPointFromIndex (index - 1) track.trackTree
            , DomainModel.earthPointFromIndex index track.trackTree
            , DomainModel.earthPointFromIndex (index + 1) track.trackTree
            )
    in
    arc3dFromThreePoints pa pb pc


arc3dFromThreePoints : EarthPoint -> EarthPoint -> EarthPoint -> Maybe (Arc3d Meters LocalCoords)
arc3dFromThreePoints pa pb pc =
    -- Must have three points to play with!
    let
        ( beforeLength, afterLength ) =
            ( Point3d.distanceFrom pa.space pb.space, Point3d.distanceFrom pb.space pc.space )

        trianglePlane =
            SketchPlane3d.throughPoints pa.space pb.space pc.space
    in
    case trianglePlane of
        -- Points necessarily co-planar but type requires us to check!
        Just plane ->
            let
                amountToStealFromFirstSegment =
                    Quantity.min (meters 4.0) (Quantity.half beforeLength)

                amountToStealFromSecondSegment =
                    Quantity.min (meters 4.0) (Quantity.half afterLength)

                commonAmountToSteal =
                    Quantity.min amountToStealFromFirstSegment amountToStealFromSecondSegment

                arcStart =
                    Point3d.interpolateFrom
                        pb.space
                        pa.space
                        (Quantity.ratio commonAmountToSteal beforeLength)

                arcEnd =
                    Point3d.interpolateFrom
                        pb.space
                        pc.space
                        (Quantity.ratio commonAmountToSteal afterLength)

                ( planarA, planarB, planarC ) =
                    -- I think if we project into 2d, the classic logic will hold.
                    ( arcStart |> Point3d.projectInto plane
                    , pb.space |> Point3d.projectInto plane
                    , arcEnd |> Point3d.projectInto plane
                    )

                ( r1Equation, r2Equation ) =
                    ( lineEquationFromTwoPoints
                        (Point2d.toRecord inMeters planarA)
                        (Point2d.toRecord inMeters planarB)
                    , lineEquationFromTwoPoints
                        (Point2d.toRecord inMeters planarB)
                        (Point2d.toRecord inMeters planarC)
                    )

                ( perpFromFirstTangentPoint, perpFromSecondTangentPoint ) =
                    ( linePerpendicularTo r1Equation (Point2d.toRecord inMeters planarA)
                    , linePerpendicularTo r2Equation (Point2d.toRecord inMeters planarC)
                    )

                circleCenter =
                    lineIntersection perpFromFirstTangentPoint perpFromSecondTangentPoint

                findArc centre =
                    let
                        radius =
                            distance centre (Point2d.toRecord inMeters planarA)

                        bisector =
                            Vector2d.from
                                (Point2d.fromRecord meters centre)
                                planarB

                        midArcPoint =
                            Point2d.fromRecord meters centre
                                |> Point2d.translateBy
                                    (Vector2d.scaleTo (meters radius) bisector)

                        midPoint3d =
                            midArcPoint |> Point3d.on plane
                    in
                    Arc3d.throughPoints arcStart midPoint3d arcEnd
            in
            Maybe.withDefault Nothing <| Maybe.map findArc circleCenter

        Nothing ->
            Nothing


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            let
                newOptions =
                    options |> tryBendSmoother theTrack
            in
            ( newOptions, previewActions newOptions colour theTrack )

        _ ->
            ( options, [ HidePreview "bend" ] )


previewActions options colour track =
    case options.smoothedBend of
        Just bend ->
            [ ShowPreview
                { tag = "bend"
                , shape = PreviewCircle
                , colour = colour
                , points = bend.nodes
                }
            ]

        Nothing ->
            [ HidePreview "bend" ]


undoEntryFrom : Options -> TrackLoaded msg -> Actions.UndoEntry msg
undoEntryFrom options track =
    -- Note does not use default undo because of start and end.
    let
        ( fromStart, fromEnd ) =
            TrackLoaded.getRangeFromMarkers track

        oldPoints =
            DomainModel.extractPointsInRange
                (fromStart + 1)
                (fromEnd + 1)
                track.trackTree
    in
    { action = Actions.BendSmootherApplyWithOptions options
    , originalPoints = List.map Tuple.second oldPoints
    , fromStart = fromStart
    , fromEnd = fromEnd
    , currentPosition = track.currentPosition
    , markerPosition = track.markerPosition
    }


update :
    Msg
    -> Options
    -> Element.Color
    -> TrackLoaded msg
    -> ( Options, List (ToolAction msg) )
update msg options previewColour track =
    case msg of
        SetBendTrackPointSpacing spacing ->
            let
                newOptions =
                    { options | bendTrackPointSpacing = spacing }
                        |> tryBendSmoother track
            in
            ( newOptions, previewActions newOptions previewColour track )

        ApplySmoothBend ->
            ( options
            , [ Actions.BendSmootherApplyWithOptions options
              , Actions.WithUndo (undoEntryFrom options track)
              , Actions.TrackHasChanged
              ]
            )

        SetMode mode ->
            let
                newOptions =
                    { options | mode = mode }
            in
            ( newOptions, [] )

        SetSegments segments ->
            let
                newOptions =
                    { options | segments = segments }
            in
            ( newOptions, [] )


viewBendControls :
    I18NOptions.Location
    -> Bool
    -> (Msg -> msg)
    -> Options
    -> Maybe (TrackLoaded msg)
    -> Element msg
viewBendControls location imperial wrapper options track =
    let
        i18n =
            I18N.text location toolId

        fixBendButton smooth =
            button
                neatToolsBorder
                { onPress = Just <| wrapper ApplySmoothBend
                , label =
                    case smooth of
                        Just isSmooth ->
                            paragraph [] <|
                                [ text <|
                                    String.Interpolate.interpolate
                                        (I18N.localisedString location toolId "smooth")
                                        [ showShortMeasure imperial (Length.meters isSmooth.radius) ]
                                ]

                        Nothing ->
                            i18n "none"
                }
    in
    case track of
        Just _ ->
            column
                [ padding 5
                , spacing 5
                , width fill
                , centerX
                ]
                [ el [ centerX ] <| bendSmoothnessSlider location imperial options wrapper
                , el [ centerX ] <| fixBendButton options.smoothedBend
                ]

        Nothing ->
            noTrackMessage location


viewPointControls : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
viewPointControls location imperial wrapper options track =
    case track of
        Just _ ->
            let
                fixButton =
                    button
                        neatToolsBorder
                        { onPress = Just <| wrapper ApplySmoothBend
                        , label = text "Smooth points"
                        }
            in
            column
                [ padding 5
                , spacing 5
                , width fill
                , centerX
                ]
                [ el [ centerX ] <| segmentSlider imperial options wrapper
                , el [ centerX ] <| fixButton
                ]

        Nothing ->
            noTrackMessage location


view : I18NOptions.Location -> Bool -> (Msg -> msg) -> Options -> Maybe (TrackLoaded msg) -> Element msg
view location imperial wrapper options track =
    let
        i18n =
            I18N.text location toolId
    in
    column
        [ padding 10
        , spacing 5
        , width fill
        , centerX
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ el [ centerX ] <|
            Input.radioRow
                [ spacing 5 ]
                { options =
                    [ Input.option SmoothBend <| i18n "Bend"
                    , Input.option SmoothPoint <| i18n "Point"
                    ]
                , onChange = wrapper << SetMode
                , selected = Just options.mode
                , label = Input.labelHidden "mode"
                }
        , case options.mode of
            SmoothBend ->
                viewBendControls location imperial wrapper options track

            SmoothPoint ->
                viewPointControls location imperial wrapper options track
        ]


bendSmoothnessSlider : I18NOptions.Location -> Bool -> Options -> (Msg -> msg) -> Element msg
bendSmoothnessSlider location imperial options wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetBendTrackPointSpacing
        , label =
            Input.labelBelow [] <|
                text <|
                    String.Interpolate.interpolate
                        (I18N.localisedString location toolId "spacing")
                        [ showShortMeasure imperial (Length.meters options.bendTrackPointSpacing) ]
        , min =
            Length.inMeters <|
                if imperial then
                    Length.feet 3.0

                else
                    Length.meters 1.0
        , max =
            Length.inMeters <|
                if imperial then
                    Length.feet 30.0

                else
                    Length.meters 10.0
        , step = Nothing
        , value = options.bendTrackPointSpacing
        , thumb = Input.defaultThumb
        }


segmentSlider : Bool -> Options -> (Msg -> msg) -> Element msg
segmentSlider imperial options wrap =
    Input.slider
        commonShortHorizontalSliderStyles
        { onChange = wrap << SetSegments << round
        , label =
            Input.labelBelow [] <|
                text <|
                    "Segments: "
                        ++ String.fromInt options.segments
        , min = 1.0
        , max = 7.0
        , step = Just 1.0
        , value = toFloat options.segments
        , thumb = Input.defaultThumb
        }



-- Here be the geometry.


roadToGeometry : RoadSection -> G.Road
roadToGeometry road =
    { startAt =
        { x = Length.inMeters <| xCoordinate <| road.startPoint.space
        , y = Length.inMeters <| yCoordinate <| road.startPoint.space
        }
    , endsAt =
        { x = Length.inMeters <| xCoordinate <| road.endPoint.space
        , y = Length.inMeters <| yCoordinate <| road.endPoint.space
        }
    }


lookForSmoothBendOption :
    Float
    -> TrackLoaded msg
    -> Int
    -> Int
    -> Maybe SmoothedBend
lookForSmoothBendOption trackPointSpacing track pointA pointD =
    --NOTE that this really does all the work; "Apply" just takes this and uses it.
    let
        ( roadAB, roadCD ) =
            ( DomainModel.asRecord <| DomainModel.leafFromIndex pointA track.trackTree
            , DomainModel.asRecord <| DomainModel.leafFromIndex (pointD - 1) track.trackTree
            )

        -- Try to make minimal changes from v1. Is that wise?
        ( roadIn, roadOut ) =
            ( roadToGeometry roadAB, roadToGeometry roadCD )

        arcFinderGeneral p =
            if isBefore roadIn p && isAfter roadOut p then
                divergentRoadsArc p roadAB roadCD

            else if isAfter roadIn p && isBefore roadOut p then
                convergentRoadsArc p roadAB roadCD

            else
                Nothing

        maybeArc =
            case findIntercept roadIn roadOut of
                Nothing ->
                    parallelFindSemicircle roadAB roadCD

                Just p ->
                    arcFinderGeneral p
    in
    case maybeArc of
        Just arc ->
            let
                nodes =
                    makeSmoothBend trackPointSpacing roadAB roadCD arc

                distanceToBend =
                    DomainModel.distanceFromIndex pointA track.trackTree
                        |> Quantity.plus
                            (case nodes of
                                p1 :: _ ->
                                    Point3d.distanceFrom
                                        (.space <| DomainModel.earthPointFromIndex pointA track.trackTree)
                                        p1.space

                                _ ->
                                    Quantity.zero
                            )

                previewsWithAdjustedDistance =
                    -- Untidy distance adjustment
                    TrackLoaded.asPreviewPoints track distanceToBend nodes
            in
            Just
                { nodes = previewsWithAdjustedDistance
                , centre = Arc2d.centerPoint arc
                , radius = Length.inMeters <| Arc2d.radius arc
                , startIndex = pointA
                , endIndex = pointD
                }

        Nothing ->
            Nothing


withElevation : Float -> Point2d Length.Meters LocalCoords -> Point3d Meters LocalCoords
withElevation elevation p2 =
    let
        { x, y } =
            Point2d.toMeters p2
    in
    Point3d.fromMeters { x = x, y = y, z = elevation }


makeSmoothBend :
    Float
    -> RoadSection
    -> RoadSection
    -> Arc2d Meters LocalCoords
    -> List EarthPoint
makeSmoothBend trackPointSpacing roadAB roadCD arc =
    -- Note return list here includes points A and D.
    --TODO: Interpolate timestamps if present. BUT HERE IS EARTHPOINTS.
    let
        trueArcLength =
            (abs <| Angle.inRadians <| Arc2d.sweptAngle arc)
                * (Length.inMeters <| Arc2d.radius arc)

        numberPointsOnArc =
            ceiling <| trueArcLength / trackPointSpacing

        segments =
            Arc2d.segments (numberPointsOnArc - 1) arc
                |> Polyline2d.segments

        realArcLength =
            List.sum <| List.map (inMeters << LineSegment2d.length) segments

        ( p1, p2 ) =
            -- The first (last) tangent point is also the first (last) point on the arc
            -- so we don't need to pass these as arguments.
            ( Arc2d.startPoint arc |> Point2d.toRecord inMeters
            , Arc2d.endPoint arc |> Point2d.toRecord inMeters
            )

        ( elevationAtA, elevationAtD ) =
            ( Length.inMeters <| zCoordinate roadAB.startPoint.space
            , Length.inMeters <| zCoordinate roadCD.endPoint.space
            )

        ( timeAtA, timeAtD ) =
            ( roadAB.sourceData |> Tuple.first |> .timestamp
            , roadCD.sourceData |> Tuple.second |> .timestamp
            )

        ( tang1, tang2 ) =
            -- Say the arc entry is at same elevation as end points
            ( Point3d.fromTuple Length.meters ( p1.x, p1.y, elevationAtA )
            , Point3d.fromTuple Length.meters ( p2.x, p2.y, elevationAtD )
            )

        ( entryStraightLength, exitStraightLength ) =
            ( Length.inMeters <| Point3d.distanceFrom roadAB.startPoint.space tang1
            , Length.inMeters <| Point3d.distanceFrom tang2 roadCD.endPoint.space
            )

        totalNewLength =
            -- if we ignore gradient for now
            entryStraightLength + realArcLength + exitStraightLength

        ( tangent1Elevation, tangent2Elevation ) =
            ( elevationAtA + (entryStraightLength / totalNewLength) * (elevationAtD - elevationAtA)
            , elevationAtD + (exitStraightLength / totalNewLength) * (elevationAtA - elevationAtD)
            )

        ( tangent1Time, tangent2Time ) =
            ( Utils.interpolateTimes (entryStraightLength / totalNewLength) timeAtA timeAtD
            , Utils.interpolateTimes (exitStraightLength / totalNewLength) timeAtD timeAtA
            )

        ( newEntryPoint, newExitPoint ) =
            ( { space =
                    Point3d.translateBy
                        (Vector3d.fromMeters { x = 0, y = 0, z = tangent1Elevation - elevationAtA })
                        tang1
              , time = tangent1Time
              }
            , { space =
                    Point3d.translateBy
                        (Vector3d.fromMeters { x = 0, y = 0, z = tangent2Elevation - elevationAtD })
                        tang2
              , time = tangent2Time
              }
            )

        eleIncrement =
            (tangent2Elevation - tangent1Elevation) / (toFloat numberPointsOnArc - 1)

        elevate point2d i =
            withElevation
                (tangent1Elevation + toFloat i * eleIncrement)
                point2d

        newArcPoints =
            List.map2
                elevate
                (List.map LineSegment2d.startPoint <| List.drop 1 segments)
                (List.range 1 (numberPointsOnArc - 1))

        newArcTimes =
            Utils.equalIntervals
                (List.length segments)
                tangent1Time
                tangent2Time

        newEarthPoints =
            [ roadAB.startPoint, newEntryPoint ]
                ++ List.map2 DomainModel.withTime newArcTimes newArcPoints
                ++ [ newExitPoint, roadCD.endPoint ]
    in
    List.drop 1 <| List.take (List.length newEarthPoints - 1) newEarthPoints


toPlanarPoint : EarthPoint -> G.Point
toPlanarPoint pt =
    let
        { x, y } =
            Point3d.toRecord inMeters pt.space
    in
    { x = x, y = y }


divergentRoadsArc : G.Point -> RoadSection -> RoadSection -> Maybe (Arc2d Meters LocalCoords)
divergentRoadsArc p r1 r2 =
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( toPlanarPoint r1.startPoint
              , toPlanarPoint r1.endPoint
              )
            , ( toPlanarPoint r2.startPoint
              , toPlanarPoint r2.endPoint
              )
            )

        ( midAB, midCD ) =
            ( interpolateLine 0.5 pa pb, interpolateLine 0.5 pc pd )

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( firstTangentPoint, secondTangentPoint ) =
            -- For divergence, choose midpoint farthest from interesct p.
            if distance p midAB >= distance p midCD then
                ( midAB, pointAlongRoad (pointsToGeometry p pc) (distance p midAB) )

            else
                ( pointAlongRoad (pointsToGeometry p pb) (distance p midCD), midCD )

        ( perpFromFirstTangentPoint, perpFromSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation firstTangentPoint, linePerpendicularTo r2Equation secondTangentPoint )

        circleCenter =
            lineIntersection perpFromFirstTangentPoint perpFromSecondTangentPoint

        findArc : G.Point -> Maybe (Arc2d Meters LocalCoords)
        findArc centre =
            let
                radius =
                    distance centre firstTangentPoint

                bisectorAsRoad =
                    { startAt = p, endsAt = centre }

                midArcPoint =
                    pointAlongRoad
                        bisectorAsRoad
                        (radius + distance p centre)
            in
            Arc2d.throughPoints
                (Point2d.meters firstTangentPoint.x firstTangentPoint.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters secondTangentPoint.x secondTangentPoint.y)
    in
    Maybe.withDefault Nothing <| Maybe.map findArc circleCenter


convergentRoadsArc : G.Point -> RoadSection -> RoadSection -> Maybe (Arc2d Meters LocalCoords)
convergentRoadsArc p r1 r2 =
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( toPlanarPoint r1.startPoint
              , toPlanarPoint r1.endPoint
              )
            , ( toPlanarPoint r2.startPoint
              , toPlanarPoint r2.endPoint
              )
            )

        ( midAB, midCD ) =
            ( interpolateLine 0.5 pa pb, interpolateLine 0.5 pc pd )

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( firstTangentPoint, secondTangentPoint ) =
            if distance p midAB <= distance p midCD then
                ( midAB, pointAlongRoad (pointsToGeometry p pd) (distance p midAB) )

            else
                ( pointAlongRoad (pointsToGeometry p pa) (distance p midCD), midCD )

        ( perpFromFirstTangentPoint, perpFromSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation firstTangentPoint, linePerpendicularTo r2Equation secondTangentPoint )

        circleCenter =
            lineIntersection perpFromFirstTangentPoint perpFromSecondTangentPoint

        findArc centre =
            let
                radius =
                    distance centre firstTangentPoint

                bisectorAsRoad =
                    { startAt = centre, endsAt = p }

                midArcPoint =
                    pointAlongRoad bisectorAsRoad radius
            in
            Arc2d.throughPoints
                (Point2d.meters firstTangentPoint.x firstTangentPoint.y)
                (Point2d.meters midArcPoint.x midArcPoint.y)
                (Point2d.meters secondTangentPoint.x secondTangentPoint.y)
    in
    Maybe.withDefault Nothing <| Maybe.map findArc circleCenter


parallelFindSemicircle : RoadSection -> RoadSection -> Maybe (Arc2d Meters LocalCoords)
parallelFindSemicircle r1 r2 =
    let
        ( ( pa, pb ), ( pc, pd ) ) =
            ( ( toPlanarPoint r1.startPoint
              , toPlanarPoint r1.endPoint
              )
            , ( toPlanarPoint r2.startPoint
              , toPlanarPoint r2.endPoint
              )
            )

        ( _, midBC ) =
            ( interpolateLine 0.5 pa pb
            , interpolateLine 0.5 pb pc
            )

        ( _, midDA ) =
            ( interpolateLine 0.5 pc pd
            , interpolateLine 0.5 pd pa
            )

        middle =
            -- As lines are parallel, we can use this as the circle centre.
            interpolateLine 0.5 midBC midDA

        ( r1Equation, r2Equation ) =
            ( lineEquationFromTwoPoints pa pb, lineEquationFromTwoPoints pc pd )

        ( radiusToFirstTangentPoint, radiusToSecondTangentPoint ) =
            ( linePerpendicularTo r1Equation middle, linePerpendicularTo r2Equation middle )

        ( firstTangentPoint, secondTangentPoint ) =
            ( lineIntersection r1Equation radiusToFirstTangentPoint
            , lineIntersection r2Equation radiusToSecondTangentPoint
            )
    in
    case ( firstTangentPoint, secondTangentPoint ) of
        ( Just t1, Just t2 ) ->
            let
                radius =
                    distance middle t1
            in
            if radius > 0.0 && radius < 1000.0 then
                let
                    centreLine =
                        { startAt = middle, endsAt = midBC }

                    midArcPoint =
                        pointAlongRoad centreLine radius
                in
                Arc2d.throughPoints
                    (Point2d.meters t1.x t1.y)
                    (Point2d.meters midArcPoint.x midArcPoint.y)
                    (Point2d.meters t2.x t2.y)

            else
                Nothing

        _ ->
            Nothing
