module BezierSplines exposing (..)

{-
   This is the "math" for the splines, the controls are in the Tools section.
-}

import CubicSpline3d exposing (CubicSpline3d)
import DomainModel exposing (EarthPoint, PeteTree, RoadSection)
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Time
import Triangle3d exposing (Triangle3d)
import Utils
import Vector3d


type alias ControlPoint =
    -- For clarity.
    Point3d Meters LocalCoords


type alias SplineFoldState =
    { roadMinusOne : Maybe RoadSection
    , roadMinusTwo : Maybe RoadSection
    , newPoints : List EarthPoint
    }


combinePointAndTime : Point3d Meters LocalCoords -> Maybe Time.Posix -> EarthPoint
combinePointAndTime place time =
    { space = place, time = time }


bezierSplinesThroughExistingPoints : Bool -> Float -> Float -> Int -> Int -> PeteTree -> List EarthPoint
bezierSplinesThroughExistingPoints isLoop tension tolerance startIndx endIndex treeNode =
    -- TODO: Wrap around on loop.
    let
        foldFn : RoadSection -> SplineFoldState -> SplineFoldState
        foldFn road state =
            case ( state.roadMinusOne, state.roadMinusTwo ) of
                ( Nothing, Nothing ) ->
                    -- Defer action until we have three road pieces.
                    { state | roadMinusOne = Just road }

                ( Just previousRoad, Nothing ) ->
                    { state
                        | roadMinusTwo = state.roadMinusOne
                        , roadMinusOne = Just road
                    }

                ( Nothing, Just cantHappen ) ->
                    state

                ( Just roadMinusOne, Just roadMinusTwo ) ->
                    let
                        triangle1 =
                            -- Minor inefficiency re-creating triangles is not worth worrying about.
                            Triangle3d.from
                                roadMinusTwo.startPoint.space
                                roadMinusTwo.endPoint.space
                                roadMinusOne.endPoint.space

                        triangle2 =
                            Triangle3d.from
                                roadMinusOne.startPoint.space
                                roadMinusOne.endPoint.space
                                road.endPoint.space

                        ( ( c1, b1, a1 ), ( c2, b2, a2 ) ) =
                            -- Might not be the order you expected.
                            ( controlPointsFromTriangle tension triangle1
                            , controlPointsFromTriangle tension triangle2
                            )

                        spline : CubicSpline3d Meters LocalCoords
                        spline =
                            -- From previous road start to end, using control points
                            -- from adjacent edges.
                            CubicSpline3d.fromControlPoints b1 c1 a2 b2

                        polylineFromSpline : Polyline3d Meters LocalCoords
                        polylineFromSpline =
                            CubicSpline3d.approximate
                                (Length.meters <| 0.2 * tolerance)
                                spline

                        vertices : List (Point3d Meters LocalCoords)
                        vertices =
                            Polyline3d.vertices polylineFromSpline
                                |> List.drop 1
                                |> List.reverse

                        ( timeAtStart, timeAtEnd ) =
                            ( road.startPoint.time, road.endPoint.time )

                        times : List (Maybe Time.Posix)
                        times =
                            List.range 0 (List.length vertices - 1)
                                |> List.map
                                    (\vertexNum ->
                                        Utils.interpolateTimes
                                            (toFloat vertexNum / toFloat (List.length vertices))
                                            timeAtStart
                                            timeAtEnd
                                    )
                    in
                    { state
                        | roadMinusTwo = state.roadMinusOne
                        , roadMinusOne = Just road
                        , newPoints =
                            List.map2 combinePointAndTime vertices times
                                ++ state.newPoints
                    }

        foldOutput =
            if isLoop then
                DomainModel.traverseTreeBetweenLimitsToDepth
                    startIndx
                    endIndex
                    (always Nothing)
                    0
                    treeNode
                    foldFn
                    (SplineFoldState
                        (Just <| DomainModel.getLastLeaf treeNode)
                        Nothing
                        []
                    )

            else
                DomainModel.traverseTreeBetweenLimitsToDepth
                    startIndx
                    endIndex
                    (always Nothing)
                    0
                    treeNode
                    foldFn
                    (SplineFoldState Nothing Nothing [])
    in
    foldOutput.newPoints |> List.reverse


controlPointsFromTriangle :
    Float
    -> Triangle3d Meters LocalCoords
    -> ( ControlPoint, ControlPoint, ControlPoint )
controlPointsFromTriangle tension triangle =
    let
        ( _, b, _ ) =
            Triangle3d.vertices triangle

        ( entryEdge, exitEdge, oppositeEdge ) =
            Triangle3d.edges triangle

        ( ab, ac, bc ) =
            ( Length.inMeters <| LineSegment3d.length entryEdge
            , Length.inMeters <| LineSegment3d.length oppositeEdge
            , Length.inMeters <| LineSegment3d.length exitEdge
            )

        ( entryFactor, exitFactor ) =
            ( -1.0 * tension * ab / (ab + bc)
            , tension * bc / (ab + bc)
            )

        controlPointVector =
            Vector3d.from
                (LineSegment3d.startPoint oppositeEdge)
                (LineSegment3d.endPoint oppositeEdge)

        ( entryScaleVector, exitScalevector ) =
            ( Vector3d.scaleBy entryFactor controlPointVector
            , Vector3d.scaleBy exitFactor controlPointVector
            )

        ( entryPoint, exitPoint ) =
            ( Point3d.translateBy entryScaleVector b
            , Point3d.translateBy exitScalevector b
            )
    in
    ( entryPoint, b, exitPoint )


bezierSplineApproximation : Bool -> Float -> Float -> Int -> Int -> PeteTree -> List EarthPoint
bezierSplineApproximation isLoop tension tolerance startIndx endIndex treeNode =
    --TODO: Wrap around on loop.
    let
        midPoint : RoadSection -> Point3d Meters LocalCoords
        midPoint road =
            Point3d.midpoint road.startPoint.space road.endPoint.space

        foldFn : RoadSection -> SplineFoldState -> SplineFoldState
        foldFn road state =
            case state.roadMinusOne of
                Nothing ->
                    -- Defer action until we have three road pieces.
                    { state | roadMinusOne = Just road }

                Just roadMinusOne ->
                    let
                        ( ( b1, c1 ), ( a2, b2 ) ) =
                            ( ( midPoint roadMinusOne, roadMinusOne.endPoint.space )
                            , ( road.startPoint.space, midPoint road )
                            )

                        spline : CubicSpline3d Meters LocalCoords
                        spline =
                            -- From previous road start to end, using control points
                            -- from adjacent edges.
                            CubicSpline3d.fromControlPoints b1 c1 a2 b2

                        polylineFromSpline : Polyline3d Meters LocalCoords
                        polylineFromSpline =
                            CubicSpline3d.approximate
                                (Length.meters <| 0.2 * tolerance)
                                spline

                        vertices : List (Point3d Meters LocalCoords)
                        vertices =
                            Polyline3d.vertices polylineFromSpline
                                |> List.drop 1
                                |> List.reverse

                        ( timeAtStart, timeAtEnd ) =
                            ( road.startPoint.time, road.endPoint.time )

                        times : List (Maybe Time.Posix)
                        times =
                            timeAtStart
                                :: Utils.equalIntervals
                                    (List.length vertices)
                                    timeAtStart
                                    timeAtEnd
                                ++ [ timeAtEnd ]
                    in
                    { state
                        | roadMinusTwo = state.roadMinusOne
                        , roadMinusOne = Just road
                        , newPoints =
                            List.map2 combinePointAndTime vertices times
                                ++ state.newPoints
                    }

        foldOutput =
            DomainModel.traverseTreeBetweenLimitsToDepth
                startIndx
                endIndex
                (always Nothing)
                0
                treeNode
                foldFn
                (SplineFoldState Nothing Nothing [])
    in
    foldOutput.newPoints |> List.reverse
