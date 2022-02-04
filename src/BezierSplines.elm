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
import Triangle3d exposing (Triangle3d)
import Vector3d


type alias ControlPoint =
    Point3d Meters LocalCoords


type alias SplineFoldState =
    { roadMinusOne : Maybe RoadSection
    , roadMinusTwo : Maybe RoadSection
    , newPoints : List EarthPoint
    }


bezierSplinesThroughExistingPoints : Bool -> Float -> Float -> Int -> Int -> PeteTree -> List EarthPoint
bezierSplinesThroughExistingPoints isLoop tension tolerance startIndx endIndex treeNode =
    -- I think we can do this with a fold, but we need two adjacent segments to make a triangle.
    -- For each triangle, we figure out the control points, make a spline, approximate it.
    -- The caller can convert to GPX coordinates and splice into the tree.
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
                                roadMinusTwo.startPoint
                                roadMinusTwo.endPoint
                                roadMinusOne.endPoint

                        triangle2 =
                            Triangle3d.from
                                roadMinusOne.startPoint
                                roadMinusOne.endPoint
                                road.endPoint

                        ( ( c1, b1, a1 ), ( c2, b2, a2 ) ) =
                            -- Might not be the order you expected.
                            ( controlPointsFromTriangle triangle1
                            , controlPointsFromTriangle triangle2
                            )

                        spline : CubicSpline3d Meters LocalCoords
                        spline =
                            -- From previous road start to end, using control points
                            -- from adjacent edges.
                            CubicSpline3d.fromControlPoints b1 c1 a2 b2

                        polylineFromSpline : Polyline3d Meters LocalCoords
                        polylineFromSpline =
                            CubicSpline3d.approximate (Length.meters tolerance) spline

                        asSegments : List (LineSegment3d Length.Meters LocalCoords)
                        asSegments =
                            Polyline3d.segments polylineFromSpline

                        asPointsAgain : List EarthPoint
                        asPointsAgain =
                            List.map
                                LineSegment3d.startPoint
                                (List.take 1 asSegments)
                                ++ List.map
                                    LineSegment3d.endPoint
                                    asSegments
                    in
                    { state
                        | roadMinusTwo = state.roadMinusOne
                        , roadMinusOne = Just road

                        --, newPoints = [ c1, c2 ] ++ state.newPoints
                        , newPoints = (asPointsAgain |> List.reverse) ++ state.newPoints
                    }

        controlPointsFromTriangle :
            Triangle3d Meters LocalCoords
            -> ( ControlPoint, ControlPoint, ControlPoint )
        controlPointsFromTriangle triangle =
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
    foldOutput.newPoints



{-

   bezierApproximation : Bool -> Float -> Float -> List TrackPoint -> List TrackPoint
   bezierApproximation _ _ tolerance points =
       -- This variant uses existing points as controls, and lets the result approximate the route.
       -- Arguments compatible; loopiness and tension not used.
       let
           rawPoints =
               List.map .xyz points

           makeSpline first second third =
               let
                   ( start, end ) =
                       ( Point3d.midpoint first second
                       , Point3d.midpoint second third
                       )
               in
               CubicSpline3d.fromControlPoints
                   start
                   second
                   second
                   end

           makeSplines =
               List.map3
                   makeSpline
                   rawPoints
                   (List.drop 1 rawPoints)
                   (List.drop 2 rawPoints)

           asPolylines =
               List.map
                   (CubicSpline3d.approximate (Length.meters tolerance))
                   makeSplines

           asSegments =
               List.concatMap
                   Polyline3d.segments
                   asPolylines

           asPointsAgain =
               List.map
                   LineSegment3d.startPoint
                   (List.take 1 asSegments)
                   ++ List.map
                       LineSegment3d.endPoint
                       asSegments
       in
       List.take 1 points
           ++ List.map trackPointFromPoint asPointsAgain
           ++ List.drop (List.length points - 1) points
-}
-- END
