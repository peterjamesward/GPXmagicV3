module RoadIndex exposing (..)

{-
   Here we compile a dictionary of Leaf so that we can see which Leafs:
   * cross each other,
   * use the same piece of road in the same direction
   * use the same piece of road in the opposite direction
   This is simpler than Graph but may be useful there.
   In v3 we have no explicit SpatialIndex but the PeteTree has bounding box information
   and should be nearly as effective if not as efficient. (see `queryRoadsUsingFilter`)
-}

import Axis2d
import DomainModel exposing (PeteTree, RoadSection, foldOverRoute)
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d
import Quantity
import Quantity.Interval as Interval
import SketchPlane3d
import SpatialIndex exposing (SpatialContent, SpatialNode)
import UtilsForViews exposing (flatBox)


type alias PointXY =
    -- 2D projections of Earth Points.
    Point2d Meters LocalCoords


type IntersectionType
    = Crossing PointXY
    | SameDirection
    | ContraDirection


type alias Intersection =
    { thisSegment : Int
    , otherSegment : Int
    , category : IntersectionType
    }


type alias RoadIndex =
    SpatialNode ( Int, RoadSection ) Meters LocalCoords


findFeatures : PeteTree -> List Intersection
findFeatures treeNode =
    -- Top level API, folds over the tree to build the dictionary.
    let
        planarBox =
            DomainModel.boundingBox treeNode |> flatBox

        ( _, _, intersections ) =
            foldOverRoute
                checkLeafForIntersections
                treeNode
                ( 0
                , SpatialIndex.empty planarBox (Length.meters 5.0)
                , []
                )
    in
    List.reverse intersections


checkLeafForIntersections :
    RoadSection
    -> ( Int, RoadIndex, List Intersection )
    -> ( Int, RoadIndex, List Intersection )
checkLeafForIntersections myRoad ( myLeafNumber, index, intersects ) =
    let
        thisSegment =
            LineSegment3d.from myRoad.startPoint.space myRoad.endPoint.space
                |> LineSegment3d.projectInto SketchPlane3d.xy

        prepContent : SpatialContent ( Int, RoadSection ) Meters LocalCoords
        prepContent =
            { content = ( myLeafNumber, myRoad )
            , box = flatBox myRoad.boundingBox
            }

        thisAxis =
            Axis2d.through
                (myRoad.startPoint.space |> Point3d.projectInto SketchPlane3d.xy)
                myRoad.directionAtStart

        candidates : List ( Int, RoadSection )
        candidates =
            SpatialIndex.query index prepContent.box
                |> List.map .content

        roadHasOverlap : ( Int, RoadSection ) -> Maybe Intersection
        roadHasOverlap ( otherIndex, otherRoad ) =
            -- Work out what type of overlap this is and add to index.
            let
                otherSegment =
                    LineSegment3d.from otherRoad.startPoint.space otherRoad.endPoint.space
                        |> LineSegment3d.projectInto SketchPlane3d.xy

                intersectPoint =
                    -- See if lines cross (not Nothing)
                    LineSegment2d.intersectionPoint thisSegment otherSegment

                axisIntersection =
                    -- Nothing means co-linear, in either direction
                    otherSegment |> LineSegment2d.intersectionWithAxis thisAxis

                axisSeparation =
                    otherSegment |> LineSegment2d.signedDistanceFrom thisAxis

                proximal =
                    axisSeparation |> Interval.contains Quantity.zero

                notAdjacent =
                    abs (myLeafNumber - otherIndex) > 1

                parallelAndClose =
                    proximal && axisIntersection == Nothing

                ( startAlongAxis, endAlongAxis ) =
                    -- Should allow us to determine direction
                    ( otherRoad.startPoint.space
                        |> Point3d.projectInto SketchPlane3d.xy
                        |> Point2d.signedDistanceAlong thisAxis
                    , otherRoad.endPoint.space
                        |> Point3d.projectInto SketchPlane3d.xy
                        |> Point2d.signedDistanceAlong thisAxis
                    )

                sameDirection =
                    startAlongAxis |> Quantity.lessThanOrEqualTo Quantity.zero

                intersection : Maybe Intersection
                intersection =
                    if notAdjacent then
                        case ( intersectPoint, parallelAndClose, sameDirection ) of
                            ( Just pt, _, _ ) ->
                                Just
                                    { thisSegment = myLeafNumber
                                    , otherSegment = otherIndex
                                    , category = Crossing pt
                                    }

                            ( Nothing, True, True ) ->
                                Just
                                    { thisSegment = myLeafNumber
                                    , otherSegment = otherIndex
                                    , category = SameDirection
                                    }

                            ( Nothing, True, False ) ->
                                Just
                                    { thisSegment = myLeafNumber
                                    , otherSegment = otherIndex
                                    , category = ContraDirection
                                    }

                            _ ->
                                Nothing

                    else
                        Nothing
            in
            intersection
    in
    ( myLeafNumber + 1
    , SpatialIndex.add prepContent index
    , List.filterMap roadHasOverlap candidates ++ intersects
    )
