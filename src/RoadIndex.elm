module RoadIndex exposing (Intersection, findFeatures)

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
import BoundingBox3d exposing (intersects)
import Dict exposing (Dict)
import DomainModel exposing (PeteTree, RoadSection, foldOverRoute, queryRoadsUsingFilter)
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d
import LocalCoords exposing (LocalCoords)
import Point2d exposing (Point2d)
import Point3d
import Quantity
import SketchPlane3d


type alias Point =
    -- 2D projections of Earth Points.
    Point2d Meters LocalCoords


type IntersectionType
    = Intersection Point
    | SameDirection
    | ContraDirection


type alias Intersection =
    { otherSegment : Int
    , category : IntersectionType
    }


type alias RoadIndex =
    -- Dict will contain only "prior" occurences of the road section. (?)
    Dict Int (List Intersection)


findFeatures : PeteTree -> RoadIndex
findFeatures treeNode =
    -- Top level API, folds over the tree to build the dictionary.
    let
        forEachLeaf : RoadSection -> ( Int, RoadIndex ) -> ( Int, RoadIndex )
        forEachLeaf road ( myLeafNumber, dict ) =
            let
                thisSegment =
                    LineSegment3d.from road.startPoint road.endPoint
                        |> LineSegment3d.projectInto SketchPlane3d.xy

                thisAxis =
                    Axis2d.through
                        (road.startPoint |> Point3d.projectInto SketchPlane3d.xy)
                        road.directionAtStart

                perhapsSameRoad : Int -> Int -> RoadSection -> Bool
                perhapsSameRoad startIdx endIdx otherRoad =
                    -- Is this section of the tree of interest?
                    -- Yes, if bounding box intersects and it is prior in the route.
                    (endIdx < myLeafNumber)
                        && (otherRoad.boundingBox |> intersects road.boundingBox)

                roadHasOverlap : Int -> RoadSection -> RoadIndex -> RoadIndex
                roadHasOverlap otherIndex otherRoad innerDict =
                    -- Work out what type of overlap this is and add to index.
                    let
                        otherSegment =
                            LineSegment3d.from otherRoad.startPoint otherRoad.endPoint
                                |> LineSegment3d.projectInto SketchPlane3d.xy

                        intersectPoint =
                            -- See if lines cross (not Nothing)
                            LineSegment2d.intersectionPoint thisSegment otherSegment

                        axisIntersection =
                            -- Nothing means co-linear, in either direction
                            otherSegment |> LineSegment2d.intersectionWithAxis thisAxis

                        ( startAlongAxis, endAlongAxis ) =
                            -- Should allow us to determine direction
                            ( otherRoad.startPoint
                                |> Point3d.projectInto SketchPlane3d.xy
                                |> Point2d.signedDistanceAlong thisAxis
                            , otherRoad.endPoint
                                |> Point3d.projectInto SketchPlane3d.xy
                                |> Point2d.signedDistanceAlong thisAxis
                            )

                        sameDirection =
                            startAlongAxis |> Quantity.lessThanOrEqualTo Quantity.zero

                        intersection : Maybe Intersection
                        intersection =
                            case ( intersectPoint, axisIntersection, sameDirection ) of
                                ( Just pt, _, _ ) ->
                                    Just
                                        { otherSegment = otherIndex
                                        , category = Intersection pt
                                        }

                                ( Nothing, Just _, True ) ->
                                    Just
                                        { otherSegment = otherIndex
                                        , category = SameDirection
                                        }

                                ( Nothing, Just _, False ) ->
                                    Just
                                        { otherSegment = otherIndex
                                        , category = ContraDirection
                                        }

                                _ ->
                                    Nothing

                        existingEntries =
                            Dict.get myLeafNumber innerDict
                                |> Maybe.withDefault []
                    in
                    case intersection of
                        Just intersect ->
                            innerDict
                                |> Dict.insert myLeafNumber (intersect :: existingEntries)

                        Nothing ->
                            innerDict

                priors =
                    queryRoadsUsingFilter
                        perhapsSameRoad
                        treeNode
                        roadHasOverlap
                        dict
            in
            ( myLeafNumber + 1, priors )

        ( _, completeIndex ) =
            foldOverRoute forEachLeaf treeNode ( 0, Dict.empty )
    in
    completeIndex