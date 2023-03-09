module Tools.Graph exposing
    (  PointIndexEntry
       --, addSelfLoop
       --, addTraversal
       --, combineNearbyPoints

    , addEdge
    ,  deleteEdgeTraversal
       --,  edgeCanBeDeleted
       --, enterRoutePlanningMode
       --, getTrack
       --, loopCanBeAdded

    ,  identifyPointsToBeMerged
       --, makeNewRoute

    , removeEdge
    , traversalCanBeAdded
    ,  trivialGraph
       --, undoWalkRoute

    , updatedEdge
    )

-- Attempt to co-locate the logic to do with having a level of indirection
-- between the road (nodes) and the trackpoints, so we can traverse sections
-- of track points multiple times and in each direction.

import Arc3d exposing (Arc3d)
import Axis2d
import Axis3d
import BoundingBox2d
import BoundingBox3d
import Dict exposing (Dict)
import DomainModel exposing (EarthPoint, GPXSource, PeteTree(..), RoadSection, skipCount, trueLength)
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point2d
import Point3d
import Quantity exposing (Quantity)
import Set exposing (Set)
import SketchPlane3d
import SpatialIndex
import Tools.GraphOptions exposing (..)
import TrackLoaded exposing (TrackLoaded)
import Tuple3
import Utils
import UtilsForViews


edgeKey : Edge msg -> String
edgeKey edge =
    --TBC if we want via in the key. I think we do for canonical edges.
    --For loaded tracks, we can use the track name.
    edge.lowNode ++ edge.highNode ++ edge.via


nodeKey : EarthPoint -> String
nodeKey point =
    let
        { x, y, z } =
            Point3d.toRecord Length.inMeters point.space
    in
    String.fromFloat x ++ String.fromFloat y


addEdge : TrackLoaded msg -> Graph msg -> Graph msg
addEdge track graph =
    --Use existing nodes if available.
    --Do we add edge if duplicate? Actually yes, else what?
    --Shall we use track name as edge key?
    --Shall we decree that a new end within one meter of an existing node is the same point?
    --No! That's a job for "Consolidate", which is better because it averages, not first-led.
    let
        ( edgeStart, edgeEnd, midPoint ) =
            ( DomainModel.startPoint track.trackTree
            , DomainModel.endPoint track.trackTree
            , DomainModel.midPoint track.trackTree
            )

        ( startKey, endKey ) =
            ( nodeKey edgeStart, nodeKey edgeEnd )

        nodeDict =
            graph.nodes
                |> Dict.insert startKey edgeStart
                |> Dict.insert endKey edgeEnd

        newEdge =
            { lowNode = startKey
            , highNode = endKey
            , via = nodeKey midPoint
            , track = track
            , originalDirection = Natural
            }

        edgeDict =
            Dict.insert track.trackName newEdge graph.edges
    in
    { graph
        | nodes = nodeDict
        , edges = edgeDict
    }


updatedEdge : TrackLoaded msg -> TrackLoaded msg -> Graph msg -> Graph msg
updatedEdge oldTrack newTrack graph =
    -- User has edited a track, so the graph must point to the newest version.
    -- Easy and safe to remove old track and add new in case name changed.
    graph |> removeEdge oldTrack |> addEdge newTrack


removeEdge : TrackLoaded msg -> Graph msg -> Graph msg
removeEdge track graph =
    -- User has removed a track, so the graph must point to the newest version.
    { graph | edges = Dict.remove track.trackName graph.edges }


traversalCanBeAdded : String -> List Traversal -> Graph msg -> Bool
traversalCanBeAdded newEdge userRoute graph =
    False



{-
   -- Edge can be added if either node is same as final node of last traversal,
   -- or if there are no traversals.
   case
       ( List.Extra.last userRoute
       , Dict.get newEdge graph.edges
       )
   of
       ( Just lastTraversal, Just clickedEdge ) ->
           case Dict.get lastTraversal.edge graph.edges of
               Just currentLastEdge ->
                   let
                       finalNode =
                           if lastTraversal.direction == Natural then
                               currentLastEdge.highNode

                           else
                               currentLastEdge.lowNode
                   in
                   finalNode == clickedEdge.lowNode || finalNode == clickedEdge.highNode

               Nothing ->
                   False

       ( Nothing, Just _ ) ->
           -- Any edge can be the first edge used.
           True

       _ ->
           False
-}
{-

   edgeCanBeDeleted : Int -> List Traversal -> Graph msg -> Bool
   edgeCanBeDeleted edge userRoute graph =
       -- Edge can be deleted if it's not the only edge and it's not used in the route.
       Dict.size graph.edges
           > 1
           && (not <|
                   List.any (\traversal -> traversal.edge == edge) userRoute
              )


   loopCanBeAdded : Int -> List Traversal -> Graph msg -> Bool
   loopCanBeAdded node userRoute graph =
       False
-}
{-
   -- Loop can be added if node is same as final node of last traversal.
   case
       List.Extra.last userRoute
   of
       Just traversal ->
           case Dict.get traversal.edge graph.edges of
               Just finalEdge ->
                   let
                       finalNode =
                           if traversal.direction == Natural then
                               finalEdge.highNode

                           else
                               finalEdge.lowNode
                   in
                   finalNode == node

               Nothing ->
                   False

       Nothing ->
           False
-}


deleteEdgeTraversal : Int -> List Traversal -> Graph msg -> Graph msg
deleteEdgeTraversal edge userRoute graph =
    graph



{-
   --TODO: Check edge is not used in route.
   -- Remove edge from dictionary.
   -- If either end node has no other edges, remove them as well.
   case Dict.get edge graph.edges of
       Just edgeInfo ->
           { graph | edges = Dict.remove edge graph.edges }
               |> pruneOrphanedNodes
               |> removeIfRedundantPlace edgeInfo.lowNode
               |> removeIfRedundantPlace edgeInfo.highNode

       Nothing ->
           graph
-}
{-
   pruneOrphanedNodes : Graph msg -> Graph msg
   pruneOrphanedNodes graph =
       -- Deletion of an edge may leave unconnected nodes, remove them.
       let
           nodeHasEdge k v =
               not <| List.isEmpty <| combinedEdgesForNode k graph
       in
       { graph | nodes = Dict.filter nodeHasEdge graph.nodes }
-}


joinTracks : TrackLoaded msg -> TrackLoaded msg -> TrackLoaded msg
joinTracks track1 track2 =
    let
        ( asGpx1, asGpx2 ) =
            ( DomainModel.getAllGPXPointsInNaturalOrder track1.trackTree
            , DomainModel.getAllGPXPointsInNaturalOrder track2.trackTree
            )
    in
    case
        DomainModel.treeFromSourcesWithExistingReference
            track1.referenceLonLat
            (asGpx1 ++ asGpx2)
    of
        Just tree ->
            { track1 | trackTree = tree }

        Nothing ->
            track1


reverseTrack : TrackLoaded msg -> TrackLoaded msg
reverseTrack track =
    let
        asGpx1 =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
    in
    case
        DomainModel.treeFromSourcesWithExistingReference
            track.referenceLonLat
            (List.reverse asGpx1)
    of
        Just tree ->
            { track | trackTree = tree }

        Nothing ->
            track



{-
   removeIfRedundantPlace : Int -> Graph msg -> Graph msg
   removeIfRedundantPlace node graph =
       -- Deletion of edge may reduce end Places to having only two Roads, remove them.
       -- Details depend on whether this node is the low or high numbered edge end.
       -- `listEdgesForNode` now returns full edge details.
       --TOOD: Defuglification.
       case listEdgesForNode node graph of
           ( [ asLow1, asLow2 ], [] ) ->
               -- Combined edge is from lowest to highest of the "high ends", flipping the first.
               let
                   ( edge1Index, edge1Info ) =
                       asLow1

                   ( edge2Index, edge2Info ) =
                       asLow2
               in
               if edge1Info.highNode <= edge2Info.highNode then
                   let
                       newEdge1 =
                           { lowNode = edge1Info.highNode
                           , highNode = edge2Info.highNode
                           , via = edge1Info.via
                           , track = joinTracks (reverseTrack edge1Info.track) edge2Info.track
                           , originalDirection = edge2Info.originalDirection
                           }
                   in
                   { graph
                       | edges =
                           graph.edges
                               |> Dict.remove edge1Index
                               |> Dict.remove edge2Index
                               |> Dict.insert edge1Index newEdge1
                       , nodes = Dict.remove node graph.nodes
                   }

               else
                   --if high2 < high1 then
                   let
                       newEdge2 =
                           { lowNode = edge2Info.highNode
                           , highNode = edge1Info.highNode
                           , via = edge2Info.via
                           , track = joinTracks (reverseTrack edge2Info.track) edge1Info.track
                           , originalDirection = edge1Info.originalDirection
                           }
                   in
                   { graph
                       | edges =
                           graph.edges
                               |> Dict.remove edge1Index
                               |> Dict.remove edge2Index
                               |> Dict.insert edge1Index newEdge2
                       , nodes = Dict.remove node graph.nodes
                   }

           ( [], [ asHigh1, asHigh2 ] ) ->
               let
                   ( edge1Index, edge1Info ) =
                       asHigh1

                   ( edge2Index, edge2Info ) =
                       asHigh2
               in
               if edge1Info.lowNode <= edge2Info.lowNode then
                   let
                       newEdge1 =
                           { lowNode = edge1Info.lowNode
                           , highNode = edge2Info.lowNode
                           , via = edge1Info.via
                           , track = joinTracks edge1Info.track (reverseTrack edge2Info.track)
                           , originalDirection = edge1Info.originalDirection
                           }
                   in
                   { graph
                       | edges =
                           graph.edges
                               |> Dict.remove edge1Index
                               |> Dict.remove edge2Index
                               |> Dict.insert edge1Index newEdge1
                       , nodes = Dict.remove node graph.nodes
                   }

               else
                   --if low2 < low1 then
                   let
                       newEdge2 =
                           { lowNode = edge2Info.lowNode
                           , highNode = edge1Info.lowNode
                           , via = edge2Info.via
                           , track = joinTracks edge2Info.track (reverseTrack edge1Info.track)
                           , originalDirection = edge1Info.originalDirection
                           }
                   in
                   { graph
                       | edges =
                           graph.edges
                               |> Dict.remove edge1Index
                               |> Dict.remove edge2Index
                               |> Dict.insert edge1Index newEdge2
                       , nodes = Dict.remove node graph.nodes
                   }

           ( [ asLow ], [ asHigh ] ) ->
               let
                   ( edge1Index, edge1Info ) =
                       asLow

                   ( edge2Index, edge2Info ) =
                       asHigh

                   newEdge =
                       { lowNode = edge1Info.lowNode
                       , highNode = edge2Info.lowNode
                       , via = edge1Info.via
                       , track = joinTracks edge2Info.track edge1Info.track
                       , originalDirection = edge1Info.originalDirection
                       }
               in
               { graph
                   | edges =
                       graph.edges
                           |> Dict.remove edge1Index
                           |> Dict.remove edge2Index
                           |> Dict.insert edge1Index newEdge
                   , nodes =
                       if node == edge1Info.highNode then
                           -- Don't remove if self-loop
                           graph.nodes

                       else
                           Dict.remove node graph.nodes
               }

           _ ->
               -- Not exactly two edges, do nothing.
               graph
-}
{-
   listEdgesForNode : Int -> Graph msg -> ( List ( Int, Edge msg ), List ( Int, Edge msg ) )
   listEdgesForNode node graph =
       let
           withLowNode =
               graph.edges
                   |> Dict.filter
                       (\_ edgeInfo -> edgeInfo.lowNode == node)
                   |> Dict.toList

           withHighNode =
               graph.edges
                   |> Dict.filter
                       (\_ edgeInfo -> edgeInfo.highNode == node)
                   |> Dict.toList
       in
       ( withLowNode, withHighNode )
-}
{-
   combinedEdgesForNode : Int -> Graph msg -> List ( Int, Edge msg )
   combinedEdgesForNode node graph =
       let
           ( asLow, asHigh ) =
               listEdgesForNode node graph
       in
       asLow ++ asHigh
-}
{-
   addTraversal : Int -> List Traversal -> Graph msg -> List Traversal
   addTraversal newEdge userRoute graph =
       case
           ( List.Extra.last userRoute
           , Dict.get newEdge graph.edges
           )
       of
           ( Just traversal, Just addedEdgeInfo ) ->
               case Dict.get traversal.edge graph.edges of
                   Just lastEdgeInfo ->
                       let
                           newEdgeDirection =
                               -- Special case if added section is a loop.
                               if addedEdgeInfo.lowNode == addedEdgeInfo.highNode then
                                   Natural

                               else
                                   let
                                       finalNode =
                                           if traversal.direction == Natural then
                                               lastEdgeInfo.highNode

                                           else
                                               lastEdgeInfo.lowNode
                                   in
                                   if finalNode == addedEdgeInfo.lowNode then
                                       Natural

                                   else
                                       Reverse
                       in
                       userRoute
                           ++ [ { edge = newEdge, direction = newEdgeDirection } ]

                   Nothing ->
                       userRoute

           ( Nothing, Just _ ) ->
               userRoute ++ [ { edge = newEdge, direction = Natural } ]

           _ ->
               userRoute
-}
{-

   addSelfLoop : Int -> List Traversal -> Graph msg -> Graph msg
   addSelfLoop node userRoute graph =
       case
           List.Extra.last userRoute
       of
           Just traversal ->
               case Dict.get traversal.edge graph.edges of
                   Just edgeInfo ->
                       let
                           ( _, edgeDirection, endPoint ) =
                               if traversal.direction == Natural then
                                   ( edgeInfo.highNode
                                   , DomainModel.getLastLeaf edgeInfo.track.trackTree |> .directionAtEnd
                                   , DomainModel.earthPointFromIndex
                                       (skipCount edgeInfo.track.trackTree)
                                       edgeInfo.track.trackTree
                                   )

                               else
                                   ( edgeInfo.lowNode
                                   , DomainModel.getFirstLeaf edgeInfo.track.trackTree
                                       |> .directionAtStart
                                       |> Direction2d.reverse
                                   , DomainModel.earthPointFromIndex 0 edgeInfo.track.trackTree
                                   )

                           loopOpposite =
                               endPoint.space
                                   |> Point3d.translateBy
                                       (Vector3d.withLength
                                           (Quantity.twice options.minimumRadiusAtPlaces)
                                           (edgeDirection |> Direction3d.on SketchPlane3d.xy)
                                       )

                           loopCentre =
                               Point3d.midpoint endPoint.space loopOpposite

                           axis =
                               Axis3d.withDirection Direction3d.positiveZ loopCentre

                           ( arcStart, arcEnd ) =
                               ( endPoint.space |> Point3d.rotateAround axis (Angle.degrees 30)
                               , endPoint.space |> Point3d.rotateAround axis (Angle.degrees -30)
                               )

                           arc =
                               Arc3d.throughPoints arcStart loopOpposite arcEnd
                       in
                       case arc of
                           Just isArc ->
                               let
                                   edgePoints =
                                       isArc
                                           |> Arc3d.approximate (Length.meters 0.1)
                                           |> Polyline3d.vertices
                                           |> List.map DomainModel.withoutTime
                                           |> List.map (DomainModel.gpxFromPointWithReference graph.referenceLonLat)

                                   newEdgeTree =
                                       DomainModel.treeFromSourcesWithExistingReference
                                           edgeInfo.track.referenceLonLat
                                           edgePoints

                                   newEdgeTrack =
                                       Maybe.map (TrackLoaded.newTrackFromTree edgeInfo.track.referenceLonLat)
                                           newEdgeTree
                               in
                               case newEdgeTrack of
                                   Just newTrack ->
                                       let
                                           newEdgeInfo =
                                               { lowNode = node
                                               , highNode = node
                                               , via = makeXY <| DomainModel.withoutTime loopOpposite
                                               }

                                           newEdgeIndex =
                                               Dict.size graph.edges
                                       in
                                       { graph
                                           | edges =
                                               Dict.insert
                                                   newEdgeIndex
                                                   { lowNode = newEdgeInfo.lowNode
                                                   , highNode = newEdgeInfo.highNode
                                                   , via = newEdgeInfo.via
                                                   , track = newTrack
                                                   , originalDirection = Natural
                                                   }
                                                   graph.edges
                                       }

                                   Nothing ->
                                       graph

                           Nothing ->
                               graph

                   Nothing ->
                       graph

           Nothing ->
               graph
-}


type alias PointIndexEntry =
    { trackName : String
    , pointIndex : Int
    , point : Point3d.Point3d Meters LocalCoords
    }


type alias LeafIndexEntry =
    { trackName : String
    , leafIndex : Int
    }


type alias PointIndex =
    SpatialIndex.SpatialNode PointIndexEntry Length.Meters LocalCoords


type alias LeafIndex =
    SpatialIndex.SpatialNode LeafIndexEntry Length.Meters LocalCoords


type alias Clustering =
    { pairs : List PointNearbyPoint
    , clusters : List Cluster
    , usedPoints : Set ( String, Int )
    }


identifyPointsToBeMerged : Length.Length -> Graph msg -> List Cluster
identifyPointsToBeMerged tolerance graph =
    {-
       Data flow outline.
       1. Make spatial indexes of points and leaves, for quick but approximate nearness queries.
       2. For each point, look for leaves within tolerance.
           a. For each such leaf, get distance along leaf and foot of perpendicular.
       3. Collect these perpendicular "feet" by Leaf, sorted by `distanceAlong`.
       4. Update tree by inserting points into each affected Leaf (descending leaf index order.
       Now have tree', enhanced by "virtual points" where leaves are close to points.
       5. For each point, find nearby points within tolerance.
       8. For each such cluster, derive the centroid.
       9. If a point is in more than one cluster, use only the nearest centroid.
       9. For each point in all adjusted clusters, derive mapping to centroid.
       10. Apply mappings by updating points.
       Now have tree'' which has adjusted points at cluster centroids.
    -}
    let
        globalBoundingBox =
            -- We just make our index area slightly larger because we inflate all the contents.
            graph.edges
                |> Dict.values
                |> List.map (.track >> .trackTree >> DomainModel.boundingBox >> UtilsForViews.flatBox)
                |> BoundingBox2d.aggregateN
                |> Maybe.withDefault (BoundingBox2d.singleton Point2d.origin)
                |> BoundingBox2d.expandBy tolerance

        pointWithTolerance pt =
            BoundingBox2d.withDimensions
                ( Quantity.twice tolerance, Quantity.twice tolerance )
                (Point3d.projectInto SketchPlane3d.xy pt)

        -- Try to keep all the top level code here for a quick overview.
        globalLeafIndex : LeafIndex
        globalLeafIndex =
            Dict.foldl
                (\k v -> addTrackLeavesToIndex v)
                (SpatialIndex.empty globalBoundingBox (Length.meters 100.0))
                graph.edges

        pointsProjectedOntoNearbyLeaves : List ProjectedPointOnLeaf
        pointsProjectedOntoNearbyLeaves =
            List.concatMap
                findAllLeavesNearAllPointsOnTrack
                (Dict.values graph.edges)

        --_ =
        --    Debug.log "projections" pointsProjectedOntoNearbyLeaves
        edgesWithProjectedPoints : Dict String (Edge msg)
        edgesWithProjectedPoints =
            let
                projectionsOnto trackName =
                    pointsProjectedOntoNearbyLeaves
                        |> List.filter (\projection -> projection.toTrack == trackName)
            in
            Dict.map
                (\k edge ->
                    addProjectedPointsIntoTrack
                        (projectionsOnto edge.track.trackName)
                        edge
                )
                graph.edges

        --_ =
        --    Debug.log "edgesWithProjectedPoints" <|
        --        Dict.map
        --            (\k edge -> skipCount edge.track.trackTree)
        --            edgesWithProjectedPoints
        globalPointIndex : PointIndex
        globalPointIndex =
            -- This is an index of points base and projected.
            -- It's used to find clusters of points than may be combined.
            Dict.foldl
                (\k v -> addTrackPointsToIndex v)
                (SpatialIndex.empty globalBoundingBox (Length.meters 100.0))
                edgesWithProjectedPoints

        allNearbyPointPairs : List PointNearbyPoint
        allNearbyPointPairs =
            -- Think of is a source-destination mapping table.
            -- Is input to cluster finding.
            edgesWithProjectedPoints
                |> Dict.values
                |> List.concatMap nearbyPointsForTrack

        --_ =
        --    Debug.log "allNearbyPointPairs" allNearbyPointPairs
        clustersFromPointPairs : List PointNearbyPoint -> List Cluster
        clustersFromPointPairs pairs =
            -- Change to clustering.
            -- Find closest pair, find centroid,
            -- If any "nearby" points are within tolerance of centroid, add to cluster.
            -- Repeat until all clusters found.
            findClusters
                { pairs = List.sortBy (.separation >> Length.inMeters) allNearbyPointPairs
                , clusters = []
                , usedPoints = Set.empty
                }
                |> .clusters

        -- Lower level functions follow.
        addTrackLeavesToIndex : Edge msg -> LeafIndex -> LeafIndex
        addTrackLeavesToIndex edge index =
            DomainModel.foldOverRoute
                (addRoadSectionToIndex edge.track.trackName)
                edge.track.trackTree
                ( 0, index )
                |> Tuple.second

        addRoadSectionToIndex : String -> RoadSection -> ( Int, LeafIndex ) -> ( Int, LeafIndex )
        addRoadSectionToIndex trackName road ( thisLeafIndex, index ) =
            ( thisLeafIndex + 1
            , SpatialIndex.add
                { content = { trackName = trackName, leafIndex = thisLeafIndex }
                , box = UtilsForViews.flatBox road.boundingBox
                }
                index
            )

        addTrackPointsToIndex : Edge msg -> PointIndex -> PointIndex
        addTrackPointsToIndex edge index =
            DomainModel.foldOverEarthPoints
                (addTrackPointToIndex edge.track.trackName)
                edge.track.trackTree
                ( 0, index )
                |> Tuple.second

        addTrackPointToIndex : String -> EarthPoint -> ( Int, PointIndex ) -> ( Int, PointIndex )
        addTrackPointToIndex trackName earthPoint ( thisPointIndex, index ) =
            ( thisPointIndex + 1
            , SpatialIndex.add
                { content =
                    { trackName = trackName
                    , pointIndex = thisPointIndex
                    , point = earthPoint.space
                    }
                , box = UtilsForViews.flatBox <| BoundingBox3d.singleton earthPoint.space
                }
                index
            )

        findAllLeavesNearAllPointsOnTrack : Edge msg -> List ProjectedPointOnLeaf
        findAllLeavesNearAllPointsOnTrack edge =
            let
                findAllLeavesNearPointOnTrack :
                    EarthPoint
                    -> ( Int, List ProjectedPointOnLeaf )
                    -> ( Int, List ProjectedPointOnLeaf )
                findAllLeavesNearPointOnTrack earthPoint ( pointIndex, collect ) =
                    -- Use spatial leaf index then refine with geometry. Exclude contiguous.
                    let
                        pt =
                            earthPoint

                        thisPoint2d =
                            Point3d.projectInto SketchPlane3d.xy pt.space

                        results =
                            SpatialIndex.query globalLeafIndex (pointWithTolerance pt.space)
                                |> List.map .content

                        isThisLeafClose : LeafIndexEntry -> Maybe ProjectedPointOnLeaf
                        isThisLeafClose { trackName, leafIndex } =
                            case Dict.get trackName graph.edges of
                                Just toEdge ->
                                    let
                                        toLeaf =
                                            DomainModel.asRecord <|
                                                DomainModel.leafFromIndex leafIndex toEdge.track.trackTree

                                        axis2d =
                                            Axis2d.through
                                                (toLeaf.startPoint.space |> Point3d.projectInto SketchPlane3d.xy)
                                                toLeaf.directionAtStart

                                        axis3d =
                                            Axis3d.throughPoints toLeaf.startPoint.space toLeaf.endPoint.space
                                                |> Maybe.withDefault Axis3d.z

                                        ( along, from, foot ) =
                                            -- Proximity test in 2D as altitudes may vary greatly.
                                            ( pt.space |> Point3d.signedDistanceAlong axis3d
                                            , thisPoint2d |> Point2d.signedDistanceFrom axis2d |> Quantity.abs
                                            , pt.space |> Point3d.projectOntoAxis axis3d
                                            )

                                        isShortPerp =
                                            from |> Quantity.lessThanOrEqualTo tolerance

                                        isNotBeyondEndPoints =
                                            Quantity.greaterThanZero along
                                                && (along |> Quantity.lessThan toLeaf.trueLength)

                                        ( isNotNearStart, isNotNearEnd ) =
                                            -- Ignore points sufficiently close to ends that clustering will mop them up.
                                            ( pt.space
                                                |> Point3d.distanceFrom toLeaf.startPoint.space
                                                |> Quantity.greaterThan tolerance
                                            , pt.space
                                                |> Point3d.distanceFrom toLeaf.endPoint.space
                                                |> Quantity.greaterThan tolerance
                                            )

                                        --_ =
                                        --    Debug.log "tests"
                                        --        { isShortPerp = isShortPerp
                                        --        , isAfterStart = isAfterStart
                                        --        , isBeforeEnd = isBeforeEnd
                                        --        , isNotConnected = isNotConnected
                                        --        }
                                        isNotConnected =
                                            -- Exclude leaves on the source track that neighbour source point.
                                            trackName
                                                /= edge.track.trackName
                                                || (leafIndex /= pointIndex && leafIndex + 1 /= pointIndex)
                                    in
                                    if
                                        isNotConnected
                                            && isShortPerp
                                            --&& isNotNearStart
                                            --&& isNotNearEnd
                                            && isNotBeyondEndPoints
                                    then
                                        Just
                                            { fromTrack = edge.track.trackName
                                            , fromPoint = pointIndex
                                            , toTrack = trackName
                                            , toLeaf = leafIndex
                                            , distanceAlong = along
                                            , projectedPoint = foot
                                            }

                                    else
                                        Nothing

                                Nothing ->
                                    --No edge?
                                    Nothing
                    in
                    ( pointIndex + 1
                    , (results |> List.filterMap isThisLeafClose) ++ collect
                    )
            in
            DomainModel.foldOverEarthPoints
                findAllLeavesNearPointOnTrack
                edge.track.trackTree
                ( 0, [] )
                |> Tuple.second

        addProjectedPointsIntoTrack : List ProjectedPointOnLeaf -> Edge msg -> Edge msg
        addProjectedPointsIntoTrack projections toEdge =
            let
                track =
                    toEdge.track

                updatedTree =
                    --NOTE: Must add to highest numbered leaf first or leaf numbers confused!
                    --NOTE: Dict.foldr not doing what I expect.
                    Dict.foldr
                        insertPointsInLeaf
                        track.trackTree
                        (perpendicularFeetGroupedByLeaf projections)

                updatedTrack =
                    { track | trackTree = updatedTree }
            in
            { toEdge | track = updatedTrack }

        {-
           We have the feet of the perpendiculars to nearby leaves.
           3. Collect these perpendicular "feet" by Leaf, sorted by `distanceAlong`.
        -}
        perpendicularFeetGroupedByLeaf : List ProjectedPointOnLeaf -> Dict Int (List ProjectedPointOnLeaf)
        perpendicularFeetGroupedByLeaf projections =
            -- Can't see a suitable function in List.Extra, so do it by hand.
            let
                addToLeafDict newEntry dict =
                    case Dict.get newEntry.toLeaf dict of
                        Just prevEntries ->
                            Dict.insert
                                newEntry.toLeaf
                                (newEntry :: prevEntries)
                                dict

                        Nothing ->
                            Dict.insert
                                newEntry.toLeaf
                                [ newEntry ]
                                dict

                leafDictUnsorted =
                    List.foldl
                        addToLeafDict
                        Dict.empty
                        projections

                sortEachLeafEntries : Int -> List ProjectedPointOnLeaf -> List ProjectedPointOnLeaf
                sortEachLeafEntries _ unsorted =
                    unsorted
                        |> List.Extra.uniqueBy (.distanceAlong >> Length.inMeters)
                        |> List.sortBy (.distanceAlong >> Length.inMeters)
            in
            Dict.map sortEachLeafEntries leafDictUnsorted

        {-
           4. Update tree by converting each affected Leaf into a "branch" (in situ perhaps?).
        -}
        insertPointsInLeaf : Int -> List ProjectedPointOnLeaf -> PeteTree -> PeteTree
        insertPointsInLeaf leafNumber newPoints tree =
            let
                asGPX =
                    List.map
                        (.projectedPoint
                            >> DomainModel.withoutTime
                            >> DomainModel.gpxFromPointWithReference graph.referenceLonLat
                        )
                        newPoints
            in
            DomainModel.insertPointsIntoLeaf
                leafNumber
                graph.referenceLonLat
                asGPX
                tree

        nearbyPointsForTrack : Edge msg -> List PointNearbyPoint
        nearbyPointsForTrack edge =
            DomainModel.foldOverEarthPoints
                (pointsNearPoint edge.track.trackName)
                edge.track.trackTree
                ( 0, [] )
                |> Tuple.second

        pointsNearPoint :
            String
            -> EarthPoint
            -> ( Int, List PointNearbyPoint )
            -> ( Int, List PointNearbyPoint )
        pointsNearPoint searchTrack searchPoint ( searchIndex, collector ) =
            -- Prelude to finding clusters of points.
            -- Use spatial point index then refine with geometry.
            let
                searchLocus2d =
                    Point3d.projectInto SketchPlane3d.xy searchPoint.space

                resultsUnfiltered =
                    --TODO: Pushing query into search would be efficient, once it's right.
                    SpatialIndex.query globalPointIndex (pointWithTolerance searchPoint.space)
                        |> List.map .content

                --_ =
                --    Debug.log "UNFILTERED" resultsUnfiltered
                --_ =
                --    Debug.log "FILTERED" results
                results =
                    resultsUnfiltered
                        |> List.filter
                            --ignore if too far away horizontally.
                            (\{ trackName, pointIndex, point } ->
                                point
                                    |> Point3d.projectInto SketchPlane3d.xy
                                    |> Point2d.distanceFrom searchLocus2d
                                    |> Quantity.lessThanOrEqualTo tolerance
                            )
                        |> List.filter
                            --ignore if found ourself, as we should
                            (\{ trackName, pointIndex, point } ->
                                trackName /= searchTrack || pointIndex /= searchIndex
                            )

                resultsAsRecords =
                    results
                        |> List.map
                            (\{ trackName, pointIndex, point } ->
                                { aTrack = searchTrack
                                , aPointIndex = searchIndex
                                , aPoint = searchPoint.space
                                , bTrack = trackName
                                , bPointIndex = pointIndex
                                , bPoint = point
                                , separation = point |> Point3d.distanceFrom searchPoint.space
                                }
                            )
            in
            ( searchIndex + 1
            , resultsAsRecords ++ collector
            )

        deduplicateNearbyPoints : List PointNearbyPoint -> List PointNearbyPoint
        deduplicateNearbyPoints nearbys =
            -- List will have A -> B and B -> A.
            -- Logically entries are symmetric so we can flip A and B based on natural sort order,
            -- then sort and "trivially" deduplicate.
            -- TURNS OUT it's really handy to have both entries, when we build clusters.
            let
                normalise nearby =
                    if nearby.aTrack <= nearby.bTrack && nearby.aPointIndex <= nearby.bPointIndex then
                        nearby

                    else
                        { aTrack = nearby.bTrack
                        , aPointIndex = nearby.bPointIndex
                        , aPoint = nearby.bPoint
                        , bTrack = nearby.aTrack
                        , bPointIndex = nearby.aPointIndex
                        , bPoint = nearby.aPoint
                        , separation = nearby.separation
                        }
            in
            List.map normalise nearbys
                |> Utils.deDupe (==)

        findClusters : Clustering -> Clustering
        findClusters clustersInfo =
            -- If first pair has two free points, start a new cluster.
            -- If first pair does not, discard and recurse.
            -- If list empty, return.
            case clustersInfo.pairs of
                pair1 :: morePairs ->
                    let
                        ( pointA, pointB ) =
                            ( ( pair1.aTrack, pair1.aPointIndex )
                            , ( pair1.bTrack, pair1.bPointIndex )
                            )
                    in
                    if
                        Set.member pointA clustersInfo.usedPoints
                            || Set.member pointB clustersInfo.usedPoints
                    then
                        findClusters { clustersInfo | pairs = morePairs }

                    else
                        { clustersInfo | pairs = morePairs }
                            |> growClusterFromSeed pair1
                            |> findClusters

                [] ->
                    clustersInfo

        growClusterFromSeed :
            PointNearbyPoint
            -> Clustering
            -> Clustering
        growClusterFromSeed pair1 clustersInfo =
            -- If any "nearby" unused points are within tolerance of centroid, add to cluster.
            -- Return cluster and unused pairs.
            let
                centroid =
                    Point3d.centroid pair1.aPoint [ pair1.bPoint ]

                seedCluster : Cluster
                seedCluster =
                    { centroid = centroid
                    , pointsToAdjust =
                        [ ( pair1.aTrack, pair1.aPointIndex, pair1.aPoint )
                        , ( pair1.bTrack, pair1.bPointIndex, pair1.bPoint )
                        ]
                    }
            in
            extendCluster
                seedCluster
                { clustersInfo
                    | usedPoints =
                        clustersInfo.usedPoints
                            |> Set.insert ( pair1.aTrack, pair1.aPointIndex )
                            |> Set.insert ( pair1.bTrack, pair1.bPointIndex )
                }

        extendCluster : Cluster -> Clustering -> Clustering
        extendCluster cluster clustersInfo =
            let
                currentClusterMembers =
                    List.map
                        (\triplet -> ( Tuple3.first triplet, Tuple3.second triplet ))
                        cluster.pointsToAdjust
                        |> Set.fromList

                possibleExtensions =
                    -- Any pairs that begin with any member of this cluster
                    -- and far point is unused
                    -- and far point is close enough to centroid
                    clustersInfo.pairs
                        |> List.filter
                            (\pair ->
                                Set.member ( pair.aTrack, pair.aPointIndex ) currentClusterMembers
                            )
                        |> List.filter
                            (\pair ->
                                not <|
                                    Set.member ( pair.bTrack, pair.bPointIndex ) clustersInfo.usedPoints
                            )
                        |> List.filter
                            (\pair ->
                                pair.bPoint
                                    |> Point3d.distanceFrom cluster.centroid
                                    |> Quantity.lessThanOrEqualTo tolerance
                            )
            in
            if possibleExtensions == [] then
                --We're done
                { clustersInfo | clusters = cluster :: clustersInfo.clusters }

            else
                --Keep going
                let
                    curentClusterPoints =
                        List.map Tuple3.third cluster.pointsToAdjust

                    pointsToAddToCentroid =
                        List.map .bPoint possibleExtensions

                    newCentroid =
                        (curentClusterPoints ++ pointsToAddToCentroid)
                            |> Point3d.centroidN
                            |> Maybe.withDefault cluster.centroid

                    newPointsInfo =
                        possibleExtensions
                            |> List.map (\new -> ( new.bTrack, new.bPointIndex, new.bPoint ))

                    extendedCluster =
                        { cluster
                            | centroid = newCentroid
                            , pointsToAdjust = newPointsInfo ++ cluster.pointsToAdjust
                        }
                in
                extendCluster
                    extendedCluster
                    { clustersInfo
                        | usedPoints =
                            possibleExtensions
                                |> List.foldl
                                    (\new -> Set.insert ( new.bTrack, new.bPointIndex ))
                                    clustersInfo.usedPoints
                    }
    in
    clustersFromPointPairs allNearbyPointPairs



{-
   applyCentroidsToTracks : : Length.Length -> Graph msg -> Graph msg
   applyCentroidsToTracks tolerance graphs =
              {-
                 9. For each point in all clusters, derive mapping to centroid.
                 (Step 9 is merely a restatement of Cluster, so will skip.)
                 10. Apply mappings by updating points (`updatePointByIndexInSitu` perhaps?).
              -}
          let
              treeWithCentroidsApplied : PeteTree
              treeWithCentroidsApplied =
                  let
                      mapCluster : Cluster -> PeteTree -> PeteTree
                      mapCluster cluster outputTree =
                          --Each move modifies tree so must be a fold.
                          let
                              asGPS =
                                  DomainModel.gpxFromPointWithReference
                                      track.referenceLonLat
                                      (DomainModel.withoutTime cluster.centroid)
                          in
                          List.foldl
                              (movePoint asGPS)
                              outputTree
                              cluster.pointsToAdjust

                      movePoint : GPXSource -> Int -> PeteTree -> PeteTree
                      movePoint centroid pointNumber tree =
                          DomainModel.updatePointByIndexInSitu
                              pointNumber
                              centroid
                              track.referenceLonLat
                              tree
                  in
                  List.foldl
                      mapCluster
                      treeWithAddedPoints
                      clustersWithCentroids

              {-
                 Now have tree'' which has adjusted points at cluster centroids.
                 Proof of pudding awaited ...
              -}
          in
          (
          treeWithCentroidsApplied
          )
-}


type alias EdgeFinder msg =
    { startNodeIndex : Int
    , currentEdge : List ( EarthPoint, GPXSource )
    , edgeResolverDict : Dict ( String, String, String ) ( String, PeteTree )
    , edgesDict : Dict String (Edge msg)
    , traversals : List Traversal
    }



{-
   buildGraph : TrackLoaded msg -> Graph msg
   buildGraph track =
       {-
          As in v1 & 2, the only way I know is to see which track points have more than two neighbours.
          Hence build a Dict using XY and the entries being a list of points that share the location.
       -}
       let
           countNeighbours : RoadSection -> Dict String (Set String) -> Dict String (Set String)
           countNeighbours road countDict =
               -- Nicer than v2 thanks to use of road segments.
               -- Note we are interested in neighbours with distinct XYs.
               let
                   ( startXY, endXY ) =
                       ( nodeKey road.startPoint
                       , nodeKey road.endPoint
                       )

                   ( startNeighbours, endNeighbours ) =
                       ( Dict.get startXY countDict |> Maybe.withDefault Set.empty
                       , Dict.get endXY countDict |> Maybe.withDefault Set.empty
                       )
               in
               countDict
                   |> Dict.insert startXY (Set.insert endXY startNeighbours)
                   |> Dict.insert endXY (Set.insert startXY endNeighbours)

           pointNeighbours : Dict String (Set String)
           pointNeighbours =
               -- What neighbours hath each track point?
               -- Note that the List.head will be earliest in the route, hence preferred.
               DomainModel.foldOverRouteRL
                   countNeighbours
                   track.trackTree
                   Dict.empty

           ( trackStartXY, trackEndXY ) =
               ( nodeKey <| DomainModel.earthPointFromIndex 0 track.trackTree
               , nodeKey <| DomainModel.earthPointFromIndex (skipCount track.trackTree) track.trackTree
               )

           nodes =
               -- Two neighbours is just an edge point, anything else is a node.
               -- But make sure the endpoints are there, as loops can cause a problem here.
               pointNeighbours
                   |> Dict.filter
                       (\pt neighbours ->
                           Set.size neighbours
                               /= 2
                               || pt
                               == trackStartXY
                               || pt
                               == trackEndXY
                       )
                   |> Dict.keys
                   |> List.indexedMap Tuple.pair
                   |> Dict.fromList

           swap ( a, b ) =
               ( b, a )

           inverseNodes : Dict String String
           inverseNodes =
               -- We need to lookup each point to see if it's a node.
               nodes |> Dict.toList |> List.map swap |> Dict.fromList

           ( firstPoint, firstGpx ) =
               DomainModel.getDualCoords track.trackTree 0

           finalEdgeFinder : EdgeFinder msg
           finalEdgeFinder =
               {-
                  Walk the route again, but check each point against node index.
                  If not a node, accrue a possible new edge.
                  If a node, look into edge dict to see if we have already an Edge
                  for the node pair, but note that that is not unique.
                  The real test for an edge is whether all (or sample) of trackpoints
                  coincide, forwards or backward. We can reduce the testing to one-way
                  by convention of always putting lower node index first in dict lookup.
               -}
               DomainModel.foldOverRoute
                   splitIntoEdges
                   track.trackTree
                   initialEdgeFinder

           initialEdgeFinder : EdgeFinder msg
           initialEdgeFinder =
               { startNodeIndex = Dict.get (nodeKey firstPoint) inverseNodes |> Maybe.withDefault 0
               , currentEdge = [ ( firstPoint, firstGpx ) ]
               , edgeResolverDict = Dict.empty
               , edgesDict = Dict.empty
               , traversals = []
               }

           splitIntoEdges :
               RoadSection
               -> EdgeFinder msg
               -> EdgeFinder msg
           splitIntoEdges road inputState =
               let
                   pointXY =
                       nodeKey road.endPoint

                   pointGpx =
                       Tuple.second road.sourceData
               in
               case Dict.get pointXY inverseNodes of
                   Nothing ->
                       -- Not a node, just add to current edge.
                       { inputState | currentEdge = ( road.endPoint, pointGpx ) :: inputState.currentEdge }

                   Just nodeIndex ->
                       -- At a node, completed an edge, but have we encountered this edge before?
                       let
                           newEdge : List ( EarthPoint, GPXSource )
                           newEdge =
                               ( road.endPoint, pointGpx ) :: inputState.currentEdge

                           orientedEdgeCouldBeLeaf : List ( EarthPoint, GPXSource )
                           orientedEdgeCouldBeLeaf =
                               if nodeIndex >= inputState.startNodeIndex then
                                   -- Conventional order, good, but must flip the edge
                                   List.reverse newEdge

                               else
                                   newEdge

                           orientedEdge : List ( EarthPoint, GPXSource )
                           orientedEdge =
                               -- Not good if no midpoints, as can't select.
                               case orientedEdgeCouldBeLeaf of
                                   [ ( startEarth, startGpx ), ( endEarth, endGpx ) ] ->
                                       let
                                           midEarth =
                                               DomainModel.withoutTime <|
                                                   Point3d.midpoint startEarth.space endEarth.space

                                           midGpx =
                                               DomainModel.gpxFromPointWithReference
                                                   track.referenceLonLat
                                                   midEarth
                                       in
                                       [ ( startEarth, startGpx )
                                       , ( midEarth, midGpx )
                                       , ( endEarth, endGpx )
                                       ]

                                   _ ->
                                       orientedEdgeCouldBeLeaf

                           discriminator : String
                           discriminator =
                               -- As there can be more than one edge 'tween  two nodes,
                               -- we take the index 1 point to discriminate. That's why
                               -- the edge orientation matters.
                               orientedEdge
                                   |> List.Extra.getAt 1
                                   |> Maybe.map Tuple.first
                                   |> Maybe.map nodeKey
                                   |> Maybe.withDefault pointXY

                           ( lowNode, highNode ) =
                               ( min inputState.startNodeIndex nodeIndex
                               , max inputState.startNodeIndex nodeIndex
                               )
                       in
                       case Dict.get ( lowNode, highNode, discriminator ) inputState.edgeResolverDict of
                           Just ( edgeIndex, _ ) ->
                               -- So, we don't add this edge
                               -- but we record the traversal
                               let
                                   traversal =
                                       { edge = edgeIndex
                                       , direction =
                                           if lowNode == inputState.startNodeIndex then
                                               Natural

                                           else
                                               Reverse
                                       }
                               in
                               { inputState
                                   | startNodeIndex = nodeIndex
                                   , currentEdge = [ ( road.endPoint, pointGpx ) ]
                                   , traversals = traversal :: inputState.traversals
                               }

                           Nothing ->
                               -- We put this into the resolver dictionary to check for reuse,
                               -- and into the outputs dictionary,
                               -- _and_ we record the traversal.
                               let
                                   newEdgeTree =
                                       DomainModel.treeFromSourcesWithExistingReference
                                           track.referenceLonLat
                                           (List.map Tuple.second orientedEdge)
                                           |> Maybe.withDefault (Leaf road)

                                   newEdgeTrack =
                                       TrackLoaded.newTrackFromTree
                                           track.referenceLonLat
                                           newEdgeTree

                                   newEdgeIndex =
                                       Dict.size inputState.edgesDict

                                   traversal =
                                       { edge = newEdgeIndex
                                       , direction =
                                           if lowNode == inputState.startNodeIndex then
                                               Natural

                                           else
                                               Reverse
                                       }
                               in
                               { startNodeIndex = nodeIndex
                               , currentEdge = [ ( road.endPoint, pointGpx ) ]
                               , edgeResolverDict =
                                   Dict.insert
                                       ( lowNode, highNode, discriminator )
                                       ( newEdgeIndex, newEdgeTree )
                                       inputState.edgeResolverDict
                               , edgesDict =
                                   Dict.insert
                                       newEdgeIndex
                                       { lowNode = lowNode
                                       , highNode = highNode
                                       , via = discriminator
                                       , track = newEdgeTrack
                                       , originalDirection = traversal.direction
                                       }
                                       inputState.edgesDict
                               , traversals = traversal :: inputState.traversals
                               }
       in
       { nodes = nodes
       , edges = finalEdgeFinder.edgesDict

       --, userRoute = List.reverse finalEdgeFinder.traversals
       , referenceLonLat = track.referenceLonLat
       }
-}


trivialGraph : TrackLoaded msg -> Graph msg
trivialGraph track =
    {-
       This just gives us the start and end points, maybe one node if track is looped.
       It's a good place to start and means we can then start visualising.
    -}
    let
        ( startNode, endNode, discriminator ) =
            ( DomainModel.earthPointFromIndex 0 track.trackTree
            , DomainModel.earthPointFromIndex (skipCount track.trackTree) track.trackTree
            , DomainModel.earthPointFromIndex 1 track.trackTree
            )

        nodes =
            Dict.fromList
                [ ( nodeKey startNode, startNode ), ( nodeKey endNode, endNode ) ]

        edge =
            { lowNode = nodeKey startNode
            , highNode = nodeKey endNode
            , via = nodeKey discriminator
            , track = track
            , originalDirection = Natural
            }

        edges =
            Dict.fromList
                [ ( edgeKey edge
                  , edge
                  )
                ]

        traversal =
            { edge = 0, direction = Natural }
    in
    { nodes = nodes
    , edges = edges
    , referenceLonLat = track.referenceLonLat
    }


type alias Junction =
    { arc : Maybe (Arc3d Meters LocalCoords)
    , trim : Quantity Float Meters
    }



{-

   enterRoutePlanningMode : Options msg -> TrackLoaded msg -> ( Options msg, PeteTree )
   enterRoutePlanningMode options track =
       ( { options
           | graph = buildGraph track
           , analyzed = True
           , originalTrack = Just track
           , suggestedNewTree = Nothing
           , suggestedNewGraph = Nothing
           , graphUndos = []
         }
       , options.suggestedNewTree |> Maybe.withDefault track.trackTree
       )
-}
{-

   combineNearbyPoints : Options msg -> TrackLoaded msg -> ( Options msg, PeteTree )
   combineNearbyPoints options track =
       ( { options | suggestedNewTree = Nothing }
       , options.suggestedNewTree
           |> Maybe.withDefault track.trackTree
           |> DomainModel.getAllGPXPointsInNaturalOrder
           |> TrackLoaded.removeAdjacentDuplicates
           |> DomainModel.treeFromSourcesWithExistingReference track.referenceLonLat
           |> Maybe.withDefault track.trackTree
       )
-}
{-

   makeNewRoute : Options msg -> Options msg
   makeNewRoute options =
       {-
          This will walk the route, apply offset, push the old track on the Undo stack
          and then become a "trivialGraph" of the new route.
          Also note we nudge down by 1cm any edges that are revisited.
          Don't forget to push the old points on Undo stack.

           Nope, not doing a fold at traversal level since it gets unduly complicated.
           We need to consider a traversal and both its neighbours, to work out any edge
           shortening, so a triple map is conceptually easier, and a simple recursive
           function more so.
       -}
       let
           graph =
               options.graph

           useNudgeTool nudgeOption track index =
               -- Simple wrapper to use internal operation in Nudge; not efficient!
               Tools.Nudge.nudgeTrackPoint
                   nudgeOption
                   1.0
                   index
                   track

           dummyJunction : Junction
           dummyJunction =
               { arc = Nothing, trim = Quantity.zero }

           junctions : List Junction
           junctions =
               -- Took me so long to see this. Getting old?
               -- There will be one fewer junctions than edges.
               List.map2
                   computeJunction
                   graph.userRoute
                   (List.drop 1 graph.userRoute)

           trim =
               -- CHANGE. Actually prune the trees to get the right leaf.
               -- Will need to do this in the traversal rendering as well.
               -- This changes the "minimum radius" semantics.
               options.minimumRadiusAtPlaces

           renderedArcs : List (List EarthPoint)
           renderedArcs =
               List.map renderJunction junctions

           isNotFirstUseOfEdge : List Bool
           isNotFirstUseOfEdge =
               -- Good practice for RGT is to depress subsequent edge pass by 1cm; avoids flicker.
               let
                   ( _, flags ) =
                       List.foldl
                           (\{ edge } ( traversed, outputs ) ->
                               ( Set.insert edge traversed
                               , Set.member edge traversed :: outputs
                               )
                           )
                           ( Set.empty, [] )
                           graph.userRoute
               in
               List.reverse flags

           trimmedTraversals : List (List EarthPoint)
           trimmedTraversals =
               List.map4
                   trimTraversal
                   (dummyJunction :: junctions)
                   graph.userRoute
                   isNotFirstUseOfEdge
                   (junctions ++ [ dummyJunction ])

           trimTraversal : Junction -> Traversal -> Bool -> Junction -> List EarthPoint
           trimTraversal preceding { edge, direction } repetition following =
               -- Emit offset points but not within the trim areas.
               case Dict.get edge graph.edges of
                   Just edgeInfo ->
                       -- It's a real edge, offset flipped if reversed.
                       -- Compute offset points on unflipped edge then flip if reversed.
                       let
                           correctedOffset =
                               case direction of
                                   Natural ->
                                       options.centreLineOffset

                                   Reverse ->
                                       Quantity.negate options.centreLineOffset

                           ( firstOffsetIndex, lastOffsetIndex ) =
                               -- Other than start and end of route, trim back the edge to allow for the Place arc.
                               ( DomainModel.indexFromDistance trim edgeInfo.track.trackTree + 1
                               , DomainModel.indexFromDistance
                                   (trueLength edgeInfo.track.trackTree |> Quantity.minus trim)
                                   edgeInfo.track.trackTree
                                   - 1
                               )

                           defaultNudge =
                               Tools.Nudge.defaultOptions

                           nudgeOptions : Tools.NudgeOptions.Options
                           nudgeOptions =
                               { defaultNudge
                                   | horizontal = correctedOffset
                                   , vertical =
                                       if repetition then
                                           Length.centimeters -1

                                       else
                                           Quantity.zero
                               }

                           nudgedPoints =
                               List.range firstOffsetIndex lastOffsetIndex
                                   |> List.map (useNudgeTool nudgeOptions edgeInfo.track.trackTree)
                       in
                       case direction of
                           Natural ->
                               nudgedPoints

                           Reverse ->
                               List.reverse nudgedPoints

                   Nothing ->
                       []

           newEarthPoints =
               List.take 1 trimmedTraversals
                   ++ List.Extra.interweave renderedArcs (List.drop 1 trimmedTraversals)

           computeJunction : Traversal -> Traversal -> Junction
           computeJunction inbound outbound =
               -- This is the bit of new geometry. We need the "end" direction of the inbound edge
               -- (allowing for Direction) and the "start" direction of the outbound.
               -- We work out the arc needed to give the minimum radius at centre-line.
               -- We work out how much this impedes on the edges (the "trim").
               -- We compute the "arc" according to the direction and offset.
               -- Note we actually make a 2D arc, then interpolate altitudes to get 3D.
               -- First thing is all the necessary dictionary lookups.
               case ( Dict.get inbound.edge graph.edges, Dict.get outbound.edge graph.edges ) of
                   ( Just inEdge, Just outEdge ) ->
                       let
                           actualVertex =
                               case inbound.direction of
                                   Natural ->
                                       DomainModel.earthPointFromIndex
                                           (skipCount inEdge.track.trackTree)
                                           inEdge.track.trackTree

                                   Reverse ->
                                       DomainModel.earthPointFromIndex
                                           0
                                           inEdge.track.trackTree

                           ( _, inboundTrimPoint ) =
                               case inbound.direction of
                                   Natural ->
                                       DomainModel.interpolateTrack
                                           (trueLength inEdge.track.trackTree |> Quantity.minus trim)
                                           inEdge.track.trackTree

                                   Reverse ->
                                       DomainModel.interpolateTrack
                                           trim
                                           inEdge.track.trackTree

                           ( _, outboundTrimPoint ) =
                               case outbound.direction of
                                   Natural ->
                                       DomainModel.interpolateTrack
                                           trim
                                           outEdge.track.trackTree

                                   Reverse ->
                                       DomainModel.interpolateTrack
                                           (trueLength outEdge.track.trackTree |> Quantity.minus trim)
                                           outEdge.track.trackTree

                           ( inboundDirection, outboundDirection ) =
                               ( Direction3d.from inboundTrimPoint.space actualVertex.space
                                   |> Maybe.withDefault Direction3d.positiveZ
                                   |> Direction3d.projectInto planeFor2dArc
                                   |> Maybe.withDefault Direction2d.positiveX
                               , Direction3d.from actualVertex.space outboundTrimPoint.space
                                   |> Maybe.withDefault Direction3d.positiveZ
                                   |> Direction3d.projectInto planeFor2dArc
                                   |> Maybe.withDefault Direction2d.positiveX
                               )

                           ( inboundRoad, outboundRoad ) =
                               ( LineSegment3d.from inboundTrimPoint.space actualVertex.space
                               , LineSegment3d.from actualVertex.space outboundTrimPoint.space
                               )

                           ( offsetVectorInbound, offsetVectorOutbound ) =
                               ( Vector2d.withLength options.centreLineOffset
                                   (Direction2d.rotateClockwise inboundDirection)
                               , Vector2d.withLength options.centreLineOffset
                                   (Direction2d.rotateClockwise outboundDirection)
                               )

                           meanHeight =
                               Quantity.half <|
                                   Quantity.plus
                                       (Point3d.zCoordinate inboundTrimPoint.space)
                                       (Point3d.zCoordinate outboundTrimPoint.space)

                           -- If we now apply offset to the start and end (which we can), we
                           -- make the offset arc not the centre line arc here.
                           planeFor2dArc =
                               SketchPlane3d.xy
                                   |> SketchPlane3d.translateBy (Vector3d.xyz Quantity.zero Quantity.zero meanHeight)

                           ( inboundTrim2d, outboundTrim2d ) =
                               ( inboundTrimPoint.space |> Point3d.projectInto planeFor2dArc
                               , outboundTrimPoint.space |> Point3d.projectInto planeFor2dArc
                               )

                           ( inboundRoad2d, outboundRoad2d ) =
                               ( inboundRoad |> LineSegment3d.projectInto planeFor2dArc
                               , outboundRoad |> LineSegment3d.projectInto planeFor2dArc
                               )

                           geometryPoint point =
                               Point2d.toRecord inMeters point

                           lineEquationFromSegment segment =
                               Geometry101.lineEquationFromTwoPoints
                                   (geometryPoint <| LineSegment2d.startPoint segment)
                                   (geometryPoint <| LineSegment2d.endPoint segment)

                           ( inboundLineEquation, outboundLineEquation ) =
                               ( lineEquationFromSegment inboundRoad2d
                               , lineEquationFromSegment outboundRoad2d
                               )

                           ( perpToInbound, perToOutbound ) =
                               ( Geometry101.linePerpendicularTo
                                   inboundLineEquation
                                   (geometryPoint inboundTrim2d)
                               , Geometry101.linePerpendicularTo
                                   outboundLineEquation
                                   (geometryPoint outboundTrim2d)
                               )

                           arcCentre =
                               Maybe.map (Point2d.fromRecord meters) <|
                                   Geometry101.lineIntersection
                                       perpToInbound
                                       perToOutbound

                           arc : Maybe (Arc2d Meters LocalCoords)
                           arc =
                               case arcCentre of
                                   Just centre ->
                                       let
                                           ( offsetInboundTrimPoint, _ ) =
                                               ( inboundTrim2d |> Point2d.translateBy offsetVectorInbound
                                               , outboundTrim2d |> Point2d.translateBy offsetVectorOutbound
                                               )

                                           turnAngle =
                                               Direction2d.angleFrom inboundDirection outboundDirection
                                       in
                                       Just <| Arc2d.sweptAround centre turnAngle offsetInboundTrimPoint

                                   Nothing ->
                                       Nothing
                       in
                       -- We make a 3d arc through the same points.
                       case arc of
                           Just foundArc ->
                               { arc = Just <| Arc3d.on planeFor2dArc foundArc
                               , trim = trim
                               }

                           Nothing ->
                               dummyJunction

                   _ ->
                       dummyJunction

           renderJunction : Junction -> List EarthPoint
           renderJunction junction =
               case junction.arc of
                   Just arc ->
                       arc
                           |> Arc3d.approximate (Length.meters 0.1)
                           |> Polyline3d.vertices
                           |> List.map DomainModel.withoutTime

                   Nothing ->
                       []

           newTrack : Maybe (TrackLoaded msg)
           newTrack =
               newEarthPoints
                   |> List.concat
                   |> List.map (DomainModel.gpxFromPointWithReference graph.referenceLonLat)
                   |> DomainModel.treeFromSourcesWithExistingReference graph.referenceLonLat
                   |> Maybe.map (TrackLoaded.newTrackFromTree graph.referenceLonLat)
       in
       case ( options.originalTrack, newTrack ) of
           ( Just oldTrack, Just track ) ->
               -- All has worked.
               let
                   trackWithUndo =
                       TrackLoaded.addToUndoStack Actions.MakeRouteFromGraph track
               in
               { options
                   | graph = trivialGraph trackWithUndo
                   , selectedTraversal = 0
                   , analyzed = False
                   , originalTrack = Nothing
                   , editingTrack = 0
                   , undoGraph = Just graph
                   , undoOriginalTrack = options.originalTrack
               }

           _ ->
               -- Not so much worked.
               options
-}
{-

   undoWalkRoute : Options msg -> Options msg
   undoWalkRoute options =
       case options.undoGraph of
           Just undoGraph ->
               { options
                   | graph = undoGraph
                   , selectedTraversal = 0
                   , analyzed = True
                   , originalTrack = options.undoOriginalTrack
                   , editingTrack = 0
                   , undoGraph = Nothing
               }

           Nothing ->
               options

-}
-- END
