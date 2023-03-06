module Tools.Graph exposing
    (  PointIndexEntry
       --, addSelfLoop
       --, addTraversal
       --, combineNearbyPoints

    , addEdge
    , deleteEdgeTraversal
    ,  edgeCanBeDeleted
       --, enterRoutePlanningMode
       --, getTrack

    , loopCanBeAdded
    ,  removeEdge
       --, makeNewRoute

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
import FlatColors.AmericanPalette
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
    --By the wonders of immutable data, and sharing thereof, this does not duplicate the names.
    { trackName : String
    , pointIndex : Int
    }


type alias PointIndex =
    SpatialIndex.SpatialNode PointIndexEntry Length.Meters LocalCoords


identifyPointsToBeMerged : Length.Length -> Graph msg -> List Cluster
identifyPointsToBeMerged tolerance graph =
    {-
       THIS NOW TO WORK ON ALL VISIBLE TRACKS - i.e. all tracks in the graph.
          Data flow outline.
          1. Make spatial indexes of points and leaves, for quick but approximate nearness queries.
          2. For each point, look for leaves within tolerance.
              a. For each such leaf, get distance along leaf and foot of perpendicular.
          3. Collect these perpendicular "feet" by Leaf, sorted by `distanceAlong`.
          4. Update tree by converting each affected Leaf into a shrub (in situ perhaps?).
          Now have tree', enhanced by "virtual points" where leaves are close to points.
          5. For each point, find nearby points within tolerance.
          6. Sort "points with vicini" in descending order of vicini count.
          7. Work through this sorted list, forming groups (a set of previously unclaimed vicini).
          8. For each cluster, derive the centroid.
          9. For each point in all clusters, derive mapping to centroid.
          10. Apply mappings by updating points (in situ perhaps?).
          Now have tree'' which has adjusted points at cluster centroids.
          Proof of pudding awaited ...
    -}
    let
        addTolerance box =
            BoundingBox2d.expandBy tolerance box

        flatBox =
            -- Basis for our spatial index, union of track bounding boxes
            graph.edges
                |> Dict.foldl
                    (\_ edge acc ->
                        BoundingBox3d.union
                            acc
                            (DomainModel.boundingBox edge.track.trackTree)
                    )
                    (BoundingBox3d.singleton Point3d.origin)
                |> UtilsForViews.flatBox

        growBox =
            -- We just make our index area slightly larger but I forget why.
            addTolerance flatBox

        pointWithTolerance pt =
            -- We make each point fuzzy for finding nearby points and leaves.
            addTolerance <|
                BoundingBox2d.singleton <|
                    Point3d.projectInto SketchPlane3d.xy pt

        emptyPointIndex : PointIndex
        emptyPointIndex =
            -- The last parameter here is not the quality, it
            -- only affects the index efficiency.
            SpatialIndex.empty growBox (Length.meters 100.0)

        indexPoint : RoadSection -> ( String, Int, PointIndex ) -> ( String, Int, PointIndex )
        indexPoint leaf ( trackName, pointNumber, indexBuild ) =
            ( trackName
            , pointNumber + 1
            , SpatialIndex.add
                { content = { trackName = trackName, pointIndex = pointNumber }
                , box =
                    BoundingBox2d.singleton <|
                        Point3d.projectInto SketchPlane3d.xy leaf.endPoint.space
                }
                indexBuild
            )

        {-
           2. For each point, look for leaves within tolerance. (across all edges)
               a. For each such leaf, get distance along leaf and foot of perpendicular.
        -}
        findNearbyLeaves : TrackLoaded msg -> Int -> List InsertedPointOnLeaf
        findNearbyLeaves track pointNumber =
            -- Use spatial leaf index then refine with geometry.
            let
                pt =
                    DomainModel.earthPointFromIndex pointNumber track.trackTree

                thisPoint2d =
                    Point3d.projectInto SketchPlane3d.xy pt.space

                results =
                    SpatialIndex.query track.leafIndex (pointWithTolerance pt.space)
                        |> List.map (.content >> .leafIndex)
                        |> List.Extra.unique

                isThisLeafClose : Int -> Maybe InsertedPointOnLeaf
                isThisLeafClose leafNumber =
                    let
                        leaf =
                            DomainModel.asRecord <| DomainModel.leafFromIndex leafNumber track.trackTree

                        axis2d =
                            Axis2d.through
                                (leaf.startPoint.space |> Point3d.projectInto SketchPlane3d.xy)
                                leaf.directionAtStart

                        axis3d =
                            Axis3d.throughPoints leaf.startPoint.space leaf.endPoint.space
                                |> Maybe.withDefault Axis3d.z

                        ( along, from, foot ) =
                            -- Proximity test in 2D as altitudes may vary greatly.
                            ( pt.space |> Point3d.signedDistanceAlong axis3d
                            , thisPoint2d |> Point2d.signedDistanceFrom axis2d |> Quantity.abs
                            , pt.space |> Point3d.projectOntoAxis axis3d
                            )

                        isShortPerp =
                            from |> Quantity.lessThanOrEqualTo tolerance

                        isAfterStart =
                            along |> Quantity.greaterThanZero

                        isBeforeEnd =
                            along |> Quantity.lessThan (trueLength (Leaf leaf))

                        isNotConnected =
                            leafNumber /= pointNumber && leafNumber + 1 /= pointNumber
                    in
                    if isNotConnected && isShortPerp && isAfterStart && isBeforeEnd then
                        Just
                            { trackName = track.trackName
                            , sourcePointNumber = pointNumber
                            , leafNumber = leafNumber
                            , distanceAlong = along
                            , earthPoint = DomainModel.withoutTime foot
                            }

                    else
                        Nothing
            in
            results |> List.filterMap isThisLeafClose

        findNearbyLeavesFoldFn :
            RoadSection
            -> ( ( TrackLoaded msg, Int ), List InsertedPointOnLeaf )
            -> ( ( TrackLoaded msg, Int ), List InsertedPointOnLeaf )
        findNearbyLeavesFoldFn road ( ( track, leafNumber ), outputs ) =
            ( ( track, leafNumber + 1 )
            , case findNearbyLeaves track (leafNumber + 1) of
                [] ->
                    outputs

                nearby ->
                    nearby ++ outputs
            )

        {-
           We have the feet of the perpendiculars to nearby leaves.
           3. Collect these perpendicular "feet" by Leaf, sorted by `distanceAlong`.
        -}
        perpendicularFeetGroupedByLeaf : Dict ( String, Int ) (List InsertedPointOnLeaf)
        perpendicularFeetGroupedByLeaf =
            --TODO: Work across all edges' tracks.
            let
                -- Want "nearby" for all points. Our model traverses leaves, so we
                -- preload the start point and use the end point of each leaf.
                findAllNearbyLeaves =
                    graph.edges
                        |> Dict.foldl
                            (\_ edge accum ->
                                findNearbyForTrack edge.track ++ accum
                            )
                            []

                findNearbyForTrack track =
                    DomainModel.foldOverRoute
                        findNearbyLeavesFoldFn
                        track.trackTree
                        ( ( track, 0 ), findNearbyLeaves track 0 )
                        |> Tuple.second

                addToLeafDict newEntry dict =
                    case Dict.get ( newEntry.trackName, newEntry.leafNumber ) dict of
                        Just prevEntries ->
                            Dict.insert
                                ( newEntry.trackName, newEntry.leafNumber )
                                (newEntry :: prevEntries)
                                dict

                        Nothing ->
                            Dict.insert
                                ( newEntry.trackName, newEntry.leafNumber )
                                [ newEntry ]
                                dict

                leafDictUnsorted : Dict ( String, Int ) (List InsertedPointOnLeaf)
                leafDictUnsorted =
                    List.foldl
                        addToLeafDict
                        Dict.empty
                        findAllNearbyLeaves

                sortEachLeafEntries : ( String, Int ) -> List InsertedPointOnLeaf -> List InsertedPointOnLeaf
                sortEachLeafEntries ( trackName, leafIndex ) unsorted =
                    unsorted
                        |> List.Extra.uniqueBy (.distanceAlong >> Length.inMeters)
                        |> List.sortBy (.distanceAlong >> Length.inMeters)
            in
            Dict.map sortEachLeafEntries leafDictUnsorted

        {-
           4. Update tree by converting each affected Leaf into a "branch" (in situ perhaps?).
        -}
        insertPointsInLeaf : ( String, Int ) -> List InsertedPointOnLeaf -> PeteTree -> PeteTree
        insertPointsInLeaf ( trackName, leafNumber ) newPoints tree =
            --TODO: Pan-edge.
            let
                asGPX =
                    List.map
                        (.earthPoint >> DomainModel.gpxFromPointWithReference track.referenceLonLat)
                        newPoints
            in
            DomainModel.insertPointsIntoLeaf
                leafNumber
                track.referenceLonLat
                asGPX
                tree

        treeWithAddedPoints : PeteTree
        treeWithAddedPoints =
            --NOTE: Must add to highest numbered leaf first or leaf numbers confused!
            --NOTE: Dict.foldr not doing what I expect.
            Dict.foldr
                insertPointsInLeaf
                track.trackTree
                perpendicularFeetGroupedByLeaf

        {-
           Now have tree', enhanced by "virtual points" where leaves are close to points.
           5. For each point, find nearby points within tolerance.
        -}
        ( _, pointIndex ) =
            --NOTE: We index the revised tree so we pick up the extra points.
            -- Pre-pop with first point so the fold can focus on the leaf end points.
            --TODO: Fold over edges.
            DomainModel.foldOverRoute
                indexPoint
                treeWithAddedPoints
                ( 1
                , SpatialIndex.add
                    { content = { pointIndex = 0 }
                    , box =
                        pointWithTolerance <|
                            .space <|
                                DomainModel.earthPointFromIndex 0 treeWithAddedPoints
                    }
                    emptyPointIndex
                )

        pointsNearPoint : Int -> PeteTree -> List Int
        pointsNearPoint pointNumber tree =
            -- Prelude to finding clusters of points.
            -- Use spatial point index then refine with geometry.
            --NOTE: This will NOW NOT include the query point.
            --NOTE: This MUST NOT include points that are merely close along the track; the
            -- intent is to find points separated along the track but close in space.
            --Hence we can filter using this.
            let
                pt =
                    DomainModel.earthPointFromIndex pointNumber tree

                thisPoint2d =
                    Point3d.projectInto SketchPlane3d.xy pt.space

                results =
                    SpatialIndex.query pointIndex (pointWithTolerance pt.space)
                        |> List.map (.content >> .pointIndex)
            in
            results
                |> List.filter
                    (\ptidx ->
                        DomainModel.earthPointFromIndex ptidx tree
                            |> .space
                            |> Point3d.projectInto SketchPlane3d.xy
                            |> Point2d.equalWithin tolerance thisPoint2d
                    )

        {-
           I thought it would be better to exclude points that are close by dint of being along the route.
           Turns out this is empirically less pleasing, adding more Nodes to the Graph.
                          |> List.filter
                              (\ptidx ->
                                  DomainModel.distanceFromIndex ptidx tree
                                      |> Quantity.minus thisPointTrackDistance
                                      |> Quantity.abs
                                      |> Quantity.greaterThan tolerance
                              )
        -}
        pointsNearPointFoldWrapper :
            RoadSection
            -> ( Int, List ( Int, List Int ) )
            -> ( Int, List ( Int, List Int ) )
        pointsNearPointFoldWrapper road ( leafNumber, collection ) =
            case pointsNearPoint (leafNumber + 1) treeWithAddedPoints of
                [] ->
                    ( leafNumber + 1, collection )

                notEmpty ->
                    ( leafNumber + 1, ( leafNumber + 1, notEmpty ) :: collection )

        --nearbyPointsForEachPoint : List ( Int, List Int )
        ( _, nearbyPointsForEachPoint ) =
            -- Injecting the point zero case is slightly clumsy.
            DomainModel.foldOverRoute
                pointsNearPointFoldWrapper
                treeWithAddedPoints
                ( 0
                , case
                    pointsNearPoint 0 treeWithAddedPoints
                  of
                    [] ->
                        []

                    notEmpty ->
                        [ ( 0, notEmpty ) ]
                )

        {-
           6. Sort "points with vicini" in descending order of vicini count.
           7. Work through this sorted list, forming groups (a set of previously unclaimed vicini).
        -}
        groupsOfNearbyPoints : List (List ( String, Int ))
        groupsOfNearbyPoints =
            -- This now spans tracks!
            let
                groupsInDescendingSizeOrder =
                    -- Since the queries return DON'T the home point, we don't need the first Int.
                    nearbyPointsForEachPoint
                        |> List.map (\( home, others ) -> home :: others)
                        |> List.sortBy (List.length >> negate)

                retainUnclaimedGroupMembers :
                    List Int
                    -> ( Set Int, List (List Int) )
                    -> ( Set Int, List (List Int) )
                retainUnclaimedGroupMembers group ( claimed, retained ) =
                    let
                        remaining =
                            group |> List.filter (\i -> not <| Set.member i claimed)
                    in
                    case remaining of
                        -- Nothing left here, drop it.
                        _ :: _ :: _ ->
                            -- Only interested if not empty and not singleton
                            ( Set.fromList remaining |> Set.union claimed
                            , remaining :: retained
                            )

                        _ ->
                            ( claimed, retained )

                ( _, groupsWithPriorClaimsRemoved ) =
                    List.foldl
                        retainUnclaimedGroupMembers
                        ( Set.empty, [] )
                        groupsInDescendingSizeOrder
            in
            groupsWithPriorClaimsRemoved

        {-
           8. For each cluster, derive the centroid.
        -}
        clustersWithCentroids : List Cluster
        clustersWithCentroids =
            let
                makeProperCluster : List Int -> Cluster
                makeProperCluster pointNumbers =
                    { centroid =
                        case
                            pointNumbers
                                |> List.map
                                    (\pt ->
                                        DomainModel.earthPointFromIndex pt treeWithAddedPoints
                                    )
                        of
                            [] ->
                                --We already know this is a non-empty list.
                                Point3d.origin

                            pt1 :: more ->
                                Point3d.centroid pt1.space (List.map .space more)
                    , pointsToAdjust = pointNumbers
                    }
            in
            List.map makeProperCluster groupsOfNearbyPoints

        {-
           9. For each point in all clusters, derive mapping to centroid.
           (Step 9 is merely a restatement of Cluster, so will skip.)
           10. Apply mappings by updating points (`updatePointByIndexInSitu` perhaps?).
        -}
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
    ( clustersWithCentroids
    , treeWithCentroidsApplied
    )


type alias EdgeFinder msg =
    { startNodeIndex : Int
    , currentEdge : List ( EarthPoint, GPXSource )
    , edgeResolverDict : Dict ( String, String, String ) ( String, PeteTree )
    , edgesDict : Dict String (Edge msg)
    , traversals : List Traversal
    }



{-

   showNewPoints : List Cluster -> TrackLoaded msg -> List (Entity LocalCoords)
   showNewPoints pointInfo track =
       let
           locations =
               pointInfo |> List.map .centroid

           material =
               Material.color Color.white

           highlightPoint point =
               Scene3d.point { radius = Pixels.pixels 3 } material point
       in
       List.map highlightPoint locations
-}
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
