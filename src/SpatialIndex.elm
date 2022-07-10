module SpatialIndex exposing
    ( SpatialContent
    , SpatialNode
    , add
    , empty
    , query
    , queryAllContaining
    , queryNearestToAxisUsing
    , queryWithFilter
    , queryWithFold
    , toList
    )

{-
   This is a simple quadtree based method for tracking bounding boxes.
   Its only requirement is to detect overlaps, with reasonable efficiency.
-}

import Axis2d
import Axis3d
import BoundingBox2d
import Length exposing (Meters)
import LineSegment2d
import List.Extra
import Point2d
import Point3d
import Polygon2d
import Quantity exposing (Quantity(..))
import Quantity.Interval as Interval
import Rectangle2d
import SketchPlane3d


type alias SpatialContent contentType units coords =
    { content : contentType
    , box : BoundingBox2d.BoundingBox2d units coords
    }


type SpatialNode contentType units coords
    = SpatialNode
        { box : BoundingBox2d.BoundingBox2d units coords
        , minSize : Quantity Float units
        , contents : List (SpatialContent contentType units coords)
        , nw : SpatialNode contentType units coords
        , ne : SpatialNode contentType units coords
        , se : SpatialNode contentType units coords
        , sw : SpatialNode contentType units coords
        }
    | Blank


empty : BoundingBox2d.BoundingBox2d units coords -> Quantity Float units -> SpatialNode contentType units coords
empty box minSize =
    SpatialNode
        { box = box
        , minSize = minSize
        , contents = []
        , nw = Blank
        , ne = Blank
        , se = Blank
        , sw = Blank
        }


add :
    SpatialContent contentType units coords
    -> SpatialNode contentType units coords
    -> SpatialNode contentType units coords
add content current =
    -- If the content will fit into a child, pass it down the child, new child if needed.
    -- Otherwise, add it to the contents at this level.
    case current of
        Blank ->
            -- Oops.
            Blank

        SpatialNode node ->
            let
                { minX, maxX, minY, maxY } =
                    BoundingBox2d.extrema node.box

                centre =
                    BoundingBox2d.centerPoint node.box

                canSplit =
                    let
                        ( xInterval, yInterval ) =
                            BoundingBox2d.intervals node.box
                    in
                    (Interval.width xInterval |> Quantity.greaterThan node.minSize)
                        && (Interval.width yInterval |> Quantity.greaterThan node.minSize)

                { nw, ne, sw, se } =
                    { nw = BoundingBox2d.from centre (Point2d.xy minX maxY)
                    , ne = BoundingBox2d.from centre (Point2d.xy maxX maxY)
                    , sw = BoundingBox2d.from centre (Point2d.xy minX minY)
                    , se = BoundingBox2d.from centre (Point2d.xy maxX minY)
                    }

                addToChild :
                    SpatialNode contentType units coords
                    -> BoundingBox2d.BoundingBox2d units coords
                    -> SpatialNode contentType units coords
                addToChild child box =
                    case child of
                        SpatialNode _ ->
                            add content child

                        Blank ->
                            add content (empty box node.minSize)
            in
            if canSplit && (content.box |> BoundingBox2d.isContainedIn nw) then
                SpatialNode
                    { node
                        | nw = addToChild node.nw nw
                    }

            else if canSplit && (content.box |> BoundingBox2d.isContainedIn ne) then
                SpatialNode
                    { node
                        | ne = addToChild node.ne ne
                    }

            else if canSplit && (content.box |> BoundingBox2d.isContainedIn sw) then
                SpatialNode
                    { node
                        | sw = addToChild node.sw sw
                    }

            else if canSplit && (content.box |> BoundingBox2d.isContainedIn se) then
                SpatialNode
                    { node
                        | se = addToChild node.se se
                    }

            else
                SpatialNode { node | contents = content :: node.contents }


query :
    SpatialNode contentType units coords
    -> BoundingBox2d.BoundingBox2d units coords
    -> List (SpatialContent contentType units coords)
query current queryArea =
    -- I think it may be much faster with deep trees to avoid all the
    -- internal concatenation at every level and do it once here.
    queryInternal current queryArea (always True) []
        |> List.concat


queryWithFilter :
    SpatialNode contentType units coords
    -> BoundingBox2d.BoundingBox2d units coords
    -> (contentType -> Bool)
    -> List (SpatialContent contentType units coords)
queryWithFilter current queryArea queryFilter =
    queryInternal current queryArea queryFilter []
        |> List.concat


queryInternal :
    SpatialNode contentType units coords
    -> BoundingBox2d.BoundingBox2d units coords
    -> (contentType -> Bool)
    -> List (List (SpatialContent contentType units coords))
    -> List (List (SpatialContent contentType units coords))
queryInternal current queryArea queryFilter accumulator =
    -- We return content whose bounding box intersects
    -- with the bounding box of the specimen. We do this by looking in a relevant child
    -- or in our own list, depending on the extent of the speciment compared to our children.
    -- This could probably be written using `queryWithFold`, as an exercise.
    case current of
        Blank ->
            accumulator

        SpatialNode node ->
            -- Longhand writing a depth-first traversal using the accumulator.
            if queryArea |> BoundingBox2d.intersects node.box then
                let
                    fromThisNode : List (SpatialContent contentType units coords)
                    fromThisNode =
                        node.contents
                            |> List.filter
                                (\possible ->
                                    (possible.box |> BoundingBox2d.intersects queryArea)
                                        && (possible.content |> queryFilter)
                                )
                in
                (fromThisNode :: accumulator)
                    |> queryInternal node.nw queryArea queryFilter
                    |> queryInternal node.ne queryArea queryFilter
                    |> queryInternal node.se queryArea queryFilter
                    |> queryInternal node.sw queryArea queryFilter

            else
                accumulator


queryWithFold :
    SpatialNode contentType units coords
    -> BoundingBox2d.BoundingBox2d units coords
    -> (SpatialContent contentType units coords -> accum -> accum)
    -> accum
    -> accum
queryWithFold current queryArea folder accumulator =
    case current of
        Blank ->
            accumulator

        SpatialNode node ->
            let
                fromThisNode : List (SpatialContent contentType units coords)
                fromThisNode =
                    node.contents |> List.filter (.box >> BoundingBox2d.intersects queryArea)
            in
            if node.box |> BoundingBox2d.intersects queryArea then
                List.foldl folder accumulator fromThisNode
                    |> queryWithFold node.nw queryArea folder
                    |> queryWithFold node.ne queryArea folder
                    |> queryWithFold node.se queryArea folder
                    |> queryWithFold node.sw queryArea folder

            else
                accumulator


queryAllContaining :
    SpatialNode contentType units coords
    -> Point2d.Point2d units coords
    -> List (SpatialContent contentType units coords)
queryAllContaining current point =
    case current of
        Blank ->
            []

        SpatialNode node ->
            if node.box |> BoundingBox2d.contains point then
                [ List.filter
                    (.box >> BoundingBox2d.contains point)
                    node.contents
                , queryAllContaining node.nw point
                , queryAllContaining node.ne point
                , queryAllContaining node.se point
                , queryAllContaining node.sw point
                ]
                    |> List.concat

            else
                []


type alias FoldStateForNearest contentType units coords =
    { currentBestMetric : Quantity Float units
    , currentBestContent : List (SpatialContent contentType units coords)
    }


queryNearestToAxisUsing :
    SpatialNode contentType Meters coords
    -> Axis3d.Axis3d Meters coords
    -> (contentType -> Quantity Float Meters)
    -> FoldStateForNearest contentType Meters coords
    -> FoldStateForNearest contentType Meters coords
queryNearestToAxisUsing current axis valuation initialState =
    -- Repurposed to improve our hit detection. Not loss of generality in units.
    let
        updateNearestWithContent :
            SpatialContent contentType Meters coords
            -> FoldStateForNearest contentType Meters coords
            -> FoldStateForNearest contentType Meters coords
        updateNearestWithContent indexEntry state =
            let
                metric =
                    valuation indexEntry.content
            in
            if metric |> Quantity.equalWithin Length.inch state.currentBestMetric then
                -- Add to the output set, let caller choose
                { state | currentBestContent = indexEntry :: state.currentBestContent }

            else if metric |> Quantity.lessThan state.currentBestMetric then
                -- This beats the old list, start a new one
                { currentBestContent = [ indexEntry ]
                , currentBestMetric = metric
                }

            else
                -- Can't improve on input
                state

        helperWithAxis :
            SpatialNode contentType Meters coords
            -> Axis2d.Axis2d Meters coord
            -> FoldStateForNearest contentType Meters coords
            -> FoldStateForNearest contentType Meters coords
        helperWithAxis node axis2d inputState =
            case node of
                Blank ->
                    inputState

                SpatialNode hasContent ->
                    -- First test to see if bounding box is safely BEYOND the threshold,
                    -- in which case, pass on the list immediately.
                    -- If any of our content MATCH the threshold, add them to the list.
                    -- If LESS than threshold, form a new list.
                    -- Otherwise, pass the list on.
                    let
                        boxWithThreshold =
                            hasContent.box |> BoundingBox2d.expandBy inputState.currentBestMetric

                        { minX, maxX, minY, maxY } =
                            BoundingBox2d.extrema boxWithThreshold

                        ( ( se, ne ), ( nw, sw ) ) =
                            ( ( Point2d.xy maxX minY, Point2d.xy maxX maxY )
                            , ( Point2d.xy minX minY, Point2d.xy minX maxY )
                            )

                        ( ( north, south ), ( east, west ) ) =
                            ( ( LineSegment2d.from ne nw
                              , LineSegment2d.from se sw
                              )
                            , ( LineSegment2d.from ne se
                              , LineSegment2d.from nw sw
                              )
                            )

                        boundsWithinThreshold =
                            -- If axis crosses our extended boundary, cannot ignore this node.
                            -- Without this pruning, we'd be looking at every point!
                            [ LineSegment2d.intersectionWithAxis axis2d north
                            , LineSegment2d.intersectionWithAxis axis2d south
                            , LineSegment2d.intersectionWithAxis axis2d east
                            , LineSegment2d.intersectionWithAxis axis2d west
                            ]
                                |> List.filterMap identity
                                |> List.isEmpty
                                |> not
                    in
                    if boundsWithinThreshold then
                        --We must examine contents at our node level AND ask our children.
                        let
                            updatedNearest =
                                List.foldl updateNearestWithContent inputState hasContent.contents
                        in
                        updatedNearest
                            |> helperWithAxis hasContent.ne axis2d
                            |> helperWithAxis hasContent.nw axis2d
                            |> helperWithAxis hasContent.se axis2d
                            |> helperWithAxis hasContent.sw axis2d

                    else
                        --Nothing of interest in this node.
                        inputState

        helperWithPoint :
            SpatialNode contentType Meters coords
            -> Point2d.Point2d Meters coords
            -> FoldStateForNearest contentType Meters coords
            -> FoldStateForNearest contentType Meters coords
        helperWithPoint node point2d inputState =
            queryAllContaining node point2d
                |> List.foldl updateNearestWithContent inputState
    in
    -- Can't prune with bounding boxes if we seek the nearest.
    -- Find nearest at this level, ask children if they can better it.
    -- If there's a tie, return all candidates, caller decides.
    case axis |> Axis3d.projectInto SketchPlane3d.xy of
        Just axis2d ->
            helperWithAxis
                current
                axis2d
                initialState

        Nothing ->
            -- Vertical ray, use closest to point, not to axis
            helperWithPoint
                current
                Point2d.origin
                initialState


toList : SpatialNode contentType units coords -> List (SpatialContent contentType units coords)
toList current =
    -- Helper reduces lift shuffling.
    List.concat <| toListInternal current []


toListInternal :
    SpatialNode contentType units coords
    -> List (List (SpatialContent contentType units coords))
    -> List (List (SpatialContent contentType units coords))
toListInternal current accum =
    case current of
        SpatialNode node ->
            node.contents
                :: (toListInternal node.nw <|
                        toListInternal node.ne <|
                            toListInternal node.se <|
                                toListInternal node.sw <|
                                    accum
                   )

        Blank ->
            accum
