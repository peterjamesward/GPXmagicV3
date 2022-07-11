module DomainModel exposing
    ( EarthPoint
    , GPXSource
    , PeteTree(..)
    , RoadSection
    , TrackPoint
    , asRecord
    , boundingBox
    , distanceFromIndex
    , earthPointFromIndex
    , effectiveLatitude
    , endPoint
    , extractPointsInRange
    , foldOverRoute
    , foldOverRouteRL
    , foldOverRouteRLwithDepthLimit
    , getAllEarthPointsInNaturalOrder
    , getAllGPXPointsInDict
    , getAllGPXPointsInNaturalOrder
    , getDualCoords
    , getFirstLeaf
    , getLastLeaf
    , gpxDistance
    , gpxFromPointWithReference
    , gpxPointFromIndex
    , gradientFromNode
    , indexFromDistance
    , indexFromDistanceRoundedDown
    , indexFromDistanceRoundedUp
    , insertPointsIntoLeaf
    , interpolateTrack
    , leafFromIndex
    , lngLatPair
    , makeLeaf
    , makeRoadSection
    , nearestToLonLat
    , nearestToRay
    , pointFromGpxWithReference
    , queryRoadsUsingFilter
    , rebuildTree
    , replaceRange
    , skipCount
    , sourceData
    , startPoint
    , trackPointsForOutput
    , traverseTreeBetweenLimitsToDepth
    , treeFromSourcePoints
    , treeFromSourcesWithExistingReference
    , trueLength
    , updatePointByIndexInSitu
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox2d
import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Json.Encode as E
import LeafIndex exposing (LeafIndex, LeafIndexEntry)
import Length exposing (Length, Meters, inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import SketchPlane3d
import SpatialIndex
import Sphere3d exposing (Sphere3d)
import Spherical as Spherical exposing (range)


type alias GPXSource =
    -- Being a raw line of data from GPX file.
    { longitude : Direction2d LocalCoords
    , latitude : Angle
    , altitude : Quantity Float Meters
    }


type alias EarthPoint =
    Point3d Meters LocalCoords


type alias TrackPoint =
    -- A type designed for output, not for computation!
    { distanceFromStart : Quantity Float Meters
    , longitude : Angle
    , latitude : Angle
    , altitude : Quantity Float Meters
    , gradient : Float
    , trueLength : Quantity Float Meters
    }


type alias RoadSection =
    -- Can be based between two 'fundamental' points from GPX, or an assembly of them.
    -- Bounding box and Sphere needed for culling in nearness tests.
    -- Keeping track of longitude tricky because of IDL.
    { sourceData : ( GPXSource, GPXSource )
    , startPoint : EarthPoint
    , endPoint : EarthPoint

    -- For rapid location of points using non-map views...
    , boundingBox : BoundingBox3d Meters LocalCoords

    --, sphere : Sphere3d Meters LocalCoords
    , trueLength : Quantity Float Meters
    , skipCount : Int

    -- For efficient detection of map clicks...
    , medianLongitude : Direction2d LocalCoords
    , eastwardExtent : Angle
    , westwardExtent : Angle

    -- Basic route statistics...
    , altitudeGained : Quantity Float Meters
    , altitudeLost : Quantity Float Meters
    , distanceClimbing : Quantity Float Meters
    , distanceDescending : Quantity Float Meters
    , steepestClimb : Float

    -- Bunch of stuff we need in the tree to be able to locate problem points efficiently...
    , gradientAtStart : Float
    , gradientAtEnd : Float
    , gradientChangeMaximumAbs : Float
    , directionAtStart : Direction2d LocalCoords
    , directionAtEnd : Direction2d LocalCoords
    , directionChangeMaximumAbs : Angle
    }


type
    PeteTree
    -- Absurdly simple tree works quite well.
    = Leaf RoadSection
    | Node
        { nodeContent : RoadSection
        , left : PeteTree
        , right : PeteTree
        }


asRecord : PeteTree -> RoadSection
asRecord treeNode =
    -- Because is daft writing accessors for every field.
    case treeNode of
        Leaf section ->
            section

        Node node ->
            node.nodeContent


sourceData : PeteTree -> ( GPXSource, GPXSource )
sourceData treeNode =
    treeNode |> asRecord |> .sourceData


effectiveLatitude : PeteTree -> Angle
effectiveLatitude treeNode =
    treeNode |> sourceData |> Tuple.first |> .latitude


startPoint : PeteTree -> EarthPoint
startPoint treeNode =
    treeNode |> asRecord |> .startPoint


endPoint : PeteTree -> EarthPoint
endPoint treeNode =
    treeNode |> asRecord |> .endPoint


isLongitudeContained : Direction2d LocalCoords -> PeteTree -> Bool
isLongitudeContained longitude treeNode =
    let
        turnFromMedianToGiven =
            Direction2d.angleFrom (medianLongitude treeNode) longitude
    in
    (turnFromMedianToGiven |> Quantity.greaterThanOrEqualTo (westwardTurn treeNode))
        && (turnFromMedianToGiven |> Quantity.lessThanOrEqualTo (eastwardTurn treeNode))


rotationAwayFrom : Direction2d LocalCoords -> PeteTree -> Angle
rotationAwayFrom longitude treeNode =
    -- By how much, in any direction, would we need to move the longitude
    -- to make it "contained". This is for selecting a branch if neither side is "contained".
    let
        nodeEast =
            medianLongitude treeNode |> Direction2d.rotateBy (eastwardTurn treeNode)

        nodeWest =
            medianLongitude treeNode |> Direction2d.rotateBy (westwardTurn treeNode)
    in
    Quantity.min
        (Quantity.abs <| Direction2d.angleFrom longitude nodeEast)
        (Quantity.abs <| Direction2d.angleFrom longitude nodeWest)


bestAvailableDistanceGuess : GPXSource -> PeteTree -> Length
bestAvailableDistanceGuess target node =
    -- Use above two functions to decide whether a node MAY be close to a given point.
    if isLongitudeContained target.longitude node then
        Quantity.zero

    else
        rotationAwayFrom target.longitude node
            |> Angle.inDegrees
            |> (*) Spherical.metresPerDegree
            |> (*) (Angle.cos target.latitude)
            |> Length.meters


trueLength : PeteTree -> Quantity Float Meters
trueLength treeNode =
    treeNode |> asRecord |> .trueLength


skipCount : PeteTree -> Int
skipCount treeNode =
    case treeNode of
        Leaf leaf ->
            1

        Node node ->
            node.nodeContent.skipCount


boundingBox : PeteTree -> BoundingBox3d Meters LocalCoords
boundingBox treeNode =
    treeNode |> asRecord |> .boundingBox



--sphere : PeteTree -> Sphere3d Meters LocalCoords
--sphere treeNode =
--    treeNode |> asRecord |> .sphere


medianLongitude : PeteTree -> Direction2d LocalCoords
medianLongitude treeNode =
    treeNode |> asRecord |> .medianLongitude


eastwardTurn : PeteTree -> Angle
eastwardTurn treeNode =
    treeNode |> asRecord |> .eastwardExtent


westwardTurn : PeteTree -> Angle
westwardTurn treeNode =
    treeNode |> asRecord |> .westwardExtent


pointFromGpxWithReference : GPXSource -> GPXSource -> EarthPoint
pointFromGpxWithReference reference gpx =
    Point3d.xyz
        (Direction2d.angleFrom reference.longitude gpx.longitude
            |> Angle.inDegrees
            |> (*) Spherical.metresPerDegree
            |> (*) (Angle.cos gpx.latitude)
            |> Length.meters
        )
        (gpx.latitude
            |> Quantity.minus reference.latitude
            |> Angle.inDegrees
            |> (*) Spherical.metresPerDegree
            |> Length.meters
        )
        gpx.altitude


gpxFromPointWithReference : GPXSource -> EarthPoint -> GPXSource
gpxFromPointWithReference reference point =
    let
        ( x, y, z ) =
            Point3d.toTuple Length.inMeters point

        longitude =
            Angle.degrees <|
                x
                    / Angle.cos latitude
                    / Spherical.metresPerDegree
                    + (Angle.inDegrees <| Direction2d.toAngle reference.longitude)

        latitude =
            Angle.degrees <| y / Spherical.metresPerDegree + Angle.inDegrees reference.latitude

        altitude =
            z
    in
    GPXSource
        (Direction2d.fromAngle longitude)
        latitude
        (Length.meters altitude)


makeLeaf : GPXSource -> GPXSource -> GPXSource -> PeteTree
makeLeaf reference earth1 earth2 =
    Leaf <| makeRoadSection reference earth1 earth2


makeRoadSection : GPXSource -> GPXSource -> GPXSource -> RoadSection
makeRoadSection reference earth1 earth2 =
    let
        ( local1, local2 ) =
            ( pointFromGpxWithReference reference earth1
            , pointFromGpxWithReference reference earth2
            )
    in
    makeRoadSectionKnowingLocalCoords ( earth1, local1 ) ( earth2, local2 )


makeRoadSectionKnowingLocalCoords :
    ( GPXSource, EarthPoint )
    -> ( GPXSource, EarthPoint )
    -> RoadSection
makeRoadSectionKnowingLocalCoords ( earth1, local1 ) ( earth2, local2 ) =
    let
        box =
            BoundingBox3d.from local1 local2

        range : Length.Length
        range =
            Length.meters <|
                Spherical.range
                    ( Direction2d.toAngle earth1.longitude, earth1.latitude )
                    ( Direction2d.toAngle earth2.longitude, earth2.latitude )

        medianLon =
            -- Careful, don't average because of -pi/+pi, work out half the turn.
            earth1.longitude
                |> Direction2d.rotateBy
                    (Direction2d.angleFrom earth1.longitude earth2.longitude |> Quantity.half)

        altitudeChange =
            Point3d.zCoordinate local2 |> Quantity.minus (Point3d.zCoordinate local1)

        gradient =
            if
                (Quantity.abs range |> Quantity.greaterThanZero)
                    && (Quantity.abs altitudeChange |> Quantity.greaterThanZero)
            then
                100.0 * Length.inMeters altitudeChange / Length.inMeters range

            else
                0.0

        bearing =
            -- NB bearing is from North, clockwise.
            Spherical.findBearingToTarget
                ( Angle.inRadians earth1.latitude, Angle.inRadians <| Direction2d.toAngle earth1.longitude )
                ( Angle.inRadians earth2.latitude, Angle.inRadians <| Direction2d.toAngle earth2.longitude )

        direction =
            pi / 2 - bearing |> Angle.radians |> Direction2d.fromAngle
    in
    { sourceData = ( earth1, earth2 )
    , startPoint = local1
    , endPoint = local2
    , boundingBox = box

    --, sphere = containingSphere box
    , trueLength = range
    , skipCount = 1
    , medianLongitude = medianLon
    , eastwardExtent =
        Quantity.max Quantity.zero <|
            Quantity.max
                (Direction2d.angleFrom medianLon earth1.longitude)
                (Direction2d.angleFrom medianLon earth2.longitude)
    , westwardExtent =
        Quantity.min Quantity.zero <|
            Quantity.min
                (Direction2d.angleFrom medianLon earth1.longitude)
                (Direction2d.angleFrom medianLon earth2.longitude)
    , altitudeGained = Quantity.max Quantity.zero altitudeChange
    , altitudeLost = Quantity.max Quantity.zero <| Quantity.negate altitudeChange
    , distanceClimbing =
        if altitudeChange |> Quantity.greaterThanZero then
            range

        else
            Quantity.zero
    , distanceDescending =
        if altitudeChange |> Quantity.lessThanZero then
            range

        else
            Quantity.zero
    , steepestClimb = max 0.0 gradient
    , gradientAtStart = gradient
    , gradientAtEnd = gradient
    , gradientChangeMaximumAbs = abs gradient
    , directionAtStart = direction
    , directionAtEnd = direction
    , directionChangeMaximumAbs = Angle.degrees 0
    }


combineInfo : PeteTree -> PeteTree -> RoadSection
combineInfo info1 info2 =
    let
        box =
            BoundingBox3d.union (boundingBox info1) (boundingBox info2)

        sharedMedian =
            medianLongitude info1
                |> Direction2d.rotateBy
                    (Direction2d.angleFrom (medianLongitude info1) (medianLongitude info2) |> Quantity.half)
    in
    { sourceData = ( Tuple.first (sourceData info1), Tuple.second (sourceData info2) )
    , startPoint = startPoint info1
    , endPoint = endPoint info2
    , boundingBox = box

    --, sphere = containingSphere box
    , trueLength = Quantity.plus (trueLength info1) (trueLength info2)
    , skipCount = skipCount info1 + skipCount info2
    , medianLongitude = sharedMedian
    , eastwardExtent =
        Quantity.max
            (medianLongitude info1
                |> Direction2d.rotateBy (eastwardTurn info1)
                |> Direction2d.angleFrom sharedMedian
            )
            (medianLongitude info2
                |> Direction2d.rotateBy (eastwardTurn info2)
                |> Direction2d.angleFrom sharedMedian
            )
    , westwardExtent =
        Quantity.min
            (medianLongitude info1
                |> Direction2d.rotateBy (westwardTurn info1)
                |> Direction2d.angleFrom sharedMedian
            )
            (medianLongitude info2
                |> Direction2d.rotateBy (westwardTurn info2)
                |> Direction2d.angleFrom sharedMedian
            )
    , altitudeGained =
        Quantity.plus
            (info1 |> asRecord |> .altitudeGained)
            (info2 |> asRecord |> .altitudeGained)
    , altitudeLost =
        Quantity.plus
            (info1 |> asRecord |> .altitudeLost)
            (info2 |> asRecord |> .altitudeLost)
    , distanceClimbing =
        Quantity.plus
            (info1 |> asRecord |> .distanceClimbing)
            (info2 |> asRecord |> .distanceClimbing)
    , distanceDescending =
        Quantity.plus
            (info1 |> asRecord |> .distanceDescending)
            (info2 |> asRecord |> .distanceDescending)
    , steepestClimb =
        max (info1 |> asRecord |> .steepestClimb)
            (info2 |> asRecord |> .steepestClimb)
    , gradientAtStart = info1 |> asRecord |> .gradientAtStart
    , gradientAtEnd = info2 |> asRecord |> .gradientAtEnd
    , gradientChangeMaximumAbs =
        Maybe.withDefault 0.0 <|
            List.maximum
                [ info1 |> asRecord |> .gradientChangeMaximumAbs
                , info2 |> asRecord |> .gradientChangeMaximumAbs
                , abs <|
                    (info1 |> asRecord |> .gradientAtEnd)
                        - (info2 |> asRecord |> .gradientAtStart)
                ]
    , directionAtStart = info1 |> asRecord |> .directionAtStart
    , directionAtEnd = info2 |> asRecord |> .directionAtEnd
    , directionChangeMaximumAbs =
        Maybe.withDefault Quantity.zero <|
            Quantity.maximum
                [ info1 |> asRecord |> .directionChangeMaximumAbs
                , info2 |> asRecord |> .directionChangeMaximumAbs
                , Quantity.abs <|
                    Direction2d.angleFrom
                        (info1 |> asRecord |> .directionAtEnd)
                        (info2 |> asRecord |> .directionAtStart)
                ]
    }


rebuildTree : GPXSource -> Maybe PeteTree -> Maybe PeteTree
rebuildTree referencePoint treeNode =
    case treeNode of
        Just something ->
            something
                |> getAllGPXPointsInNaturalOrder
                |> treeFromSourcesWithExistingReference referencePoint

        Nothing ->
            Nothing


replaceRange :
    Int
    -> Int
    -> GPXSource
    -> List GPXSource
    -> PeteTree
    -> Maybe PeteTree
replaceRange fromStart fromEnd withReferencePoint newPoints currentTree =
    let
        leftBit =
            takePointsFromLeft fromStart currentTree

        rightBit =
            takePointsFromRight fromEnd currentTree
    in
    (leftBit ++ newPoints ++ rightBit)
        |> treeFromSourcesWithExistingReference withReferencePoint



--replaceRangeInternal fromStart fromEnd withReferencePoint newPoints currentTree
--    |> rebuildTree withReferencePoint


takePointsFromLeft : Int -> PeteTree -> List GPXSource
takePointsFromLeft numPoints tree =
    if numPoints == 0 then
        []

    else if numPoints == 1 then
        [ gpxPointFromIndex 0 tree ]

    else
        -- We need leaves
        let
            leftLeaves =
                takeFromLeft (numPoints - 1) tree
        in
        Maybe.map getAllGPXPointsInNaturalOrder leftLeaves
            |> Maybe.withDefault []


takePointsFromRight : Int -> PeteTree -> List GPXSource
takePointsFromRight numPoints tree =
    if numPoints == 0 then
        []

    else if numPoints == 1 then
        [ getLastLeaf tree |> .sourceData |> Tuple.second ]

    else
        -- We need leaves
        let
            rightLeaves =
                takeFromRight (numPoints - 1) tree
        in
        Maybe.map getAllGPXPointsInNaturalOrder rightLeaves
            |> Maybe.withDefault []


replaceRangeInternal :
    Int
    -> Int
    -> GPXSource
    -> List GPXSource
    -> PeteTree
    -> Maybe PeteTree
replaceRangeInternal fromStart fromEnd withReferencePoint newPoints currentTree =
    {--
        This is our key edit function for external use.

        Internally, we make minimal tree changes to make it so, at lowest level possible.
        Of course, we reconstruct affected nodes.
        The fromStart, fromEnd is because it makes life easier for Undo, and it has symmetry.
        So, the rules:
        If you can delegate to one child, do so, and safeJoin result to sibling.
        Otherwise, you have to handle it.
        (I briefly thought about passing one half to each child, but that would be bad.)

        Simplest and sort of neatest way here is to make a single list (yes, list) from the
        parts we keep and the new parts, then use existing logic to form a new node.
        Locally balanced, not too shabby, if many edits are sort of "like for like".
        Key semantic point - this works in POINT index space and the values are inclusive
        (otherwise, how could we do a whole track edit?).

        To be clear, if our points are <ABC> and we use `replaceRange 0 0 []`,
        the track would disappear, and we would return a Nothing. (It's not our job to stop that.)
        Likewise `replaceRange 1 1 []' returns <AC> (a single Leaf).

        Also, note the caller MUST have previously derived GPX co-ords, not local metric points,
        using functions that belong in TrackLoaded.
    --}
    case currentTree of
        Leaf _ ->
            buildNewNodeWithRange fromStart fromEnd withReferencePoint newPoints currentTree

        Node node ->
            let
                containedInLeft =
                    -- These are '<' not '<=', validated by tests.
                    fromStart
                        < skipCount node.left
                        && (fromEnd - skipCount node.right > 0)

                containedInRight =
                    fromEnd
                        < skipCount node.right
                        && (fromStart - skipCount node.left > 0)
            in
            case ( containedInLeft, containedInRight ) of
                ( True, True ) ->
                    -- Really, how can that be? Better take control.
                    buildNewNodeWithRange
                        fromStart
                        fromEnd
                        withReferencePoint
                        newPoints
                        currentTree

                ( False, False ) ->
                    -- The buck stops here.
                    buildNewNodeWithRange
                        fromStart
                        fromEnd
                        withReferencePoint
                        newPoints
                        currentTree

                ( True, False ) ->
                    -- Delegate to left
                    safeJoin
                        (replaceRange
                            fromStart
                            (fromEnd - skipCount node.right)
                            withReferencePoint
                            newPoints
                            node.left
                        )
                        (Just node.right)

                ( False, True ) ->
                    -- Delegate to right
                    safeJoin
                        (Just node.left)
                        (replaceRange
                            (fromStart - skipCount node.left)
                            fromEnd
                            withReferencePoint
                            newPoints
                            node.right
                        )


buildNewNodeWithRange : Int -> Int -> GPXSource -> List GPXSource -> PeteTree -> Maybe PeteTree
buildNewNodeWithRange fromStart fromEnd withReferencePoint newPoints currentTree =
    {-
       The decision is made, we simply make a new node.
       No need for finesse, at least initially.
       We should be a few levels down the tree in most cases.
    -}
    let
        currentGpx =
            recreateGpxSources <| Just currentTree

        intro =
            List.take fromStart currentGpx

        outro =
            List.drop (1 + skipCount currentTree - fromEnd) currentGpx

        updatedGpx =
            intro ++ newPoints ++ outro
    in
    treeFromSourcesWithExistingReference withReferencePoint updatedGpx


joiningNode : PeteTree -> PeteTree -> PeteTree
joiningNode left right =
    -- Joins two nodes, with no special care needed.
    Node
        { nodeContent = combineInfo left right
        , left = left
        , right = right
        }


safeJoin : Maybe PeteTree -> Maybe PeteTree -> Maybe PeteTree
safeJoin left right =
    -- Wrapper around joining node so we can use Maybes.
    case ( left, right ) of
        ( Just leftTree, Just rightTree ) ->
            Just <| joiningNode leftTree rightTree

        ( Just leftTree, Nothing ) ->
            left

        ( Nothing, Just rightTree ) ->
            right

        ( Nothing, Nothing ) ->
            Nothing


joinReplacingEndPointsWithNewLeaf : PeteTree -> PeteTree -> Maybe PeteTree
joinReplacingEndPointsWithNewLeaf left right =
    -- Joins two nodes but in the case where we do not want to keep the
    -- innermost points of each side but replace them with a new leaf.
    let
        newLeaf =
            Just <|
                Leaf <|
                    makeRoadSectionKnowingLocalCoords (penultimatePoint left) (secondPoint right)

        truncatedLeft =
            takeFromLeft (skipCount left - 1) left

        truncatedRight =
            takeFromRight (skipCount right - 1) right
    in
    if skipCount left > skipCount right then
        -- Attach to smaller side
        safeJoin truncatedLeft (safeJoin newLeaf truncatedRight)

    else
        safeJoin (safeJoin truncatedLeft newLeaf) truncatedRight


safeJoinReplacingEndPointsWithNewLeaf : Maybe PeteTree -> Maybe PeteTree -> Maybe PeteTree
safeJoinReplacingEndPointsWithNewLeaf mLeft mRight =
    case ( mLeft, mRight ) of
        ( Just left, Just right ) ->
            joinReplacingEndPointsWithNewLeaf left right

        ( Just left, Nothing ) ->
            takeFromLeft (skipCount left - 1) left

        ( Nothing, Just right ) ->
            takeFromRight (skipCount right - 1) right

        ( Nothing, Nothing ) ->
            Nothing


treeFromSourcePoints : List GPXSource -> Maybe PeteTree
treeFromSourcePoints track =
    -- Build the skeletal tree of nodes, then attach the leaves from the input list.
    -- Should be much quicker than recursively splitting the list, for large lists.
    -- First point is arbitrary reference for conformal projection (TBC).
    let
        referencePoint =
            -- From which, arbitrarily, we compute metre offsets.
            -- We won't be here without a track, so default is harmless.
            List.head track
                |> Maybe.withDefault (GPXSource Direction2d.x Quantity.zero Quantity.zero)
    in
    treeFromSourcesWithExistingReference referencePoint track


insertPointsIntoLeaf : Int -> GPXSource -> List GPXSource -> PeteTree -> PeteTree
insertPointsIntoLeaf leafNumber reference newInternalPoints tree =
    -- This provides a way to "refine" a leaf without recreating the tree.
    -- Could be premature optimisation but feels like the right thing to do.
    let
        helper leafOffset treeNode =
            case treeNode of
                Leaf leaf ->
                    let
                        ( gpxStart, gpxEnd ) =
                            leaf.sourceData
                    in
                    case
                        treeFromSourcesWithExistingReference
                            reference
                            (gpxStart :: newInternalPoints ++ [ gpxEnd ])
                    of
                        Just newTree ->
                            newTree

                        Nothing ->
                            treeNode

                Node node ->
                    -- recurse to find the target
                    if leafOffset < skipCount node.left then
                        joiningNode
                            (helper leafOffset node.left)
                            node.right

                    else
                        joiningNode
                            node.left
                            (helper (leafOffset - skipCount node.left) node.right)
    in
    helper leafNumber tree


treeFromSourcesWithExistingReference : GPXSource -> List GPXSource -> Maybe PeteTree
treeFromSourcesWithExistingReference referencePoint track =
    -- This version for use when editing track and reference is known.
    let
        numberOfSegments =
            List.length track - 1

        treeBuilder : Int -> List GPXSource -> ( Maybe PeteTree, List GPXSource )
        treeBuilder n pointStream =
            case ( n < 2, pointStream ) of
                ( True, v1 :: v2 :: vvvv ) ->
                    -- Take two vectors for this Leaf, but only consume one.
                    ( Just <| Leaf <| makeRoadSection referencePoint v1 v2, v2 :: vvvv )

                ( True, anythingElse ) ->
                    -- Hmm. This shouldn't have happened if we've done our numbers right.
                    ( Nothing, anythingElse )

                ( False, vvvv ) ->
                    -- Make a non-leaf Node, recursively
                    let
                        leftSize =
                            n // 2

                        rightSize =
                            n - leftSize

                        ( left, remainingAfterLeft ) =
                            treeBuilder leftSize vvvv

                        ( right, remainingAfterRight ) =
                            treeBuilder rightSize remainingAfterLeft
                    in
                    case ( left, right ) of
                        -- Should have returned _something_ but we're forced to check
                        ( Just leftSubtree, Just rightSubtree ) ->
                            ( Just <| joiningNode leftSubtree rightSubtree
                            , remainingAfterRight
                            )

                        _ ->
                            ( Nothing, remainingAfterRight )
    in
    treeBuilder numberOfSegments track |> Tuple.first


leafFromIndex : Int -> PeteTree -> PeteTree
leafFromIndex index treeNode =
    case treeNode of
        Leaf info ->
            treeNode

        Node info ->
            if index < skipCount info.left then
                leafFromIndex index info.left

            else
                leafFromIndex (index - skipCount info.left) info.right


earthPointFromIndex : Int -> PeteTree -> EarthPoint
earthPointFromIndex index treeNode =
    case treeNode of
        Leaf info ->
            if index <= 0 then
                info.startPoint

            else
                info.endPoint

        Node info ->
            if index < skipCount info.left then
                earthPointFromIndex index info.left

            else
                earthPointFromIndex (index - skipCount info.left) info.right


gpxPointFromIndex : Int -> PeteTree -> GPXSource
gpxPointFromIndex index treeNode =
    case treeNode of
        Leaf info ->
            if index <= 0 then
                Tuple.first info.sourceData

            else
                Tuple.second info.sourceData

        Node info ->
            if index < skipCount info.left then
                gpxPointFromIndex index info.left

            else
                gpxPointFromIndex (index - skipCount info.left) info.right


distanceFromIndex : Int -> PeteTree -> Length.Length
distanceFromIndex index treeNode =
    case treeNode of
        Leaf info ->
            if index <= 0 then
                Length.meters 0

            else
                info.trueLength

        Node info ->
            if index <= skipCount info.left then
                distanceFromIndex index info.left

            else
                Quantity.plus
                    (trueLength info.left)
                    (distanceFromIndex (index - skipCount info.left) info.right)


indexFromDistanceRoundedDown : Length.Length -> PeteTree -> Int
indexFromDistanceRoundedDown distance treeNode =
    -- Must behave this way for flythrough
    case treeNode of
        Leaf info ->
            0

        Node info ->
            if distance |> Quantity.lessThanOrEqualTo (trueLength info.left) then
                indexFromDistanceRoundedDown distance info.left

            else
                skipCount info.left
                    + indexFromDistanceRoundedDown (distance |> Quantity.minus (trueLength info.left)) info.right


indexFromDistanceRoundedUp : Length.Length -> PeteTree -> Int
indexFromDistanceRoundedUp distance treeNode =
    -- Must behave this way for Smart Smoother
    case treeNode of
        Leaf info ->
            1

        Node info ->
            if distance |> Quantity.lessThanOrEqualTo (trueLength info.left) then
                indexFromDistanceRoundedUp distance info.left

            else
                skipCount info.left
                    + indexFromDistanceRoundedUp (distance |> Quantity.minus (trueLength info.left)) info.right


indexFromDistance : Length.Length -> PeteTree -> Int
indexFromDistance distance treeNode =
    case treeNode of
        Leaf info ->
            if distance |> Quantity.lessThanOrEqualTo (Quantity.half info.trueLength) then
                0

            else
                1

        Node info ->
            if distance |> Quantity.lessThanOrEqualTo (trueLength info.left) then
                indexFromDistance distance info.left

            else
                skipCount info.left
                    + indexFromDistance (distance |> Quantity.minus (trueLength info.left)) info.right


interpolateTrack : Length.Length -> PeteTree -> ( Int, EarthPoint )
interpolateTrack distance treeNode =
    case treeNode of
        Leaf info ->
            ( 0
            , Point3d.interpolateFrom
                info.startPoint
                info.endPoint
                (Quantity.ratio distance info.trueLength)
            )

        Node info ->
            if distance |> Quantity.lessThanOrEqualTo (trueLength info.left) then
                interpolateTrack distance info.left

            else
                let
                    ( rightIndex, rightPoint ) =
                        interpolateTrack (distance |> Quantity.minus (trueLength info.left)) info.right
                in
                ( rightIndex + skipCount info.left
                , rightPoint
                )


nearestToRay :
    Axis3d Meters LocalCoords
    -> PeteTree
    -> LeafIndex
    -> Int
    -> Int
nearestToRay ray tree leafIndex current =
    -- Find track point nearest to ray, but where there's a tie, use closest (numerically) to current point.
    let
        valuationFunction : LeafIndexEntry -> Quantity Float Meters
        valuationFunction leafEntry =
            -- For simplicity, look only at end point, so point and leaf numbers match.
            -- Means caller must deal with the final point.
            let
                leafToTest =
                    asRecord <| leafFromIndex leafEntry.leafIndex tree
            in
            Quantity.min
                (Point3d.distanceFromAxis ray leafToTest.startPoint)
                (Point3d.distanceFromAxis ray leafToTest.endPoint)

        nearestLeafs =
            SpatialIndex.queryNearestToAxisUsing
                leafIndex
                ray
                valuationFunction
                { currentBestMetric = Quantity.positiveInfinity
                , currentBestContent = []
                }

        nearestPoints =
            List.Extra.unique <|
                List.map nearestPointForLeaf nearestLeafs.currentBestContent

        nearestPointForLeaf leafEntry =
            let
                indexOfLeaf =
                    leafEntry.content.leafIndex

                leafToTest =
                    asRecord <| leafFromIndex indexOfLeaf tree
            in
            if
                Point3d.distanceFromAxis ray leafToTest.startPoint
                    |> Quantity.lessThanOrEqualTo
                        (Point3d.distanceFromAxis ray leafToTest.endPoint)
            then
                indexOfLeaf

            else
                indexOfLeaf + 1

        _ =
            Debug.log "POINTS" nearestPoints
    in
    nearestPoints
        |> List.Extra.minimumBy (\pointIndex -> abs (pointIndex - current))
        |> Maybe.withDefault current


gpxDistance : GPXSource -> GPXSource -> Length.Length
gpxDistance p1 p2 =
    let
        lon p =
            p.longitude |> Direction2d.toAngle
    in
    Length.meters <|
        range
            ( Direction2d.toAngle p1.longitude, p1.latitude )
            ( Direction2d.toAngle p2.longitude, p2.latitude )


nearestToLonLat :
    GPXSource
    -> Int
    -> PeteTree
    -> Int
nearestToLonLat click current treeNode =
    -- Easier to conceive if we frame this as "can you beat the given?".
    let
        currentGpx =
            gpxPointFromIndex current treeNode

        canDoBetter : Int -> ( Int, Length ) -> PeteTree -> Maybe ( Int, Length )
        canDoBetter thisIndex ( bestIndex, bestDistance ) thisNode =
            case thisNode of
                Leaf leaf ->
                    let
                        ( startDistance, endDistance ) =
                            -- Will not touch all leaves, doubling up is OK
                            ( gpxDistance (Tuple.first leaf.sourceData) click
                            , gpxDistance (Tuple.second leaf.sourceData) click
                            )

                        bestThisLeaf =
                            if startDistance |> Quantity.lessThan endDistance then
                                ( thisIndex, startDistance )

                            else
                                ( thisIndex + 1, endDistance )
                    in
                    if bestThisLeaf |> Tuple.second |> Quantity.lessThan bestDistance then
                        Just bestThisLeaf

                    else
                        Nothing

                Node node ->
                    -- Only recurse if our bounding box suggests we can do better.
                    if
                        bestAvailableDistanceGuess click thisNode
                            |> Quantity.lessThan bestDistance
                    then
                        case
                            canDoBetter
                                thisIndex
                                ( bestIndex, bestDistance )
                                node.left
                        of
                            Just ( leftIndex, leftDistance ) ->
                                -- Left is new champion, for how long>
                                case
                                    canDoBetter
                                        (thisIndex + skipCount node.left)
                                        ( leftIndex, leftDistance )
                                        node.right
                                of
                                    Just ( rightIndex, rightDistance ) ->
                                        -- Right did better
                                        Just ( rightIndex, rightDistance )

                                    Nothing ->
                                        -- Left is still better
                                        Just ( leftIndex, leftDistance )

                            Nothing ->
                                -- Left came up empty, try the right
                                case
                                    canDoBetter
                                        (thisIndex + skipCount node.left)
                                        ( bestIndex, bestDistance )
                                        node.right
                                of
                                    Just ( rightIndex, rightDistance ) ->
                                        -- Right did better
                                        Just ( rightIndex, rightDistance )

                                    Nothing ->
                                        Nothing

                    else
                        Nothing
    in
    case canDoBetter 0 ( current, gpxDistance currentGpx click ) treeNode of
        Just ( bestIndex, bestRange ) ->
            bestIndex

        Nothing ->
            current


containingSphere : BoundingBox3d Meters LocalCoords -> Sphere3d Meters LocalCoords
containingSphere box =
    let
        here =
            BoundingBox3d.centerPoint box

        ( xs, ys, zs ) =
            BoundingBox3d.dimensions box

        radius =
            Quantity.half <|
                Quantity.sqrt <|
                    Quantity.sum
                        [ Quantity.squared xs
                        , Quantity.squared ys
                        , Quantity.squared zs
                        ]
    in
    Sphere3d.withRadius radius here


lngLatPair : ( Angle, Angle, Length.Length ) -> E.Value
lngLatPair ( longitude, latitude, _ ) =
    E.list E.float [ Angle.inDegrees longitude, Angle.inDegrees latitude ]


getDualCoords : PeteTree -> Int -> ( EarthPoint, GPXSource )
getDualCoords tree index =
    --Rather glaring inefficiency here but 2x is still N log N.
    ( earthPointFromIndex index tree
    , gpxPointFromIndex index tree
    )


takeFromLeft : Int -> PeteTree -> Maybe PeteTree
takeFromLeft leavesFromLeft treeNode =
    if leavesFromLeft <= 0 then
        Nothing

    else if leavesFromLeft >= skipCount treeNode then
        Just treeNode

    else
        case treeNode of
            Leaf roadSection ->
                -- Awkward, can't split a leaf
                Nothing

            Node record ->
                safeJoin
                    (takeFromLeft leavesFromLeft record.left)
                    (takeFromLeft (leavesFromLeft - skipCount record.left) record.right)


takeFromRight : Int -> PeteTree -> Maybe PeteTree
takeFromRight leavesFromRight treeNode =
    if leavesFromRight <= 0 then
        Nothing

    else if leavesFromRight >= skipCount treeNode then
        Just treeNode

    else
        case treeNode of
            Leaf roadSection ->
                -- Awkward, can't split a leaf
                Nothing

            Node record ->
                safeJoin
                    (takeFromRight (leavesFromRight - skipCount record.right) record.left)
                    (takeFromRight leavesFromRight record.right)


splitTreeAt : Int -> PeteTree -> ( Maybe PeteTree, Maybe PeteTree )
splitTreeAt leavesToTheLeft thisNode =
    -- This may be less efficient than the single pass version, but look at it.
    -- Note that the given point is included in both sides. I.e. this splits by road segment.
    ( takeFromLeft leavesToTheLeft thisNode
    , takeFromRight (skipCount thisNode - leavesToTheLeft) thisNode
    )


safeSplitTreeAt : Int -> Maybe PeteTree -> ( Maybe PeteTree, Maybe PeteTree )
safeSplitTreeAt index mTree =
    case mTree of
        Just isTree ->
            splitTreeAt index isTree

        Nothing ->
            ( Nothing, Nothing )


getFirstLeaf : PeteTree -> RoadSection
getFirstLeaf someNode =
    case someNode of
        Leaf leaf ->
            leaf

        Node node ->
            getFirstLeaf node.left


getLastLeaf : PeteTree -> RoadSection
getLastLeaf someNode =
    case someNode of
        Leaf leaf ->
            leaf

        Node node ->
            getLastLeaf node.right


secondPoint : PeteTree -> ( GPXSource, EarthPoint )
secondPoint tree =
    let
        leaf =
            getFirstLeaf tree
    in
    ( Tuple.second leaf.sourceData, leaf.endPoint )


penultimatePoint : PeteTree -> ( GPXSource, EarthPoint )
penultimatePoint tree =
    let
        leaf =
            getLastLeaf tree
    in
    ( Tuple.first leaf.sourceData, leaf.startPoint )


extractPointsInRange : Int -> Int -> PeteTree -> List ( EarthPoint, GPXSource )
extractPointsInRange fromStart fromEnd trackTree =
    -- Going for an efficient but more likely correct approach.
    -- "Make it right, then make it fast."
    let
        myFoldFn : RoadSection -> List ( EarthPoint, GPXSource ) -> List ( EarthPoint, GPXSource )
        myFoldFn road accum =
            ( road.endPoint, Tuple.second road.sourceData )
                :: ( road.startPoint, Tuple.first road.sourceData )
                :: List.drop 1 accum
    in
    if fromStart + fromEnd == skipCount trackTree then
        [ getDualCoords trackTree fromStart ]

    else
        List.reverse <|
            traverseTreeBetween
                fromStart
                (skipCount trackTree - fromEnd)
                trackTree
                myFoldFn
                []


safeEnumerateEndPoints : Maybe PeteTree -> List ( EarthPoint, GPXSource )
safeEnumerateEndPoints mTree =
    case mTree of
        Just tree ->
            enumerateEndPoints tree []

        Nothing ->
            []


recreateGpxSources : Maybe PeteTree -> List GPXSource
recreateGpxSources mTree =
    case mTree of
        Just fromTree ->
            (getFirstLeaf fromTree |> .sourceData |> Tuple.first)
                :: (enumerateEndPoints fromTree [] |> List.map Tuple.second)

        Nothing ->
            []


enumerateEndPoints : PeteTree -> List ( EarthPoint, GPXSource ) -> List ( EarthPoint, GPXSource )
enumerateEndPoints treeNode accum =
    -- The name describes the output, not the method!
    -- Note it gives end points, you need to add the start point somewhere!
    case treeNode of
        Leaf leaf ->
            ( leaf.endPoint, Tuple.second leaf.sourceData ) :: accum

        Node node ->
            accum
                |> enumerateEndPoints node.right
                |> enumerateEndPoints node.left


foldOverRoute : (RoadSection -> a -> a) -> PeteTree -> a -> a
foldOverRoute foldFn treeNode startValues =
    traverseTreeBetween
        0
        (skipCount treeNode)
        treeNode
        foldFn
        startValues


foldOverRouteRL : (RoadSection -> a -> a) -> PeteTree -> a -> a
foldOverRouteRL foldFn treeNode accum =
    -- A right to left walk allow the fold fn to cons and not need reversing.
    case treeNode of
        Leaf leaf ->
            foldFn leaf accum

        Node node ->
            accum
                |> foldOverRouteRL foldFn node.right
                |> foldOverRouteRL foldFn node.left


foldOverRouteRLwithDepthLimit : Int -> (RoadSection -> a -> a) -> PeteTree -> a -> a
foldOverRouteRLwithDepthLimit depth foldFn treeNode accum =
    -- A right to left walk allow the fold fn to cons and not need reversing.
    case treeNode of
        Leaf leaf ->
            foldFn leaf accum

        Node node ->
            if depth > 0 then
                accum
                    |> foldOverRouteRLwithDepthLimit (depth - 1) foldFn node.right
                    |> foldOverRouteRLwithDepthLimit (depth - 1) foldFn node.left

            else
                foldFn node.nodeContent accum


getAllGPXPointsInNaturalOrder : PeteTree -> List GPXSource
getAllGPXPointsInNaturalOrder treeNode =
    -- A right-to-left traversal that is POINT focused.
    -- Really handy for output or for tree rebuilding.
    let
        internalFoldFn : RoadSection -> List GPXSource -> List GPXSource
        internalFoldFn road accum =
            Tuple.second road.sourceData :: accum

        endPoints =
            foldOverRouteRL internalFoldFn treeNode []
    in
    gpxPointFromIndex 0 treeNode :: endPoints


getAllEarthPointsInNaturalOrder : PeteTree -> List EarthPoint
getAllEarthPointsInNaturalOrder treeNode =
    -- A right-to-left traversal that is POINT focused.
    -- Really handy for output or for tree rebuilding.
    let
        internalFoldFn : RoadSection -> List EarthPoint -> List EarthPoint
        internalFoldFn road accum =
            road.endPoint :: accum

        endPoints =
            foldOverRouteRL internalFoldFn treeNode []
    in
    earthPointFromIndex 0 treeNode :: endPoints


getAllGPXPointsInDict : PeteTree -> Dict Int GPXSource
getAllGPXPointsInDict treeNode =
    -- Same but allows quick index lookup.
    let
        internalFoldFn :
            RoadSection
            -> ( Int, Dict Int GPXSource )
            -> ( Int, Dict Int GPXSource )
        internalFoldFn road ( index, dict ) =
            ( index + 1
            , Dict.insert index (Tuple.second road.sourceData) dict
            )

        ( _, outputs ) =
            foldOverRoute
                internalFoldFn
                treeNode
                ( 1, Dict.insert 0 (gpxPointFromIndex 0 treeNode) Dict.empty )
    in
    outputs


treeToRoadSectionList : PeteTree -> List RoadSection
treeToRoadSectionList someNode =
    -- By way of example use of all-purpose traversal function,
    -- this will do the whole tree with no depth limit.
    foldOverRoute (::) someNode []


traverseTreeBetween :
    Int
    -> Int
    -> PeteTree
    -> (RoadSection -> a -> a)
    -> a
    -> a
traverseTreeBetween startingAt endingAt someNode foldFn accum =
    traverseTreeBetweenLimitsToDepth startingAt endingAt (always Nothing) 0 someNode foldFn accum


traverseTreeBetweenLimitsToDepth :
    Int
    -> Int
    -> (RoadSection -> Maybe Int)
    -> Int
    -> PeteTree
    -> (RoadSection -> a -> a)
    -> a
    -> a
traverseTreeBetweenLimitsToDepth startingAt endingAt depthFunction currentDepth thisNode foldFn accum =
    -- NOTE this does a left-right traversal and conses the road sections,
    -- so the road comes out "backwards" in terms of road segments.
    {-
       Do the FF thing.
       If startingAt >= my skipcount, return accum
       If startingAt > 0 but < skipCount then pass to children
           left child with same start offset
           right child with usually deduction of left skip count.
           Don't call right child is beyond the `endingAt`.
    -}
    if startingAt >= skipCount thisNode then
        -- We have nothing to contribute
        accum

    else if endingAt <= 0 then
        -- Already passed the end, nothing should be added.
        accum

    else
        case thisNode of
            Leaf leafNode ->
                -- Leaves on the line.
                foldFn leafNode accum

            Node node ->
                let
                    maximumDepth =
                        depthFunction node.nodeContent
                            |> Maybe.withDefault 999
                in
                if currentDepth >= maximumDepth then
                    -- Can go no deeper, provide our info
                    foldFn node.nodeContent accum

                else
                    -- Give the children a try
                    accum
                        |> traverseTreeBetweenLimitsToDepth
                            startingAt
                            endingAt
                            depthFunction
                            (currentDepth + 1)
                            node.left
                            foldFn
                        |> traverseTreeBetweenLimitsToDepth
                            (startingAt - skipCount node.left)
                            (endingAt - skipCount node.left)
                            depthFunction
                            (currentDepth + 1)
                            node.right
                            foldFn


trackPointsForOutput : PeteTree -> List TrackPoint
trackPointsForOutput tree =
    --NOTE: This ignores the final point, caller must add.
    let
        foldFn : RoadSection -> List TrackPoint -> List TrackPoint
        foldFn node accum =
            let
                distance =
                    case accum of
                        prevPoint :: _ ->
                            prevPoint.distanceFromStart |> Quantity.plus prevPoint.trueLength

                        [] ->
                            Quantity.zero
            in
            { distanceFromStart = distance
            , longitude = Direction2d.toAngle <| .longitude <| Tuple.first <| node.sourceData
            , latitude = .latitude <| Tuple.first <| node.sourceData
            , altitude = .altitude <| Tuple.first <| node.sourceData
            , gradient = node.gradientAtStart
            , trueLength = node.trueLength
            }
                :: accum
    in
    foldOverRoute foldFn tree []


gradientFromNode treeNode =
    Quantity.ratio
        (Point3d.zCoordinate (endPoint treeNode)
            |> Quantity.minus
                (Point3d.zCoordinate (startPoint treeNode))
        )
        (trueLength treeNode)
        |> (*) 100.0


queryRoadsUsingFilter :
    (Int -> Int -> RoadSection -> Bool)
    -> PeteTree
    -> (Int -> RoadSection -> a -> a)
    -> a
    -> a
queryRoadsUsingFilter filterFn treeNode foldFn accum =
    let
        helper : Int -> Int -> PeteTree -> a -> a
        helper starting ending someNode myAccumulator =
            if filterFn starting ending (asRecord treeNode) then
                -- Seems to be of interest to caller
                case someNode of
                    Leaf leaf ->
                        foldFn starting leaf myAccumulator

                    Node node ->
                        myAccumulator
                            |> helper starting (ending - skipCount node.right) node.left
                            |> helper (starting + skipCount node.left) ending node.right

            else
                myAccumulator
    in
    helper 0 (skipCount treeNode) treeNode accum


updatePointByIndexInSitu : Int -> GPXSource -> GPXSource -> PeteTree -> PeteTree
updatePointByIndexInSitu index newGPX referencePoint tree =
    -- Note that all points except end points appear in two places, and they can be distant in the tree.
    -- Still, idea is to change only ancestor nodes, obviously including the topmost.
    if index < 0 || index > skipCount tree then
        tree

    else
        case tree of
            Leaf leaf ->
                if index == 0 then
                    Leaf <| makeRoadSection referencePoint newGPX (Tuple.second leaf.sourceData)

                else
                    -- index == 1
                    Leaf <| makeRoadSection referencePoint (Tuple.first leaf.sourceData) newGPX

            Node node ->
                joiningNode
                    (updatePointByIndexInSitu index newGPX referencePoint node.left)
                    (updatePointByIndexInSitu (index - skipCount node.left) newGPX referencePoint node.right)
