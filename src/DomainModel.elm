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
    , estimateTimeAtDistance
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
    , interpolateGpxByTime
    , interpolateTrack
    , leafFromIndex
    , lngLatPair
    , makeLeaf
    , makeRoadSection
    , nearestToEarthPoint
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
    , withTime
    , withoutTime
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Direction3d
import Json.Encode as E
import LeafIndex exposing (LeafIndex, LeafIndexEntry)
import Length exposing (Length, Meters, inMeters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import SpatialIndex
import Spherical as Spherical exposing (range)
import Time
import Utils


type alias GPXSource =
    -- Being a raw line of data from GPX file.
    { longitude : Direction2d LocalCoords
    , latitude : Angle
    , altitude : Quantity Float Meters
    , timestamp : Maybe Time.Posix
    }


type alias EarthPoint =
    --Ooh, 4D.
    { space : Point3d Meters LocalCoords
    , time : Maybe Time.Posix
    }


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
    , trueLength : Quantity Float Meters
    , skipCount : Int
    , transitTime : Maybe Time.Posix

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


withoutTime : Point3d Meters LocalCoords -> EarthPoint
withoutTime pt =
    { space = pt, time = Nothing }


withTime : Maybe Time.Posix -> Point3d Meters LocalCoords -> EarthPoint
withTime time pt =
    { space = pt, time = time }


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


pointFromGpxWithReference : GPXSource -> GPXSource -> EarthPoint
pointFromGpxWithReference reference gpx =
    { space =
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
    , time = gpx.timestamp
    }


gpxFromPointWithReference : GPXSource -> EarthPoint -> GPXSource
gpxFromPointWithReference reference point =
    let
        ( x, y, z ) =
            Point3d.toTuple Length.inMeters point.space

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
        point.time


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
            BoundingBox3d.from local1.space local2.space

        range : Length.Length
        range =
            Length.meters <|
                Spherical.range
                    ( Direction2d.toAngle earth1.longitude, earth1.latitude )
                    ( Direction2d.toAngle earth2.longitude, earth2.latitude )

        altitudeChange =
            Point3d.zCoordinate local2.space |> Quantity.minus (Point3d.zCoordinate local1.space)

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
    , trueLength = range
    , skipCount = 1
    , transitTime =
        case ( earth1.timestamp, earth2.timestamp ) of
            ( Just startTime, Just endTime ) ->
                Just <| Time.millisToPosix <| Time.posixToMillis endTime - Time.posixToMillis startTime

            _ ->
                Nothing
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

        ( asRecord1, asRecord2 ) =
            ( asRecord info1, asRecord info2 )
    in
    { sourceData = ( Tuple.first asRecord1.sourceData, Tuple.second asRecord2.sourceData )
    , startPoint = asRecord1.startPoint
    , endPoint = asRecord2.endPoint
    , boundingBox = box
    , trueLength = Quantity.plus asRecord1.trueLength asRecord2.trueLength
    , skipCount = asRecord1.skipCount + asRecord2.skipCount
    , transitTime =
        case ( asRecord1.transitTime, asRecord2.transitTime ) of
            ( Just time1, Just time2 ) ->
                Just <| Time.millisToPosix <| Time.posixToMillis time1 + Time.posixToMillis time2

            _ ->
                Nothing
    , altitudeGained = Quantity.plus asRecord1.altitudeGained asRecord2.altitudeGained
    , altitudeLost = Quantity.plus asRecord1.altitudeLost asRecord2.altitudeLost
    , distanceClimbing = Quantity.plus asRecord1.distanceClimbing asRecord2.distanceClimbing
    , distanceDescending = Quantity.plus asRecord1.distanceDescending asRecord2.distanceDescending
    , steepestClimb = max asRecord1.steepestClimb asRecord2.steepestClimb
    , gradientAtStart = asRecord1.gradientAtStart
    , gradientAtEnd = asRecord2.gradientAtEnd
    , gradientChangeMaximumAbs =
        Maybe.withDefault 0.0 <|
            List.maximum
                [ asRecord1.gradientChangeMaximumAbs
                , asRecord2.gradientChangeMaximumAbs
                , abs (asRecord1.gradientAtEnd - asRecord2.gradientAtStart)
                ]
    , directionAtStart = asRecord1.directionAtStart
    , directionAtEnd = asRecord2.directionAtEnd
    , directionChangeMaximumAbs =
        Maybe.withDefault Quantity.zero <|
            Quantity.maximum
                [ asRecord1.directionChangeMaximumAbs
                , asRecord2.directionChangeMaximumAbs
                , Quantity.abs <|
                    Direction2d.angleFrom
                        asRecord1.directionAtEnd
                        asRecord2.directionAtStart
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
                |> Maybe.withDefault (GPXSource Direction2d.x Quantity.zero Quantity.zero Nothing)
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


estimateTimeAtDistance : Length.Length -> PeteTree -> Maybe Time.Posix
estimateTimeAtDistance distance tree =
    let
        index =
            indexFromDistance distance tree

        leaf =
            asRecord <| leafFromIndex index tree

        leafStartDistance =
            distanceFromIndex index tree

        proportionOfDistance =
            Quantity.ratio
                (distance |> Quantity.minus leafStartDistance)
                leaf.trueLength
    in
    Utils.interpolateTimes
        proportionOfDistance
        leaf.startPoint.time
        leaf.endPoint.time


indexFromTime : Maybe Time.Posix -> PeteTree -> Int
indexFromTime relativeTime treeNode =
    case treeNode of
        Leaf info ->
            if relativeTime |> Utils.timeLessThanOrEqualTo info.transitTime then
                0

            else
                1

        Node info ->
            let
                leftAsRecord =
                    asRecord info.left
            in
            if relativeTime |> Utils.timeLessThanOrEqualTo leftAsRecord.transitTime then
                indexFromTime relativeTime info.left

            else
                skipCount info.left
                    + indexFromTime
                        (relativeTime |> Utils.subtractTimes leftAsRecord.transitTime)
                        info.right


interpolateGpxByTime : PeteTree -> Time.Posix -> GPXSource
interpolateGpxByTime tree relativeTime =
    --Find leaf using transit times, interpolate the leaf. If no times, should not be used.
    let
        bestLeafIndex =
            indexFromTime (Just relativeTime) tree

        bestLeaf =
            asRecord <| leafFromIndex bestLeafIndex tree

        timeIntoLeaf =
            Utils.subtractTimes bestLeaf.startPoint.time (Just relativeTime)

        proportion =
            case ( bestLeaf.transitTime, timeIntoLeaf ) of
                ( Just transitTime, Just leafTime ) ->
                    (toFloat <| Time.posixToMillis leafTime) / (toFloat <| Time.posixToMillis transitTime)

                _ ->
                    0

        ( gpxStart, gpxEnd ) =
            bestLeaf.sourceData
    in
    --TODO: Should be OK to do this.
    { longitude = Utils.interpolateLongitude proportion gpxStart.longitude gpxEnd.longitude
    , latitude = Utils.interpolateLatitude proportion gpxStart.latitude gpxEnd.latitude
    , altitude = Utils.interpolateAltitude proportion gpxStart.altitude gpxEnd.altitude
    , timestamp = Utils.interpolateTimes proportion gpxStart.timestamp gpxEnd.timestamp
    }


interpolateTrack : Length.Length -> PeteTree -> ( Int, EarthPoint )
interpolateTrack distance treeNode =
    case treeNode of
        Leaf info ->
            ( 0
            , { space =
                    Point3d.interpolateFrom
                        info.startPoint.space
                        info.endPoint.space
                        (Quantity.ratio distance info.trueLength)
              , time =
                    Utils.interpolateTimes
                        (Quantity.ratio distance info.trueLength)
                        info.startPoint.time
                        info.endPoint.time
              }
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
                (Point3d.distanceFromAxis ray leafToTest.startPoint.space)
                (Point3d.distanceFromAxis ray leafToTest.endPoint.space)

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
                Point3d.distanceFromAxis ray leafToTest.startPoint.space
                    |> Quantity.lessThanOrEqualTo
                        (Point3d.distanceFromAxis ray leafToTest.endPoint.space)
            then
                indexOfLeaf

            else
                indexOfLeaf + 1
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
    -> GPXSource
    -> LeafIndex
    -> Int
nearestToLonLat click current treeNode referenceLonLat leafIndex =
    let
        asEarthPoint =
            pointFromGpxWithReference referenceLonLat click
    in
    nearestToEarthPoint asEarthPoint current treeNode leafIndex


nearestToEarthPoint :
    EarthPoint
    -> Int
    -> PeteTree
    -> LeafIndex
    -> Int
nearestToEarthPoint earthPoint current treeNode leafIndex =
    let
        ray =
            Axis3d.withDirection Direction3d.negativeZ earthPoint.space
    in
    nearestToRay ray treeNode leafIndex current


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
        (Point3d.zCoordinate (.space <| endPoint treeNode)
            |> Quantity.minus
                (Point3d.zCoordinate (.space <| startPoint treeNode))
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
