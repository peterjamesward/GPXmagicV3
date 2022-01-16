module DomainModel exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction2d exposing (Direction2d)
import Json.Encode as E
import Length exposing (Length, Meters, inMeters)
import LocalCoords exposing (LocalCoords)
import Maybe.Extra as Maybe
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Sphere3d exposing (Sphere3d)
import Spherical as Spherical exposing (range)


type alias GPXSource =
    -- Being a raw line of data from GPX file.
    { longitude : Direction2d LocalCoords
    , latitude : Angle
    , altitude : Length.Length
    }


type alias EarthPoint =
    Point3d Length.Meters LocalCoords


type alias RoadSection =
    -- Can be based between two 'fundamental' points from GPX, or an assembly of them.
    -- Bounding box and Sphere needed for culling in nearness tests.
    -- Keeping track of longitude tricky because of IDL.
    { sourceData : ( GPXSource, GPXSource )
    , startPoint : EarthPoint
    , endPoint : EarthPoint

    -- For rapid location of points using non-map views...
    , boundingBox : BoundingBox3d Meters LocalCoords
    , sphere : Sphere3d Meters LocalCoords
    , trueLength : Length.Length
    , skipCount : Int

    -- For efficient detection of map clicks...
    , medianLongitude : Direction2d LocalCoords
    , eastwardExtent : Angle
    , westwardExtent : Angle

    -- Basic route statistics...
    , altitudeGained : Length.Length
    , altitudeLost : Length.Length
    , distanceClimbing : Length.Length
    , distanceDescending : Length.Length
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
    -- Absurdly simple tree may work (does, rather spiffingly).
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


trueLength : PeteTree -> Length
trueLength treeNode =
    treeNode |> asRecord |> .trueLength


skipCount : PeteTree -> Int
skipCount treeNode =
    case treeNode of
        Leaf leaf ->
            1

        Node node ->
            node.nodeContent.skipCount


boundingBox : PeteTree -> BoundingBox3d Length.Meters LocalCoords
boundingBox treeNode =
    treeNode |> asRecord |> .boundingBox


sphere : PeteTree -> Sphere3d Length.Meters LocalCoords
sphere treeNode =
    treeNode |> asRecord |> .sphere


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
        (gpx.altitude |> Quantity.minus reference.altitude)


gpxFromPointWithReference : GPXSource -> EarthPoint -> GPXSource
gpxFromPointWithReference reference point =
    let
        ( x, y, z ) =
            Point3d.toTuple inMeters point

        longitude =
            x
                / cos latitude
                / Spherical.metresPerDegree
                + (Angle.inDegrees <| Direction2d.toAngle reference.longitude)

        latitude =
            y / Spherical.metresPerDegree + Angle.inDegrees reference.latitude

        altitude =
            z
    in
    GPXSource
        (Direction2d.fromAngle <| Angle.degrees longitude)
        (Angle.degrees latitude)
        (Length.meters altitude)


makeRoadSection : GPXSource -> GPXSource -> GPXSource -> RoadSection
makeRoadSection reference earth1 earth2 =
    let
        local1 =
            pointFromGpxWithReference reference earth1

        local2 =
            pointFromGpxWithReference reference earth2

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
            if (range |> Quantity.greaterThanZero) && (altitudeChange |> Quantity.greaterThanZero) then
                100.0 * Quantity.ratio altitudeChange range

            else
                0.0

        bearing =
            Angle.radians <|
                Spherical.findBearingToTarget
                    ( Angle.inRadians earth1.latitude, Angle.inRadians <| Direction2d.toAngle earth1.longitude )
                    ( Angle.inRadians earth2.latitude, Angle.inRadians <| Direction2d.toAngle earth2.longitude )
    in
    { sourceData = ( earth1, earth2 )
    , startPoint = local1
    , endPoint = local2
    , boundingBox = box
    , sphere = containingSphere box
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
    , directionAtStart = Direction2d.fromAngle bearing
    , directionAtEnd = Direction2d.fromAngle bearing
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
    , sphere = containingSphere box
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


joiningNode : PeteTree -> PeteTree -> PeteTree
joiningNode left right =
    Node
        { nodeContent = combineInfo left right
        , left = left
        , right = right
        }


treeFromList : List GPXSource -> Maybe PeteTree
treeFromList track =
    -- Build the skeletal tree of nodes, then attach the leaves from the input list.
    -- Should be much quicker than recursively splitting the list, for large lists.
    -- First point is arbitrary reference for conformal projection (TBC).
    let
        referencePoint =
            -- From which, arbitrarily, we compute metre offsets.
            -- We won't be here without a track, so default is harmless.
            List.head track
                |> Maybe.withDefault (GPXSource Direction2d.x Quantity.zero Quantity.zero)

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


nearestToRay :
    Axis3d Meters LocalCoords
    -> PeteTree
    -> Int
nearestToRay ray treeNode =
    -- Build a new query here.
    -- Try: compute distance to each box centres.
    -- At each level, pick "closest" child and recurse.
    -- Not good enough. Need deeper search, say for all intersected boxes.
    -- Bit of recursive magic to get the "index" number.
    let
        helper withNode skip =
            case withNode of
                Leaf leaf ->
                    let
                        startDistance =
                            leaf.startPoint |> Point3d.distanceFromAxis ray

                        endDistance =
                            leaf.endPoint |> Point3d.distanceFromAxis ray
                    in
                    if startDistance |> Quantity.lessThanOrEqualTo endDistance then
                        ( skip, startDistance )

                    else
                        ( skip + 1, endDistance )

                Node node ->
                    let
                        ( leftIntersects, rightIntersects ) =
                            ( Axis3d.intersectionWithSphere (sphere node.left) ray /= Nothing
                            , Axis3d.intersectionWithSphere (sphere node.right) ray /= Nothing
                            )

                        leftDistance =
                            -- How close are we to the neighbourhood?
                            sphere node.left
                                |> Sphere3d.centerPoint
                                |> Point3d.distanceFromAxis ray
                                |> Quantity.minus (sphere node.left |> Sphere3d.radius)

                        rightDistance =
                            sphere node.right
                                |> Sphere3d.centerPoint
                                |> Point3d.distanceFromAxis ray
                                |> Quantity.minus (sphere node.right |> Sphere3d.radius)
                    in
                    case ( leftIntersects, rightIntersects ) of
                        ( True, True ) ->
                            -- Could go either way
                            let
                                ( leftBestIndex, leftBestDistance ) =
                                    helper node.left skip

                                ( rightBestIndex, rightBestDistance ) =
                                    helper node.right (skip + skipCount node.left)
                            in
                            if leftBestDistance |> Quantity.lessThanOrEqualTo rightBestDistance then
                                ( leftBestIndex, leftBestDistance )

                            else
                                ( rightBestIndex, rightBestDistance )

                        ( True, False ) ->
                            helper node.left skip

                        ( False, True ) ->
                            helper node.right (skip + skipCount node.left)

                        ( False, False ) ->
                            if leftDistance |> Quantity.lessThanOrEqualTo rightDistance then
                                helper node.left skip

                            else
                                helper node.right (skip + skipCount node.left)
    in
    Tuple.first <| helper treeNode 0


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
    -> PeteTree
    -> Int
nearestToLonLat click treeNode =
    -- Only for click detect on Map view.
    let
        helper withNode skip =
            case withNode of
                Leaf leaf ->
                    -- Use whichever point is closest.
                    let
                        startDistance =
                            gpxDistance click <| Tuple.first leaf.sourceData

                        endDistance =
                            gpxDistance click <| Tuple.second leaf.sourceData
                    in
                    if startDistance |> Quantity.lessThanOrEqualTo endDistance then
                        ( skip, startDistance )

                    else
                        ( skip + 1, endDistance )

                Node node ->
                    -- The trick here is effective culling, but better to search
                    -- unnecessarily than to miss the right point.
                    let
                        ( inLeftSpan, inRightSpan ) =
                            ( isLongitudeContained click.longitude node.left
                            , isLongitudeContained click.longitude node.right
                            )

                        --_ =
                        --    Debug.log "SPANS" ( inLeftSpan, inRightSpan )
                    in
                    case ( inLeftSpan, inRightSpan ) of
                        ( True, True ) ->
                            -- Could go either way, best check both.
                            let
                                ( leftBestIndex, leftBestDistance ) =
                                    helper node.left skip

                                ( rightBestIndex, rightBestDistance ) =
                                    helper node.right (skip + skipCount node.left)
                            in
                            if leftBestDistance |> Quantity.lessThanOrEqualTo rightBestDistance then
                                ( leftBestIndex, leftBestDistance )

                            else
                                ( rightBestIndex, rightBestDistance )

                        ( True, False ) ->
                            helper node.left skip

                        ( False, True ) ->
                            helper node.right (skip + skipCount node.left)

                        ( False, False ) ->
                            let
                                ( leftDistance, rightDistance ) =
                                    ( rotationAwayFrom click.longitude node.left
                                    , rotationAwayFrom click.longitude node.right
                                    )
                            in
                            if leftDistance |> Quantity.lessThanOrEqualTo rightDistance then
                                helper node.left skip

                            else
                                helper node.right (skip + skipCount node.left)
    in
    Tuple.first <| helper treeNode 0


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


lngLatPair : ( Angle, Angle ) -> E.Value
lngLatPair ( longitude, latitude ) =
    E.list E.float [ Angle.inDegrees longitude, Angle.inDegrees latitude ]


buildPreview : List Int -> PeteTree -> List ( EarthPoint, GPXSource )
buildPreview indices tree =
    let
        getDualCoords index =
            --TODO: Rather glaring inefficiency here.
            ( earthPointFromIndex index tree
            , gpxPointFromIndex index tree
            )
    in
    List.map getDualCoords indices


deleteSinglePoint : Int -> GPXSource -> PeteTree -> PeteTree
deleteSinglePoint index refLonLat treeNode =
    -- Logically, where index of 0 means the first leaf is discarded.
    -- We don't actually ever split a leaf.
    -- Default here is to return the original.
    -- I see my error. We should:
    -- THIS NEEDS to consider case of only having two leaves, so we can't trim them.
    -- Generally, there will need to be a new road section between the penultimate _point_
    -- of the left and the _second_ point of the right.
    -- We can find these prior to any trimming.
    -- Then we can trim the left and right, and stitch together the new section.
    case treeNode of
        Node node ->
            if index <= 0 then
                -- First point deletion special case
                treeNode
                    |> splitTreeAt 1 refLonLat
                    |> Tuple.second
                    |> Maybe.withDefault treeNode

            else if index == skipCount treeNode then
                -- Last point also special case
                treeNode
                    |> splitTreeAt (skipCount treeNode - 1) refLonLat
                    |> Tuple.first
                    |> Maybe.withDefault treeNode

            else
                -- Having dispensed with special cases above, there should be something each side.
                case treeNode |> splitTreeAt index refLonLat of
                    ( Just left, Just right ) ->
                        let
                            leftJoinPoint =
                                penultimatePoint left |> Tuple.first

                            rightJoinPoint =
                                secondPoint right |> Tuple.first

                            newLeaf =
                                Leaf <| makeRoadSection refLonLat leftJoinPoint rightJoinPoint

                            trimmedLeft =
                                left
                                    |> splitTreeAt (skipCount left - 1) refLonLat
                                    |> Tuple.first

                            trimmedRight =
                                right
                                    |> splitTreeAt 1 refLonLat
                                    |> Tuple.second
                        in
                        Maybe.withDefault treeNode <|
                            safeJoin refLonLat trimmedLeft <|
                                safeJoin refLonLat (Just newLeaf) trimmedRight

                    _ ->
                        treeNode

        Leaf leaf ->
            -- Can't split a leaf
            treeNode


splitTreeAt : Int -> GPXSource -> PeteTree -> ( Maybe PeteTree, Maybe PeteTree )
splitTreeAt leavesToTheLeft refLonLat thisNode =
    case thisNode of
        Leaf leaf ->
            if leavesToTheLeft <= 0 then
                ( Nothing, Just thisNode )

            else
                ( Just thisNode, Nothing )

        Node aNode ->
            -- From my scribbles, this turns out quite neatly.
            if leavesToTheLeft <= 0 then
                ( Nothing, Just thisNode )

            else if leavesToTheLeft >= skipCount thisNode then
                ( Just thisNode, Nothing )

            else
                let
                    ( leftOfLeft, rightOfLeft ) =
                        aNode.left |> splitTreeAt leavesToTheLeft refLonLat

                    ( leftOfRight, rightOfRight ) =
                        aNode.right |> splitTreeAt (leavesToTheLeft - skipCount aNode.left) refLonLat
                in
                -- In theory, only one side can actually be split...
                ( safeJoin refLonLat leftOfLeft leftOfRight
                , safeJoin refLonLat rightOfLeft rightOfRight
                )


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


safeJoin : GPXSource -> Maybe PeteTree -> Maybe PeteTree -> Maybe PeteTree
safeJoin refLonLat left right =
    case ( left, right ) of
        ( Just leftTree, Just rightTree ) ->
            Just <| joinTrees refLonLat leftTree rightTree

        ( Just leftTree, Nothing ) ->
            left

        ( Nothing, Just rightTree ) ->
            right

        ( Nothing, Nothing ) ->
            Nothing


joinTrees : GPXSource -> PeteTree -> PeteTree -> PeteTree
joinTrees refLonLat leftTree rightTree =
    -- TODO: Does not need refLonLat!
    -- Make a leaf joining the extreme right point of the left to extreme left of the right.
    -- Use a joining node to add this leaf to the smallest side (by skipcount)
    -- Use another joining node to combine.
    let
        rightmostOfLeft =
            getLastLeaf leftTree |> .sourceData |> Tuple.second

        leftmostOfRight =
            getFirstLeaf rightTree |> .sourceData |> Tuple.first

        newLeaf =
            Leaf <| makeRoadSection refLonLat rightmostOfLeft leftmostOfRight
    in
    if skipCount leftTree <= skipCount rightTree then
        joiningNode (joiningNode leftTree newLeaf) rightTree

    else
        joiningNode leftTree (joiningNode newLeaf rightTree)
