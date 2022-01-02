module DomainModel exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction2d exposing (Direction2d)
import Direction3d
import Json.Encode as E
import Length exposing (Length, Meters, inMeters)
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import SketchPlane3d
import Sphere3d exposing (Sphere3d)
import Spherical as Spherical exposing (range)
import Vector3d exposing (Vector3d)


type alias GPXSource =
    -- Being a raw line of data from GPX file.
    { longitude : Angle.Angle
    , latitude : Angle.Angle
    , altitude : Quantity Float Meters
    }


type alias EarthVector =
    -- Experiment with polar coordinates direct from GPX data
    Vector3d Meters LocalCoords


type alias EarthPoint =
    Point3d Meters LocalCoords


type alias RoadSection =
    -- Can be based between two 'fundamental' points from GPX, or an assembly of them.
    -- I THINK it makes sense to store only the vectors.
    -- Bounding box and Sphere needed for culling in nearness tests.
    -- Keeping track of longitude tricky because of IDL.
    { startVector : EarthVector
    , endVector : EarthVector
    , boundingBox : BoundingBox3d Meters LocalCoords
    , sphere : Sphere3d Meters LocalCoords
    , trueLength : Quantity Float Meters
    , skipCount : Int
    , startLongitude : Direction2d LocalCoords
    , westward : Angle.Angle
    , eastward : Angle.Angle
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


startVector : PeteTree -> EarthVector
startVector treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.startVector

        Node node ->
            node.nodeContent.startVector


endVector : PeteTree -> EarthVector
endVector treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.endVector

        Node node ->
            node.nodeContent.endVector


trueLength : PeteTree -> Length
trueLength treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.trueLength

        Node node ->
            node.nodeContent.trueLength


skipCount : PeteTree -> Int
skipCount treeNode =
    case treeNode of
        Leaf leaf ->
            1

        Node node ->
            node.nodeContent.skipCount


boundingBox : PeteTree -> BoundingBox3d Length.Meters LocalCoords
boundingBox treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.boundingBox

        Node node ->
            node.nodeContent.boundingBox


sphere : PeteTree -> Sphere3d Length.Meters LocalCoords
sphere treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.sphere

        Node node ->
            node.nodeContent.sphere

startLongitude : PeteTree -> Direction2d LocalCoords
startLongitude treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.startLongitude

        Node node ->
            node.nodeContent.startLongitude

eastward : PeteTree -> Angle
eastward treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.eastward

        Node node ->
            node.nodeContent.eastward

westward : PeteTree -> Angle
westward treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.westward

        Node node ->
            node.nodeContent.westward


makeRoadSection : EarthVector -> EarthVector -> RoadSection
makeRoadSection v1 v2 =
    let
        ( local1, local2 ) =
            ( Point3d.origin |> Point3d.translateBy v1
            , Point3d.origin |> Point3d.translateBy v2
            )

        box =
            BoundingBox3d.from local1 local2

        ( earth1, earth2 ) =
            ( gpxFromVector v1, gpxFromVector v2 )

        range : Length.Length
        range =
            Length.meters <|
                Spherical.range
                    ( earth1.longitude, earth1.latitude )
                    ( earth2.longitude, earth2.latitude )

        startLon =
            Vector3d.direction v1
                |> Maybe.withDefault Direction3d.x
                |> Direction3d.projectInto SketchPlane3d.xy
                |> Maybe.withDefault Direction2d.x

        endLon =
            Vector3d.direction v2
                |> Maybe.withDefault Direction3d.x
                |> Direction3d.projectInto SketchPlane3d.xy
                |> Maybe.withDefault Direction2d.x

        longitudeChange =
            Direction2d.angleFrom startLon endLon
    in
    { startVector = v1
    , endVector = v2
    , boundingBox = box
    , sphere = containingSphere box
    , trueLength = range
    , skipCount = 1
    , startLongitude = startLon
    , westward = Quantity.max Quantity.zero longitudeChange
    , eastward = Quantity.min Quantity.zero longitudeChange
    }


treeFromList : List EarthVector -> Maybe PeteTree
treeFromList track =
    -- Build the skeletal tree of nodes, then attach the leaves from the input list.
    -- Should be much quicker than recursively splitting the list, for large lists.
    let
        numberOfSegments =
            List.length track - 1

        combineInfo : PeteTree -> PeteTree -> RoadSection
        combineInfo info1 info2 =
            let
                box =
                    BoundingBox3d.union (boundingBox info1) (boundingBox info2)
            in
            { startVector = startVector info1
            , endVector = endVector info2
            , boundingBox = box
            , sphere = containingSphere box
            , trueLength = Quantity.plus (trueLength info1) (trueLength info2)
            , skipCount = skipCount info1 + skipCount info2
            , startLongitude = startLongitude info1
            , westward = Quantity.max (westward info1) (westward info2)
            , eastward = Quantity.min (eastward info1) (eastward info2)
            }

        treeBuilder : Int -> List EarthVector -> ( Maybe PeteTree, List EarthVector )
        treeBuilder n vectorStream =
            case ( n < 2, vectorStream ) of
                ( True, v1 :: v2 :: vvvv ) ->
                    -- Take two vectors for this Leaf, but only consume one.
                    ( Just <| Leaf <| makeRoadSection v1 v2, v2 :: vvvv )

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
                            ( Just <|
                                Node
                                    { nodeContent = combineInfo leftSubtree rightSubtree
                                    , left = leftSubtree
                                    , right = rightSubtree
                                    }
                            , remainingAfterRight
                            )

                        _ ->
                            ( Nothing, remainingAfterRight )
    in
    treeBuilder numberOfSegments track |> Tuple.first


earthVectorFromIndex : Int -> PeteTree -> EarthVector
earthVectorFromIndex index treeNode =
    case treeNode of
        Leaf info ->
            if index <= 0 then
                info.startVector

            else
                info.endVector

        Node info ->
            if index < skipCount info.left then
                earthVectorFromIndex index info.left

            else
                earthVectorFromIndex (index - skipCount info.left) info.right


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
                            Point3d.origin
                                |> Point3d.translateBy leaf.startVector
                                |> Point3d.distanceFromAxis ray

                        endDistance =
                            Point3d.origin
                                |> Point3d.translateBy leaf.endVector
                                |> Point3d.distanceFromAxis ray
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


makeEarthVector lon lat alt =
    let
        direction =
            Direction3d.xyZ lon lat

        radius =
            alt |> Quantity.plus (Length.meters Spherical.meanRadius)
    in
    Vector3d.withLength radius direction


nearestToLonLat :
    GPXSource
    -> PeteTree
    -> Int
nearestToLonLat click treeNode =
    -- Try: compute distance to each box centres.
    -- At each level, pick "closest" child and recurse.
    -- Not good enough. Need deeper search, say for all intersected boxes.
    -- Bit of recursive magic to get the "index" number.
    let
        searchVector =
            makeEarthVector click.longitude click.latitude click.altitude

        searchAxis =
            Axis3d.throughPoints
                Point3d.origin
                (Point3d.origin |> Point3d.translateBy searchVector)
                |> Maybe.withDefault Axis3d.x

        helper withNode skip =
            case withNode of
                Leaf leaf ->
                    -- Use whichever point is closest.
                    let
                        -- Using dot product as closeness comparator. Nearest to +1 wins.
                        ( startDistance, endDistance ) =
                            ( Vector3d.dot (startVector withNode) searchVector
                            , Vector3d.dot (endVector withNode) searchVector
                            )
                    in
                    if startDistance |> Quantity.lessThanOrEqualTo endDistance then
                        ( skip, startDistance )

                    else
                        ( skip + 1, endDistance )

                Node node ->
                    -- Culling experiment. Use dot products to see if the search vector is
                    -- "in the region" of the left or right child?
                    -- If we are in the region of one only, use that one.
                    -- If both, try both.
                    -- If neither, use the closeness test and choose the closest...
                    let
                        leftSpan =
                            Vector3d.dot (startVector node.left) (endVector node.left)

                        rightSpan =
                            Vector3d.dot (startVector node.right) (endVector node.right)

                        leftProximity =
                            -- Remember, for dots, greater is closer.
                            (Vector3d.dot searchVector (startVector node.left)
                                |> Quantity.greaterThanOrEqualTo leftSpan
                            )
                                && (Vector3d.dot searchVector (endVector node.left)
                                        |> Quantity.greaterThanOrEqualTo leftSpan
                                   )

                        rightProximity =
                            (Vector3d.dot searchVector (startVector node.right)
                                |> Quantity.greaterThanOrEqualTo rightSpan
                            )
                                && (Vector3d.dot searchVector (endVector node.right)
                                        |> Quantity.greaterThanOrEqualTo rightSpan
                                   )

                        ( leftDistance, rightDistance ) =
                            ( Quantity.max
                                (Vector3d.dot searchVector (startVector node.left))
                                (Vector3d.dot searchVector (endVector node.left))
                            , Quantity.max
                                (Vector3d.dot searchVector (startVector node.right))
                                (Vector3d.dot searchVector (endVector node.right))
                            )
                    in
                    case ( leftProximity, rightProximity ) of
                        ( True, True ) ->
                            -- Could go either way
                            let
                                ( leftBestIndex, leftBestDistance ) =
                                    helper node.left skip

                                ( rightBestIndex, rightBestDistance ) =
                                    helper node.right (skip + skipCount node.left)
                            in
                            if leftBestDistance |> Quantity.greaterThanOrEqualTo rightBestDistance then
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


gpxFromVector : EarthVector -> GPXSource
gpxFromVector vector =
    let
        direction =
            Vector3d.direction vector
    in
    case direction of
        Just d ->
            { longitude = d |> Direction3d.azimuthIn SketchPlane3d.xy
            , latitude = d |> Direction3d.elevationFrom SketchPlane3d.xy
            , altitude = Vector3d.length vector
            }

        Nothing ->
            { longitude = Quantity.zero
            , latitude = Quantity.zero
            , altitude = Length.meters Spherical.meanRadius
            }
