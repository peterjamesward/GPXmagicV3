module DomainModel exposing (..)

import Angle exposing (Angle)
import Axis2d
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (black)
import Json.Encode as E
import Length exposing (Length, Meters, inMeters)
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d exposing (Sphere3d)
import Spherical as Spherical exposing (range)


type alias GPXPoint =
    -- Being a raw line of data from GPX file.
    { longitude : Angle
    , latitude : Angle
    , altitude : Quantity Float Meters
    }


type alias LocalPoint =
    Point3d Meters LocalCoords


type alias GPXTrack =
    -- The reference lonLat is essential to remove the cosine correction.
    { points : List GPXPoint
    , referenceLonLat : GPXPoint
    }


type alias RoadSection =
    -- A piece of road, in local, conformal, space.
    -- Can be based between two 'fundamental' points from GPX, or an assembly of them.
    { startsAt : LocalPoint
    , endsAt : LocalPoint
    , mapStartAt : GPXPoint
    , mapEndAt : GPXPoint
    , boundingBox : BoundingBox3d Meters LocalCoords
    , sphere : Sphere3d Meters LocalCoords
    , trueLength : Quantity Float Meters
    , skipCount : Int
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



-- The repetition here makes me think I could tidy up the base structure in some way.


startsAt : PeteTree -> LocalPoint
startsAt treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.startsAt

        Node node ->
            node.nodeContent.startsAt


endsAt : PeteTree -> LocalPoint
endsAt treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.endsAt

        Node node ->
            node.nodeContent.endsAt


mapStartAt : PeteTree -> GPXPoint
mapStartAt treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.mapStartAt

        Node node ->
            node.nodeContent.mapStartAt


mapEndAt : PeteTree -> GPXPoint
mapEndAt treeNode =
    case treeNode of
        Leaf leaf ->
            leaf.mapEndAt

        Node node ->
            node.nodeContent.mapEndAt


centre : PeteTree -> LocalPoint
centre treeNode =
    Point3d.interpolateFrom (startsAt treeNode) (endsAt treeNode) 0.5


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


convertGpxWithReference : GPXPoint -> GPXPoint -> LocalPoint
convertGpxWithReference reference point =
    let
        ( refLon, refLat ) =
            ( reference.longitude |> Angle.inDegrees
            , reference.latitude |> Angle.inDegrees
            )

        ( pointLon, pointLat ) =
            ( point.longitude |> Angle.inDegrees
            , point.latitude |> Angle.inDegrees
            )

        scale =
            reference.latitude |> Angle.inRadians |> cos
    in
    Point3d.meters
        ((pointLon - refLon) * scale * Spherical.metresPerDegree)
        ((pointLat - refLat) * Spherical.metresPerDegree)
        (point.altitude |> Length.inMeters)


convertLocalWithReference : GPXPoint -> LocalPoint -> GPXPoint
convertLocalWithReference reference point =
    let
        ( refLon, refLat ) =
            ( reference.longitude |> Angle.inDegrees
            , reference.latitude |> Angle.inDegrees
            )

        scale =
            reference.latitude |> Angle.inRadians |> cos
    in
    { longitude = Angle.degrees <| refLon + ((inMeters <| Point3d.yCoordinate point) / Spherical.metresPerDegree / scale)
    , latitude = Angle.degrees <| refLat + ((inMeters <| Point3d.xCoordinate point) / Spherical.metresPerDegree)
    , altitude = Point3d.zCoordinate point
    }


localPointsFromGpxTrack : GPXTrack -> List ( GPXPoint, LocalPoint )
localPointsFromGpxTrack { referenceLonLat, points } =
    -- Apply "conformal" map projection (or near enough).
    points
        |> List.map
            (\gpx ->
                ( gpx, convertGpxWithReference referenceLonLat gpx )
            )


segmentsFromPoints : List ( GPXPoint, LocalPoint ) -> List RoadSection
segmentsFromPoints points =
    -- Start with lowest level, constructed directly from known points,
    let
        makeRoadSection ( gpx1, local1 ) ( gpx2, local2 ) =
            let
                box =
                    BoundingBox3d.from local1 local2
            in
            { startsAt = local1
            , endsAt = local2
            , mapStartAt = gpx1
            , mapEndAt = gpx2
            , boundingBox = box
            , sphere = containingSphere box
            , trueLength =
                Length.meters <|
                    Spherical.range
                        ( gpx1.latitude, gpx1.longitude )
                        ( gpx2.latitude, gpx2.longitude )
            , skipCount = 1
            }
    in
    List.map2
        makeRoadSection
        points
        (List.drop 1 points)


treeFromRoadSections : List RoadSection -> Maybe PeteTree
treeFromRoadSections sections =
    let
        combineInfo info1 info2 =
            let
                box =
                    BoundingBox3d.union info1.boundingBox info2.boundingBox
            in
            { startsAt = info1.startsAt
            , endsAt = info2.endsAt
            , mapStartAt = info1.mapStartAt
            , mapEndAt = info2.mapEndAt
            , boundingBox = box
            , sphere = containingSphere box
            , trueLength = Quantity.plus info1.trueLength info2.trueLength
            , skipCount = info1.skipCount + info2.skipCount
            }
    in
    case sections of
        [] ->
            Nothing

        [ s1 ] ->
            Just <| Leaf s1

        _ ->
            let
                ( firstHalf, secondHalf ) =
                    sections |> List.Extra.splitAt (List.length sections // 2)

                ( leftChild, rightChild ) =
                    ( treeFromRoadSections firstHalf, treeFromRoadSections secondHalf )
            in
            case ( leftChild, rightChild ) of
                -- Should get two back but compiler requires we be thorough, quite rightly.
                ( Just (Leaf left), Just (Leaf right) ) ->
                    Just <|
                        Node
                            { nodeContent = combineInfo left right
                            , left = Leaf left
                            , right = Leaf right
                            }

                ( Just (Leaf left), Just (Node right) ) ->
                    Just <|
                        Node
                            { nodeContent = combineInfo left right.nodeContent
                            , left = Leaf left
                            , right = Node right
                            }

                ( Just (Node left), Just (Leaf right) ) ->
                    Just <|
                        Node
                            { nodeContent = combineInfo left.nodeContent right
                            , left = Node left
                            , right = Leaf right
                            }

                ( Just (Node left), Just (Node right) ) ->
                    Just <|
                        Node
                            { nodeContent = combineInfo left.nodeContent right.nodeContent
                            , left = Node left
                            , right = Node right
                            }

                ( Just left, Nothing ) ->
                    Just left

                ( Nothing, Just right ) ->
                    Just right

                ( Nothing, Nothing ) ->
                    Nothing


treeFromList : GPXTrack -> Maybe PeteTree
treeFromList track =
    track
        |> localPointsFromGpxTrack
        |> segmentsFromPoints
        |> treeFromRoadSections


pointFromIndex : Int -> PeteTree -> LocalPoint
pointFromIndex index treeNode =
    case treeNode of
        Leaf info ->
            if index <= 0 then
                info.startsAt

            else
                info.endsAt

        Node info ->
            if index < skipCount info.left then
                pointFromIndex index info.left

            else
                pointFromIndex (index - skipCount info.left) info.right


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
                            leaf.startsAt |> Point3d.distanceFromAxis ray

                        endDistance =
                            leaf.endsAt |> Point3d.distanceFromAxis ray
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


lngLatPair : GPXPoint -> E.Value
lngLatPair { longitude, latitude, altitude } =
    E.list E.float [ Angle.inDegrees longitude, Angle.inDegrees latitude ]
