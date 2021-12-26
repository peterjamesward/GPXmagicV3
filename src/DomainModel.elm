module DomainModel exposing (..)

import Angle exposing (Angle)
import BoundingBox3d exposing (BoundingBox3d)
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
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
    { points : List GPXPoint
    , referenceLonLat : GPXPoint
    }


type alias RoadSection =
    -- A piece of road, in local, conformal, space.
    -- Can be based between two 'fundamental' points from GPX, or an assembly of them.
    { startsAt : LocalPoint
    , endsAt : LocalPoint
    , boundingBox : BoundingBox3d Meters LocalCoords
    , trueLength : Quantity Float Meters
    , gpxGapCount : Int
    }


type
    PeteTree
    -- Absurdly simple tree may work.
    = Leaf RoadSection
    | Node
        { nodeContent : RoadSection
        , left : PeteTree
        , right : PeteTree
        }


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


localPointsFromGpxTrack : GPXTrack -> List ( GPXPoint, LocalPoint )
localPointsFromGpxTrack { referenceLonLat, points } =
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
            { startsAt = local1
            , endsAt = local2
            , boundingBox = BoundingBox3d.from local1 local2
            , trueLength =
                Length.meters <|
                    Spherical.range
                        ( gpx1.latitude, gpx1.longitude )
                        ( gpx2.latitude, gpx2.longitude )
            , gpxGapCount = 1
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
            { startsAt = info1.startsAt
            , endsAt = info2.endsAt
            , boundingBox = BoundingBox3d.union info1.boundingBox info2.boundingBox
            , trueLength = Quantity.plus info1.trueLength info2.trueLength
            , gpxGapCount = info1.gpxGapCount + info2.gpxGapCount
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



--TODO: Build tree
--TODO: Delete a leaf/leaves
--TODO: Insert a leaf/leaves
--TODO: Update leaves
--TODO: Box content query
--TODO: Box nearest query
--TODO: Queries with filters
--TODO: Queries with folds
