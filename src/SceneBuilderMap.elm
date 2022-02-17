module SceneBuilderMap exposing (..)

import Actions exposing (PreviewData, PreviewShape(..))
import Angle exposing (Angle)
import BoundingBox3d exposing (BoundingBox3d)
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (..)
import Element
import Json.Encode as E
import Length exposing (Meters)
import LocalCoords exposing (LocalCoords)
import Quantity
import TrackLoaded exposing (TrackLoaded)


renderPreview : PreviewData -> E.Value
renderPreview { tag, shape, colour, points } =
    case shape of
        PreviewCircle ->
            pointsToJSON <| List.map Tuple.second points

        PreviewLine ->
            lineToJSON <| List.map Tuple.second points

        PreviewToolSupplied _ ->
            -- Can't do arbitrary stuff on the Map.
            E.null


lineToJSON : List GPXSource -> E.Value
lineToJSON points =
    -- JSON suitable for Mapbox API to add polyline for route.
    let
        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            List.map
                (\{ longitude, latitude, altitude } ->
                    DomainModel.lngLatPair ( Direction2d.toAngle longitude, latitude, altitude )
                )
                points
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]


pointsToJSON : List GPXSource -> E.Value
pointsToJSON points =
    -- Similar but each point is a feature so it is draggable.
    --var geojson = {
    --    'type': 'FeatureCollection',
    --    'features': [
    --        {
    --            'type': 'Feature',
    --            'geometry': {
    --                'type': 'Point',
    --                'coordinates': [0, 0]
    --            }
    --        }
    --    ]
    --};
    let
        features =
            List.map makeFeatureFromGPX points
    in
    E.object
        [ ( "type", E.string "FeatureCollection" )
        , ( "features", E.list identity features )
        ]


mapLocation : GPXSource -> ( Angle, Angle, Length.Length )
mapLocation point =
    let
        { longitude, latitude, altitude } =
            point
    in
    ( Direction2d.toAngle longitude, latitude, altitude )


latLonPair : ( Angle, Angle, Length.Length ) -> E.Value
latLonPair ( lon, lat, ele ) =
    E.list E.float [ Angle.inDegrees lon, Angle.inDegrees lat ]


latLonPairFromGpx : GPXSource -> E.Value
latLonPairFromGpx { longitude, latitude, altitude } =
    E.list E.float
        [ Angle.inDegrees <| Direction2d.toAngle longitude
        , Angle.inDegrees latitude
        ]


renderMapJson : TrackLoaded msg -> E.Value
renderMapJson track =
    -- This version gives track suitable for map.addTrack.
    -- Sadly, mapbox requires a different format for the track points.
    let
        boxSide =
            --TODO: put box side in model
            Length.kilometers 4

        makeVisibleSegment : PeteTree -> E.Value
        makeVisibleSegment node =
            lngLatPair <| mapLocation <| Tuple.second <| sourceData node

        renderTree : Int -> PeteTree -> List E.Value -> List E.Value
        renderTree depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    makeVisibleSegment someNode :: accum

                Node notLeaf ->
                    if depth <= 0 then
                        makeVisibleSegment someNode :: accum

                    else
                        accum
                            |> renderTree (depth - 1) notLeaf.right
                            |> renderTree (depth - 1) notLeaf.left

        renderTreeSelectively :
            BoundingBox3d Meters LocalCoords
            -> Int
            -> PeteTree
            -> List E.Value
            -> List E.Value
        renderTreeSelectively box depth someNode accum =
            case someNode of
                Leaf leafNode ->
                    makeVisibleSegment someNode :: accum

                Node notLeaf ->
                    if notLeaf.nodeContent.boundingBox |> BoundingBox3d.intersects box then
                        -- Ignore depth cutoff near or in the box
                        accum
                            |> renderTreeSelectively box (depth - 1) notLeaf.right
                            |> renderTreeSelectively box (depth - 1) notLeaf.left

                    else
                        -- Outside box, apply cutoff.
                        accum
                            |> renderTree (depth - 1) notLeaf.right
                            |> renderTree (depth - 1) notLeaf.left

        renderFirstPoint treeNode =
            lngLatPair <| mapLocation <| Tuple.first <| sourceData treeNode

        current =
            startPoint <| leafFromIndex track.currentPosition track.trackTree

        detailBox =
            BoundingBox3d.withDimensions ( boxSide, boxSide, boxSide ) current

        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            renderFirstPoint track.trackTree
                :: renderTreeSelectively detailBox track.renderDepth track.trackTree []
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]


renderMapJsonWithoutCulling : TrackLoaded msg -> E.Value
renderMapJsonWithoutCulling track =
    -- This version gives track suitable for map.addTrack.
    -- Sadly, mapbox requires a different format for the track points.
    let
        geometry =
            E.object
                [ ( "type", E.string "LineString" )
                , ( "coordinates", E.list identity coordinates )
                ]

        coordinates =
            DomainModel.getAllGPXPointsInNaturalOrder track.trackTree
                |> List.map latLonPairFromGpx
    in
    E.object
        [ ( "type", E.string "Feature" )
        , ( "properties", E.object [] )
        , ( "geometry", geometry )
        ]


makeFeatureFromGPX : GPXSource -> E.Value
makeFeatureFromGPX { longitude, latitude, altitude } =
    makeFeature ( longitude |> Direction2d.toAngle, latitude, altitude )


makeFeature tp =
    E.object
        [ ( "type", E.string "Feature" )
        , ( "geometry", makePoint tp )
        ]


makePoint lonLat =
    E.object
        [ ( "type", E.string "Point" )
        , ( "coordinates", latLonPair lonLat )
        ]


trackPointsToJSON : TrackLoaded msg -> E.Value
trackPointsToJSON track =
    -- Similar but each point is a feature so it is draggable.
    --var geojson = {
    --    'type': 'FeatureCollection',
    --    'features': [
    --        {
    --            'type': 'Feature',
    --            'geometry': {
    --                'type': 'Point',
    --                'coordinates': [0, 0]
    --            }
    --        }
    --    ]
    --};
    let
        fullRenderBoxSize =
            Length.kilometers 4

        fullRenderBox =
            earthPointFromIndex track.currentPosition track.trackTree
                |> BoundingBox3d.singleton
                |> BoundingBox3d.expandBy fullRenderBoxSize

        depthFn : RoadSection -> Maybe Int
        depthFn road =
            if road.boundingBox |> BoundingBox3d.intersects fullRenderBox then
                Nothing

            else
                Just 10

        foldFn : RoadSection -> List E.Value -> List E.Value
        foldFn road output =
            let
                ( lon, lat, alt ) =
                    mapLocation <| Tuple.first road.sourceData
            in
            makeFeature ( lon, lat, alt ) :: output

        missingLastPoint =
            track.trackTree
                |> getLastLeaf
                |> .sourceData
                |> Tuple.second
                |> mapLocation
                |> makeFeature

        features =
            missingLastPoint
                :: DomainModel.traverseTreeBetweenLimitsToDepth
                    0
                    (skipCount track.trackTree)
                    depthFn
                    0
                    track.trackTree
                    foldFn
                    []
    in
    E.object
        [ ( "type", E.string "FeatureCollection" )
        , ( "features", E.list identity features )
        ]
