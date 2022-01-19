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
import TrackLoaded exposing (TrackLoaded)


renderPreview : PreviewData -> E.Value
renderPreview { tag, shape, colour, points } =
    case shape of
        PreviewCircle ->
            pointsToJSON <| List.map Tuple.second points

        PreviewLine ->
            lineToJSON <| List.map Tuple.second points


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
                    DomainModel.lngLatPair ( Direction2d.toAngle longitude, latitude )
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
            List.map makeFeature points

        makeFeature tp =
            E.object
                [ ( "type", E.string "Feature" )
                , ( "geometry", point tp )
                ]

        coordinates pt =
            DomainModel.lngLatPair ( Direction2d.toAngle pt.longitude, pt.latitude )

        point tp =
            E.object
                [ ( "type", E.string "Point" )
                , ( "coordinates", coordinates tp )
                ]
    in
    E.object
        [ ( "type", E.string "FeatureCollection" )
        , ( "features", E.list identity features )
        ]


renderMapJson : TrackLoaded msg -> E.Value
renderMapJson track =
    let
        boxSide =
            --TODO: put box side in model
            Length.kilometers 4

        mapLocation : GPXSource -> ( Angle, Angle )
        mapLocation point =
            let
                { longitude, latitude, altitude } =
                    point
            in
            ( Direction2d.toAngle longitude, latitude )

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
