module SceneBuilderMap exposing
    ( imperialProfileChart
    , latLonPairFromGpx
    , renderMapJsonWithoutCulling
    , renderPreview
    , trackPointsToJSON
    , trackPointsToJSONwithoutCulling
    )

import Angle exposing (Angle)
import BoundingBox3d
import Direction2d
import DomainModel exposing (..)
import Json.Encode as E
import Length
import PreviewData exposing (PreviewData, PreviewShape(..))
import TrackLoaded exposing (TrackLoaded)


renderPreview : PreviewData -> E.Value
renderPreview { tag, shape, colour, points } =
    case shape of
        PreviewCircle ->
            pointsToJSON <| List.map .gpx points

        PreviewLine ->
            lineToJSON <| List.map .gpx points

        PreviewToolSupplied _ ->
            -- Can't do arbitrary stuff on the Map.
            E.null

        PreviewProfile _ ->
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



{- EXAMPLE AS BASIS
   d0 = [{x:1, y:1}, {x:2, y:11},{x:3, y:1},{x:4, y:11},{x:5, y:10}, {x:6, y:8},{x:7, y:15},{x:8, y:2}]

   d1= [{x:1, y:1}, {x:2, y:5}, {x:4, y:1}, {x:5, y:3}, {x:5.5, y:4},{x:7, y:5},{x:8, y:3}]

   const ctx = document.getElementById("chart").getContext("2d");
   const chart = new Chart(ctx, {
     type: "line",
     data: {
       datasets: [
         {
           backgroundColor: "rgba(50,200,50,0.6)",
           borderColor: "rgba(50,200,50,0.6)",
           data: d0,
           fill: "stack"
         },
         {
           backgroundColor: "rgba(200,50,50,0.6)",
           borderColor: "rgba(200,50,50,0.6)",
           data: d1,
           fill: "stack"
         }
       ]
     },
     options: {
       scales: {
         x: {
           type: "linear"
         },
         y: {
           stacked: true,
         }
       },
       elements: {
         line: {
            tension: 0.2
         },
         point: {
           radius: 0
         }
       },
       tooltips: {
         mode: "nearest",
         intersect: false,
         axis: "x"
       },
       hover: {
         mode: "nearest",
         intersect: false,
         axis: "x"
       }
     }
   });
-}


imperialProfileChart : TrackLoaded msg -> E.Value
imperialProfileChart track =
    -- Provide distance in yards and height in feet for Steve Taylor's profile chart.
    -- Use JSON as per chart.js demands.
    -- Indeed, declare the entire chart here, not in JS.
    let
        chartStuff =
            E.object
                [ ( "type", E.string "line" )
                , ( "data"
                  , E.object
                        [ ( "datasets", E.list identity [ profileDataset ] )
                        ]
                  )
                , ( "options", options )
                ]

        options =
            E.object
                [ ( "scales"
                  , E.object
                        [ ( "x", E.object [ ( "type", E.string "linear" ) ] )
                        , ( "y", E.object [ ( "type", E.string "linear" ) ] )
                        ]
                  )
                ]

        profileDataset =
            E.object
                [ ( "backgroundColor", E.string "rgba(50,200,50,0.6)" )
                , ( "borderColor", E.string "rgba(50,200,60,0.6" )
                , ( "data", E.list identity coordinates )
                , ( "fill", E.string "stack" )
                ]

        coordinates : List E.Value
        coordinates =
            -- Simple, safe, slow.
            List.map makeProfilePoint (List.range 0 (skipCount track.trackTree))

        makeProfilePoint : Int -> E.Value
        makeProfilePoint sequence =
            let
                gpx =
                    DomainModel.gpxPointFromIndex sequence track.trackTree

                altitude =
                    Length.inFeet gpx.altitude

                distance =
                    DomainModel.distanceFromIndex sequence track.trackTree
                        |> Length.inYards
            in
            E.object
                [ ( "x", E.float distance )
                , ( "y", E.float altitude )
                ]
    in
    chartStuff


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
                |> .space
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


trackPointsToJSONwithoutCulling : TrackLoaded msg -> E.Value
trackPointsToJSONwithoutCulling track =
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
        depthFn : RoadSection -> Maybe Int
        depthFn road =
            Nothing

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
