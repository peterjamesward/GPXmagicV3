port module MapPortController exposing (..)

import Actions exposing (ToolAction(..))
import Angle
import BoundingBox3d
import Dict exposing (Dict)
import Direction2d
import DomainModel exposing (..)
import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E
import Length
import MapTypes
import MapboxKey exposing (mapboxKey)
import PaneContext exposing (PaneContext, paneIdToString)
import Pixels exposing (Pixels)
import Point3d
import PreviewData exposing (PreviewData)
import Quantity exposing (Quantity)
import SceneBuilderMap exposing (latLonPairFromGpx)
import SceneBuilderProfile
import SystemSettings exposing (SystemSettings)
import Tools.NamedSegmentOptions
import Tools.Tracks
import TrackLoaded exposing (TrackLoaded)
import ViewProfileChartContext


type MapMsg
    = MapPortMessage E.Value


defaultMapState : MapTypes.MapClickLocation
defaultMapState =
    { lastClickLon = 0.0
    , lastClickLat = 0.0
    }


port mapCommands : E.Value -> Cmd msg


port mapResponses : (E.Value -> msg) -> Sub msg


createMap : String -> MapTypes.MapInfo -> ( Quantity Int Pixels, Quantity Int Pixels ) -> Cmd msg
createMap style info ( width, height ) =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Init" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float info.centreLon )
            , ( "lat", E.float info.centreLat )
            , ( "zoom", E.float info.mapZoom )
            , ( "style", E.string style )
            , ( "width", E.int <| Pixels.inPixels width )
            , ( "height", E.int <| Pixels.inPixels height )
            ]


refreshMap : Cmd msg
refreshMap =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Repaint" )
            , ( "token", E.string mapboxKey )
            ]


centreMapOnCurrent : TrackLoaded msg -> Cmd msg
centreMapOnCurrent track =
    let
        { longitude, latitude } =
            gpxPointFromIndex track.currentPosition track.trackTree
    in
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Centre" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
            , ( "lat", E.float <| Angle.inDegrees latitude )
            ]


zoomMapToFitTrack : TrackLoaded msg -> Cmd msg
zoomMapToFitTrack track =
    let
        { minX, maxX, minY, maxY, minZ } =
            BoundingBox3d.extrema <|
                BoundingBox3d.expandBy (Length.kilometers 2) <|
                    boundingBox track.trackTree

        ( swCorner, neCorner ) =
            ( Point3d.xyz minX minY minZ, Point3d.xyz maxX maxY minZ )

        ( swGpx, neGpx ) =
            ( DomainModel.gpxFromPointWithReference track.referenceLonLat <| DomainModel.withoutTime swCorner
            , DomainModel.gpxFromPointWithReference track.referenceLonLat <| DomainModel.withoutTime neCorner
            )

        ( swLonLat, neLonLat ) =
            ( [ E.float <| Angle.inDegrees <| Direction2d.toAngle swGpx.longitude
              , E.float <| Angle.inDegrees swGpx.latitude
              ]
            , [ E.float <| Angle.inDegrees <| Direction2d.toAngle neGpx.longitude
              , E.float <| Angle.inDegrees neGpx.latitude
              ]
            )

        bbox =
            [ E.list identity swLonLat, E.list identity neLonLat ]
    in
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Bounds" )
            , ( "token", E.string mapboxKey )
            , ( "bbox", E.list identity bbox )
            ]


update :
    MapMsg
    -> Maybe (TrackLoaded msg)
    -> MapTypes.MapClickLocation
    -> ( MapTypes.MapClickLocation, List (ToolAction msg) )
update mapMsg track lastState =
    case mapMsg of
        MapPortMessage value ->
            processMapPortMessage lastState track value


toggleDragging : Bool -> TrackLoaded msg -> Cmd msg
toggleDragging isDragging track =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Drag" )
            , ( "Enable", E.bool isDragging )
            , ( "label", E.string track.trackName )
            , ( "points", SceneBuilderMap.trackPointsToJSON track ) -- Make track points draggable
            ]


enablePlanning : Cmd msg
enablePlanning =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Planning" ) ]


getPoints : Cmd msg
getPoints =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "GetPoints" ) ]


resetMapAfterDrawing : Cmd msg
resetMapAfterDrawing =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "StopPlanning" ) ]


requestElevations : Cmd msg
requestElevations =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Elev" )
            ]


fetchElevationsForPoints : List GPXSource -> Cmd msg
fetchElevationsForPoints rawData =
    -- See if we can use the Map to give us some altitude for land use data.
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "LandUse" )
            , ( "data", E.list latLonPairFromGpx rawData )
            ]


setMapStyle : String -> Cmd msg
setMapStyle url =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Style" )
            , ( "style", E.string url )
            ]


addInactiveTrackToMap : TrackLoaded msg -> Cmd msg
addInactiveTrackToMap track =
    --addFullTrackToMap track
    -- This is to add the route as a polyline, with selective rendering
    let
        { longitude, latitude, altitude } =
            gpxPointFromIndex track.currentPosition track.trackTree
    in
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Inactive" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
            , ( "lat", E.float <| Angle.inDegrees latitude )
            , ( "label", E.string track.trackName ) -- worth a try
            , ( "data", SceneBuilderMap.renderMapJsonWithoutCulling track ) -- Route as polyline
            ]


addFullTrackToMap : TrackLoaded msg -> Cmd msg
addFullTrackToMap track =
    -- This is to add the route as a polyline, without selective rendering
    let
        { longitude, latitude } =
            gpxPointFromIndex track.currentPosition track.trackTree
    in
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Track" )
            , ( "token", E.string mapboxKey )
            , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
            , ( "lat", E.float <| Angle.inDegrees latitude )
            , ( "zoom", E.float 10.0 )
            , ( "label", E.string track.trackName )
            , ( "data", SceneBuilderMap.renderMapJsonWithoutCulling track ) -- Route as polyline
            , ( "points", SceneBuilderMap.trackPointsToJSONwithoutCulling track ) -- Make track points draggable
            ]


removeTrackFromMap : TrackLoaded msg -> Cmd msg
removeTrackFromMap track =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Remove" )
            , ( "token", E.string mapboxKey )
            , ( "label", E.string track.trackName ) -- worth a try
            ]


addAllTracksToMap : Tools.Tracks.Options msg -> Cmd msg
addAllTracksToMap options =
    let
        addToMap track active =
            if active then
                Cmd.batch [ addFullTrackToMap track, addMarkersToMap track ]

            else
                addInactiveTrackToMap track

        removeFromMap track active =
            if active then
                Cmd.none

            else
                removeTrackFromMap track
    in
    Cmd.batch <|
        Tools.Tracks.mapOverInvisibleTracks removeFromMap options
            ++ Tools.Tracks.mapOverVisibleTracks addToMap options


paintCanvasProfileChart :
    ViewProfileChartContext.ProfileContext
    -> SystemSettings
    -> TrackLoaded msg
    -> Dict String PreviewData
    -> Cmd msg
paintCanvasProfileChart profileContext settings track previews =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Profile" )
            , ( "container", E.string <| "altitude." ++ profileContext.contextSuffix )
            , ( "chart"
              , SceneBuilderProfile.profileChart
                    profileContext
                    settings
                    track
                    previews
              )
            ]


paintCanvasGradientChart : ViewProfileChartContext.ProfileContext -> SystemSettings -> TrackLoaded msg -> Cmd msg
paintCanvasGradientChart profileContext settings track =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Gradient" )
            , ( "container", E.string <| "gradient." ++ profileContext.contextSuffix )
            , ( "chart", SceneBuilderProfile.gradientChart profileContext settings track )
            ]


showPreview : String -> String -> String -> E.Value -> Cmd msg
showPreview tag shape colour geoJson =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "ShowPreview" )
            , ( "token", E.string mapboxKey )
            , ( "label", E.string tag )
            , ( "shape", E.string shape )
            , ( "colour", E.string colour )
            , ( "data", geoJson )
            ]


hidePreview : String -> Cmd msg
hidePreview tag =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "HidePreview" )
            , ( "token", E.string mapboxKey )
            , ( "label", E.string tag )
            ]


addMarkersToMap :
    TrackLoaded msg
    -> Cmd msg
addMarkersToMap track =
    let
        encodePos { longitude, latitude, altitude } =
            E.object
                [ ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
                , ( "lat", E.float <| Angle.inDegrees latitude )
                ]
    in
    mapCommands <|
        case track.markerPosition of
            Just purple ->
                E.object
                    [ ( "Cmd", E.string "Mark" )
                    , ( "orange", encodePos <| gpxPointFromIndex track.currentPosition track.trackTree )
                    , ( "purple", encodePos <| gpxPointFromIndex purple track.trackTree )
                    ]

            Nothing ->
                E.object
                    [ ( "Cmd", E.string "Mark" )
                    , ( "orange", encodePos <| gpxPointFromIndex track.currentPosition track.trackTree )
                    ]


processMapPortMessage :
    MapTypes.MapClickLocation
    -> Maybe (TrackLoaded msg)
    -> E.Value
    -> ( MapTypes.MapClickLocation, List (ToolAction msg) )
processMapPortMessage lastState mTrack json =
    let
        jsonMsg =
            D.decodeValue (D.field "msg" D.string) json

        ( lat, lon ) =
            ( D.decodeValue (D.field "lat" D.float) json
            , D.decodeValue (D.field "lon" D.float) json
            )

        ( container, distance ) =
            ( D.decodeValue (D.field "container" D.string) json
            , D.decodeValue (D.field "x" D.float) json
            )

        elevations =
            D.decodeValue
                (D.field "elevations" (D.list (D.nullable D.float)))
                json

        waypoints =
            D.decodeValue
                (D.field "waypoints" (D.list (D.list D.float)))
                json
    in
    case jsonMsg of
        Ok "map ready" ->
            ( lastState
            , [ --TryRemoteLoadIfGiven
                MapRefresh
              ]
            )

        Ok "click" ->
            --{ 'msg' : 'click'
            --, 'lat' : e.lat()
            --, 'lon' : e.lon()
            --} );
            case ( lat, lon, mTrack ) of
                ( Ok lat1, Ok lon1, Just track ) ->
                    if lat1 == lastState.lastClickLat && lon1 == lastState.lastClickLon then
                        ( lastState, [] )

                    else
                        let
                            gpxPoint =
                                { longitude = Direction2d.fromAngle <| Angle.degrees lon1
                                , latitude = Angle.degrees lat1
                                , altitude = Length.meters 0.0
                                , timestamp = Nothing
                                }

                            index =
                                DomainModel.nearestToLonLat
                                    gpxPoint
                                    track.currentPosition
                                    track.trackTree
                                    track.referenceLonLat
                                    track.leafIndex
                        in
                        ( { lastState
                            | lastClickLon = lon1
                            , lastClickLat = lat1
                          }
                        , [ SetCurrentFromMapClick index
                          , SaveLastMapClick lon1 lat1
                          , PointerChange
                          ]
                        )

                _ ->
                    ( lastState, [] )

        Ok "profileClick" ->
            -- Not really a map thing but not worth another port.
            -- Need to kick it into Main, where we have imperial, and
            -- should then probably go to PaneLayoutManager.
            --{ 'msg' : 'profileClick'
            --, 'container' : name of the container for the canvas
            --, 'x' : distance
            --}
            case ( container, distance ) of
                ( Ok container1, Ok distance1 ) ->
                    ( lastState
                    , [ ProfileClick container1 distance1
                      , PointerChange
                      ]
                    )

                _ ->
                    ( lastState, [] )

        Ok "drag" ->
            case mTrack of
                Just track ->
                    ( lastState, draggedOnMap json track )

                Nothing ->
                    ( lastState, [] )

        Ok "elevations" ->
            case elevations of
                Ok mapElevations ->
                    ( lastState, [ ApplyMapElevations mapElevations ] )

                _ ->
                    ( lastState, [] )

        Ok "landuse" ->
            case elevations of
                Ok mapElevations ->
                    ( lastState, [ ApplyLandUseAltitudes mapElevations ] )

                _ ->
                    ( lastState, [] )

        Ok "waypoints" ->
            case waypoints of
                Ok mapPoints ->
                    ( lastState, [ FetchMatchingRoute mapPoints ] )

                _ ->
                    ( lastState, [] )

        _ ->
            ( lastState, [] )


draggedOnMap : E.Value -> TrackLoaded msg -> List (ToolAction msg)
draggedOnMap json track =
    -- Map has told us the old and new coordinates of a point.
    -- Return Nothing if drag did not change track.
    let
        lon1 =
            D.decodeValue (D.at [ "start", "lng" ] D.float) json

        lat1 =
            D.decodeValue (D.at [ "start", "lat" ] D.float) json

        lon2 =
            D.decodeValue (D.at [ "end", "lng" ] D.float) json

        lat2 =
            D.decodeValue (D.at [ "end", "lat" ] D.float) json
    in
    if lon1 == lon2 && lat1 == lat2 then
        []

    else
        case ( ( lon1, lat1 ), ( lon2, lat2 ) ) of
            ( ( Ok startLon, Ok startLat ), ( Ok endLon, Ok endLat ) ) ->
                [ PointMovedOnMap startLon startLat endLon endLat
                , TrackHasChanged
                ]

            _ ->
                []
