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
    -> TrackLoaded msg
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
            , ( "points", SceneBuilderMap.trackPointsToJSON track ) -- Make track points draggable
            ]


enablePlanning : Cmd msg
enablePlanning =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Planning" ) ]


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


addTrackToMap : TrackLoaded msg -> Cmd msg
addTrackToMap track =
    addFullTrackToMap track



{-
   -- This is to add the route as a polyline, with selective rendering
   let
       { longitude, latitude, altitude } =
           gpxPointFromIndex track.currentPosition track.trackTree
   in
   mapCommands <|
       E.object
           [ ( "Cmd", E.string "Track" )
           , ( "token", E.string mapboxKey )
           , ( "lon", E.float <| Angle.inDegrees <| Direction2d.toAngle longitude )
           , ( "lat", E.float <| Angle.inDegrees latitude )
           , ( "zoom", E.float 10.0 )
           , ( "data", SceneBuilderMap.renderMapJson track ) -- Route as polyline
           , ( "points", SceneBuilderMap.trackPointsToJSON track ) -- Make track points draggable
           ]
-}


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
            , ( "data", SceneBuilderMap.renderMapJsonWithoutCulling track ) -- Route as polyline
            , ( "points", SceneBuilderMap.trackPointsToJSONwithoutCulling track ) -- Make track points draggable
            ]


paintCanvasProfileChart :
    ViewProfileChartContext.ProfileContext
    -> SystemSettings
    -> TrackLoaded msg
    -> List Tools.NamedSegmentOptions.NamedSegment
    -> Dict String PreviewData
    -> Cmd msg
paintCanvasProfileChart profileContext settings track segments previews =
    mapCommands <|
        E.object
            [ ( "Cmd", E.string "Profile" )
            , ( "container", E.string <| "altitude." ++ profileContext.contextSuffix )
            , ( "chart"
              , SceneBuilderProfile.profileChart
                    profileContext
                    settings
                    track
                    segments
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


msgDecoder : Decoder String
msgDecoder =
    field "msg" string


processMapPortMessage :
    MapTypes.MapClickLocation
    -> TrackLoaded msg
    -> E.Value
    -> ( MapTypes.MapClickLocation, List (ToolAction msg) )
processMapPortMessage lastState track json =
    let
        jsonMsg =
            D.decodeValue msgDecoder json

        ( lat, lon ) =
            ( D.decodeValue (D.field "lat" D.float) json
            , D.decodeValue (D.field "lon" D.float) json
            )

        ( container, distance ) =
            ( D.decodeValue (D.field "container" D.string) json
            , D.decodeValue (D.field "x" D.float) json
            )

        elevations =
            D.decodeValue (D.field "elevations" (D.list (D.nullable D.float))) json
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
            case ( lat, lon ) of
                ( Ok lat1, Ok lon1 ) ->
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
            ( lastState, draggedOnMap json track )

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
