module Tools.StravaDataLoad exposing (..)

import BoundingBox2d
import BoundingBox3d exposing (BoundingBox3d)
import Http
import Json.Decode as D exposing (Decoder, field)
import Length
import LocalCoords exposing (LocalCoords)
import OAuth exposing (Token, errorCodeToString, tokenToString, useToken)
import Point2d
import Tools.StravaTypes as StravaTypes exposing (..)
import Url.Builder as Builder exposing (string)
import UtilsForViews exposing (httpErrorString)


stravaApiRoot =
    "https://www.strava.com"


stravaProcessRoute :
    Result Http.Error StravaRoute
    -> StravaRouteStatus
stravaProcessRoute response =
    case response of
        Ok route ->
            StravaRouteOk route

        Err err ->
            StravaRouteError (httpErrorString err)


stravaRouteName : StravaRouteStatus -> Maybe String
stravaRouteName stravaRoute =
    case stravaRoute of
        StravaRouteOk route ->
            Just route.name

        _ ->
            Nothing


requestStravaSegment : (Result Http.Error StravaSegment -> msg) -> String -> Token -> Cmd msg
requestStravaSegment msg segmentId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = Builder.crossOrigin stravaApiRoot [ "api", "v3", "segments", segmentId ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg stravaSegmentDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


stravaProcessSegment :
    Result Http.Error StravaSegment
    -> BoundingBox3d Length.Meters LocalCoords
    -> StravaSegmentStatus
stravaProcessSegment response box =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema box

        segmentInBox segment =
            let
                flatBox =
                    BoundingBox2d.fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }
            in
            case ( segment.start_latlng, segment.end_latlng ) of
                ( [ start_latitude, start_longitude ], [ end_latitude, end_longitude ] ) ->
                    BoundingBox2d.contains
                        (Point2d.meters start_longitude start_latitude)
                        flatBox
                        && BoundingBox2d.contains
                            (Point2d.meters start_longitude start_latitude)
                            flatBox

                _ ->
                    False
    in
    case response of
        Ok segment ->
            if segmentInBox segment then
                SegmentOk segment

            else
                SegmentNotInRoute segment

        Err err ->
            SegmentError (httpErrorString err)


stravaSegmentDecoder : D.Decoder StravaSegment
stravaSegmentDecoder =
    D.map6 StravaSegment
        (D.at [ "name" ] D.string)
        (D.at [ "distance" ] D.float)
        (D.at [ "elevation_high" ] D.float)
        (D.at [ "elevation_low" ] D.float)
        (D.at [ "start_latlng" ] (D.list D.float))
        (D.at [ "end_latlng" ] (D.list D.float))



{-
   So the Strava <lovely folk> have changed this to
          "start_latlng": [
              51.731801,
              -0.822131
          ],
          "end_latlng": [
              51.724531,
              -0.808531
          ],
-}


requestStravaSegmentStreams : (Result Http.Error StravaSegmentStreams -> msg) -> String -> Token -> Cmd msg
requestStravaSegmentStreams msg segmentId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = Builder.crossOrigin stravaApiRoot [ "api", "v3", "segments", segmentId, "streams" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg decodeStravaSegmentStreams
        , timeout = Nothing
        , tracker = Nothing
        }


requestStravaActivity : (Result Http.Error StravaActivityStreams -> msg) -> String -> Token -> Cmd msg
requestStravaActivity msg activityId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url =
            Builder.crossOrigin stravaApiRoot
                [ "api"
                , "v3"
                , "activities"
                , activityId
                , "streams"
                ]
                [ string "keys" "latlng,altitude,time"
                , string "key_by_type" "true"
                ]
        , body = Http.emptyBody
        , expect = Http.expectJson msg decodeStravaActivityStreams
        , timeout = Nothing
        , tracker = Nothing
        }


requestStravaRoute : (Result Http.Error String -> msg) -> String -> Token -> Cmd msg
requestStravaRoute msg routeId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = Builder.crossOrigin stravaApiRoot [ "api", "v3", "routes", routeId, "export_gpx" ] []
        , body = Http.emptyBody
        , expect = Http.expectString msg
        , timeout = Nothing
        , tracker = Nothing
        }


requestStravaRouteHeader : (Result Http.Error StravaRoute -> msg) -> String -> Token -> Cmd msg
requestStravaRouteHeader msg routeId token =
    Http.request
        { method = "GET"
        , headers = useToken token []
        , url = Builder.crossOrigin stravaApiRoot [ "api", "v3", "routes", routeId ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg stravaRouteDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


stravaRouteDecoder : D.Decoder StravaRoute
stravaRouteDecoder =
    D.map4 StravaRoute
        (D.at [ "name" ] D.string)
        (D.at [ "description" ] D.string)
        (D.at [ "distance" ] D.float)
        (D.at [ "elevation_gain" ] D.float)


decodeStravaSegmentStreams : D.Decoder StravaSegmentStreams
decodeStravaSegmentStreams =
    D.map3 StravaSegmentStreams
        (field "0" decodeLatLngStream)
        (field "1" decodeStravaDistanceStream)
        (field "2" decodeStravaAltitudeStream)


decodeStravaActivityStreams : D.Decoder StravaActivityStreams
decodeStravaActivityStreams =
    D.map3 StravaActivityStreams
        (D.at [ "latlng", "data" ] (D.list decodeStravaLatLng))
        (D.at [ "altitude", "data" ] (D.list D.float))
        (D.at [ "time", "data" ] (D.list D.int))


decodeStravaLatLng : D.Decoder StravaLatLng
decodeStravaLatLng =
    D.map2 StravaLatLng
        (field "0" D.float)
        (field "1" D.float)


decodeLatLngStream : D.Decoder StravaLatLngStream
decodeLatLngStream =
    D.map5 StravaLatLngStream
        (field "type" D.string)
        (field "data" (D.list decodeStravaLatLng))
        (field "series_type" D.string)
        (field "original_size" D.int)
        (field "resolution" D.string)


decodeStravaDistanceStream : D.Decoder StravaDistanceStream
decodeStravaDistanceStream =
    D.map5 StravaDistanceStream
        (field "type" D.string)
        (field "data" (D.list D.float))
        (field "series_type" D.string)
        (field "original_size" D.int)
        (field "resolution" D.string)


decodeStravaAltitudeStream : D.Decoder StravaAltitudeStream
decodeStravaAltitudeStream =
    D.map5 StravaAltitudeStream
        (field "type" D.string)
        (field "data" (D.list D.float))
        (field "series_type" D.string)
        (field "original_size" D.int)
        (field "resolution" D.string)


decodeStravaTimeStream : D.Decoder StravaTimeStream
decodeStravaTimeStream =
    D.map5 StravaTimeStream
        (field "type" D.string)
        (field "data" (D.list D.int))
        (field "series_type" D.string)
        (field "original_size" D.int)
        (field "resolution" D.string)
