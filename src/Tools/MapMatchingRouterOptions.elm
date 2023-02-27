module Tools.MapMatchingRouterOptions exposing
    ( GeoJson
    , Location
    , Options
    , RouteState(..)
    , geoJsonDecoder
    )

import Json.Decode as D


type RouteState
    = RouteIdle
    | RouteDrawing
    | RouteComputing
    | RouteShown
    | RouteAdopted


type alias Options =
    { numPoints : Int
    , routeState : RouteState
    }



-- Types for receiving route as GeoJson


type alias GeoJson =
    { tracePoints : List Location }


type alias Location =
    { location : List Float }



-- Decoders for above


geoJsonDecoder : D.Decoder GeoJson
geoJsonDecoder =
    D.map GeoJson
        (D.field "tracepoints" (D.list locationDecoder))


locationDecoder : D.Decoder Location
locationDecoder =
    D.map Location
        (D.field "location" (D.list D.float))
