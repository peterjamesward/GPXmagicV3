module MyIP exposing (processIpInfo, requestIpInformation)

import GeoCodeDecoders exposing (IpInfo, ipInfoDecoder)
import Http
import Url.Builder as Builder



{-
    http://ip-api.com/json/

   {
       "status": "success",
       "country": "United Kingdom",
       "countryCode": "GB",
       "region": "ENG",
       "regionName": "England",
       "city": "Stanmore",
       "zip": "HA7",
       "lat": 51.6167,
       "lon": -0.3167,
       "timezone": "Europe/London",
       "isp": "BT Public Internet Service",
       "org": "",
       "as": "AS2856 British Telecommunications PLC",
       "query": "109.147.206.253"
   }
-}


apiRoot =
    "http://ip-api.com"


requestIpInformation : (Result Http.Error IpInfo -> msg) -> Cmd msg
requestIpInformation msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = Builder.crossOrigin apiRoot [ "json" ] []
        , body = Http.emptyBody
        , expect = Http.expectJson msg ipInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


processIpInfo : Result Http.Error IpInfo -> Maybe IpInfo
processIpInfo response =
    case response of
        Ok ipInfo ->
            Just ipInfo

        Err _ ->
            Nothing
