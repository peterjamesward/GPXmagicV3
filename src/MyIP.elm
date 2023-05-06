module MyIP exposing (processIpInfo, requestIpInformation)

import GeoCodeDecoders exposing (IpInfoReceived, ipInfoReceivedDecoder)
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
{-
   Structure from ipinfo.io is different:
   {
     "ip": "104.28.86.63",
     "city": "Milton Keynes",
     "region": "England",
     "country": "GB",
     "loc": "52.0045,-0.7823",
     "org": "AS13335 Cloudflare, Inc.",
     "postal": "MK4",
     "timezone": "Europe/London"
   }
-}


apiRoot =
    --"http://ip-api.com"
    "https://ipinfo.io"


requestIpInformation : (Result Http.Error IpInfoReceived -> msg) -> Cmd msg
requestIpInformation msg =
    Http.request
        { method = "GET"
        , headers = []
        , url =
            Builder.crossOrigin apiRoot
                []
                [ Builder.string "token" "46b026d8d686dd" ]
        , body = Http.emptyBody
        , expect = Http.expectJson msg ipInfoReceivedDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


processIpInfo : Result Http.Error IpInfoReceived -> Maybe IpInfoReceived
processIpInfo response =
    case response of
        Ok ipInfo ->
            Just ipInfo

        Err _ ->
            Nothing
