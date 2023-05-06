module GeoCodeDecoders exposing (IpInfoReceived, ipInfoReceivedDecoder, lonLat)

import Json.Decode as D exposing (Decoder)



{- Example from http://ip-api.com/json/

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
   Structure from ipinfo.io is different (smaller):
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


type alias IpInfoReceived =
    { ip : String
    , city : String
    , region : String
    , countryCode : String
    , loc : String
    , zip : String
    }


lonLat : IpInfoReceived -> Maybe ( Float, Float )
lonLat info =
    case info.loc |> String.split "," |> List.map String.toFloat of
        [ Just lat, Just lon ] ->
            Just ( lon, lat )

        _ ->
            Nothing


ipInfoReceivedDecoder : Decoder IpInfoReceived
ipInfoReceivedDecoder =
    D.map6 IpInfoReceived
        (D.at [ "ip" ] D.string)
        (D.at [ "city" ] D.string)
        (D.at [ "region" ] D.string)
        (D.at [ "country" ] D.string)
        (D.at [ "loc" ] D.string)
        (D.at [ "postal" ] D.string)


type alias IpInfo =
    { ip : String
    , country : String
    , countryCode : String
    , region : String
    , city : String
    , zip : String
    , latitude : Float
    , longitude : Float
    }


ipInfoDecoder : Decoder IpInfo
ipInfoDecoder =
    D.map8 IpInfo
        (D.at [ "ip" ] D.string)
        (D.at [ "country" ] D.string)
        (D.at [ "countryCode" ] D.string)
        (D.at [ "region" ] D.string)
        (D.at [ "city" ] D.string)
        (D.at [ "zip" ] D.string)
        (D.at [ "lat" ] D.float)
        (D.at [ "lon" ] D.float)
