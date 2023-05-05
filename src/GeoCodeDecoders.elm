module GeoCodeDecoders exposing (IpInfo, ipInfoDecoder)

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
        (D.at [ "query" ] D.string)
        (D.at [ "country" ] D.string)
        (D.at [ "countryCode" ] D.string)
        (D.at [ "region" ] D.string)
        (D.at [ "city" ] D.string)
        (D.at [ "zip" ] D.string)
        (D.at [ "lat" ] D.float)
        (D.at [ "lon" ] D.float)
