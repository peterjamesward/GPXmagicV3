module PageLoadLog exposing (Location, getRecentLocations, post)

import GeoCodeDecoders exposing (IpInfo, ipInfoDecoder)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Postgrest
import Postgrest.Client as P


type alias Location =
    { longitude : Float
    , latitude : Float
    }


locationDecoder : Decoder Location
locationDecoder =
    JD.map2 Location
        (JD.field "longitude" JD.float)
        (JD.field "latitude" JD.float)


encodeLogin : IpInfo -> JE.Value
encodeLogin info =
    JE.object
        [ ( "country", JE.string info.country )
        , ( "country_code", JE.string info.countryCode )
        , ( "region", JE.string info.region )
        , ( "city", JE.string info.city )
        , ( "zip", JE.string info.zip )
        , ( "ip", JE.string info.ip )
        , ( "latitude", JE.float info.latitude )
        , ( "longitude", JE.float info.longitude )
        ]


ipInfoEndpoint : P.Endpoint IpInfo
ipInfoEndpoint =
    P.endpoint (Postgrest.base ++ "iplogs") ipInfoDecoder


locationEndpoint : P.Endpoint Location
locationEndpoint =
    P.endpoint (Postgrest.base ++ "recent") locationDecoder


getRecentLocations : P.Request (List Location)
getRecentLocations =
    P.getMany locationEndpoint



{-
   asTable : List Login -> E.Element msg
   asTable logins =
       E.table tableStyles
           { data = logins
           , columns =
               [ { header = E.el headerStyles <| E.text "User ID"
                 , width = E.fill
                 , view = E.el [ E.height <| E.px 25 ] << E.text << .userId
                 }
               , { header = E.el headerStyles <| E.text "Logged in"
                 , width = E.fill
                 , view = E.text << .loggedIn
                 }
               ]
           }
-}


post : IpInfo -> P.Request IpInfo
post =
    P.postOne ipInfoEndpoint << encodeLogin
