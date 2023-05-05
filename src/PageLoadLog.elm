module PageLoadLog exposing (post)

import GeoCodeDecoders exposing (IpInfo, ipInfoDecoder)
import Json.Encode as JE
import Postgrest
import Postgrest.Client as P



{-
      -- We don't read the page logs here, we use IpInfo for writing.
      type alias Login =
          { userId : String
          , familyName : String
          , givenName : String
          , loggedIn : String -- TODO: Timestamp
          }

   decodeLogin : Decoder Login
   decodeLogin =
       map4 Login
           (field "userId" string)
           (field "familyName" string)
           (field "givenName" string)
           (field "loggedIn" string)
-}


encodeLogin : IpInfo -> JE.Value
encodeLogin info =
    JE.object
        [ ( "country", JE.string info.country )
        , ( "country_code", JE.string "" )
        , ( "region", JE.string info.region )
        , ( "city", JE.string info.city )
        , ( "zip", JE.string info.zip )
        , ( "ip", JE.string info.ip )
        , ( "latitude", JE.float info.latitude )
        , ( "longitude", JE.float info.longitude )
        ]


endpoint : P.Endpoint IpInfo
endpoint =
    P.endpoint (Postgrest.base ++ "iplogs") ipInfoDecoder



{-
   getMany : P.Request (List Login)
   getMany =
       P.getMany endpoint
-}
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
    P.postOne endpoint << encodeLogin
