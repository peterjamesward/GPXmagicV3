module JwtStuff exposing (..)

import Json.Encode
import JsonWebToken


secret =
    "hereismyspecial32charactersecret"


stripPadding : String -> String
stripPadding =
    --TODO: Make a Base64 that does not pad!
    String.replace "=" ""


signedToken =
    stripPadding <|
        JsonWebToken.encode
            JsonWebToken.hmacSha256
            identity
            secret
        <|
            Json.Encode.object
                [ ( "role", Json.Encode.string "webuser" ) ]
