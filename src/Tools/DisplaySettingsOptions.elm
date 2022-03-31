module Tools.DisplaySettingsOptions exposing (..)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Json.Decode as D
import Json.Encode as E


type alias Options =
    { roadSurface : Bool
    , centreLine : Bool
    , groundPlane : Bool
    , curtainStyle : CurtainStyle
    , terrainFineness : Float -- 0.0 => OFF
    , landUse : Bool
    }


type alias StoredOptions =
    { roadSurface : Bool
    , groundPlane : Bool
    , centreLine : Bool
    , curtainStyle : String
    }


type CurtainStyle
    = NoCurtain
    | PlainCurtain
    | PastelCurtain


encode : Options -> E.Value
encode options =
    E.object
        [ ( "surface", E.bool options.roadSurface )
        , ( "ground", E.bool options.groundPlane )
        , ( "centre", E.bool options.centreLine )
        , ( "curtain", encodeCurtain options.curtainStyle )
        ]


decoder =
    D.map4 StoredOptions
        (D.field "surface" D.bool)
        (D.field "ground" D.bool)
        (D.field "centre" D.bool)
        (D.field "curtain" D.string)


decode : E.Value -> Options -> Options
decode json current =
    case D.decodeValue decoder json of
        Ok decoded ->
            { roadSurface = decoded.roadSurface
            , centreLine = decoded.centreLine
            , groundPlane = decoded.groundPlane
            , curtainStyle = decodeCurtain decoded.curtainStyle
            , terrainFineness = 0.0
            , landUse = False
            }

        Err error ->
            current


encodeCurtain : CurtainStyle -> E.Value
encodeCurtain style =
    E.string <|
        case style of
            NoCurtain ->
                "NoCurtain"

            PlainCurtain ->
                "PlainCurtain"

            PastelCurtain ->
                "PastelCurtain"


decodeCurtain : String -> CurtainStyle
decodeCurtain value =
    case value of
        "NoCurtain" ->
            NoCurtain

        "PlainCurtain" ->
            PlainCurtain

        _ ->
            PastelCurtain
