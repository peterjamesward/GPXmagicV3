module Tools.DisplaySettingsOptions exposing (CurtainStyle(..), Options, decode, encode)

-- Putting these in a separate module means we can use with Action, without an import loop.

import Json.Decode as D
import Json.Encode as E
import LandUseDataTypes


type alias Options =
    { roadSurface : Bool
    , centreLine : Bool
    , groundPlane : Bool
    , curtainStyle : CurtainStyle
    , terrainFineness : Float -- 0.0 => OFF
    , landUse : LandUseDataTypes.LandUseDisplay
    , placeNames : Bool
    , showConstraintsAtLevel : Maybe Int
    , mapProjection : String
    , mapAllowTilt : Bool
    , mapAllowRotate : Bool
    }


type alias StoredOptions =
    { roadSurface : Bool
    , groundPlane : Bool
    , centreLine : Bool
    , curtainStyle : String
    , mapProjection : Maybe String
    , mapAllowTilt : Maybe Bool
    , mapAllowRotate : Maybe Bool
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
        , ( "tilt", E.bool options.mapAllowTilt )
        , ( "projection", E.string options.mapProjection )
        , ( "rotate", E.bool options.mapAllowRotate )
        ]


decoder =
    D.map7 StoredOptions
        (D.field "surface" D.bool)
        (D.field "ground" D.bool)
        (D.field "centre" D.bool)
        (D.field "curtain" D.string)
        (D.maybe (D.field "projection" D.string))
        (D.maybe (D.field "tilt" D.bool))
        (D.maybe (D.field "rotate" D.bool))


decode : E.Value -> Options -> Options
decode json current =
    case D.decodeValue decoder json of
        Ok decoded ->
            { roadSurface = decoded.roadSurface
            , centreLine = decoded.centreLine
            , groundPlane = decoded.groundPlane
            , curtainStyle = decodeCurtain decoded.curtainStyle
            , terrainFineness = 0.0
            , landUse = LandUseDataTypes.LandUseHidden
            , placeNames = False
            , showConstraintsAtLevel = Nothing
            , mapAllowTilt = decoded.mapAllowTilt |> Maybe.withDefault True
            , mapProjection = decoded.mapProjection |> Maybe.withDefault "globe"
            , mapAllowRotate = decoded.mapAllowRotate |> Maybe.withDefault True
            }

        Err _ ->
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
