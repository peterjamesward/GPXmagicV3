module Msg exposing (..)

import File exposing (File)
import GeoCodeDecoders exposing (IpInfo)
import Http
import Json.Encode as E
import OAuthTypes exposing (OAuthMsg)
import Time
import ViewThirdPerson
import ViewingMode exposing (..)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int
    | SetCurrentPosition Int
    | SetViewMode ViewingMode
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
    | PortMessage E.Value
    | RepaintMap
    | ClearMapClickDebounce
    | ImageMessage ViewThirdPerson.Msg