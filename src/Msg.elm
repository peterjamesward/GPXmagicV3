module Msg exposing (..)

import File exposing (File)
import GeoCodeDecoders exposing (IpInfo)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Encode as E
import OAuthTypes exposing (OAuthMsg)
import Time
import ViewingMode exposing (..)


type Msg
    = GpxRequested
    | GpxSelected File
    | GpxLoaded String
    | OAuthMessage OAuthMsg
    | AdjustTimeZone Time.Zone
    | SetRenderDepth Int
    | SetCurrentPosition Int
    | ImageClick Mouse.Event
    | SetViewMode ViewingMode
    | ReceivedIpDetails (Result Http.Error IpInfo)
    | IpInfoAcknowledged (Result Http.Error ())
    | PortMessage E.Value
    | RepaintMap
    | ClearMapClickDebounce
