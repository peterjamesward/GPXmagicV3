module Msg exposing (..)

import File exposing (File)
import Html.Events.Extra.Mouse as Mouse
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
