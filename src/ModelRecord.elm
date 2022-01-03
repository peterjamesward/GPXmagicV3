module ModelRecord exposing (..)


import DomainModel exposing (PeteTree)
import GeoCodeDecoders exposing (IpInfo)
import LocalCoords exposing (LocalCoords)
import OAuthTypes as O
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Time
import ViewingContext
import ViewingMode exposing (ViewingMode)
type alias ModelRecord =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , stravaAuthentication : O.Model
    , trackTree : Maybe PeteTree
    , renderDepth : Int
    , scene : List (Entity LocalCoords)
    , currentPosition : Int
    , viewMode : ViewingMode
    , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , ipInfo : Maybe IpInfo
    , mapClickDebounce : Bool
    , lastMapClick : ( Float, Float )
    , viewContext : ViewingContext.ViewingContext
    }

