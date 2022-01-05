module ModelRecord exposing (..)

import DomainModel exposing (GPXSource, PeteTree)
import GeoCodeDecoders exposing (IpInfo)
import LocalCoords exposing (LocalCoords)
import OAuthTypes as O
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Time
import ViewContextThirdPerson
import ViewingMode exposing (ViewingMode)


type alias ModelRecord =
    { filename : Maybe String
    , time : Time.Posix
    , zone : Time.Zone
    , stravaAuthentication : O.Model
    , trackTree : Maybe PeteTree
    , referenceLonLat : GPXSource
    , renderDepth : Int
    , scene : List (Entity LocalCoords)
    , currentPosition : Int
    , viewMode : ViewingMode
    , viewDimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , ipInfo : Maybe IpInfo
    , mapClickDebounce : Bool
    , lastMapClick : ( Float, Float )
    , viewContext : Maybe ViewContextThirdPerson.ContextThirdPerson
    }