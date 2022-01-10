module ModelRecord exposing (..)

import DomainModel exposing (GPXSource, PeteTree)
import GeoCodeDecoders exposing (IpInfo)
import LocalCoords exposing (LocalCoords)
import OAuthTypes as O
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import SplitPane
import Time
import ViewContextThirdPerson
import ViewingMode exposing (ViewingMode)



--TODO: Try distinct records for Loaded and NotLoaded state, avoiding Maybes.


type Model
    = Model ModelRecord


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
    , windowSize : (Int, Int)
    , contentAreaSize : ( Quantity Int Pixels, Quantity Int Pixels )

    -- Splitters
    , leftDockRightEdge : SplitPane.State
    , leftDockInternal : SplitPane.State
    , rightDockLeftEdge : SplitPane.State
    , rightDockInternal : SplitPane.State
    , bottomDockTopEdge : SplitPane.State
    }
